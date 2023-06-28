library(jsonlite)
library(tidygraph)
library(ggraph)
library(igraph)
library(shiny)
library(DT)
library(stringr) 
library(dplyr)  

# Load the JSON data into a variable
mc3_data <- fromJSON("data/MC3.json")

# Extract nodes and edges from the loaded data
mc3_nodes <- mc3_data$nodes
mc3_edges <- mc3_data$edges

mc3_edges <- as_tibble(mc3_data$links) %>% 
  distinct() %>%
  mutate(source = as.character(source),
         target = as.character(target),
         type = as.character(type)) %>%
  group_by(source, target, type) %>%
  summarise(weights = n()) %>%
  filter(source!=target) %>%
  ungroup()
mc3_nodes <- as_tibble(mc3_data$nodes) %>%
  mutate(country = as.character(country),
         id = as.character(id),
         product_services = as.character(product_services),
         revenue_omu = as.numeric(as.character(revenue_omu)),
         type = as.character(type)) %>%
  select(id, country, type, revenue_omu, product_services)

mc3_nodes <- mc3_nodes %>% 
  mutate(n_seafood = str_count(product_services, "seafood")) %>%
  mutate(n_frozen = str_count(product_services, "frozen")) %>%
  mutate(n_food = str_count(product_services, "food")) %>%
  mutate(n_fresh = str_count(product_services, "fresh")) %>%
  mutate(n_salmon = str_count(product_services, "salmon")) %>%
  mutate(n_canned = str_count(product_services, "canned")) %>%
  mutate(n_fish = str_count(product_services, "fish")) 

mc3_nodes_filtered <- mc3_nodes %>% 
  filter(n_fish > 0 | n_seafood > 0 | n_frozen > 0 | n_food > 0 | n_fresh > 0 | n_salmon > 0 | n_canned > 0)

mc3_edges_filtered <- mc3_edges %>%
  filter(source %in% mc3_nodes_filtered$id | target %in% mc3_nodes_filtered$id)

id1 <- mc3_edges_filtered %>%
  select(source) %>%
  rename(id = source)
id2 <- mc3_edges_filtered %>%
  select(target) %>%
  rename(id = target)
mc3_nodes1 <- rbind(id1, id2) %>%
  distinct() %>%
  left_join(mc3_nodes_filtered%>%
              select(-starts_with("n_")),
            unmatched = "drop")

mc3_graph <- tbl_graph(nodes = mc3_nodes1,
                       edges = mc3_edges_filtered,
                       directed = FALSE)%>%
  mutate(betweenness_centrality = centrality_betweenness(),
         closeness_centrality = centrality_closeness())






ui <- fluidPage(
  titlePanel("Network chart"),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel(
        "Filtered network chart",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "variable",
              label = "Select layout",
              choices = c("Fruchterman-Reingold" = "fr",
                          "Kmada and Kawai" = "kk"),
              selected = "fr"
            )
          ),
          mainPanel(
            plotOutput("distPlot1")
          )
        )
      ),
      tabPanel(
        "Filtered community detection chart",
        sidebarLayout(
          sidebarPanel(
            selectInput(
              inputId = "clusteringAlgorithm",
              label = "Select Clustering Algorithm",
              choices = c(
                "Louvain" = "louvain",
                "Walktrap" = "walktrap",
                "Edge Betweenness" = "edge_betweenness",
                "Infomap" = "infomap"
              ),
              selected = "louvain"
            ),
            numericInput(
              inputId = "betweennessThreshold",
              label = "Labeling nodes with more than specified betweenness centrality",
              value = 700,
              max = 1128
            ), 
            
            selectInput(
              inputId = "layoutAlgorithm",
              label = "Select Layout Algorithm",
              choices = c(
                "Sphere" = "sphere",
                "Fruchterman-Reingold" = "fr",
                "Kmada and Kawai" = "kk"
              ),
              selected = "sphere"
            ),
            sliderInput(
              inputId = "nodeSize",
              label = "Node Size",
              min = 0.1,
              max = 1,
              value = 0.5,
              step = 0.1
            )
          ),
          mainPanel(
            plotOutput("distPlot2", width = "190%", height = "790px")
          )
        )
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot1 <- renderPlot({
    mc3_graph %>%
      ggraph(layout = input$variable) +
      geom_edge_link() +
      geom_node_point() +
      scale_size_continuous(range = c(1, 10)) +
      theme_graph()
  })
  
  output$distPlot2 <- renderPlot({
    clusteringAlgorithm <- input$clusteringAlgorithm
    
    if (clusteringAlgorithm == "louvain") {
      louvain_partition <- cluster_louvain(mc3_graph, weights = NA)
    } else if (clusteringAlgorithm == "walktrap") {
      louvain_partition <- cluster_walktrap(mc3_graph)
    } else if (clusteringAlgorithm == "edge_betweenness") {
      louvain_partition <- cluster_edge_betweenness(mc3_graph)
    } else if (clusteringAlgorithm == "infomap") {
      louvain_partition <- cluster_infomap(mc3_graph)
    }
    
    # Assign communities to graph
    mc3_graph$community <- louvain_partition$membership
    
    # Give nodes properties, including scaling them by degree and coloring them by community
    V(mc3_graph)$size <- degree(mc3_graph) * input$nodeSize
    V(mc3_graph)$frame.color <- "white"
    V(mc3_graph)$color <- mc3_graph$community
    V(mc3_graph)$label <- V(mc3_graph)$id
    V(mc3_graph)$label.cex <- 1.5
    
    # Color edges according to their starting node
    edge.start <- ends(mc3_graph, es = E(mc3_graph), names = FALSE)[, 1]
    E(mc3_graph)$color <- V(mc3_graph)$color[edge.start]
    E(mc3_graph)$arrow.mode <- 0
    
    # Label nodes based on specific condition or criteria
    # Modify this part based on your requirements
    v_labels <- which(V(mc3_graph)$betweenness_centrality > input$betweennessThreshold) # Only those with more than 700 betweenness are labelled
    
    for (i in 1:length(V(mc3_graph))) {
      if (!(i %in% v_labels)) {
        V(mc3_graph)$label[i] <- ""
      }
    }
    
    layout <- switch(input$layoutAlgorithm,
                     "sphere" = layout_on_sphere,
                     "fr" = layout_with_fr,
                     "kk" = layout_with_kk)
    
    l1 <- layout(mc3_graph)
    plot(mc3_graph, rescale = T, layout = l1, main = "MC3 community")
  })
}

# Run the application
shinyApp(ui = ui, server = server)