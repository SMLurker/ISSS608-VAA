

#### Importing R pacakges

pacman::p_load(jsonlite,tidygraph,ggraph,visNetwork,graphlayouts,ggforce,skimr, stringr,wordcloud,wordcloud2,
               tidytext,tidyverse,igraph,ggstatsplot,ggiraph,cluster,plotly,DT,bslib)
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

###########################
# load knowledge graph file
MC3 <- fromJSON("data/MC3.json")

#---------------------- Data Wrangling ----------------------#

# MC_nodes
MC3_nodes <- as_tibble(MC3$nodes) %>%
  mutate(country = as.character(country),
         id = as.character(id),
         product_services = as.character(product_services),
         revenue_omu = as.numeric(as.character(revenue_omu)),
         type = as.character(type)) %>%
  select(id, country, type, revenue_omu, product_services)

# MC_edges
MC3_edges <- as_tibble(MC3$links) %>%
  distinct() %>%
  mutate(source = as.character(source),
         target = as.character(target),
         type = as.character(type)) %>%
  group_by(source, target, type) %>%
  summarise(weights = n()) %>%
  filter(source != target) %>%
  ungroup()

#----------------------- Word Cloud -------------------------#

# Tokenization for product of services
token_nodes <- MC3_nodes %>%
  unnest_tokens(word, product_services)

stopwords_removed <- token_nodes %>%
  anti_join(stop_words)

# Count the frequency of each word
df_wordcloud <- stopwords_removed %>%
  group_by(word) %>%
  filter(!word %in% c("character", "0", "unknown")) %>%
  summarise(freq = n()) %>%
  arrange(desc(freq))

#--------------------- Network Graph ------------------------#

# Define selected_keywords within the server function
selected_keywords <- c("fish", "seafood", "frozen", "food", "fresh", 
                       "salmon", "tuna", "products", "canned", "carp",
                       "catfish", "cod", "mackerel", "pollock", "shark", 
                       "herring", "lichen")
#------------------------------------------------------------#

# Define UI

ui <- fluidPage(
  theme = bs_theme(bg = "lightblue", 
                   fg = "black",   
                   primary = "#FCC780", 
                   base_font = font_google("Roboto"),  
                   code_font = font_google("Roboto")),
  navbarPage(
    "Vast Challenge 2023 Mini-Challenge 3",
    navbarMenu("Visualise data and keywords",
               tabPanel("Visualise data and keywords",
                        sidebarLayout(
                          sidebarPanel(
                            sliderInput("freqInput", "Minimum Frequency", min = 3, max = 100, value = 10)
                          ),
                          mainPanel(
                            wordcloud2Output("wordcloud"),
                            tags$style(".table-margin { margin-top: 50px; }"),
                            DT::dataTableOutput(outputId = "Table1"),
                            tags$div(DT::dataTableOutput(outputId = "Table2"), class = "table-margin")
                          )
                        )
               )
    ),
    navbarMenu("Network Graph",
               tabPanel("Network Graph",
                        sidebarLayout(
                          sidebarPanel(
                            selectInput(inputId = "layoutInput", "Graph Layout",
                                        choices = c("Kamada and Kawai" = "kk",
                                                    "Fruchterman Reingold" = "fr",
                                                    "Randomly" = "randomly",
                                                    "Sphere" = "sphere"),
                                        selected = "fr"),
                            
                            selectizeInput(inputId = "keywordInput", "Select Keywords",
                                           choices = selected_keywords, multiple = TRUE,
                                           selected = "fish"),
                            
                            sliderInput("degreesInput", "Degrees", 
                                        min = 3, max = 10, 
                                        value = c(3, 10), step = 1),
                            
                            numericInput(
                              inputId = "betweennessfilter",
                              label = "Filter nodes based on Betweenness Centrality\nMin: 50000, Max: 1000000",
                              value = 50000,
                              min = 50000,
                              max = 1000000
                            ),
                            
                            selectInput(inputId = "clusteringAlgorithm",
                                        label = "Select Clustering Algorithm",
                                        choices = c(
                                          "Louvain" = "louvain",
                                          "Walktrap" = "walktrap",
                                          "Edge Betweenness" = "edge_betweenness",
                                          "Infomap" = "infomap"
                                        ),
                                        selected = "louvain"
                            ),
                            
                          ),
                          mainPanel(
                            ggiraphOutput("networkgraph", height = "800px")
                          ),
                        ),
                        sidebarLayout(
                          sidebarPanel(numericInput(inputId = "communityInput",
                                                    label = "Community No.,from 1 to 23",
                                                    value = 1,
                                                    min = 23,
                                                    max = 1)
                          ),
                          mainPanel(
                            dataTableOutput("communityTable"),
                            plotlyOutput("boxplot")
                            
                          )
                        )
               )
    ),
    #########################################################
    tabPanel(
      "Anormal Business Groups",
      sidebarLayout(
        sidebarPanel(
          checkboxInput(inputId = "show1Data",
                        label = "Show Group 1 Nodetable",
                        value = FALSE),
          checkboxInput(inputId = "show2Data",
                        label = "Show Group 2 Nodetable",
                        value = FALSE),
          checkboxInput(inputId = "show3Data",
                        label = "Show Group 3 Nodetable",
                        value = FALSE),
          checkboxInput(inputId = "show4Data",
                        label = "Show Group 4 Nodetable",
                        value = FALSE),
          selectInput(inputId = "showgrp",
                      label = "Show Group:",
                      choices = c("Group all" = 0,
                                  "Group 1" = 1,
                                  "Group 2" = 2,
                                  "Group 3" = 3,
                                  "Group 4" = 4),
                      selected = "all")
        ),
        
        mainPanel(
          plotOutput("Graph"), 
          DT::dataTableOutput(outputId = "aTable"),
          DT::dataTableOutput(outputId = "bTable"),
          DT::dataTableOutput(outputId = "cTable"),
          DT::dataTableOutput(outputId = "dTable")
        )
      )
    ),
    ####################################################
    navbarMenu(
      "Network chart",
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
# Define server logic
server <- function(input, output) {
  
  # Word cloud output
  output$wordcloud <- renderWordcloud2({
    wordcloud2(df_wordcloud, size = 1.5, minSize = input$freqInput)  # Adjust the size as desired
  })
  
  output$Table1 <- DT::renderDataTable({
    DT::datatable(data = MC3_nodes,
                  options= list(pageLength = 5),
                  rownames = FALSE, caption = "MC3 Nodes")
  })
  
  output$Table2 <- DT::renderDataTable({
    DT::datatable(data = MC3_edges,
                  options= list(pageLength = 5),
                  rownames = FALSE, caption = "MC3 Edges")
  })
  
  # Network graph output
  output$networkgraph <- renderggiraph({
    set.seed(1234)
    
    # Filter the data frame
    df_extracted <- stopwords_removed %>%
      filter(grepl(paste(input$keywordInput, collapse = "|"), word))
    
    # Remove duplicate IDs
    nodes_extracted_distinct <- df_extracted %>%
      distinct(id, country, type, revenue_omu) %>%
      drop_na(revenue_omu)
    
    # Network visualization for fish and seafood related companies 
    id3 <- MC3_edges %>%
      select(source) %>%
      rename(id = source)
    
    id4 <- MC3_edges %>%
      select(id = target)
    
    MC3_nodes_extracted <- rbind(id3, id4) %>%
      distinct() %>%
      left_join(nodes_extracted_distinct, by = "id", unmatched = "drop")
    
    # Network graph layout
    setlayout <- input$layoutInput
    
    MC3_graph_extracted <- tbl_graph(nodes = MC3_nodes_extracted,
                                     edges = MC3_edges,
                                     directed = FALSE) %>%
      mutate(betweenness_centrality = centrality_betweenness()) %>%
      filter(betweenness_centrality >= input$betweennessfilter)
    
    # Node label
    degrees <- degree(MC3_graph_extracted)
    degree_setpoint <- ifelse(degrees >= input$degreesInput[1] & degrees <= input$degreesInput[2], as.character(MC3_nodes_extracted$id), "")
    
    # Cluster algorithm
    clusteringAlgorithm <- input$clusteringAlgorithm
    
    if (clusteringAlgorithm == "louvain") {
      cluster_setup <- cluster_louvain(MC3_graph_extracted, weights = NA)
    } else if (clusteringAlgorithm == "walktrap") {
      cluster_setup <- cluster_walktrap(MC3_graph_extracted)
    } else if (clusteringAlgorithm == "edge_betweenness") {
      cluster_setup <- cluster_edge_betweenness(MC3_graph_extracted)
    } else if (clusteringAlgorithm == "infomap") {
      cluster_setup <- cluster_infomap(MC3_graph_extracted)
    }
    
    # Assign communities based on clustering
    community <- as.factor(cluster_setup$membership)
    
    # Create a data frame for node attributes
    nodes_data <- data.frame(
      id = V(MC3_graph_extracted)$id,
      community = as.factor(community)
    )
    
    # Plot the network graph using ggraph
    g <- ggraph(MC3_graph_extracted, layout = setlayout) +
      geom_edge_link(aes(alpha = 0.1)) +
      geom_point_interactive(
        aes(
          x = x,
          y = y,
          size = betweenness_centrality,
          color = community,
          alpha = 0.2,
          tooltip = paste0(
            "Name:  ", id,
            ifelse(is.na(country), "", paste0("<br>Country:  ", country)),  # Use <br> for line breaks
            ifelse(is.na(type), "", paste0("<br>Type:  ", type)),
            ifelse(is.na(community), "", paste0("<br>Community:  ", community))
          )
        ),
        show.legend = TRUE
      ) +
      geom_node_text(aes(label = ifelse(degrees >= input$degreesInput[1] & degrees <= input$degreesInput[2], as.character(id), "")), size = 3) +
      scale_size_continuous(range = c(1, 10)) +
      labs(title = "Network Visualization with Betweenness centrality above 10000 degree more than 3")
    
    # Convert the ggraph plot to a ggiraph object
    girafe(code = print(g), options = list(opts_hover(css = "fill:;")))
    
    
  })
  #-------------------------Data Table-----------------------------#
  
  
  output$communityTable <- renderDataTable({
    set.seed(1234)
    # Filter the data frame
    df_extracted <- stopwords_removed %>%
      filter(grepl(paste(input$keywordInput, collapse = "|"), word))
    
    # Remove duplicate IDs
    nodes_extracted_distinct <- df_extracted %>%
      distinct(id, country, type, revenue_omu) %>%
      drop_na(revenue_omu)
    
    # Network visualization for fish and seafood related companies 
    id3 <- MC3_edges %>%
      select(source) %>%
      rename(id = source)
    
    id4 <- MC3_edges %>%
      select(id = target)
    
    MC3_nodes_extracted <- rbind(id3, id4) %>%
      distinct() %>%
      left_join(nodes_extracted_distinct, by = "id", unmatched = "drop")
    
    # Network graph layout
    setlayout <- input$layoutInput
    
    MC3_graph_extracted <- tbl_graph(nodes = MC3_nodes_extracted,
                                     edges = MC3_edges,
                                     directed = FALSE) %>%
      mutate(betweenness_centrality = centrality_betweenness()) %>%
      filter(betweenness_centrality >= input$betweennessfilter)
    
    # Node label
    degrees <- degree(MC3_graph_extracted)
    degree_setpoint <- ifelse(degrees >= input$degreesInput[1] & degrees <= input$degreesInput[2], as.character(MC3_nodes_extracted$id), "")
    
    # Cluster algorithm
    clusteringAlgorithm <- input$clusteringAlgorithm
    
    if (clusteringAlgorithm == "louvain") {
      cluster_setup <- cluster_louvain(MC3_graph_extracted, weights = NA)
    } else if (clusteringAlgorithm == "walktrap") {
      cluster_setup <- cluster_walktrap(MC3_graph_extracted)
    } else if (clusteringAlgorithm == "edge_betweenness") {
      cluster_setup <- cluster_edge_betweenness(MC3_graph_extracted)
    } else if (clusteringAlgorithm == "infomap") {
      cluster_setup <- cluster_infomap(MC3_graph_extracted)
    }
    
    # Assign communities based on clustering
    community <- as.factor(cluster_setup$membership)
    
    community_sizes <- table(community)
    
    list_community <- names(community_sizes)[1:23]
    
    nodes_cluster <- data.frame(id = V(MC3_graph_extracted)$id)
    nodes_cluster$product_services <- MC3_nodes$product_services[match(nodes_cluster$id, MC3_nodes$id)]
    nodes_cluster$country <- MC3_nodes$country[match(nodes_cluster$id, MC3_nodes$id)]
    nodes_cluster$type <- MC3_nodes$type[match(nodes_cluster$id, MC3_nodes$id)]
    nodes_cluster$revenue_omu <- MC3_nodes$revenue_omu[match(nodes_cluster$id, MC3_nodes$id)]
    
    # Get the selected community
    selected_community <- list_community[input$communityInput]
    
    # Filter nodes based on the selected community
    nodes <- V(MC3_graph_extracted)$id[community == selected_community]
    community_services <- nodes_cluster[nodes_cluster$id %in% nodes, c("id", "country", "type", "revenue_omu", "product_services")]
    
    datatable(community_services)
  })
  
  #-----------------------------------------Boxplot------------------------------#
  # Boxplot output
  output$boxplot <- renderPlotly({
    set.seed(1234)
    # Filter the data frame
    df_extracted <- stopwords_removed %>%
      filter(grepl(paste(input$keywordInput, collapse = "|"), word))
    
    # Remove duplicate IDs
    nodes_extracted_distinct <- df_extracted %>%
      distinct(id, country, type, revenue_omu) %>%
      drop_na(revenue_omu)
    
    # Network visualization for fish and seafood related companies 
    id3 <- MC3_edges %>%
      select(source) %>%
      rename(id = source)
    
    id4 <- MC3_edges %>%
      select(id = target)
    
    MC3_nodes_extracted <- rbind(id3, id4) %>%
      distinct() %>%
      left_join(nodes_extracted_distinct, by = "id", unmatched = "drop")
    
    # Network graph layout
    setlayout <- input$layoutInput
    
    MC3_graph_extracted <- tbl_graph(nodes = MC3_nodes_extracted,
                                     edges = MC3_edges,
                                     directed = FALSE) %>%
      mutate(betweenness_centrality = centrality_betweenness()) %>%
      filter(betweenness_centrality >= input$betweennessfilter)
    
    # Node label
    degrees <- degree(MC3_graph_extracted)
    degree_setpoint <- ifelse(degrees >= input$degreesInput[1] & degrees <= input$degreesInput[2], as.character(MC3_nodes_extracted$id), "")
    
    # Cluster algorithm
    clusteringAlgorithm <- input$clusteringAlgorithm
    
    if (clusteringAlgorithm == "louvain") {
      cluster_setup <- cluster_louvain(MC3_graph_extracted, weights = NA)
    } else if (clusteringAlgorithm == "walktrap") {
      cluster_setup <- cluster_walktrap(MC3_graph_extracted)
    } else if (clusteringAlgorithm == "edge_betweenness") {
      cluster_setup <- cluster_edge_betweenness(MC3_graph_extracted)
    } else if (clusteringAlgorithm == "infomap") {
      cluster_setup <- cluster_infomap(MC3_graph_extracted)
    }
    
    # Assign communities based on clustering
    community <- as.factor(cluster_setup$membership)
    
    community_sizes <- table(community)
    
    list_community <- names(community_sizes)[1:23]
    
    nodes_cluster <- data.frame(id = V(MC3_graph_extracted)$id)
    nodes_cluster$product_services <- MC3_nodes$product_services[match(nodes_cluster$id, MC3_nodes$id)]
    nodes_cluster$country <- MC3_nodes$country[match(nodes_cluster$id, MC3_nodes$id)]
    nodes_cluster$type <- MC3_nodes$type[match(nodes_cluster$id, MC3_nodes$id)]
    nodes_cluster$revenue_omu <- MC3_nodes$revenue_omu[match(nodes_cluster$id, MC3_nodes$id)]
    
    # Get the selected community
    selected_community <- list_community[input$communityInput]
    
    # Filter nodes based on the selected community
    nodes <- V(MC3_graph_extracted)$id[community == selected_community]
    community_services <- nodes_cluster[nodes_cluster$id %in% nodes, c("id", "country", "type", "revenue_omu", "product_services")]
    
    # Create a boxplot of revenue_omu
    p <- plot_ly(data = community_services, type = "box", y = ~revenue_omu) %>%
      layout(title = "Boxplot of revenue_omu", yaxis = list(title = "Revenue (omu)"))
    
    # Identify and label the outliers
    outliers <- boxplot.stats(community_services$revenue_omu)$out
    outliers_label <- community_services$id[community_services$revenue_omu %in% outliers]
    
    # Calculate the mean
    mean_value <- mean(community_services$revenue_omu)
    
    # Add annotations for the outliers with tooltips displaying the mean
    annotations <- lapply(seq_along(outliers), function(i) {
      list(
        x = 1,
        y = outliers[i],
        text = paste("ID:", outliers_label[i], "<br>Mean:", mean_value),
        showarrow = FALSE,
        hoverinfo = "text"
      )
    })
    
    # Add the annotations to the plot
    p <- p %>% layout(annotations = annotations)
    
    p
  })
  #############################################################################
  output$Graph <- renderPlot({
    
    mc3_nodes <- read_rds("data/mc3_nodes.rds")
    mc3_edges <- read_rds("data/mc3_edges.rds")
    grp <- input$showgrp
    if (grp==0){
      df_nodes_2 <- read_rds("data/df_nodes_2.rds")
    }else{
      df_nodes_2 <- read_rds("data/df_nodes_2.rds") %>%
        filter(group %in% grp)
    }
    
    
    df_edges_2 <- mc3_edges %>%
      filter(source %in% df_nodes_2$id) 
    
    id5 <- df_edges_2 %>%
      select(source) %>%
      rename(id = source)
    id6 <- df_edges_2 %>%
      select(target) %>%
      rename(id = target)
    df_nodes_3 <- rbind(id5, id6) %>%
      distinct() %>%
      left_join(mc3_nodes,
                unmatched = "drop") %>%
      left_join(df_nodes_2)
    df_graph_3 <- tbl_graph(nodes = df_nodes_3,
                            edges = df_edges_2,
                            directed = FALSE) %>%
      mutate(betweenness_centrality = centrality_betweenness(),
             closeness_centrality = centrality_closeness())
    g_3 <- df_graph_3 %>%
      mutate(betweenness_centrality = centrality_betweenness()) %>%
      ggraph(layout = "kk") + 
      geom_edge_link(aes(width=weights), 
                     alpha=0.2) +
      scale_edge_width(range = c(0.01, 0.1)) +
      geom_node_text(aes(label = ifelse(group > 1, as.character(id), "")), size = 2)+
      geom_node_point(aes(colour = group,
                          size=betweenness_centrality))
    g_3 + theme_graph()
  })
  
  
  output$aTable <- DT::renderDataTable({
    df_nodes_3 <- read_rds("data/df_nodes_3.rds") %>%
      filter(group == 1)
    if(input$show1Data){
      DT::datatable(data = df_nodes_3 ,
                    options= list(pageLength = 5),
                    rownames = FALSE)
    }
  }) 
  output$bTable <- DT::renderDataTable({
    df_nodes_3 <- read_rds("data/df_nodes_3.rds") %>%
      filter(group == 2)
    if(input$show2Data){
      DT::datatable(data = df_nodes_3 ,
                    options= list(pageLength = 5),
                    rownames = FALSE)
    }
  })
  output$cTable <- DT::renderDataTable({
    df_nodes_ <- read_rds("data/df_nodes_3.rds") %>%
      filter(group == 3)
    if(input$show3Data){
      DT::datatable(data = df_nodes_3 ,
                    options= list(pageLength = 5),
                    rownames = FALSE)
    }
  })
  output$dTable <- DT::renderDataTable({
    df_nodes_3 <- read_rds("data/df_nodes_3.rds") %>%
      filter(group == 4)
    if(input$show4Data){
      DT::datatable(data = df_nodes_3 ,
                    options= list(pageLength = 5),
                    rownames = FALSE)
    }
  })
  #########################################################
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

# Run the app
shinyApp(ui = ui, server = server)
