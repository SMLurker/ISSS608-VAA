pacman::p_load(jsonlite, tidygraph, ggraph, 
               visNetwork, graphlayouts, ggforce, 
               skimr, tidytext, tidyverse, igraph, wordcloud, cluster, 
               DT, plotly, wordcloud2, ggiraph,dplyr)

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

ui <- navbarPage(
  

  title = "ShinyNet: Exploring, Visualising and Analysing Network Data",
  theme = "flatly",
  id = "navbarID",
  tabPanel("Introduction"),
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
  
  
)


# server code

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
 
}



shinyApp(ui = ui, server = server)