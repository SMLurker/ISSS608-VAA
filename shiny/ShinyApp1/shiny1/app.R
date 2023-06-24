

#### Importing R pacakges

pacman::p_load(jsonlite,tidygraph,ggraph,visNetwork,graphlayouts,ggforce,skimr, 
               tidytext,tidyverse,igraph,ggstatsplot,ggiraph,cluster,plotly,DT)

# Define UI
ui <- fluidPage(
  navbarPage(
    "Graph Visualization",
    tabPanel(
      "Tab 1",
      tabsetPanel(
        tabPanel("Sub Tab 1.1"),
        tabPanel("Sub Tab 1.2")
      )
    ),
    
    tabPanel(
      "Anormal Business Groups",
      sidebarLayout(
        sidebarPanel(
          checkboxInput(inputId = "shownodesData",
                        label = "Show nodes datatable",
                        value = FALSE),
          checkboxInput(inputId = "showedgesData",
                        label = "Show edges datatable",
                        value = FALSE)
        ),
        mainPanel(
          plotOutput("Graph"), 
          DT::dataTableOutput(outputId = "aTable"),
          DT::dataTableOutput(outputId = "bTable")
        )
      )
    ),
    
    tabPanel(
      "Tab 2",
      fluidRow(
        column(width = 6, plotOutput("plot2"))
      )
    )
  )
)
  
  # Define server logic
  server <- function(input, output) {
    #########################################################################
    #########################################################################
    output$Graph <- renderPlot({
      
      df_graph_3 <- read_rds("data/df_graph_3.rds")
      g_3 <- df_graph_3 %>%
        mutate(betweenness_centrality = centrality_betweenness()) %>%
        ggraph(layout = "kk") + 
        geom_edge_link(aes(width = weights), alpha = 0.2) +
        scale_edge_width(range = c(0.01, 0.1)) +
        geom_node_point(aes(colour = group, size = betweenness_centrality)) +
        theme_graph()
      
      plot(g_3)
    })
    output$aTable <- DT::renderDataTable({
      df_nodes_3 <- read_rds("data/df_nodes_3.rds")
      if(input$shownodesData){
        DT::datatable(df_nodes_3)
      }
    }) 
    output$bTable <- DT::renderDataTable({
      df_edges_2 <- read_rds("data/df_edges_2.rds")
      if(input$showedgesData){
        DT::datatable(df_edges_2)
      }
    }) 
  }

# Run the app
shinyApp(ui = ui, server = server)
