# shiny app to visualize results from clustering project

# import requisite libraries, functions, and data
source("libraries.R")
source("functions.R")
source("import_data.R")

ui <- fluidPage(
  titlePanel("Clusters of Economic Freedom"),
  sidebarLayout(
    sidebarPanel(
      sliderInput("k",
                  "Number of clusters:",
                  min = 2,
                  max = 12,
                  value = 4),
      radioButtons("method", 
                   "k-means or hierarchical?", 
                   c("k-means","hierarchical"), 
                   "hierarchical"),
      selectInput("year",
                  "Year:",
                  choices = years,
                  selected = 2017),
      numericInput("seed",
                   "Random number seed (for updated k-means clustering):",
                   12345)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("cluster summaries",tableOutput("cluster_summary")),
        tabPanel("normalized data",tableOutput("cluster_table")),
        # tabPanel("sub index correlations\n(may run slow!)", plotOutput("cluster_pairs")),
        tabPanel("dendrogram",plotOutput("dendrogram")),
        tabPanel("map",plotOutput("cluster_map"))
      )
    )
  )
)

# Define server logic 
server <- function(input, output) {

  # clustered/normalized data
clustered_data <- reactive({
      cluster_wrapper(input$method,input$year,input$k,efw_scaled,input$seed) %>% 
        rename(country = country.efw)
  })

# # pair plots
# # You can uncomment this block and the tabPanel above, but it will take a long 
# # time to run each figure.
# output$cluster_pairs <- renderCachedPlot({
#   clustered_data() %>% pair_plots()},cacheKeyExpr = c(input$k,input$method,input$year))

  
  # data with cluster membership  
  output$cluster_table <- renderTable({
    clustered_data() %>% 
      dplyr::rename(cluster = cl,
                    `EFW quartile` = quartile)
  },striped = TRUE)
  
  
# summary stats
  output$cluster_summary <- renderTable({
    # if(input$husk == "original"){
    #   cluster_names <- clustered_data() %>%
    #     dplyr::select(cl,husk_cluster) %>% 
    #     unique() %>% 
    #     dplyr::rename(cluster = cl,
    #                   'cluster name' = husk_cluster) 
    #   out <- clustered_data() %>% 
    #     summarize_clustered_data() 
    #   out <- full_join(cluster_names,out) %>% 
    #     arrange(cluster)
    # } else {
      out <- clustered_data() %>%
        summarize_clustered_data()
    # }
    out <- out %>% mutate_if(is.double,funs(round(.,digits=2)))
    new_out <- t(out)
    vars <- colnames(out)
    new_out <- cbind(vars,new_out)
    colnames(new_out) <- c("variable", paste("cluster",out$cluster,sep=" "))
    new_out[2:nrow(new_out),]
  },striped = TRUE)
  
  # Create a map
  output$cluster_map <- renderPlot({
    # # Uncomment the block of code below to use pre-compiled data if Shiny is giving you a 
    # # hard time joining the map coordinate data to the clustered data.
    # # For this to work, you'll need to run `precompile_viz_data.R`
    # path <- paste("maps/mapdf_",
    #                     ifelse(input$method == "hierarchical","hclust","kmeans"),
    #                     "_",input$year,"_",input$k,".csv",sep="")
    # mapdf <- read_csv(path) %>% arrange(order)
    
    plot_title <- paste("The world in ",input$k," clusters (",input$year,", ",input$method,")",sep="")
    mapdf <- clustered_data() %>% full_join(map_coords)
    ggplot(mapdf, aes(long, lat, group = group)) +
      geom_polygon(aes(fill = as.factor(cl)),color = alpha("white", 1/2), size = 0.2) + 
      theme(legend.position = "none") +
      theme(
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank()) +
      labs(title =  plot_title,
           x = "",
           y = "") + scale_fill_viridis_d()
  })
  
  # dendrogram
  output$dendrogram <- renderPlot({
    # dendr <- read_rds(path=paste("dendrograms/dend_",input$year,".rds",sep=""))
    df <- efw_scaled %>% dplyr::filter(year==input$year)
    labs <- df$iso3c
    dendr <- df %>% 
      dplyr::select(contains("EFW")) %>% 
      dplyr::select(-country.efw,-EFW) %>% 
      dist(method = "euclidean") %>% 
      hclust(method = "ward.D2") %>% 
      as.dendrogram()
    labels(dendr) <- as.character(labs)[order.dendrogram(dendr)]
    labels_cex(dendr) <- 0.4
    # color_branches(dendr) <- input$k
    dendr <- color_branches(dendr, input$k, col=viridis(input$k))
    
    # dendr <- labels_cex(dendr, 0.4)
    # labels(dend) <- as.character(X.$ISO3C)[order.dendrogram(dend)]
    
    plot(dendr, main = paste(input$year,", k = ",input$k, sep = ""))
    # dend
  })
}
# Run the application 
shinyApp(ui = ui, server = server)
