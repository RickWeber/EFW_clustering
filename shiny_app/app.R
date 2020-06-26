# shiny app to visualize results from clustering project

# import requisite libraries, functions, and data
# source("libraries.R")
library(tidyverse)
library(countrycode)
library(dendextend)
library(maps)
library(shiny)
library(viridis)
library(GGally)
# relevant data
df <-  read_csv("all_clusters.csv")
map_coords <- map_data("world") %>%
  as_tibble() %>% 
  mutate(iso3c = countrycode(region,"country.name","iso3c"))
years <- df$year %>% unique() %>% sort(decreasing=TRUE)
# function to summarize data
summarize_clustered_data <- function(df){
  count <- df %>% 
    dplyr::rename(cluster = cl) %>%
    group_by(cluster) %>%
    summarize(count = n())
  df %>%
    dplyr::rename(cluster = cl) %>%
    group_by(cluster) %>%
    summarize_at(vars(contains("EFW")),funs(mean,sd,min,max,median)) %>%
    left_join(count)
}

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
                   c("kmeans","hierarchical"), 
                   "hierarchical"),
      selectInput("year",
                  "Year:",
                  choices = years,
                  selected = 2017),
      radioButtons("scaled",
                   "scaled or unscaled data?",
                   c("scaled","unscaled"),
                     "scaled"),
      # helpText("For more information see my",
      #          tags$a(href="https://github.com/RickWeber/EFW_clustering", "GitHub."))
      helpText("For more information see",
               tags$em("Clusters of Economic Freedom Revisited"), "in",
               tags$a(href="https://www.economics-finance.org/aefj.php" ,"the Academy of Economics and Finance Journal"),
               "Volume 11, and on",
               tags$a(href="https://github.com/RickWeber/EFW_clustering", "GitHub."))
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(
        tabPanel("cluster summaries",tableOutput("cluster_summary")),
        tabPanel("data",tableOutput("cluster_table")),
        tabPanel("dendrogram",plotOutput("dendrogram")),
        # tabPanel("pair plots\n(runs slow!)",plotOutput("pairplots")),
        tabPanel("map",plotOutput("cluster_map"))
      )
    )
  )
)
# Summarize data
summarize_clustered_data <- function(df){
  count <- df %>% 
    dplyr::rename(cluster = cl) %>%
    group_by(cluster) %>%
    summarize(count = n())
  
  df %>%
    dplyr::rename(cluster = cl) %>%
    #        add_count(cluster) %>%
    group_by(cluster) %>%
    summarize_at(vars(contains("EFW")),funs(mean,sd,min,max,median)) %>%
    left_join(count)
}
# Define server logic 
server <- function(input, output) {
  
  # clustered/normalized data
  clustered_data <- reactive({
    if(input$scaled=="scaled"){
      df %>% dplyr::filter(year==input$year,
                           k==input$k,
                           method==input$method,
                           scaled==TRUE) %>% 
        rename(country=country.efw)
    } else {
      df %>% dplyr::filter(year==input$year,
                           k==input$k,
                           method==input$method,
                           scaled==FALSE) %>% 
        rename(country=country.efw)
    }
    
  })
 
  # data with cluster membership  
  output$cluster_table <- renderTable({
    clustered_data() %>% 
      dplyr::rename(cluster = cl,
                    `EFW quartile` = quartile)
  },striped = TRUE)
  
  
  # summary stats
  output$cluster_summary <- renderTable({
    out <- clustered_data() %>%
      summarize_clustered_data()
    out <- out %>% mutate_if(is.double,funs(round(.,digits=2)))
    new_out <- t(out)
    vars <- colnames(out)
    new_out <- cbind(vars,new_out)
    colnames(new_out) <- c("variable", paste("cluster",out$cluster,sep=" "))
    new_out[2:nrow(new_out),]
  },striped = TRUE)
  
  # Create a map
  output$cluster_map <- renderCachedPlot({
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
  },cacheKeyExpr = c(input$k,input$method,input$year,input$scaled))
  
  # dendrogram
  output$dendrogram <- renderPlot({
    df <- clustered_data()
    labs <- df$iso3c
    dendr <- df %>% 
      dplyr::select(contains("EFW")) %>% 
      dplyr::select(-EFW) %>% 
      dist(method = "euclidean") %>% 
      hclust(method = "ward.D2") %>% 
      as.dendrogram()
    labels(dendr) <- as.character(labs)[order.dendrogram(dendr)]
    labels_cex(dendr) <- 0.4
    dendr <- color_branches(dendr, input$k, col=viridis(input$k))
    plot(dendr, main = paste(input$year,", k = ",input$k, sep = ""))
  })
  # pair plots
  # output$pairplots <- renderCachedPlot({ 
  #   plot <- clustered_data() %>%
  #     dplyr::select(cl,EFW,EFW1,EFW2,EFW3,EFW4,EFW5) %>%
  #     ggpairs(.,aes(col = as.factor(cl),alpha = 1/3),
  #             upper = list(continuous = "density"),
  #             lower = list(continuous = wrap("points",size = 0.5)),
  #             diag = list(continuous = "densityDiag",
  #                         discrete =  "barDiag"), progress = TRUE) +
  #     theme_bw()
  # },cacheKeyExpr = c(input$k,input$method,input$year,input$scaled))
}

# Run the application 
shinyApp(ui = ui, server = server)
