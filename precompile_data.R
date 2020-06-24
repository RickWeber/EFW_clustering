# setup
source("script0.R")

# # precompile the map data if the shiny app isn't behaving
# purrr::map(c("hclust","kmeans"),
#            function(method){
#                purrr::map(2:12,
#                function(k){
#                    purrr::map(years,
#                               function(yr){
#                                   data <- cluster_wrapper(method,yr,k) %>% full_join(map_coords)
#                                   filename <- paste("mapdf",
#                                                     method,yr,k,
#                                                     sep = "_")
#                                   path <- paste("maps/",filename,".csv",sep = "")
#                                   write_csv(data,path)
#                               })
#                })
#            })

# Cluster all the data for all available years, for k=2 through 12, 
# using hierarchical clustering and k-means

# All the data with the updated methods
efw_hclust <- cluster_all_yNk("hclust") %>% mutate(method =  "hierarchical")
efw_kmeans <- cluster_all_yNk("kmeans") %>% mutate(method =  "kmeans")

# Huskinson's data
# Ideally I'd use the 2012 report data
# But I don't have easy access to it, so I'll just filter the existing stuff
## Quartiles don't line up between Huskinson's data and mine because mine includes more countries.

efw_husk <- efw_kmeans %>%
  dplyr::filter(k ==  4, year ==  2010) %>%
  dplyr::select(iso3c,contains("EFW"),quartile) %>%
  right_join(husk,by = "iso3c",suffix = c("",".husk")) %>%
  mutate(method =  "original", k =  4)

efw_data_complete <- full_join(efw_kmeans,efw_hclust) %>%
  full_join(efw_husk) %>% add_count(method,k,year,cl)


##

efw_pair_plot_complete <- efw_data_complete %>%
  group_by(method,k,year) %>%
  nest %>%
  mutate(pair_plot = ifelse(method ==  "original"),
         purrr::map(data,function(df){
           plot_title <- "Huskinson and Lawson's results (2010, k = 4), using 2012 EFW report"
           df %>%
             dplyr::select(iso3c,husk_cluster,contains("EFW")) %>%
             ggpairs(.,aes(col =  as.factor(husk_clusters), alpha = 1/3),
                     upper = list(continuous = "density"),
                     lower = list(continuous = wrap("points", size = 0.5)),
                     diag = list(continuous = "densityDiag")) +
             theme_bw()
         }),
         ifelse(method == "kmeans",
                purrr::map(data,function(df){
                  plot_title <- paste("K-means clustered")
                  df %>%
                    dplyr::select(iso3c,cl,contains("EFW")) %>%
                    ggpairs(.,aes(col = as.factor(cl), alpha =  1/3,
                                  upper = list(continuous = "density"),
                                  lower = list(continuous = wrap("points", size = 0.5)),
                                  diag = list(continuous = "densityDiag")) +
                              theme_bw()
                    ))
                
                }),
         purrr::map(data,function(df){
           plot_title <- paste("Hierarchically clustered")
           df %>%
             dplyr::select(iso3c,cl,contains("EFW")) %>%
             ggpairs(.,aes(col = as.factor(cl), alpha =  1/3,
                           upper = list(continuous = "density"),
                           lower = list(continuous = wrap("points", size = 0.5)),
                           diag = list(continuous = "densityDiag")) +
                       theme_bw()
             ))
         })))

write_rds(efw_dend_complete,"shiny_dendro.rds")
write_rds(efw_data_complete,"shiny_data.rds")