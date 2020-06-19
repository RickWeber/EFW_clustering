# precompile the data for interactive_viz.R
rm(list = ls())
# load libraries
source("libraries.R")

# import functions
source("functions.R")

# import EFW data
source("import_data.R")

## purrr::map(c("hclust","kmeans"),
##            function(method){
##                purrr::map(2:12,
##                           function(k){
##                               purrr::map(years,
##                                          function(yr){
##                                              data <- cluster_wrapper(method,yr,k)
##                                              filename <- paste("mapdf",
##                                                                method,yr,k,
##                                                                sep = "_")
##                                              path <- paste("maps/",filename,".rds",sep = "")
##                                              write_rds(data,path)
##                                          })
##                           })
##            })

time_to_run <- system.time(
purrr::map(c("hclust","kmeans"),
           function(method){
               purrr::map(2:12,
               function(k){
                   purrr::map(years,
                              function(yr){
                                  data <- cluster_wrapper(method,yr,k) %>% full_join(map_coords)
                                  # dend <- dendrogram_wrapper(yr=yr,k=k)
                                  filename <- paste("mapdf",
                                                    method,yr,k,
                                                    sep = "_")
                                  path <- paste("maps/",filename,".csv",sep = "")
                                  write_csv(data,path)
                              })
               })
           })
)

time_to_run <- system.time(
    purrr::map(years,
               function(yr){
                   data <-  efw_scaled %>% 
                       dplyr::filter(year==yr,complete.cases(.)) 
                   labels <- data$iso3c
                   dend <- data %>% 
                       dist(.,method="euclidean") %>% 
                       hclust(.,method="ward.D2") %>% 
                       as.dendrogram()
                   dend <- place_labels(dend,labels)
                   filename <- paste("dend",yr,sep="_")
                   path <- paste("dendrograms/",filename,".rds",sep = "")
                   write_rds(x=dend,path=path)
               })
)
time_to_run
# renumber Huskinson's cluster, and count cluster members
husk <- husk %>% reset_cluster_order %>% add_count(cl)

# All the data with the updated methods
efw_viz_hclust <- cluster_all_yNk("hclust") %>%
    mutate(method =  "hierarchical") %>%
    add_count(k,year,cl)
efw_viz_kmeans <- cluster_all_yNk("kmeans") %>%
    mutate(method =  "kmeans") %>%
    add_count(k,year,cl)

# Huskinson's data
# Ideally I'd use the 2012 report data
# But I don't have easy access to it, so I'll just filter the existing stuff
## Quartiles don't line up between Huskinson's data and mine because mine includes more countries.

efw_viz_husk <- efw_viz_kmeans %>%
    dplyr::filter(k ==  4, year ==  2010) %>%
    dplyr::select(iso3c,contains("EFW"),quartile) %>%
    right_join(husk,by = "iso3c",suffix = c("",".husk")) %>%
    mutate(method =  "original", k =  4) 

## efw_data_complete <- full_join(efw_viz_kmeans,efw_viz_hclust) %>%
##     full_join(efw_viz_husk)


### Pass these to shiny and process on that end
write_rds(efw_viz_hclust,"efw_viz_hclust.rds")
write_rds(efw_viz_husk,"efw_viz_husk.rds")
write_rds(efw_viz_kmeans,"efw_viz_kmeans.rds")
write_rds(mapdf,"efw_viz_mapdf.rds")


shiny_hclust <- efw_viz_hclust %>%
    group_by(k,year) %>%
    nest %>%
    mutate(dend = purrr::map(data,
                             function(df){
                                 df %>%
                                     dplyr::filter(complete.cases(.)) %>%
                                     dist(.,method = "euclidean") %>%
                                     hclust(.,method = "ward.D2") %>%
                                     as.dendrogram}),
           ## pair_plot = purrr::map(data,
           ##                        function(df){
           ##                            df %>%
           ##                                dplyr::select(cl,contains("EFW"),-country.efw) %>%
           ##                                ggpairs(.,aes(col =  as.factor(cl), alpha =  1/3),
           ##                                        upper = list(continuous =  "density"),
           ##                                        lower =  list(continuous =  wrap("points",size = 0.5)),
           ##                                        diag =  list(continuous = "densityDiag")) +
           ##                                theme_bw()
           ##                        }),
           mapdf =  purrr::map(data,
                               function(df){
                                   full_join(df,map_coords) %>%
                                       arrange(order)
                               })
           ## map_plot =  purrr::map(data,
           ##                     function(df){
           ##                         full_join(df,map_coords) %>%
           ##                             arrange(order) %>%
           ##                             ggplot(.,
           ##                                    aes(long,lat,group = group))+
           ##                             geom_polygon(aes(fill = as.factor(cl)),
           ##                                          color = alpha("white",1/2),
           ##                                          size = 0.2) +
           ##                             theme(legend.position = "none") +
           ##                             theme(
           ##                                 axis.text.x = element_blank(),
           ##                                 axis.ticks.y = element_blank(),
           ##                                 axis.text.x = element_blank(),
           ##                                 axis.ticks.y = element_blank(),
           ##                                 panel.background = element_blank()) +
           ##                             scale_fill_viridis_d()
           ##                     })
           )

shiny_kmeans <- efw_viz_kmeans %>%
    group_by(k,year) %>%
    nest %>%
    mutate(
        mapdf =  purrr::map(data,
                            function(df){
                                full_join(df,map_coords) %>%
                                    arrange(order)
                            })
        ## pair_plot = purrr::map(data,
        ##                           function(df){
        ##                               df %>%
        ##                                   dplyr::select(cl,contains("EFW"),-country.efw) %>%
        ##                                   ggpairs(.,aes(col =  as.factor(cl), alpha =  1/3),
        ##                                           upper = list(continuous =  "density"),
        ##                                           lower =  list(continuous =  wrap("points",size = 0.5)),
        ##                                           diag =  list(continuous = "densityDiag")) +
        ##                                   theme_bw()
        ##                           }),

           ## map_plot =  purrr::map(data,
           ##                     function(df){
           ##                         full_join(df,map_coords) %>%
           ##                             arrange(order) %>%
           ##                             ggplot(.,
           ##                                    aes(long,lat,group = group))+
           ##                             geom_polygon(aes(fill = as.factor(cl)),
           ##                                          color = alpha("white",1/2),
           ##                                          size = 0.2) +
           ##                             theme(legend.position = "none") +
           ##                             theme(
           ##                                 axis.text.x = element_blank(),
           ##                                 axis.ticks.y = element_blank(),
           ##                                 axis.text.x = element_blank(),
           ##                                 axis.ticks.y = element_blank(),
           ##                                 panel.background = element_blank()) +
           ##                             scale_fill_viridis_d()
           ##                     })
           )

shiny_husk <- efw_viz_husk %>%
    group_by(k,year) %>%
    nest %>%
    mutate(
        mapdf =  purrr::map(data,
                            function(df){
                                full_join(df,map_coords) %>%
                                    arrange(order)
                            })
        ## pair_plot = purrr::map(data,
        ##                           function(df){
        ##                               df %>%
        ##                                   dplyr::select(cl,contains("EFW"),-country.efw) %>%
        ##                                   ggpairs(.,aes(col =  as.factor(cl), alpha =  1/3),
        ##                                           upper = list(continuous =  "density"),
        ##                                           lower =  list(continuous =  wrap("points",size = 0.5)),
        ##                                           diag =  list(continuous = "densityDiag")) +
        ##                                   theme_bw()
        ##                           }),

           ## map_plot =  purrr::map(data,
           ##                     function(df){
           ##                         full_join(df,map_coords) %>%
           ##                             arrange(order) %>%
           ##                             ggplot(.,
           ##                                    aes(long,lat,group = group))+
           ##                             geom_polygon(aes(fill = as.factor(cl)),
           ##                                          color = alpha("white",1/2),
           ##                                          size = 0.2) +
           ##                             theme(legend.position = "none") +
           ##                             theme(
           ##                                 axis.text.x = element_blank(),
           ##                                 axis.ticks.y = element_blank(),
           ##                                 axis.text.x = element_blank(),
           ##                                 axis.ticks.y = element_blank(),
           ##                                 panel.background = element_blank()) +
           ##                             scale_fill_viridis_d()
           ##                     })
           )

write_rds(shiny_husk,"shiny_husk.rds")
write_rds(shiny_hclust,"shiny_hclust.rds")
write_rds(shiny_kmeans,"shiny_kmeans.rds")


## # This generates 3 files, two of which are > 500MB. So the app will be slow to load and memory heavy. But let's get it working before trying to improve it.

## # On second thought, those files are just too big. Time to cull back all the stuff I've done and get back to processing the data on the shiny side. 

## Okay, after lots of frustration, I'm going to try to put the data into databases and have the shiny app deal with the database.

dbconnect <- dbConnect(RSQLite::SQLite(), "maps/maps.db")
shiny_husk <- read_rds("shiny_husk.rds")
shiny_husk <- shiny_husk %>% dplyr::select(mapdf) %>% unnest 

dbWriteTable(dbconnect,"shiny_husk",shiny_husk)
shiny_hclust <- read_rds("shiny_hclust.rds")
shiny_hclust <- shiny_hclust %>% dplyr::select(mapdf) %>% unnest
dbWriteTable(dbconnect,"shiny_hclust",shiny_hclust)
## shiny_kmeans <- read_rds("shiny_kmeans.rds")
shiny_kmeans <- shiny_kmeans %>% dplyr::select(mapdf) %>% unnest
dbWriteTable(dbconnect,"shiny_kmeans",shiny_kmeans)

shiny_hclust %>%
    dplyr::filter(year == 1970) %>%
    dplyr::select(mapdf) %>%
    unnest %>%
    dbWriteTable(dbconnect,"shiny_hclust",.)

purrr::map(years,
           function(yr){
               shiny_hclust %>%
                   dplyr::filter(year == yr) %>%
                   dplyr::select(mapdf) %>%
                   unnest %>%
                   dbAppendTable(dbconnect,"shiny_hclust",.)
           })

shiny_kmeans %>%
    dplyr::filter(year == 1970) %>%
    dplyr::select(mapdf) %>%
    unnest %>%
    dbWriteTable(dbconnect,"shiny_hclust",.)

purrr::map(years,
           function(yr){
               shiny_kmeans %>%
                   dplyr::filter(year == yr) %>%
                   dplyr::select(mapdf) %>%
                   unnest %>%
                   dbAppendTable(dbconnect,"shiny_hclust",.)
           })
