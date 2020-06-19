## Helper functions

clean_for_cluster <- function(efw_data = efw_scaled, yr = 2017){
  efw_data <- efw_data %>% dplyr::filter(year == yr, complete.cases(.)) 
  return(efw_data[,c("EFW1","EFW2","EFW3","EFW4","EFW5")])
}

# quick wrapper to hierarchically cluster pre-cleaned data
cluster_by_k <- function(efw_data = clean_for_cluster(efw_scaled,2017), k = 4){
  d <- dist(efw_data, method = "euclidean")
  cl <- hclust(d, method = "ward.D2")
  return(cutree(cl, k = k))
}

# hierarchical clustering
cluster_by_year_and_k <- function(efw_data = efw_scaled, yr = 2017, k = 4){
  out <- efw_data %>% dplyr::filter(year == yr, complete.cases(.))
  efw_data <- clean_for_cluster(efw_data,yr)
  clusters = cluster_by_k(efw_data,k) %>% as.integer()
  out <- cbind(out,cl = clusters) %>% as_tibble() %>% reset_cluster_order()
  return(out)
}

kmeans_by_year <- function(efw_data = efw_scaled, yr = 2017, k = 4, seed = 12345){
  out <- efw_data %>% dplyr::filter(year == yr, complete.cases(.))
  efw_data <- clean_for_cluster(efw_data,yr)
  set.seed(seed) # since kmeans is non-deterministic
  clusters <- kmeans(efw_data,k,iter.max = 100, nstart = 10, algorithm =  "MacQueen")$cluster %>% as.integer
  out <- cbind(out,cl = clusters) %>% as.tibble %>% reset_cluster_order()
  return(out)
}

cluster_wrapper <- function(method = "hierarchical", yr = 2017, k =  4, efw_data = efw_scaled,...){
    if(method == "hierarchical"){
        cluster_by_year_and_k(efw_data,yr,k)
    } else { # k means
        kmeans_by_year(efw_data,yr,k,...)
    }
}

cluster_all_years <- function(method = "hclust", k = 4, efw_data = efw_scaled){
  if(method == "hclust"){
    cluster_data <- purrr::map(years, function(x) {
      cluster_by_year_and_k(efw_data = efw_data,yr = x, k = k)
    })
  } else {
    cluster_data <- purrr::map(years, function(x) {
      kmeans_by_year(efw_data = efw_data, yr = x, k = k) 
    })
  }
  tribble(~year, ~data, years, cluster_data) %>% unnest %>% 
    group_by(year) %>% unnest %>% ungroup %>% dplyr::select(-year1)
} 

cluster_all_yNk <- function(method =  "hclust", Ks =  2:12){
purrr::map(Ks, function(x){
        cluster_all_years(method = method, x) %>% mutate(k = as.integer(x))
    }) %>%
    tribble(~k, ~data, Ks, .) %>%
    unnest %>%
    group_by(k) %>%
    unnest %>%
    ungroup %>%
    dplyr::select(-k1)
}

# relabel clusters so a target country gets the target label. 
fix_cluster_label <- function(df_w_cl, iso = "USA", target_label = 1){
  k <- max(df_w_cl$cl)
  initial_label <- df_w_cl[df_w_cl$iso3c == iso,"cl"] %>% unique %>% as.integer
  if(initial_label == target_label) return(df_w_cl)
  # change labels to leave the target_label unoccupied
  df_w_cl <- df_w_cl %>% mutate(cl = ifelse(cl == target_label, 0, cl))
  df_w_cl <- df_w_cl %>% mutate(cl = ifelse(cl == initial_label, target_label, cl))
  # find vacant label
  count_by_cluster <- df_w_cl %>% group_by(cl) %>% summarize(n = n())
  for(i in 1:k){
    if (i %in% count_by_cluster$cl) next
    vacant_label <- i
  }
  # move label that was occupying the target label into the vacant label and return results
  df_w_cl %>% mutate(cl = ifelse(cl == 0, vacant_label, cl),
                     cl = as.integer(cl))
}

# relabel clusters so USA is always in cluster 1 and Venezuela is always in cluster k
reset_cluster_order <- function(df_w_cl){
  k <- max(df_w_cl$cl)
  df_w_cl %>% fix_cluster_label("USA",1) %>% fix_cluster_label("VEN",k)
}

# Functions to bootstrap for k means and hierarchical clustering
cluster_metrics <- function(data, clusters, dist_matrix) {
  list(db       = clusterSim::index.DB(data, clusters)$DB,
       G1       = clusterSim::index.G1(data, clusters),
       dunn     = clValid::dunn(dist_matrix, clusters),
       sil      = clusterSim::index.S(dist_matrix, clusters), 
       clusters = length(unique(clusters))
  )
}

sample_for_bootstrap <- function(df, n = 100){
  purrr::map(1:n, ~ {
    df %>% dplyr::as_tibble() %>% dplyr::sample_n(size = nrow(.), replace = TRUE)
  })
}

bootstrap_by_year_k <- function(efw_data = efw_scaled, yr = 2017, n = 100){
  bootstrap_samples <- clean_for_cluster(efw_data, yr) %>% 
    sample_for_bootstrap(.,n)
  
  metrics_tib <- map_df(bootstrap_samples, function(boot) {
    d <- dist(boot, method = "euclidean")
    # cl <- kmeans(d,k,iter.max = 25, nstart = 10, algorithm = "MacQueen")
    
    map_df(2:12, function(k){
      clstrs <- kmeans(boot,k,iter.max = 100, nstart = 10, algorithm =  "MacQueen")$cluster
      cluster_metrics(boot, clusters = clstrs, dist_matrix = d)
    })
  })
  
  metrics_tib <- metrics_tib %>%
    dplyr::mutate(bootstrap = factor(rep(1:n, each = length(2:12)))) %>%
    tidyr::gather(key = "Metric", value = "Value", -clusters, -bootstrap)
  
  p <- ggplot(metrics_tib, aes(as.factor(clusters), Value)) +
    facet_wrap(~ Metric, scales = "free_y") +
    geom_line(size = 0.1, aes(group = bootstrap)) +
    geom_line(stat = "summary", fun.y = "mean", aes(group = 1)) +
    stat_summary(fun.data="mean_cl_boot",
                 geom="crossbar", width = 0.5, fill = "white") +
    ggtitle(paste("Bootstrap results for ",yr,sep=""))
  theme_bw()
  
  return(list(bootstrap_metrics = metrics_tib,plot = p))
}

bootstrap_by_year <- function(efw_data = efw_scaled, yr = 2017, n = 100){
  bootstrap_samples <- clean_for_cluster(efw_data, yr) %>% 
    sample_for_bootstrap(.,n)
  
  metrics_tib <- map_df(bootstrap_samples, function(boot) {
    d <- dist(boot, method = "euclidean")
    cl <- hclust(d, method = "ward.D2")
    
    map_df(2:12, function(k) {
      cut <- cutree(cl, k = k)
      cluster_metrics(boot, clusters = cut, dist_matrix = d)
    })
  })
  
  metrics_tib <- metrics_tib %>%
    dplyr::mutate(bootstrap = factor(rep(1:n, each = length(2:12)))) %>%
    tidyr::gather(key = "Metric", value = "Value", -clusters, -bootstrap)
  
  p <- ggplot(metrics_tib, aes(as.factor(clusters), Value)) +
    facet_wrap(~ Metric, scales = "free_y") +
    geom_line(size = 0.1, aes(group = bootstrap)) +
    geom_line(stat = "summary", fun.y = "mean", aes(group = 1)) +
    stat_summary(fun.data="mean_cl_boot",
                 geom="crossbar", width = 0.5, fill = "white") +
    ggtitle(paste("Bootstrap results for ",yr,sep=""))
  theme_bw()
  
  return(list(bootstrap_metrics = metrics_tib,plot = p))
}

# generate a map
cluster_map <- function(clustered_data,plot_title){
    mapdf <- clustered_data %>%
        dplyr::select(iso3c,cl) %>%
        right_join(map_coords)
    mapdf <- mapdf[order(mapdf$order),]

    print_map <- ggplot(mapdf, aes(long, lat, group = group)) +
        geom_polygon(aes(fill = as.factor(cl)),color = alpha("white", 1/2), size = 0.2) + 
        theme(legend.position = "none") +
        theme(
            axis.title.x=element_blank(),
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            axis.title.y=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            panel.background = element_blank()) +
        labs(title = plot_title, 
             x = "",
             y = "") + 
        scale_fill_viridis_d()
    print_map
}

cluster_map_wrapper <- function(method="hierarchical",yr=2017,k=4,efw_data=efw_scaled){
  plot_title <- paste("The world in ",
                      k," clusters (",
                      yr,", ",method,")",
                      sep="")
  cluster_wrapper(method,yr,k,efw_data) %>% 
    cluster_map(.,plot_title)
}

# Summarize data
summarize_clustered_data <- function(df){
    df %>%
        dplyr::rename(cluster = cl) %>%
        add_count(cluster) %>%
        group_by(cluster) %>%
        summarize_at(vars(contains("EFW")),funs(mean,sd,min,max,median))
}

summarize_clustered_data_n <- function(df){
  df %>%
    dplyr::rename(cluster = cl) %>%
    add_count(cluster) %>%
    group_by(cluster) %>%
    add_count(cluster) %>% 
    summarize_at(vars(n, contains("EFW")),funs(mean,sd,min,max,median))
}

# Create a dendrogram
dendrogram_wrapper <- function(efw_data =  efw_scaled,yr = 2017,k = 4){
    labs <- efw_data$iso3c
    dend <- efw_data %>%
        dplyr::filter(year == yr) %>%
        dplyr::select(EFW1,EFW2,EFW3,EFW4,EFW5) %>%
        dist(.,method = "euclidean") %>%
        hclust(.,method = "ward.D2") %>%
        ## dendro_data(.,labels = efw_data$iso3c,leaf_labels = TRUE)
        as.dendrogram
    ## labels(dend) <- efw_data$iso3c
    # Still have to fix the labelling
    place_labels(dend,labs)
    dend
}

# Create pair plots comparing the joint distribution across EFW and sub-indices
pair_plots <- function(clustered_data){
    clustered_data %>%
        dplyr::select(cl,EFW,EFW1,EFW2,EFW3,EFW4,EFW5) %>%
        ggpairs(.,aes(col = as.factor(cl),alpha = 1/3),
                upper = list(continuous = "density"),
                lower = list(continuous = wrap("points",size = 0.5)),
                diag = list(continuous = "densityDiag",
                            discrete =  "barDiag"), progress = TRUE) +
        theme_bw()
}




# wrappers to append clusters to predictive data





## dendro_all_yNk <- function(){
##     purrr::map(2:12, function(x){
##     }) %>%
##         tribble(~k, ~data, 2:12, .) %>% unnest %>%
##         group_by(k) %>% unnest %>% ungroup %>% dplyr::select(-k1)
## }

## kmeans_by_year <- function(efw_data = efw_scaled, yr = 2017, k = 4, seed = 12345){

## cluster_both_ways <- function(k =  4){
##     cluster_data <- purrr::map(c("hclust","kmeans"),
##                                function(x){
##                                   cluster_all_years(method = x,k = k)
##                                }
##     tribble(~year, ~data, years, cluster_data) %>% unnest %>% 
##         group_by(year) %>% unnest %>% ungroup %>% dplyr::select(-year1)
## }
