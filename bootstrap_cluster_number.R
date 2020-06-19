# Bootstrap the data for each year, generate diagnostics and plots
# This takes a long time to run, so I'm saving the results to file
# Uncomment to re-run.
# set.seed(12345)
# 
# bootstrap_hierarchical <- purrr::map(years, function(x) bootstrap_by_year(efw_scaled,yr = x,250))
# names(bootstrap_hierarchical) <- years
# 
# bootstrap_kmeans <- purrr::map(years, function(x) bootstrap_by_year_k(efw_scaled,yr = x,250))
# names(bootstrap_kmeans) <- years
# 
# save(bootstrap_hierarchical, bootstrap_kmeans, file = "bootstrap_results_250.RData")
# load("bootstrap_results.RData")
load("bootstrap_results_250.RData")
# 
# # 2017 results
bootstrap_kmeans$`2017`$plot + 
  aes(alpha = 0.2) + theme_minimal() + 
  labs(x="Number of clusters",
       title = "Cluster metrics for k-means clusters")
bootstrap_hierarchical$`2017`$plot + 
  aes(alpha = 0.2) + theme_minimal() +
labs(x="Number of clusters",
    title = "Cluster metrics for hierarchical clusters")
# 2010 results
# bootstrap_kmeans$`2010`$plot
# bootstrap_hierarchical$`2010`$plot

# Other years look similar. They mostly inidicate that larger values of k fit the data better.
# On the other hand, the bootstrap samples show quite a bit of noise. 
# If my goal were to extract maximal information from the dataset I might use a higher value of k,
# but that would come at the cost of interpretability, so I'll just follow Huskinson and Lawson and
# pick k of 4 
# (The Silhouette scores seem to indicate that this is a reasonable enough choice.)

# Increasing the bootstrap sample from 100 to 250 didn't change the overall story: the data "wants" more clusters
# But I'm going to settle for a smaller number.