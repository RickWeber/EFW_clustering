## Clusters of Economic Freedom Revisited
### Technical appendix

#### Quick Start
After cloning this repository to your local machine, run `script0.R`. That will set up the libraries, define the functions used, and import the data. 

Running `app.R` will generate a page allowing you to see the clustered data for
all available years in table and map form. Run it after running `script0.R`.

A running version of that app is available online [here](https://rickweber.shinyapps.io/Clusters_of_Economic_Freedom/). Please note that this version has limited availability. If you want to properly dig into this project, you are encouraged to clone this repository and work on your local machine.

#### Just the data please...
The file 
[all_clusters.csv](https://raw.githubusercontent.com/RickWeber/EFW_clustering/master/all_clusters.csv)
has cluster membership using hierarchical clustering (using Ward's method) and k-means clustering, for all available years, on scaled and unscaled data, divided into between 2 and 12 clusters. 

Please note that replicating this dataset on your own computer may result in slightly different results when using k-means clustering because the algorithm is non-deterministic.

#### Bootstrapping
`bootstrap_cluster_number.R` contains the code to bootstrap the data over all years in an effort to determine how many clusters the data should be divided into. The metrics provide some support for using more clusters rather than fewer, but using more clusters comes at the expense of interpretability.

#### Comparing model performance
`regressions.R` is the script used in the paper to compare the performance of regression models with and without cluster membership included. `regressions_unscaled.R` repeats the exercise with unscaled EFW scores. `regressions_lots_of_comparisons.R` includes comparisons between models using the overall EFW score and regressions using the area scores. 

#### Clusters over time
The file `cluster_membership_over_time.R` illustrates a difficulty: the cluster label is somewhat arbitrary. This project partially overcomes this issue by using Venezuela and the United States as fixed points, and relabelling everything so these two countries always have the the same cluster labels (cluster 1 for the United states and cluster k, where k is the number of clusters, for Venezuela). But when k is 4, clusters 2 and 3 can be somewhat scrambled. 

For another approach, see [this repository focused on this issue](https://github.com/RickWeber/kmeans_over_time).