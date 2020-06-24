## Clusters of Economic Freedom Revisited
### Technical appendix

#### Quick Start
After cloning this repository to your local machine, run `script0.R`. That will set up the libraries, define the functions used, and import the data. 

Running `app.R` will generate a page allowing you to see the clustered data for
all available years in table and map form. Run it after running `script0.R`.

Another version of that app is available [here](https://rickweber.shinyapps.io/Clusters_of_Economic_Freedom/)

#### Just the data please...
The file 
[all_clusters.csv](https://raw.githubusercontent.com/RickWeber/EFW_clustering/master/all_clusters.csv)
has cluster membership using hierarchical clustering (using Ward's method) and k-means clustering, for all available years, on scaled and unscaled data, divided into between 2 and 12 clusters. 

#### Bootstrapping
`bootstrap_cluster_number.R` contains the code to bootstrap the data over all years in an effort to determine how many clusters the data should be divided into. The metrics provide some support for using more clusters rather than fewer, but using more clusters comes at the expense of interpretability.


#### Clusters over time
The file `cluster_membership_over_time.R` illustrates a difficulty: the cluster label is somewhat arbitrary. This project partially overcomes this issue by using Venezuela and the United States as fixed points, and relabelling everything so these two countries always have the the same cluster labels (cluster 1 for the United states and cluster k, where k is the number of clusters, for Venezuela). But when k is 4, clusters 2 and 3 can be somewhat scrambled. 

For another approach, see [this repository focused on this issue](https://github.com/RickWeber/kmeans_over_time).