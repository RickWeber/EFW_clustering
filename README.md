## Clusters of Economic Freedom Revisited
### Technical appendix

#### Cluster Membership
See this 
[table](https://github.com/RickWeber/EFW_clustering/blob/master/cluster_membership_2017.md)
for cluster membership sorted by population. This table is for 2017, splitting the world into four clusters using normalized values of the Economic Freedom of the World area scores.

#### Quick Start
After cloning this repository to your local machine, run `script0.R`. That will set up the libraries, define the functions used, and import the data. 

Running `app.R` will generate a page allowing you to see the clustered data for
all available years in table and map form. Run it after running `script0.R`. 

A running version of that app is available online [here](https://rickweber.shinyapps.io/Clusters_of_Economic_Freedom/). Please note that this version has limited availability (25 hours per month). If you want to properly dig into this project, you are encouraged to clone this repository and work on your local machine. `shiny_app/app.R` contains the code used to generate that version of the visualization engine. 

#### Just the data please...
The file 
[all_clusters.csv](https://raw.githubusercontent.com/RickWeber/EFW_clustering/master/all_clusters.csv)
has cluster membership using hierarchical clustering (using Ward's method) and k-means clustering, for all available years, on scaled and unscaled data, divided into between 2 and 12 clusters.

The headers for that file are as follows: 

* `year` is the year of the observation,
* `iso3c` is the three character abreviation of the country,
* `country.efw` is the country name as presented in the [Economic Freedom of the World](https://www.fraserinstitute.org/studies/economic-freedom-of-the-world-2019-annual-report) dataset,
* `EFW` is the overall Economic Freedom index score,
* `EFW[1-5]` are the Economic Freeedom area scores, 
* `quartile` represents which quartile the country was in for that year on the overall index with 1 representing "most free" and 4 representing "least free",
* `cl` is the cluster label for that country in that year (cluster membership is calculated separately for each year),
* `k` is the number of clusters the observations were divided into,
* `method` is either "hierarchical" or "kmeans",
* `scaled` is a boolean value indicating whether the data was normalized before clustering.

When using this data, be sure to filter by a consistent value of `k` (most important), as well as `method` and `scaled`. 

Please note that replicating this dataset on your own computer may result in slightly different results when using k-means clustering because the algorithm is non-deterministic.

#### Bootstrapping
`bootstrap_cluster_number.R` contains the code to bootstrap the data over all years in an effort to determine how many clusters the data should be divided into. The metrics provide some support for using more clusters rather than fewer, but using more clusters comes at the expense of interpretability.

#### Comparing model performance
`regressions.R` is the script used in the paper to compare the performance of regression models with and without cluster membership included. `regressions_unscaled.R` repeats the exercise with unscaled EFW scores. `regressions_lots_of_comparisons.R` includes comparisons between models using the overall EFW score and regressions using the area scores. 

#### Clusters over time
The file `cluster_membership_over_time.R` illustrates a difficulty: the cluster label is somewhat arbitrary. This project partially overcomes this issue by using Venezuela and the United States as fixed points, and relabelling everything so these two countries always have the the same cluster labels (cluster 1 for the United states and cluster k, where k is the number of clusters, for Venezuela). But when k is 4, clusters 2 and 3 can be somewhat scrambled. 

For another approach, see [this repository focused on this issue](https://github.com/RickWeber/kmeans_over_time).

#### License
The underlying Economic Freedom of the World data is not my own. Please visit [this link](https://www.fraserinstitute.org/resource-file?nid=13069&fid=12711) for the original source.

The cluster memberships calculated here, as well as the R code used to generate them are licensed under the [CC-BY-4.0](https://creativecommons.org/licenses/by/4.0/) license. You are free to share and use the data as you see fit, but I ask that you please cite my work.

#### Citation
This project is the technical appendix to my paper **Clusters of Economic Freedom Revisited** in Volume 11 of [The Academy of Economics and Finance Journal](https://www.economics-finance.org/jef-5.php).
