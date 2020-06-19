rm(list=ls())

# load libraries
source("libraries.R")

# import functions
source("functions.R")

# import EFW data
source("import_data.R")

# compare performance of kmeans and hclust
# source("comparison.R")

# compare performance of simple models with and without cluster dummy variables
source("regressions.R")

# interactive visualizations
# source("interactive_viz.R")

# Figures and setup for draft.Rmd
# clustered_data_2017 <- cluster_wrapper(efw_data = efw) %>% # replace line below with this line for unscaled data
clustered_data_2017 <- cluster_wrapper(efw_data = efw_scaled) %>% 
  dplyr::rename(cluster = cl,
                country =  country.efw,
                "EFW quartile" = quartile,
                "EFW overall" = EFW,
                "Size of gov't" = EFW1,
                "Legal system" = EFW2,
                "Sound money" = EFW3,
                "Free trade" = EFW4,
                "Regulation" = EFW5)

#### Figure 1 ####

png("figure1.png", units = "in", width = 7.5, height = 5, res = 300)
clustered_data_2017 %>% mutate(cl = cluster) %>%
  cluster_map(.,"Figure 1: The world in 4 clusters, 2017")
dev.off()

#### Figure 2 ####

facet_labels <- list(
  "civil_liberties" = "Civil Liberties",
  "political_freedom" = "Political Freedom",
  "eci" = "Economic Complexity",
  "eciplus" = "Economic Complexity \n (ECI+ measure)",
  "hdi" = "Human Development Index",
  "inc_shr_bot10" = "Income Share of Bottom 10%",
  "gdppc_ppp" = "GDP per capita \n (PPP)",
  "gdppc_ppp_pct_change" = "GDP per capita \n growth rate (PPP)",
  "gdppc_pct_change" = "GDP per capita \n growth rate"
)

facet_labeller <- function(variable,value){
  return(facet_labels[value])
}

r2_gain_plot <- comparison %>% 
  ggplot(aes(year, gain)) +
  geom_line() + facet_wrap(~lhs, labeller=facet_labeller) + theme_minimal() +
  labs( y = bquote('change in adjusted R'^2))


png("figure2.png", units = "in", width = 7.5, height = 7.5, res = 300)
r2_gain_plot + 
  labs(title = bquote('Figure 2: Effect of cluster dummy variables on adjusted R'^2)) +
  geom_hline(yintercept = 0, color = "red", linetype = 2, size = 1/4)
dev.off()

