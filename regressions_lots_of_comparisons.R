## Comparing area scores with cluster membership

# In this comparison, the baseline uses the area scores while the 
# comparison uses the overall score plus cluster membership
# Run script0.R first
df4 <- all_the_clusters(efw_scaled) %>% mutate(scaled=TRUE) %>% 
  dplyr::filter(k==4, method=="hierarchical") %>% 
  dplyr::select(year,iso3c,cl) %>% 
  inner_join(all_data)

# Left hand side variables
LHS <- c("civil_liberties","political_freedom", # from freedom house
         "eci", "eciplus", # Economic complexity
         "gdppc_ppp", "gdppc_ppp_pct_change","gdppc_pct_change", # GDP per capita
         "inc_shr_bot10","hdi")  # income to poorest 10% and human development index %>% 

# Run two regressions for each LHS variable for each year
# The first is the baseline and predicts LHS by EFW.scaled
# The second adds cluster dummies
reg_all <- merge(LHS, years) %>%
  as_tibble() %>%
  rename(lhs = x, year = y) %>%
  mutate(lhs = as.character(lhs)) %>% 
  group_by(lhs,year) %>%
  full_join(df4) %>% 
  nest() %>%
  mutate(model_base_overall = purrr::pmap(list(lhs,year),function(x,y){
    tryCatch({
      df <- df4 %>% dplyr::filter(year == y)
      for1 <- paste(eval(x),"~EFW.scaled")
      return(lm(for1,data = df))
    },error = function(err){
      return(NULL)
    })
  }),
  model_base_areas = purrr::pmap(list(lhs,year),function(x,y){
    tryCatch({
      df <- df4 %>% dplyr::filter(year == y)
      for1 <- paste(eval(x),"~EFW1.scaled+EFW2.scaled+EFW3.scaled+EFW4.scaled+EFW5.scaled")
      return(lm(for1,data = df))
    },error = function(err){
      return(NULL)
    })
  }),
  model_cl_overall = purrr::pmap(list(lhs,year),function(x,y){
    tryCatch({
      df <- df4 %>% dplyr::filter(year == y)
      for2 <- paste(eval(x),"~EFW.scaled + as.factor(cl) - 1")
      return(lm(for2,data = df))
    },error = function(err){
      return(NULL)
    })
  }),
  model_cl_areas = purrr::pmap(list(lhs,year),function(x,y){
    tryCatch({
      df <- df4 %>% dplyr::filter(year == y)
      for2 <- paste(eval(x),"~EFW1.scaled+EFW2.scaled+EFW3.scaled+EFW4.scaled+EFW5.scaled + as.factor(cl) - 1")
      return(lm(for2,data = df))
    },error = function(err){
      return(NULL)
    })
  })) %>%
  mutate(glance_base_overall =  purrr::map(model_base_overall, broom::glance),
         glance_base_areas =  purrr::map(model_base_areas, broom::glance),
         glance_cl_overall =  purrr::map(model_cl_overall, broom::glance),
         glance_cl_areas =  purrr::map(model_cl_areas, broom::glance))

mod1 <- reg_all %>% unnest(glance_base_overall)
mod2 <- reg_all %>% unnest(glance_base_areas)
mod3 <- reg_all %>% unnest(glance_cl_overall)
mod4 <- reg_all %>% unnest(glance_cl_areas)

comparison1 <- inner_join(mod1,mod2,by = c("year","lhs"), suffix = c("_overall","_areas"))
comparison2 <- inner_join(mod3,mod4,by = c("year","lhs"), suffix = c("_overall","_areas"))
comparison_all <- inner_join(comparison1,comparison2,by=c("year","lhs"), suffix = c("_base","_clustered")) %>% 
  mutate(gain1 =  adj.r.squared_overall_clustered - adj.r.squared_overall_base) %>% 
  mutate(gain2 =  adj.r.squared_areas_clustered - adj.r.squared_areas_base) %>% 
  mutate(gain3 =  adj.r.squared_overall_clustered - adj.r.squared_areas_base) %>% 
  mutate(gain4 =  adj.r.squared_areas_clustered - adj.r.squared_overall_base) %>% 
  dplyr::filter(!is.na(gain1)) %>%
  dplyr::filter(!is.na(gain2)) %>%
  dplyr::filter(!is.na(gain3)) %>%
  dplyr::filter(!is.na(gain4)) %>%
  mutate(lhs = fct_relevel(lhs,c(
    "civil_liberties",
    "political_freedom",
    "eci",
    "eciplus",
    "hdi",
    "inc_shr_bot10",
    "gdppc_ppp",
    "gdppc_ppp_pct_change",
    "gdppc_pct_change"
  )))


streamlined_comparison <- comparison_all %>%
  ungroup %>%
  dplyr::select(contains("adj.r.squared"), contains("gain"))

r2_summary <- streamlined_comparison %>% summary()

comp_mean <- streamlined_comparison %>% 
  summarize_all(mean) %>% 
  mutate(measure = "mean")

comp_min <- streamlined_comparison %>% 
  summarize_all(min) %>% 
  mutate(measure = "minimum")

comp_max <- streamlined_comparison %>% 
  summarize_all(max) %>% 
  mutate(measure = "maximum")

comp_median <- streamlined_comparison %>%
  summarize_all(median) %>% 
  mutate(measure = "median")

comp_sd <- streamlined_comparison %>% 
  summarize_all(sd) %>% 
  mutate(measure = "standard deviation")

comp_sum <- comp_mean %>% 
  full_join(comp_median) %>% 
  full_join(comp_sd) %>% 
  full_join(comp_min) %>% 
  full_join(comp_max) 

comp_sum
comparison_all %>% dplyr::filter(gain1 < 0 | gain2 < 0 | gain3 < 0 | gain4 < 0)

# 46 cases where at least one of the cluster models performance worse.
# mean gain is positive even when comparing the baseline using the area scores to the 
# alternative using the overall score with clusters. 
# all told, I'd count this as an improvement overall, but let's graph it out...

comparison_all %>% 
  ggplot(aes(year, gain1)) +
  geom_line(aes(year, gain1), color = "blue") + # overall vs overall with clusters
  geom_line(aes(year, gain2), color = "green") + # areas vs areas with clusters
  geom_line(aes(year, gain3), color = "red") + # areas vs overall with clusters
  geom_line(aes(year, gain4), color = "grey") + # overall vs areas with clusters
  facet_wrap(~lhs, labeller=facet_labeller) + theme_minimal() +
  labs( y = bquote('change in adjusted R'^2)) +
  geom_hline(yintercept = 0, color = "red", linetype = 2, size = 1/4)

# These labels are out of order on my machine right now...
# Looks like performance is hurt for: 
# ECI+ when comparing the strong baseline to the weak alternative. (12 o'clock)
# GDP growth rate for 1970 (9 o'clock)
# GDP (PPP) growth rate before 2000 (3 o'clock)
# ECI when making the least favorable comparison
