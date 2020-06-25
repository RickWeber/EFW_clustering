## Comparing baseline model with clustered model using scaled data
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
reg <- merge(LHS, years) %>%
    as_tibble() %>%
    rename(lhs = x, year = y) %>%
    mutate(lhs = as.character(lhs)) %>% 
    group_by(lhs,year) %>%
    full_join(df4) %>% 
    nest() %>%
    mutate(model1 = purrr::pmap(list(lhs,year),function(x,y){
        tryCatch({
            df <- df4 %>% dplyr::filter(year == y)
            for1 <- paste(eval(x),"~EFW.scaled")
            return(lm(for1,data = df))
        },error = function(err){
            return(NULL)
        })
    }),
    model2 = purrr::pmap(list(lhs,year),function(x,y){
        tryCatch({
            df <- df4 %>% dplyr::filter(year == y)
            for2 <- paste(eval(x),"~EFW.scaled + as.factor(cl) - 1")
            return(lm(for2,data = df))
        },error = function(err){
            return(NULL)
        })
    })) %>%
    mutate(glance1 =  purrr::map(model1, broom::glance),
           glance2 =  purrr::map(model2, broom::glance))

mod1 <- reg %>% unnest(glance1)
mod2 <- reg %>% unnest(glance2)

comparison <- inner_join(mod1,mod2,by = c("year","lhs"), suffix = c("1","2")) %>%
    mutate(gain =  adj.r.squared2 - adj.r.squared1) %>% 
  dplyr::filter(!is.na(gain)) %>% 
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

streamlined_comparison0 <- comparison %>%
  ungroup %>%
  dplyr::select(lhs, contains("adj.r.squared"), gain) %>%
  rename(`Variable of interest` = lhs,
         `Change in adjusted R^2` = gain,
         `Baseline model adjusted R^2` = adj.r.squared1,
         `Dummy model adjusted R^2` = adj.r.squared2) 

streamlined_comparison <- comparison %>%
    ungroup %>%
    dplyr::select(contains("adj.r.squared"), gain)

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
  full_join(comp_max) %>% 
  dplyr::select(measure,1:3) 


## Looking at the biggest gains were in the models looking at the income share going to the bottom 10% where the smallest gain was 0.75.

## In general, this provides strong evidence that cluster membership provides information not available in the EFW index itself.
