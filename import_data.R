# import EFW data
efw <- read_excel(
  path = "data/efw-2019-master-index-data-for-researchers.xlsx",
  sheet = "EFW Panel Data 2019 Report"
)
# clean up column names
colnames(efw) <- c("year","iso3c","country.efw","EFW","EFW1","EFW2","EFW3","EFW4","EFW5")

# Calculate EFW quartiles
efw <- efw %>% 
  group_by(year) %>% 
    within(quartile <- as.integer(cut(EFW,quantile(EFW, 
                                                   probs=0:4/4,
                                                 include.lowest=T,
                                                 na.rm=T)))) %>% 
  ungroup() %>% 
    mutate(year = as.integer(year)) %>%
    mutate(quartile = -quartile + 5,
           quartile =  as.integer(quartile))
# scale data before running clustering algorithm
efw_scaled <- efw %>% 
  group_by(year) %>% 
  mutate_if(is.double,scale) %>% 
  ungroup()
# clean up colnames
colnames(efw_scaled) <- c("year","iso3c","country.efw","EFW","EFW1","EFW2","EFW3","EFW4","EFW5","quartile")

# years
years <- sort(unique(efw$year),decreasing = TRUE)
  
# map data
map_coords <- map_data("world") %>%
    as_tibble() %>%
    mutate(iso3c = countrycode(region,"country.name","iso3c"))

# comparison with Huskinson's results
efw_2010 <- efw_scaled %>%
    dplyr::filter(year == 2010)

husk <- read_csv("data/husk_clusters.csv") %>% 
    mutate(cl = `K-means cluster`) %>%
    rename(husk_cluster =  `K-means cluster`) %>%
    rename(country =  Country) %>%
    mutate(cl =  as.integer(as.factor(cl))) %>%
    reset_cluster_order() %>%
    add_count(cl) %>%
    rename(quartile =  "EFW quartile") %>%
    mutate(quartile =
               ifelse(quartile == "Most free",1,quartile),
           quartile =
               ifelse(quartile == "Second",2,quartile),
           quartile =
               ifelse(quartile == "Third",3,quartile),
           quartile =
               ifelse(quartile == "Least free",4,quartile),
           quartile = as.integer(quartile)) %>%
    rename(quartile_husk =  quartile) %>%
    left_join(efw_2010)

rm(efw_2010)

# Freedom House data
# FHurl <- "https://freedomhouse.org/sites/default/files/2020-02/2020_Aggregate_Category_and_Subcategory_Scores_FIW_2003-2020.xlsx"
# download.file(FHurl,"data/Freedom House/FH.xlsx")
fh <- read_excel(path = "data/Freedom House/FH.xlsx",
                     sheet = 2)
fh <- fh[,1:7] %>% 
  rename(country_fh = "Country/Territory",
         region_fh = "Region",
         country_terr = "C/T?",
         year = "Edition",
         overall_FH = "Status",
         political_freedom = "PR Rating",
         civil_liberties = "CL Rating") %>% 
  filter(country_terr == "c") %>% 
  mutate(iso3c = countrycode(country_fh,"country.name","iso3c"))


# Economic Complexity Index data

eci <- read_csv("data/ECI/eci_country_rankings.csv") %>% 
  rename(year = "Year",
         country_eci = "Country",
         country_id_eci = "Country ID",
         eciplus = "ECI+",
         eci = "ECI") %>% 
  mutate(iso3c = countrycode(country_eci,"country.name","iso3c"))


# Human Development Index
hdi <- read_csv("data/HDI/Human development index (HDI).csv",skip=1,na = '..') %>% 
  tidyr::gather("year","hdi",3:31) %>% 
  dplyr::select(-1) %>% 
  dplyr::rename(country = Country) %>% 
  dplyr::mutate(iso3c = countrycode(country,"country.name","iso3c"),
                year = as.integer(year))

# downloaded from http://hdr.undp.org/en/data#
# On June 11th, 2020

# World Bank data
################################################################################
# # Uncomment this block and re-run to re-download the data
# # Otherwise, import the saved version imported below
# # GDP per capita data
# gdppc_ppp <- wb(indicator = "NY.GDP.PCAP.PP.CD") %>% as_tibble() %>%
#   dplyr::mutate(iso3c = countrycode(country,"country.name","iso3c")) %>%
#   dplyr::rename(gdppc_ppp = value) %>%
#   dplyr::select(-indicator,-indicatorID)
# gdppc_ppp_pct_change <- wb(indicator = "NY.GDP.PCAP.PP.KD.ZG") %>% as_tibble() %>%
#   dplyr::rename(gdppc_ppp_pct_change = value) %>%
#   dplyr::select(-indicator,-indicatorID)
# gdppc_pct_change <- wb(indicator = "NY.GDP.PCAP.KD.ZG") %>% as_tibble() %>%
#   dplyr::rename(gdppc_pct_change = value) %>%
#   dplyr::select(-indicator,-indicatorID)
# gdp_data <- gdppc_ppp %>%
#   full_join(gdppc_ppp_pct_change) %>%
#   full_join(gdppc_pct_change)
# rm(gdppc_ppp_pct_change,gdppc_ppp,gdppc_pct_change)
# # Population data
# pop_data <- wb(indicator = "SP.POP.TOTL") %>% as_tibble() %>%
#   dplyr::rename(pop = value) %>%
#   dplyr::select(-indicator,-indicatorID)
# # Income share of bottom 10% of income distribution
# bottom10_data <- wb(indicator = "SI.DST.FRST.10") %>% as_tibble() %>%
#   dplyr::rename(inc_shr_bot10 = value) %>%
#   dplyr::select(-indicator,-indicatorID)
# 
# wb_data <- gdp_data %>%
#   full_join(pop_data) %>%
#   full_join(bottom10_data) %>%
#   dplyr::rename(year = date) %>%
#   mutate(year = as.integer(year)) %>% 
#   dplyr::filter(!is.na(iso3c)) # Drops Kosovo, Eswatini, and a lot of aggregates
# 
# write_csv(wb_data,"data/wb_data_cache.csv")
# rm(gdp_data,bottom10_data,pop_data)
################################################################################
# End of block
################################################################################
wb_data <- read_csv("data/wb_data_cache.csv")
   
## Focus on the data covering the EFW dataset
all_data <- efw %>% 
  full_join(efw_scaled, by = c("year","iso3c","country.efw","quartile"), suffix = c("",".scaled")) %>%  
  left_join(wb_cachelist$countries) %>% dplyr::select(-long,-lat) %>% 
  left_join(eci) %>% left_join(fh) %>% left_join(husk) %>% 
  rename(husk_clusters = cl) %>% 
  left_join(wb_data) %>% 
  left_join(hdi) %>%
  mutate(year = as.integer(year)) %>%
  dplyr::filter(!is.na(EFW))

