# Look at scaled data clustered into 4 groups
# run script0.R first
df4 <- all_clusters %>% dplyr::filter(k==4)

# heatmap_over_time <- df4 %>% 
#   ungroup %>% 
#   dplyr::select(year,iso3c,cl) %>% 
#   filter(!is.na(cl)) %>% 
#   pivot_wider(.,
#               id_cols=iso3c,
#               names_from=year,
#               values_from=cl) %>% 
#   # spread(year,cl) %>% 
#   dplyr::select(-iso3c) %>%
#   as.matrix %>% 
#   heatmap(.,
#           Colv=NA,
#           labRow=unique(df4$iso3c),
#           col = hcl.colors(4, palette = "viridis"))
# heatmap_over_time

df4 %>% 
  # mutate(iso3c = fct_reorder(iso3c,cl)) %>% 
  ggplot(aes(year,iso3c)) + 
  geom_tile(aes(fill=as.factor(cl))) + 
  scale_fill_viridis_d()
