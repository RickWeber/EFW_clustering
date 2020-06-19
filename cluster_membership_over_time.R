
heatmap_over_time <- df4 %>% 
  ungroup %>% 
  dplyr::select(year,iso3c,cl) %>% 
  filter(!is.na(cl)) %>% 
  spread(year,cl) %>% 
  dplyr::select(-iso3c) %>%  
  as.matrix %>% 
  heatmap(.,
          Colv=NA,
          labRow=unique(df4$iso3c),
          col = hcl.colors(4, palette = "viridis"))

df4 %>% 
  ggplot(aes(year,iso3c)) + 
  geom_tile(aes(fill=as.factor(cl))) + 
  scale_fill_viridis_d()
