lookup <- select(vegdatelezcomp, Site, Date, Sample_set, Zone_name, Zone, Simplified_zone_name) %>% 
  distinct() %>% 
  mutate(
    Date = lubridate::mdy(Date),
    Date_start = min(Date),
    Date_end = max(Date),
    .by = c(Site, Sample_set)
  ) %>% 
  mutate(
    Site_code = case_when(
      Site == "Cockroach Bay" ~ "CO",
      Site == "Fort De Soto" ~ "FD",
      Site == "Harbor Palms" ~ "HP",
      Site == "Little Manatee River (Mill Bayou)" ~ "LM",
      Site == "Manatee River (Hidden Harbor)" ~ "HH",
      Site == "Mosaic (Archie Creek)" ~ "MO",
      Site == "TECO (Big Bend)" ~ "BB",
      Site == "Upper Tampa Bay Park" ~ "UT",
      Site == "Weedon Island" ~ "WI"
    ), 
    Sample_set = factor(Sample_set , levels = c('1', '2', '3'), labels = c('01', '02', '03'))
  ) %>% 
  complete(
    Sample_set,
    nesting(Site, Site_code)
  )

write.csv(lookup, '~/Desktop/ccha_lookup.csv', row.names = F)
