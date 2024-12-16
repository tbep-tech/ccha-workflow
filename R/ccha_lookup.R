library(tidyverse)

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
  select(-Date) %>% 
  distinct()

# write.csv(lookup, '~/Desktop/ccha_lookup.csv', row.names = F)

tojn <- lookup %>% 
  filter(Sample_set == '01') %>% 
  select(Site_code, Zone, Zone_name, Simplified_zone_name) %>%
  distinct() %>% 
  na.omit()
set2 <- read.csv(here::here('data/raw/refset2.csv')) %>% 
  separate_wider_position(Reference, c('Site_code' = 2, 'Sample_set' = 2, 'Zone' = 1)) %>% 
  mutate(
    Zone = toupper(Zone),
    Date = lubridate::mdy(Date),
  ) %>% 
  distinct() %>% 
  mutate(
    Date_start = min(Date),
    Date_end = max(Date), 
    .by = Site_code
  ) %>% 
  select(-Date) %>% 
  distinct() %>% 
  left_join(tojn, by = c('Site_code', 'Zone'))

lookup <- bind_rows(lookup, set2) %>% 
  arrange(Site_code) %>% 
  fill(Site) %>% 
  mutate(
    Reference = paste0(Site_code, Sample_set, Zone),
    Sample_set = as.numeric(Sample_set)
  ) %>% 
  select(Reference, Site, Site_code, Sample_set, Zone, Zone_name, Simplified_zone_name, Date_start, Date_end)

# write.csv(lookup, here::here('~/Desktop/ccha_lookup.csv'), row.names = F)
