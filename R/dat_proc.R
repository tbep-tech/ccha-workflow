# setup -------------------------------------------------------------------

box::use(
  dplyr[...], 
  tidyr[...],
  here[...], 
  googlesheets4[read_sheet,gs4_deauth],
  googledrive[drive_deauth]
)

gs4_deauth()
drive_deauth()

# import, format data from Google Drive -----------------------------------

# data here https://drive.google.com/drive/u/0/folders/1ZbpbBfIxb5-BnXhYjymVNMzB88DqZeAO
id <- '1x_ytLD6ro--QzcCClnDvFC04m7PmbVbtReVQkNktyd4'

yr1 <- read_sheet(id, sheet = 'Yr1-transect') %>% 
  mutate(sample = 'one')
yr2 <- read_sheet(id, sheet = 'Yr2-transect') %>% 
  mutate(sample = 'two') %>% 
  rename(meter = Meter)

# combine, minor data edits
cchadat <- bind_rows(yr1, yr2) %>% 
  rename(
    species = Vegetation_species, 
  ) %>% 
  rename_all(tolower) %>% 
  mutate(
    date = as.Date(date), 
    species = case_when(
      species == 'Acrostichum danaeifolium' ~ 'Acrostichum danaefolium', 
      species == 'Baccahris sp.' ~ 'Baccharis sp.', 
      species == 'Mikania scadens' ~ 'Mikania scandens', 
      species == 'Monoanthochloe littoralis' ~ 'Monanthochloe littoralis', 
      species == 'Myrica cerifera' ~ 'Morella cerifera', 
      species == 'Open water' ~ 'Open Water', 
      species == 'Salicornia virginicus' ~ 'Salicornia virginica', 
      species %in% c('Unknown 1', 'Unknown vine') ~ 'Unknown',
      species == 'Woody debris' ~ 'Woody Debris', 
      T ~ species
    ), 
    zone_name = case_when(
      zone_name == 'Salt barren' ~ 'Salt Barren',
      zone_name == 'Schinus terebinthifolia' ~ 'Schinus terebinthifolius',
      T ~ zone_name
    ), 
    pcent_basal_cover = case_when(
      pcent_basal_cover > 100 ~ NA_real_, 
      pcent_basal_cover < 0 ~ NA_real_, 
      T ~ pcent_basal_cover
    )
  ) %>% 
  arrange(site, sample, meter)

save(cchadat, file = here('data/cchadat.RData'))

# cchaloc <- read_sheet(id, sheet = 'ZoneData') %>% 
#   select(
#     site = Site, 
#     lng = longitude, 
#     lat = latitude
#     ) %>% 
#   group_by(site) %>% 
#   summarise(
#     lng = mean(lng), 
#     lat = mean(lat), 
#     .groups = 'drop'
#   )
