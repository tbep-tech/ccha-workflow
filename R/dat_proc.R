# setup -------------------------------------------------------------------

box::use(
  dplyr[...], 
  tidyr[...],
  here[...],
  lubridate[...],
  googlesheets4[read_sheet,gs4_deauth],
  googledrive[drive_deauth]
)

gs4_deauth()
drive_deauth()

# import, format data from Google Drive -----------------------------------

# data here https://drive.google.com/drive/u/0/folders/1ZbpbBfIxb5-BnXhYjymVNMzB88DqZeAO
id <- '1x_ytLD6ro--QzcCClnDvFC04m7PmbVbtReVQkNktyd4'

# do not use individual year sheets because the zone names are not corrected
vegraw <- read_sheet(id, sheet = 'AllVeg', col_types = 'cdcccdccd')

# combine, minor data edits
vegdat <- vegraw %>% 
  rename(
    species = Vegetation_species, 
    sample = `Sample_Set`
  ) %>% 
  rename_all(tolower) %>% 
  rename(pcent_basal_cover = percent_basal_cover) %>% 
  select(-tree_class) %>% 
  mutate(
    date = gsub('\\s\\(Part\\s2\\)$', '', date),
    date = mdy(date), 
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
      species %in% c('Woody Debris', 'none/detritus') ~ 'Woody Debris, none/detritus',
      T ~ species
    ), 
    zone_name = case_when(
      zone_name == 'Coastal upland' ~ 'Coastal Upland',
      zone_name == 'Juncus marsh' ~ 'Juncus Marsh', 
      zone_name == 'Schinus terebinthifolia' ~ 'Schinus terebinthifolius', 
      zone_name == 'Salt barren' ~ 'Salt Barren',
      zone_name == 'Schinus terebinthifolia' ~ 'Schinus terebinthifolius',
      zone_name == 'Transitional marsh' ~ 'Transitional Marsh',
      T ~ zone_name
    ), 
    pcent_basal_cover = case_when(
      pcent_basal_cover > 100 ~ NA_real_, 
      pcent_basal_cover < 0 ~ NA_real_, 
      T ~ pcent_basal_cover
    )
  ) %>% 
  filter(!is.na(site)) %>% 
  arrange(site, sample, meter)

save(vegdat, file = here('data/vegdat.RData'))

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

# get tree data -----------------------------------------------------------

# data here https://docs.google.com/spreadsheets/d/1gvIqr6aPD_-hKKi7ZQvLcBL_h3jg7XIbhYEEVWWbFzU/edit#gid=1492814788
id <- '1gvIqr6aPD_-hKKi7ZQvLcBL_h3jg7XIbhYEEVWWbFzU'

treeraw <- read_sheet(id, sheet = 'PCQ calculations',  col_types = 'cdcccdccdddddddddddddcddddd')

treedat <- treeraw %>% 
  select(
    site = Site, 
    sample = Period, 
    date = Date, 
    zone_name = `Zone name`, 
    zone = Zone, 
    plot = Plot, 
    pcq_direction = `PCQ direction`, 
    species = `Tree species`,
    dist_to_tree_m = `Dist to tree (m)`, 
    dbh_cm = `DBH (cm)`, 
    tree_height = `Tree height (m)`, 
    eye_height_m = `Eye Height (m)`, 
    dist_from_tree_m = `Dist. From Tree (m)`, 
    angle = `Angle (degrees)`
  ) %>% 
  mutate(
    date = mdy(date)
  )

save(treedat, file = here('data/treedat.RData'))

