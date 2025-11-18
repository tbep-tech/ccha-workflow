# setup -------------------------------------------------------------------

box::use(
  dplyr[...], 
  tidyr[...],
  here[...],
  lubridate[...],
  sf[...],
  readxl[read_excel],
  googlesheets4[read_sheet,gs4_deauth],
  googledrive[drive_deauth, drive_ls, drive_download, as_id],
  tibble[enframe]
)

source('R/funcs.R')

gs4_deauth()
drive_deauth()

# transect start stop -------------------------------------------------------------------------

# transect start stop locations to identify landward/waterward
# orig at T:/09_TECHNICAL_PROJECTS/CRITICAL_COASTAL_HABITAT_ASSESSMENT/04_BACKGROUND/OlderVersions/CCHA1_2_Transect_Start_End.xlsx
trnstrstp <- read_excel(here('data/raw/CCHA1_2_Transect_Start_End.xlsx'), skip = 1) %>% 
  select(site = Site, matches('LAT|LONG')) %>% 
  pivot_longer(-site) %>% 
  mutate(
    site = case_when(
      site == 'Archie Creek Mosaic' ~ 'Mosaic', 
      site == 'TECO Big Bend' ~ 'Big Bend - TECO',
      T ~ site
    ),
    loc = case_when(
      grepl('4|5', name) ~ 'water', 
      grepl('6|7',  name) ~ 'land'
    ),
    loc = case_when(
      site %in% 'Fort DeSoto' & loc == 'water' ~ 'land',
      site %in% 'Fort DeSoto' & loc == 'land' ~ 'water', 
      T ~ loc
    ),
    name = gsub('\\.*|\\d', '', name)
  ) %>% 
  pivot_wider() %>% 
  st_as_sf(coords = c('LONG_DD', 'LAT_DD'), crs = 4326)

save(trnstrstp, file = here('data/trnstrstp.RData'))

# elevation data ------------------------------------------------------------------------------

# data from google drive
drv <- 'https://drive.google.com/drive/u/0/folders/1qZBKn2A5hJb8WXBeiKG4FeoEvIUWmRc5'
fls <- drive_ls(drv, type = 'spreadsheet') %>% 
  select(name, id) %>% 
  group_by(name, id) %>% 
  nest() %>% 
  mutate(
    data = purrr::map(id, function(id){
      
      dat <- read_sheet(id)
      
      dat <- dat[, 1:4]
      names(dat) <- c('point', 'northing', 'easting', 'elevation_m')
      
      dat$point <- as.character(dat$point)
      
      return(dat)
      
    })
  )

# rename sites to match vegdat
# remove points not on the transects or top measurements (seen in plot)
# reorder points not in order (by visual inspection)
# reverse those where start is not waterward side
# projection is NAD 83State Plane (Florida west) EPSG 2882
# estimate distance

sitlk <- list(
  `Big Bend TECO Elevation Survey` = "Big Bend - TECO", 
  `Cockroach Bay_10_5_16` = "Cockroach Bay", 
  Fort_Desoto_survey_10_17_2016 = "Fort DeSoto", 
  Harbor_Palms_Park_Oldsmar_9_28_16 = "Harbor Palms", 
  `Hidden Harbor Elevation Survey` = "Hidden Harbor", 
  `Little Manatee River Elevation Survey` = "Little Manatee River", 
  `Mosaic Elevation Survey` = "Mosaic", 
  `UTBP Elevation Survey` = "Upper Tampa Bay Park", 
  Weedon_Island_site_9_21_2016 = "Weedon Island"
) %>% 
  enframe(name = 'name', value = 'site')

# points to remove
ptrmv <- list(
  `Big Bend - TECO` = c('1105', '1106', '1107', '1108', '1109', '1110', '1111', '1112'),
  `Cockroach Bay` = c('co_mon2top', 'co_mon1top'),
  `Fort DeSoto` = c('fdmon1Top', 'fdmon2Top'),
  `Harbor Palms` = c('HPmon2_Top', 'HPmon1_top'),
  `Hidden Harbor` = c('1093'),
  `Little Manatee River` = NA,
  `Mosaic` = c('1', '2', '596', '597', '602', '603', '604', '605', '606'),
  `Upper Tampa Bay Park` = NA,
  `Weedon Island` = c('wiatop', 'wimon')
) %>% 
  enframe(name = 'site', value = 'ptrmv')

# verify start direction from map, NA for asis
ptord <- list(
  `Big Bend - TECO` = as.character(c(1000:1081, 1101, 1082, 1102, 1083, 1084, 1103, 1104, 1085:1100)),
  `Cockroach Bay` = NA,
  `Fort DeSoto` = NA,
  `Harbor Palms` = NA,
  `Hidden Harbor` = NA,
  `Little Manatee River` = NA,
  `Mosaic` = as.character(c(599, 598, 500:517, 556:558, 518:555, 559:584, 586, 585, 587:595)),
  `Upper Tampa Bay Park` = NA,
  `Weedon Island` = NA
) %>% 
  enframe(name = 'site', value = 'ptord')

# T indicates starting point is landward and needs to be reversd
revtyp <- list(
  `Big Bend - TECO` = T,
  `Cockroach Bay` = F,
  `Fort DeSoto` = T,
  `Harbor Palms` = F,
  `Hidden Harbor` = T,
  `Little Manatee River` = F,
  `Mosaic` = T,
  `Upper Tampa Bay Park` = F,
  `Weedon Island` = F
) %>% 
  enframe(name = 'site', value = 'revtyp')  

# T indicates locations are in meters, need to be converted to ft
cnvtyp <- list(
  `Big Bend - TECO` = F,
  `Cockroach Bay` = T,
  `Fort DeSoto` = T,
  `Harbor Palms` = T,
  `Hidden Harbor` = F,
  `Little Manatee River` = F,
  `Mosaic` = F,
  `Upper Tampa Bay Park` = F,
  `Weedon Island` = T
) %>% 
  enframe(name = 'site', value = 'cnvtyp') 

eledat <- fls %>% 
  left_join(sitlk, by = 'name') %>% 
  ungroup() %>% 
  select(site, data) %>%
  unnest('site') %>% 
  left_join(ptrmv, by = 'site') %>% 
  left_join(ptord, by = 'site') %>% 
  left_join(revtyp, by = 'site') %>% 
  left_join(cnvtyp, by = 'site') %>% 
  mutate(
    data = purrr::pmap(list(data, ptrmv, ptord, revtyp,  cnvtyp), function(data, ptrmv, ptord, revtyp, cnvtyp){
      
      out <- data

      # remove points
      if(!anyNA(ptrmv))
        out <- out %>% 
          filter(!point %in% ptrmv)

      # reorder
      if(!anyNA(ptord))
        out <- out[order(match(out$point, ptord)), ]
      
      # reverse
      if(revtyp)
        out <- out[nrow(out):1, ]
      
      # convert m to ft
      if(cnvtyp){
        out$easting <- 3.28084 * out$easting
        out$northing <- 3.28084 * out$northing
      }
      
      # get distances
      streast <- out$easting[1]
      strnort <- out$northing[1]
      out$distance_m <- sqrt((out$northing - strnort)^2 + (out$easting - streast)^2) / 3.28084
      
      return(out)
      
    })
  ) %>% 
  select(site, data) %>% 
  unnest('data') %>% 
  st_as_sf(coords = c('easting', 'northing'), crs = 2882)

# all five sites in first report have elevation as feet, convert to meters
eledat <- eledat %>% 
  mutate(
    sample = 1,
    elevation_m = case_when(
      site %in% c("Mosaic", "Big Bend - TECO", "Hidden Harbor", "Little Manatee River", "Upper Tampa Bay Park") ~ elevation_m / 3.28084, 
      T ~ elevation_m
    )
  ) %>% 
  select(sample, site, distance_m, elevation_m)

# get 2023 data (sample 3, no 2018 elevation data)
tmpfile <- tempfile(fileext = '.xlsx')
drive_download(
  as_id("https://docs.google.com/spreadsheets/d/15x62uLeQpPEDNpLE_7TxCkCZUPdvKRil/edit?gid=2040543076#gid=2040543076"),
  path = tmpfile, 
  overwrite = TRUE, 
  type = "xlsx")
eleraw <- read_excel(tmpfile, sheet = 'RTK', col_types = 'text')
unlink(tmpfile)

eledat2 <- eleraw %>% 
  filter(Type == 'Transect') %>% 
  select(
    ref = Reference, 
    site = Site, 
    distance_m = Meter, 
    lat = Point_N, 
    lon = Point_E, 
    elevation_m = Corrected_NAVD88
  ) %>% 
  mutate_at(vars(distance_m, lon, lat, elevation_m), as.numeric) %>% 
  mutate(
    site = case_when(
      site %in% c('Fort De Soto', 'Fort de Soto') ~ 'Fort DeSoto',
      site == 'Little Manatee River (Mill Bayou)' ~ 'Little Manatee River',
      site == 'Manatee River (Hidden Harbor)' ~ 'Hidden Harbor',
      site == 'Mosaic (Archie Creek)' ~ 'Mosaic',
      site == 'TECO (Big Bend)' ~ 'Big Bend - TECO',
      T ~ site
    ), 
    sample = case_when(
      grepl('01', ref) ~ 1, 
      grepl('03', ref) ~ 3
    ), 
    lon = 3.28084 * lon, 
    lat = 3.28084 * lat
  ) %>% 
  select(-ref) %>% 
  select(sample, everything()) %>% 
  arrange(site, sample, distance_m) %>% 
  st_as_sf(coords = c('lon', 'lat'), crs = 2882) %>% 
  filter(!is.na(elevation_m)) # no sample 1 elevation data

# combine sample 1 and 3, remove dup elevation values at same meter mark (only two)
eledat <- bind_rows(eledat, eledat2) %>% 
  arrange(site, sample, distance_m) %>% 
  summarise(
    across(geometry, st_union),
    elevation_m = mean(elevation_m, na.rm = T), 
    .by = c(sample, site, distance_m)
  )
  
save(eledat, file = here('data/eledat.RData'))

# vegetation data -----------------------------------------------------------------------------

data(eledat)

# raw data, phase 1 and 3
tmpfile <- tempfile(fileext = '.xlsx')
drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1ME0TerfTWLEsOvdOZ95SaXPSGeVZr2aV/edit?gid=974034719#gid=974034719"),
  path = tmpfile, 
  overwrite = TRUE, 
  type = "xlsx")
vegraw1 <- read_excel(tmpfile, sheet = 'Transect vegetation', col_types = c('numeric', 'date', 'text', 'text', 'text', 'numeric', 'text', 'numeric', 'text', 'text', 'text'))
zonraw <- read_excel(tmpfile, sheet = 'Transition locations', col_types = 'text')
unlink(tmpfile)

# raw data, phase 1 and 3
tmpfile <- tempfile(fileext = '.xlsx')
drive_download(
  as_id("https://docs.google.com/spreadsheets/d/1x_ytLD6ro--QzcCClnDvFC04m7PmbVbtReVQkNktyd4/edit?gid=974034719#gid=974034719"),
  path = tmpfile, 
  overwrite = TRUE, 
  type = "xlsx")
vegraw2 <- read_excel(tmpfile, sheet = 'AllVeg', col_types = c('text', 'numeric', 'date', 'text', 'text', 'numeric', 'text', 'text', 'numeric'))
unlink(tmpfile)

vegraw2b <- vegraw2 %>% 
  filter(Sample_Set == 2) %>% 
  select(
    Phase = Sample_Set, 
    Date,
    Site, 
    Zone_name,
    Zone, Meter,
    Vegetation_species,
    pcent_basal_cover = Percent_Basal_Cover
  ) %>% 
  mutate(Date = as.Date(Date))

# combine, minor data edits
vegdat <- vegraw1 %>% 
  mutate(
    Date = as.Date(Date)
  ) %>% 
  bind_rows(vegraw2b) %>% 
  rename(
    species = Vegetation_species, 
    sample = Phase
  ) %>% 
  rename_all(tolower) %>%  
  mutate(
    species = case_when(
      species == 'Acrostichum danaeifolium' ~ 'Acrostichum danaefolium', 
      species == 'Baccahris sp.' ~ 'Baccharis sp.', 
      species == 'Mikania scadens' ~ 'Mikania scandens', 
      species == 'Monoanthochloe littoralis' ~ 'Monanthochloe littoralis', 
      species == 'Myrica cerifera' ~ 'Morella cerifera', 
      species == 'Open water' ~ 'Open Water', 
      species == 'Salicornia virginicus' ~ 'Salicornia virginica',
      species == 'pneumatophore' ~ 'Pneumatophore',
      species %in% c('Unknown 1', 'Unknown vine') ~ 'Unknown',
      species %in% c('Woody Debris', 'none/detritus', 'woody debris', 'Woody debris') ~ 'Woody Debris, none/detritus',
      T ~ species
    ), 
    site = case_when(
      site == 'Fort De Soto' ~ 'Fort DeSoto',
      site == 'Little Manatee River (Mill Bayou)' ~ 'Little Manatee River',
      site == 'Manatee River (Hidden Harbor)' ~ 'Hidden Harbor',
      site == 'Mosaic (Archie Creek)' ~ 'Mosaic',
      site == 'TECO (Big Bend)' ~ 'Big Bend - TECO',
      T ~ site
    ),
    pcent_basal_cover = case_when(
      pcent_basal_cover > 100 ~ NA_real_, 
      pcent_basal_cover < 0 ~ NA_real_, 
      T ~ pcent_basal_cover
    ), 
    zone = toupper(zone),
    yr = year(date),
    zone_name = case_when( # this should be upland transition but labelled as upland
      sample == 1 & zone == 'E' & site == 'Mosaic' ~ 'Coastal upland transition', 
      T ~ zone_name
    ),
    zone_name_simp = case_when( # follows data/raw/vegdatele.csv from KR
      zone_name %in% c("Mangrove Fringe", "Immature Mangrove Fringe", "Tidal Creek", 
                       "Tidal creek", "YM", "Mangrove Mix",
                       "Mangrove", "Short mangrove", "Mangrove mix", "MF", 
                       "Immature mangrove fringe", "Mangrove fringe") ~                                 "Mangrove",
      zone_name %in% c("Brazilian Pepper Berm", "Brazilian pepper berm", "BPB", "Maytenus phyllanthoides", 
                       "Conocarpus erectus", "Schinus terebinthifolius", "Schinus terebinthifolia") ~   "Salt-tolerant trees",
      zone_name %in% c("Salt Marsh", "Juncus Marsh", "Spartina patens", "RF",
                       "Borrichia frutescens", "Juncus roemerianus", "Juncus marsh", 
                       "Spartina alterniflora", "Juncus transition", 
                       "Transitional marsh", "SB") ~                                                    "Salt marsh",
      zone_name %in% c("Salt Barren", "HT", "S", "ST", "Tidal Mud Flat", "Salt Barren Transition", 
                       "Salt barren", "Salt barren by mangrove fringe", "Unvegetated Salt Barren",
                       "Unvegetated salt barren", "High salt barren", "High Salt Barren") ~             "Salt barren",
      zone_name %in% c("Transitional Wetland", "Transitional wetland", "Upland transition", 
                       "FWMT", "Iva frutescens", "Coastal upland (transition)", 
                       "Coastal upland transition", "Dead trees", "Transitional") ~                     "Upland transition",
      zone_name %in% c("U", "Upland", "Coastal Upland", "Coastal upland") ~                             "Coastal upland",
      zone_name %in% c("Channel", "FWP", "Pond", "Fresh Water Pond") ~                                  "Water body",
      TRUE ~ zone_name  # Default case
    )
  ) %>% 
  filter(!is.na(site)) %>% 
  arrange(site, sample, meter) %>% 
  select(
    site, sample, yr, date, zone, zone_name_simp, meter, species, pcent_basal_cover
  )

##
# add elevation data by linear interpolation

# elevation data no geometry
eledatnogeo <- eledat %>% 
  st_set_geometry(NULL) %>% 
  select(sample, site, elevation_m, distance_m) %>% 
  group_by(sample, site) %>% 
  nest() %>% 
  rename(eledat = data)

# join site meter data with elevation data, interpolate
interpele <- vegdat %>% 
  filter(sample != 2) %>% # dont do this for second round of sampling
  select(sample, site,  meter) %>% 
  unique %>% 
  group_by(sample, site) %>% 
  nest() %>% 
  left_join(eledatnogeo, by = c('sample', 'site')) %>% 
  mutate(
    data = purrr::pmap(list(data, eledat), function(data, eledat){

      data %>%
        mutate(
          elevation_m = approx(x = eledat$distance_m, y = eledat$elevation_m, xout = meter)$y
        )

    })
  ) %>% 
  select(-eledat) %>% 
  unnest('data')

# join with vegetation data
vegdat <- vegdat %>% 
  left_join(interpele, by = c('site', 'sample', 'meter'))

# fix zone names, this is still more detail than zone_name_simp
zondat <- zonraw %>% 
  select(
    sample = Phase, 
    site = Site,
    zone_name = `Zone name`,
    zone = Zone
  ) %>% 
  mutate(
    sample = as.numeric(sample),
    site = case_when(
      site == 'Fort De Soto' ~ 'Fort DeSoto',
      site == 'Little Manatee River (Mill Bayou)' ~ 'Little Manatee River',
      site == 'Manatee River (Hidden Harbor)' ~ 'Hidden Harbor',
      site == 'Mosaic (Archie Creek)' ~ 'Mosaic',
      site == 'TECO (Big Bend)' ~ 'Big Bend - TECO',
      T ~ site
    )
  ) %>% 
  group_nest(site) %>% 
  mutate(
    data = purrr::map(data, function(x){
      x %>% 
        select(zone_name, zone) %>% 
        mutate(
          zone_name = capwords(zone_name)
        ) %>% 
        distinct %>% 
        arrange(zone)
    })
  ) %>% 
  unnest(data) %>% 
  mutate(
    zone_name = case_when(
      site == 'Cockroach Bay' & zone_name == 'Spartina Patens' ~ 'Spartina patens',
      site == 'Fort DeSoto' & zone_name == 'Maytenus Phyllanthoides' ~ 'Maytenus phyllanthoides',
      site == 'Fort DeSoto' & zone_name == 'Spartina Patens' ~ 'Spartina patens',
      site == 'Fort DeSoto' & zone_name == 'Borrichia Frutescens' ~ 'Borrichia frutescens',
      site == 'Fort DeSoto' & zone_name == 'Conocarpus Erectus' ~ 'Conocarpus erectus',
      site == 'Harbor Palms' & zone_name == 'Spartina Alterniflora' ~ 'Spartina alterniflora',
      site == 'Harbor Palms' & grepl('Juncus', zone_name) ~ 'Juncus roemerianus',
      site == 'Harbor Palms' & zone_name == 'Iva Frutescens' ~ 'Iva frutescens',
      site == 'Harbor Palms' & zone_name == 'Schinus Terebinthifolius' ~ 'Schinus terebinthifolius',
      site == 'Mosaic' & zone == 'E' ~ 'Coastal Upland Transition', 
      site == 'Upper Tampa Bay Park' & zone_name == 'Fresh Water Pond' ~ 'Pond', 
      site == 'Weedon Island' & zone_name == 'Spartina Alterniflora' ~ 'Spartina alterniflora',
      site == 'Weedon Island' & zone_name == 'Spartina Patens' ~ 'Spartina patens',
      T ~ zone_name
    )
  ) %>% 
  distinct() %>% 
  filter(!zone_name == 'End')

# join correct zone names to vegdat, remove zone names from phase 2 
vegdat <- vegdat %>% 
  left_join(zondat, by = c('site', 'zone')) %>% 
  mutate(
    zone = case_when(
      sample == 2 ~ NA_character_,
      T ~ zone
    ),
    zone_name = case_when(
      sample == 2 ~ NA_character_,
      T ~ zone_name
    ),
    zone_name_simp = case_when(
      sample == 2 ~ NA_character_,
      T ~ zone_name_simp
    )
  )
  
save(vegdat, file = here('data/vegdat.RData'))

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

# transect locations ------------------------------------------------------

tranloc <- st_read(here('data/raw/All_CCHA_Transects.shp')) %>% 
  st_make_valid() %>% 
  mutate(
    site = case_when(
      Name == 'Cockroach Bay (R)' ~ 'Cockroach Bay',
      Name == 'Ft. DeSoto (R)' ~ 'Fort DeSoto',
      Name == 'Harbor Palms (R)' ~ 'Harbor Palms', 
      Name == 'Hidden Harbor (N)' ~ 'Hidden Harbor', 
      Name == 'Little Manatee River (N)' ~ 'Little Manatee River',
      Name == 'Archie Mosaic (N)' ~ 'Mosaic', 
      Name == 'TECO Big Bend (N)' ~ 'Big Bend - TECO',
      Name == 'Upper Tampa Bay Park (N)' ~ 'Upper Tampa Bay Park', 
      Name == 'Feather Sound (R)' ~ 'Weedon Island',
      T ~ NA_character_
    )
  ) %>% 
  filter(!is.na(site)) %>% 
  select(site)

save(tranloc, file = here('data/tranloc.RData'))

# get tampa international daily min temperature --------------------------

noaa_key <- Sys.getenv('NOAA_KEY')

yrs <- 2023:2024

all_temps <- lapply(yrs, function(y){
  cat('Getting year:', y, '\n')
  gettemp_fun(yr = y, noaa_key = noaa_key)
})
