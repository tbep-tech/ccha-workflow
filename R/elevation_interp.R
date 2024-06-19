library(dplyr)
library(purrr)
library(tidyr)

eledat <- read.csv('~/Desktop/Position.csv')
vegdat <- read.csv('~/Desktop/Transect_vegetation.csv') 

# one site, sample set ------------------------------------------------------------------------

site <- 'Cockroach Bay'
sample_set <- 1

# subset site, sample set
eledatsub <- eledat |>  
  filter(Site == site) |> 
  filter(Sample_set == sample_set) |> 
  filter(Type == 'Transect')
vegdatsub <- vegdat |>  
  filter(Site == site) |> 
  filter(Sample_set == sample_set)

# add elevation by linear interp
vegdatsub <- vegdatsub %>% 
  mutate(
    elevation_m = approx(x = eledatsub$Meter, y = eledatsub$Elevation_NAVD88, xout = Meter)$y
  )

# all sites, sample sets ----------------------------------------------------------------------

# nest veg and ele data by site, sample set
vegdatnest <- vegdat |> 
  group_by(Site, Sample_set) |> 
  nest()
eledatnest <- eledat |> 
  filter(Type == 'Transect') |> 
  group_by(Site, Sample_set) |> 
  nest()

# join nested data by site, sample set, add ele column to veg data, unnest
vegdatele <- vegdatnest |> 
  left_join(eledatnest, by = c('Site', 'Sample_set')) |> 
  filter(!is.na(Sample_set)) |> 
  mutate(
    data.x = pmap(list(data.x, data.y), function(data.x., data.y) {

      elevation_m <- approx(x = data.y$Meter, y = data.y$Elevation_NAVD88, xout = data.x[[1]]$Meter)$y
      data.x[[1]] |> 
        mutate(elevation_m = elevation_m)
      
    })
  ) |> 
  select(-data.y) |> 
  unnest(data.x)

