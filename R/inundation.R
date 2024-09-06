library(tidyverse)
library(cobs)

msl2023 <- read.table('https://tidesandcurrents.noaa.gov/sltrends/data/8726520_meantrend.csv', sep = ',',
                  skip = 6, header = F) %>% 
  select(
    Year = V1, 
    Month = V2, 
    MSL = V3
  ) %>% 
  mutate(
    Date = lubridate::make_date(Year, Month, 1)
  ) %>% 
  filter(Year == 2023) %>% 
  pull(MSL) %>%
  mean() # seems a bit high, LRM site is > 50% below water with this estimate 

# from TBEP #05-19   
scen <- tibble::tribble(
    ~yrs, ~low, ~mid, ~high,
    2000,  0,    0,    0,
    2030,  0.56, 0.79, 1.25,
    2040,  0.72, 1.08, 1.77,
    2050,  0.95, 1.44, 2.56,
    2060,  1.15, 1.87, 3.48,
    2070,  1.35, 2.33, 4.56,
    2080,  1.54, 2.82, 5.71,
    2090,  1.71, 3.38, 7.05,
    2100,  1.90, 3.90, 8.50
  )

yrest <- 2000:2100
# interpolate tidal increase each year using a spline with constant rate increase
scenapprox <- scen %>% 
  pivot_longer(names_to = 'scenario', values_to = 'feet', -yrs) %>% 
  group_nest(scenario) %>% 
  mutate(
    pred = purrr::map(data, function(data){
      
      cobs_fit <- cobs(data$yrs, data$feet, constraint = "increase", nknots = 5)
      cobs_pred <- predict(cobs_fit, nz = length(yrest))

      tibble(
        yrs = yrest, 
        feet = cobs_pred[,2]
      )
  
    })
  ) %>% 
  select(-data) %>% 
  unnest('pred') %>% 
  mutate(
    MSL_m = feet * 0.3048
  ) %>%  
  filter(yrs >= 2023) %>% 
  mutate(
    chg_m = c(0, diff(MSL_m)), 
    chg_m = cumsum(chg_m),
    .by = scenario
  ) %>% 
  mutate(
    MSL_m = chg_m + msl2023,
  ) %>% 
  select(scenario, yrs, MSL_m) #%>%
  # group_nest(yrs, .key = 'MSL_m')

ggplot(scenapprox, aes(x = yrs, y = MSL_m, color = scenario)) + 
  geom_line()

vegdatele <- read.csv(here('data/raw/vegdatele.csv'))

# accretion rates, based on RTK elevation diff per year  
accrt <- vegdatele %>%
  mutate(
    yr = year(mdy(Date))
  ) %>% 
  summarise(
    meanele = mean(Elevation_NAVD88, na.rm = T), 
    .by = c(Site, yr)
  ) %>% 
  summarise(
    elediff = diff(meanele), 
    yrdiff = diff(yr), 
    .by = Site
  ) %>% 
  mutate(
    accrate = elediff / yrdiff
  ) %>% 
  select(Site, accrate)

ele <- vegdatele %>% 
  filter(Sample_set == 3) %>% 
  select(Site, Meter, Elevation_NAVD88) %>% 
  distinct() %>% 
  group_nest(Site) %>% 
  crossing(
    yrs = 2023:2100 
  )

inund <- ele %>% #full_join(ele, accrt, by = 'Site') %>% 
  full_join(scenapprox, by = 'yrs', relationship = 'many-to-many') %>% 
  # mutate(
  #   acc = c(0, cumsum(accrate[-1])),
  #   .by = Site
  # ) %>% 
  # select(-accrate) %>% 
  # mutate(
  #   data = purrr::pmap(list(acc, data), function(acc, data){
  #     
  #     data %>%
  #       mutate(
  #         Elevation_NAVD88 = Elevation_NAVD88 + acc
  #       )
  # 
  #   })
  # ) %>% 
  # select(-acc) %>% 
  unnest(data) %>% 
  summarise(
    perinund = sum(Elevation_NAVD88 < MSL_m) / n(),
    .by = c(Site, yrs, scenario)
  ) 

tmp <- inund %>% 
  filter(yrs %in% c(2023, 2100))

ggplot(tmp, aes(x = Meter, y = Elevation_NAVD88, color = Site, group = Site)) + 
  geom_line() +
  facet_wrap(yrs~scenario) +
  geom_hline(aes(yintercept = MSL_m), linetype = 'dashed') +
  theme_minimal()

ggplot(inund, aes(x = yrs, y = perinund, color = scenario, group = scenario)) +
  geom_line() +
  facet_wrap(~Site) +
  theme_minimal()
