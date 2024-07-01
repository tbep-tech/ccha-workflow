library(dplyr)
library(tidyr)
library(here)
library(flextable)

elev_stats_zone <- read.csv(here('data/raw/elev_stats_zone.csv'))

#compute summary statistics of each zone type
elev_stats_z <- elev_stats_zone %>%
  group_by(Simplified_zone_name) %>%
  summarise(MinEle_NAV = min(MinEle_NAV, na.rm = T), MaxEle_NAV = max(MaxEle_NAV, na.rm = T),
            MinEle_MTL = min(MinEle_MTL, na.rm = T), MaxEle_MTL = max(MaxEle_MTL, na.rm = T),
            MNEle_NAV = mean(MNEle_NAV, na.rm = T), sdEle_NAV = sd(stEle_NAV, na.rm = T),
            MNEle_MTL = mean(MNEle_MTL, na.rm = T), sdEle_MTL = sd(stEle_MTL, na.rm = T), 
            MinEle_NAV3 = min(MinEle_NAV3, na.rm = T), MaxEle_NAV3 = max(MaxEle_NAV3, na.rm = T),
            MinEle_MTL3 = min(MinEle_MTL3, na.rm = T), MaxEle_MTL3 = max(MaxEle_MTL3, na.rm = T),
            MNEle_NAV3 = mean(MNEle_NAV3, na.rm = T), sdEle_NAV3 = sd(stEle_NAV3, na.rm = T),
            MNEle_MTL3 = mean(MNEle_MTL3, na.rm = T), sdEle_MTL3 = sd(stEle_MTL3, na.rm = T),
            sdchange_NAV = sd(MN_change_NAV, na.rm = T), MN_change_NAV = mean(MN_change_NAV, na.rm = T),
            sdchange_MTL = sd(MN_change_MTL, na.rm = T), MN_change_MTL = mean(MN_change_MTL, na.rm = T),
            n = n())

# ridiculous formatting
totab <- elev_stats_z |> 
  pivot_longer(cols = -c(Simplified_zone_name, n), 
               names_to = 'var', values_to = 'val') |> 
  mutate(
    stat = gsub('^(Min|Max|MN|sd).*$', '\\1', var), 
    var = gsub('^(Min|Max|MN|sd|MN_)', '', var),
    val = sprintf('%.2f', val), 
    n = paste0('(', n, ')')
  ) |> 
  pivot_wider(names_from = stat, values_from = val) |> 
  unite('range', c('Min', 'Max'), sep = '-') |>
  unite('meansd', c('MN', 'sd'), sep = ' +/- ') |> 
  mutate(
    range = paste0('(', range, ')'), 
    range = ifelse(range == '(NA-NA)', '', range),
    meansd = gsub('\\+/- NA', '', meansd)
  ) |> 
  unite('stat', c('meansd', 'range'), sep = ' ') |>
  unite('zone', c('Simplified_zone_name', 'n'), sep = ' ') |> 
  pivot_wider(names_from = var, values_from = stat)
  
# use flextable to format the table
flextable(totab) %>% 
  set_header_labels(values = c('Zone (# sites)', 'Ele_NAV', 'Ele_MTL', 'Ele_NAV3', 'Ele_MTL3', 'change_NAV', 'change_MTL')) %>% 
  autofit() |> 
  font(part = 'all', fontname = 'Times New Roman') |>  
  fontsize(size = 9, part = 'body')
