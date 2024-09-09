library(tidyverse)
library(randomForest)
library(caret)
library(janitor)

torm <- c('Boardwalk', 'Open Water', 'rock', 'Unknown', 'Woody Debris, none/detritus')
data(vegdat)

# species in all three samples
shrspp <- vegdat %>% 
  select(sample, species) %>% 
  distinct() %>% 
  mutate(
    present = 1
  ) %>% 
  pivot_wider(names_from = sample, values_from = present, values_fill = 0) %>% 
  mutate(
    allpresent = ifelse(`1` + `2` + `3` == 3, 1, 0)
  ) %>% 
  filter(allpresent == 1) %>% 
  pull(species) %>% 
  sort()

# prep for rf
trndat <- vegdat %>% 
  filter(species %in% shrspp) %>% 
  filter(sample == 1) %>% 
  select(
    site, zone_name_simp, meter, species, pcent_basal_cover
  ) %>% 
  summarise(
    pcent_basal_cover = mean(pcent_basal_cover, na.rm = T),
    .by = c(site, zone_name_simp, meter, species)
  ) %>% 
  filter(!is.na(pcent_basal_cover)) %>% 
  pivot_wider(names_from = species, values_from = pcent_basal_cover, values_fill = 0) %>% 
  clean_names() %>% 
  mutate_if(is.character, as.factor)

rfmod <- randomForest(zone_name_simp ~ ., data = trndat, importance = TRUE, ntree = 500, keep.inbag = T)

prds <- predict(rfmod)
tmp <- confusionMatrix(prds, trndat$zone_name_simp)
tmp$overall['Accuracy']

tochk <- tomod %>% 
  mutate(
    zone_name_simpprd = predict(rfmod, .)
  ) %>% 
  select(
    site, 
    zone_name_simp, 
    meter,
    zone_name_simpprd
  ) %>% 
  pivot_longer(
    cols = c(zone_name_simp, zone_name_simpprd),
    names_to = 'zone_type',
    values_to = 'zone_name'
  ) %>% 
  mutate(
    zone_type = ifelse(grepl('prd', zone_type), 'Predicted', 'Actual')
  )

ggplot(tochk, aes(x = meter, y = zone_type, color = zone_name)) +
  geom_point() +
  facet_wrap(~site, scales = 'free_x') +
  theme_minimal() +
  theme(legend.position = 'bottom')

# test dataset on phase 3
toprd <- vegdat %>% 
  filter(sample == 3) %>% 
  select(
    site, zone_name_simp, meter, species, pcent_basal_cover
  ) %>% 
  summarise(
    pcent_basal_cover = mean(pcent_basal_cover, na.rm = T),
    .by = c(site, zone_name_simp, meter, species)
  ) %>% 
  pivot_wider(names_from = species, values_from = pcent_basal_cover, values_fill = 0) %>% 
  clean_names() %>% 
  mutate_if(is.character, as.factor)

prds <- predict(rfmod, newdata = toprd)
tmp <- confusionMatrix(prds, toprd$zone_name_simp)
tmp$overall['Accuracy']

tochk <- toprd %>% 
  mutate(
    zone_name_simpprd = prds
  ) %>% 
  select(
    site, 
    zone_name_simp, 
    meter,
    zone_name_simpprd
  ) %>% 
  pivot_longer(
    cols = c(zone_name_simp, zone_name_simpprd),
    names_to = 'zone_type',
    values_to = 'zone_name'
  ) %>% 
  mutate(
    zone_type = ifelse(grepl('prd', zone_type), 'Predicted', 'Actual')
  )

ggplot(tochk, aes(x = meter, y = zone_type, color = zone_name)) +
  geom_point() +
  facet_wrap(~site, scales = 'free_x') +
  theme_minimal() +
  theme(legend.position = 'bottom')
 
# predict sample 2
toprd <- vegdat %>% 
  filter(sample == 2) %>% 
  select(
    site, zone_name_simp, meter, species, pcent_basal_cover
  ) %>% 
  summarise(
    pcent_basal_cover = mean(pcent_basal_cover, na.rm = T),
    .by = c(site, zone_name_simp, meter, species)
  ) %>% 
  pivot_wider(names_from = species, values_from = pcent_basal_cover, values_fill = 0) %>% 
  clean_names() %>% 
  mutate_if(is.character, as.factor)

prds <- predict(rfmod, newdata = toprd)

tomrg <- toprd %>% 
  select(site, meter) %>% 
  mutate(
    site = as.character(site),
    sample = 2,
    zone_name_simp2 = as.character(prds)
  )

vegdat2 <- vegdat %>% 
  group_nest(site, sample, zone_name_simp, meter) %>% 
  left_join(tomrg, by = c('site', 'sample', 'meter')) %>% 
  mutate(
    zone_name_simp = case_when(
      is.na(zone_name_simp) ~ zone_name_simp2, 
      T ~ zone_name_simp
    )
  ) %>% 
  select(-zone_name_simp2) %>%
  unnest('data') %>% 
  arrange(sample, site, meter, species)


vegdatelezcomp <- vegdat2 |> 
  arrange(site, sample, meter) |>
  group_nest(site) |>
  mutate(
    data = map(data, function(x){
      
      filledzwide <- x |> 
        select(sample, meter, zone_name_simp) |> 
        distinct() |> 
        complete(sample, meter) |> 
        fill(zone_name_simp) |> 
        pivot_wider(names_from = sample, values_from = zone_name_simp, values_fn = ~.x[1])
      
      out <- left_join(x, filledzwide, by = 'meter')
      
      return(out)
      
    })
  ) |> 
  unnest(data)

# bar plots -----------------------------------------------------------------------------------

dat <- vegdatelezcomp |> 
  select(site, meter, `1`, `2`, `3`) |> 
  pivot_longer(cols = c(`1`, `2`, `3`), names_to = 'sample', values_to = 'zonefct') |> 
  unique() |> 
  arrange(site, sample, meter) |> 
  group_nest(site, sample) |> 
  mutate(
    data = map(data, function(x){
      
      rle_result <- rle(x$zonefct)
      letters_vector <- letters[1:length(rle_result$values)]
      result_vector <- rep(letters_vector, rle_result$lengths)
      
      x$lets <- result_vector
      
      return(x)
      
    })
  ) |> 
  unnest('data') |> 
  unite('lets', c('sample', 'lets'), sep = '_', remove = F) |> 
  mutate(zonefct = factor(zonefct)) |> 
  filter(!grepl('\\.5$', meter))

toplo <- dat

cols <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
levs <- levels(toplo$zonefct)
colin <- cols(length(levs))
names(colin) <- levs

p <- ggplot(toplo, aes(x = meter, y = sample, group = lets)) + 
  geom_line(aes(color = zonefct), stat = 'identity', lwd = 15, alpha = 0.7) + 
  scale_x_continuous(breaks = seq(0, max(toplo$meter), by = 20), expand = c(0, 0)) +
  guides(color = guide_legend(override.aes = list(lwd = 7))) +
  scale_colour_manual(values = colin, limits = force) +
  facet_wrap(~site, scales = 'free_x', ncol = 3) +
  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(), 
    legend.position = 'top', 
    panel.spacing.x = unit(1, 'lines'), 
    axis.text.x = element_text(size = 7)
  ) +
  labs(
    x = 'Meters', 
    y = 'Sample set', 
    color = 'Zone name'
  )

