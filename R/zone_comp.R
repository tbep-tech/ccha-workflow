library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(here)
library(ggalluvial)

vegdatele <- read.csv(here('data/raw/vegdatele.csv'))

vegdatelezcomp <- vegdatele |> 
  arrange(Site, Sample_set, Meter) |>
  group_nest(Site) |>
  mutate(
    data = map(data, function(x){

      filledzwide <- x |> 
        select(Sample_set, Meter, Simplified_zone_name) |> 
        distinct() |> 
        complete(Sample_set, Meter) |> 
        fill(Simplified_zone_name) |> 
        pivot_wider(names_from = Sample_set, values_from = Simplified_zone_name, values_fn = ~.x[1])

      out <- left_join(x, filledzwide, by = 'Meter')
      
      return(out)
      
    })
  ) |> 
  unnest(data)

# bar plots -----------------------------------------------------------------------------------

dat <- vegdatelezcomp |> 
  select(site = Site, meter = Meter, `1`, `3`) |> 
  pivot_longer(cols = c(`1`, `3`), names_to = 'sample', values_to = 'zonefct') |> 
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

png('~/Desktop/zone_comp_bar.png', width = 9, height = 6, units = 'in', res = 300)
print(p)
dev.off()

# alluvial plots ------------------------------------------------------------------------------

toplo <- dat

cols <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
levs <- levels(toplo$zonefct)
colin <- cols(length(levs))
names(colin) <- levs

p <- ggplot(toplo,
            aes(x = sample, stratum = zonefct, alluvium = meter,
                y = 1, fill = zonefct, label = zonefct)) +
  geom_flow(stat = "alluvium", lode.guidance = "frontback") +
  geom_stratum() +
  scale_fill_manual(values = colin, limits = force) +
  facet_wrap(~site, scales = 'free_y') + 
  scale_x_discrete(expand = c(0.15, 0.15)) +
  theme_minimal() +
  theme(
    legend.position  = 'top', 
    panel.spacing.x = unit(1, 'lines'), 
    axis.text.y = element_blank(), 
    # axis.title.y = element_blank(),
    panel.grid = element_blank()
  ) +
  labs(
    x = "Sample set",
    fill = "Zone name",
    y = 'Relative change'
  )

png('~/Desktop/zone_comp_alluv.png', height = 8, width = 7, res = 300, units = 'in')
print(p)
dev.off()