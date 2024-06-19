library(dplyr)
library(purrr)
library(tidyr)

vegdatele <- read.csv('~/Desktop/vegdatele.csv')

vegdatelezcomp <- vegdatele |> 
  arrange(Site, Sample_set, Meter) |>
  filter(Site == 'Mosaic (Archie Creek)') |>
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

