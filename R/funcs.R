#' Plot showing number of times a zone was identified across transects, dates
zonecnt_plo <- function(cchadat, thm){
  
  toplo <- cchadat %>%
    select(site, sample, zone_name) %>% 
    unique %>% 
    filter(!is.na(zone_name)) %>% 
    group_by(zone_name) %>% 
    summarize(
      cnt = n(), 
      .groups = 'drop'
    ) %>% 
    arrange(cnt) %>% 
    mutate(
      zone_name = factor(zone_name, levels = zone_name)
    )
  
  p <- ggplot(toplo, aes(y = zone_name, x = cnt)) + 
    geom_bar(stat = 'identity', alpha = 0.7) + 
    thm +
    theme(
      axis.text.y = element_text(size = 8)
    ) +
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, max(toplo$cnt))) +
    labs(
      y = NULL, 
      x = 'Unique counts across sites, dates'
    )
  
  return(p)

}

# Plot showing total distance zone was recorded across transect, dates
zonedst_plo <- function(cchadat, thm){
  
  toplo <- cchadat %>%
    select(site, sample, zone_name, meter) %>% 
    unique %>% 
    filter(!is.na(zone_name)) %>% 
    group_by(site, sample, zone_name) %>% 
    summarize(
      dist = max(meter) - min(meter),
      .groups = 'drop'
    ) %>% 
    group_by(zone_name) %>% 
    summarise(
      dist = sum(dist, na.rm = T)
    ) %>% 
    arrange(dist) %>% 
    mutate(
      zone_name = factor(zone_name, levels = zone_name)
    )
  
  p <- ggplot(toplo, aes(y = zone_name, x = dist)) + 
    geom_bar(stat = 'identity', alpha = 0.7) + 
    thm +
    theme(
      axis.text.y = element_text(size = 8)
    ) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(
      y = NULL, 
      x = 'Total distance recorded, all dates'
    )
  
  return(p)

}

#' site zone distance by date, continuous
sitedst_plo1 <- function(cchadat, site, thm){

  toplo <- cchadat %>% 
    filter(site == !!site) %>% 
    select(zone_name, zone, sample, meter, date) %>% 
    unique %>% 
    unite('sample', sample, date, sep = ': ') %>% 
    mutate(
      sample = factor(sample, levels = rev(unique(sample)))
    ) %>% 
    group_by(sample, zone_name)
  
  levs <- toplo %>% 
    filter(grepl('^1', sample)) %>%
    select(zone_name, zone) %>% 
    unique %>% 
    filter(!duplicated(zone)) %>% 
    pull(zone_name)
  
  toplo <- toplo %>% 
    mutate(
      zone_name = factor(zone_name, levels = levs)
    )
  
  p <- ggplot(toplo, aes(x = meter, y = sample)) + 
    geom_line(aes(color = zone_name), stat = 'identity', lwd = 20, alpha = 0.7) + 
    scale_x_continuous(breaks = seq(0, max(toplo$meter), by = 10)) +
    guides(color = guide_legend(override.aes = list(lwd = 7))) +
    thm +
    theme(
      panel.grid.major.y = element_blank()
    ) +
    labs(
      x = 'Meters', 
      y = NULL, 
      subtitle = paste0('Site: ', site),
      color = 'Zone name'
    )
  
  return(p)

}

#' site zone distance by date, by zone
sitedst_plo2 <- function(cchadat, site, thm){
  
  toplo <- cchadat %>% 
    filter(site == !!site) %>% 
    select(zone_name, zone, sample, meter, date) %>% 
    unique %>% 
    unite('sample', sample, date, sep = ': ') %>% 
    group_by(zone_name, zone, sample) %>% 
    summarize(
      dist = max(meter) - min(meter), 
      .groups = 'drop'
    ) %>% 
    mutate(
      zone_name = fct_reorder2(zone_name, sample, dist)
    )

  p <- ggplot(toplo, aes(x = sample, y = dist, color = zone_name, group = zone_name)) + 
    geom_line(alpha = 0.7) +
    geom_point(size = 3) +
    thm + 
    labs(
      x = NULL, 
      color = 'Zone name', 
      fill = 'Zone name', 
      y = 'Meters',
      subtitle = paste0('Site: ', site)
    )
  
  return(p)

}

#' site zone distance by date, species
sitedst_plo3 <- function(cchadat, site, thm){
  
  tofilt <- c('Unknown', 'Open Water', 'none/detritus', 'Woody Debris')

  toplo <- cchadat %>% 
    filter(site == !!site) %>% 
    filter(!species %in% tofilt)
  
  levs <- toplo %>% 
    filter(grepl('^1', sample)) %>%
    select(zone_name, zone) %>% 
    unique %>% 
    filter(!duplicated(zone)) %>% 
    pull(zone_name)

  toplo <- toplo %>% 
    mutate(
      zone_name = factor(zone_name, levels = levs)
    ) %>% 
    unite('sample', sample, date, sep = ': ')
  
  p <- ggplot(toplo, aes(x = meter, y = species, height = pcent_basal_cover / 100, fill = zone_name, color = zone_name)) + 
    geom_ridgeline(stat = 'identity') + 
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, max(toplo$meter), by = 20)) + 
    thm + 
    # theme(panel.grid.major.y = element_blank()) + 
    labs(
      x = 'Meters', 
      color = 'Zone name', 
      fill = 'Zone name', 
      y = 'Species relative basal cover', 
      subtitle = paste0('Site: ', site)
    ) + 
    facet_wrap(~sample, ncol = 2)
  
  return(p)

}

#' single species summary across sites, zones
sppsum_plo <- function(cchadat, sp, typ = c('fo', 'cover'), thm){
  
  typ <- match.arg(typ)
  
  if(typ == 'fo'){
  
    dgval <- 0
    ylab <- 'Freq. Occ.'
    
    toplo <- cchadat %>% 
      group_by(site, sample, meter) %>% 
      summarise(
        pres = sp %in% species,
        .groups = 'drop'
      ) %>% 
      group_by(site, sample) %>% 
      summarise(
        yval = sum(pres) / n(), 
        .groups = 'drop'
      )
   
  }
    
  if(typ == 'cover'){
    
    dgval <- 0.1
    ylab <- 'Mean % basal cover (+/- 95% CI)'
    
    toplo <- cchadat %>%
      select(site, sample, meter, species, pcent_basal_cover) %>% 
      tidyr::complete(species, tidyr::nesting(site, sample, meter), fill = list(pcent_basal_cover = 0)) %>%
      filter(species %in% !!sp) %>% 
      group_by(site, sample) %>% 
      summarise(
        yval = mean(pcent_basal_cover), 
        lov = t.test(pcent_basal_cover)$conf.int[1], 
        hiv = t.test(pcent_basal_cover)$conf.int[2], 
        .groups = 'drop'
      ) 
  
  }
  
  toplo <- toplo %>% 
    mutate(
      sample = factor(sample),
      site = fct_reorder2(site, sample, yval)
    )

  dodge <- position_dodge(width = dgval) 
  
  p <- ggplot(toplo, aes(x = sample, y = yval, color = site, group = site)) + 
    geom_line(alpha = 0.7, position = dodge) +
    geom_point(size = 3, position = dodge) +
    scale_y_continuous(limits = c(0, NA)) +
    thm + 
    labs(
      x = 'Year', 
      color = 'Site', 
      fill = 'Site', 
      y = ylab,
      subtitle = bquote('Species: ' ~ italic(.(sp)))
    ) 

  if(typ == 'cover')
    p <- p + 
      geom_errorbar(aes(ymin = lov, ymax = hiv), position = dodge, width = 0)

  return(p)
  
}