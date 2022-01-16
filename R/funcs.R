#' Plot showing number of times a zone was identified across transects, dates
zonecnt_plo <- function(vegdat, thm){
  
  toplo <- vegdat %>%
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
zonedst_plo <- function(vegdat, thm){
  
  toplo <- vegdat %>%
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
sitezonedst_plo1 <- function(vegdat, site, thm){

  toplo <- vegdat %>% 
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
sitezonedst_plo2 <- function(vegdat, site, thm){
  
  toplo <- vegdat %>% 
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
sitezonedst_plo3 <- function(vegdat, site, thm){
  
  tofilt <- c('Unknown', 'Open Water', 'none/detritus', 'Woody Debris')

  toplo <- vegdat %>% 
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

#' summarize species at a site, zone optional, used for tabular or graphical summary
sitezonesum_fun <- function(vegdat, site, zone_name = NULL, typ = c('fo', 'cover')){
  
  typ <- match.arg(typ)
 
  dat <- vegdat %>% 
    filter(site %in% !!site) 
  
  # make sure zone inputs are found at site
  if(!is.null(zone_name)){
    
    zns <- unique(dat$zone_name)
    chk <- zone_name[!zone_name %in% zns] %>% 
      paste(collapse = ', ')
    
    if(nchar(chk) != 0)
      stop(chk, ' zones not in ' , site, ', must be one of ', paste(zns, collapse = ', '))
    
    dat <- dat %>% 
      filter(zone_name %in% !!zone_name) 
    
  }
  
  # get complete data by filling species as zero
  dat <- dat %>% 
    select(site, sample, meter, zone, zone_name, species, pcent_basal_cover) %>%
    tidyr::complete(species, tidyr::nesting(site, sample, zone, zone_name, meter), fill = list(pcent_basal_cover = 0))
  
  # freq occ estimates
  if(typ == 'fo')

    out <- dat %>%
      mutate(
        pa = ifelse(pcent_basal_cover > 0, 1, 0)
      ) %>%
      select(-pcent_basal_cover) %>%
      unique %>%
      group_by(site, sample, zone, zone_name, species) %>%
      summarise(
        yval = sum(pa) / n(),
        .groups = 'drop'
      )

  # % basal cover estimates    
  if(typ == 'cover')
    
    out <- dat %>% 
      unique %>% 
      group_by(site, sample, zone, zone_name, species) %>% 
      summarise(
        yval = mean(pcent_basal_cover) / 100, 
        .groups = 'drop'
      )
  
  out <- out %>% 
    filter(yval > 0)
     
  return(out)
  
}

# tabular output from sitezonesum_fun
sitezonesum_tab <- function(vegdat, site, zone = NULL, typ = c('fo', 'cover')){

  typ <- match.arg(typ)
  
  totab <- sitezonesum_fun(vegdat, site, zone, typ)
  
  ylab <- 'Mean basal % cover'
  if(typ == 'fo')
    ylab <- '% Frequency Occurrence'

  totab <- totab %>% 
    mutate(
      sample = paste('Phase', sample)
    ) %>% 
    pivot_wider(names_from = 'sample', values_from = 'yval', values_fill = NA) %>%
    arrange(zone, species) %>% 
    unite('zone_name', zone, zone_name, sep = ': ')
  
  tab <- reactable(
    totab,
    groupBy = c('zone_name'),
    columns = list(
      site = colDef(show = F),
      zone_name = colDef(name = 'Zone', minWidth = 200),
      species = colDef(name = 'Species', minWidth = 200)
    ), 
    defaultColDef = colDef(format = colFormat(digits = 1, percent = T)), 
    resizable = T, 
    defaultExpanded = F
    )
  
  return(tab)
  
}


#' summarize species at a site, across zones, used for tabular or graphical summary
sitesum_fun <- function(vegdat, site, delim, top, zone_name = NULL, torm = c('none/detritus', 'Open Water', 'Boardwalk')){
  
  dat <- vegdat %>% 
    filter(site %in% !!site) %>% 
    select(site, sample, meter, zone, zone_name, species, pcent_basal_cover) %>%
    tidyr::complete(species, tidyr::nesting(site, sample, zone, zone_name, meter), fill = list(pcent_basal_cover = 0)) %>% 
    filter(!species %in% torm) %>% 
    mutate(species = factor(species))

  delims <- unique(dat$meter) %>% 
    as.numeric %>% 
    range %>% 
    {seq(.[1], .[2], length.out = delim + 1)}
  
  lbs <- round(delims, 0)[-length(delims)]
  
  # make sure zone inputs are found at site
  if(!is.null(zone_name)){
    
    zns <- unique(dat$zone_name)
    chk <- zone_name[!zone_name %in% zns] %>% 
      paste(collapse = ', ')
    
    if(nchar(chk) != 0)
      stop(chk, ' zones not in ' , site, ', must be one of ', paste(zns, collapse = ', '))
    
    dat <- dat %>% 
      filter(zone_name %in% !!zone_name) 
    
  }

  sums <- dat %>% 
    mutate(
      meter_grp = cut(meter, breaks = delims, labels = lbs, include.lowest = T, right = F)
    ) %>% 
    group_by(sample, species, meter_grp) %>% 
    summarize(yval = sum(pcent_basal_cover), .groups = 'drop') %>% 
    filter(yval > 0)
  
  maxout <- sums %>% 
    group_by(species) %>% 
    summarise(yval = sum(yval)) %>% 
    arrange(-yval) %>% 
    pull(species) %>% 
    .[1:top]
  
  out <- sums %>% 
    filter(species %in% maxout)
  
  return(out)

}

# plot results for sitesum_fun
sitesum_plo <- function(vegdat, site, delim, top, zone_name = NULL){
  
  toplo <- sitesum_fun(vegdat, site, delim, top, zone_name) %>% 
    mutate(
      sample = paste0('Phase ', sample)
    )
  
  cols <- RColorBrewer::brewer.pal(9, 'Set1') %>% 
    colorRampPalette(.)

  top <- min(c(top, length(unique(toplo$species))))
  leglab <- paste('Top', top, 'species')
  
  levs <- levels(toplo$species)
  colin <- cols(length(levs))
  names(colin) <- levs
  
  p <- ggplot(toplo, aes(x = meter_grp, y = yval, fill = species)) + 
    geom_bar(stat = 'identity', color = 'black') + 
    scale_x_discrete(drop = F) +
    scale_fill_manual(values = colin, limits = force) +
    facet_wrap(~sample, ncol = 1, drop = F) + 
    thm + 
    labs(
      y = 'Sum of basal % cover', 
      x = 'Meter distance', 
      fill = leglab
    )
  
  return(p)
  
}

#' single species summary across sites, zones
sppsum_plo <- function(vegdat, sp, typ = c('fo', 'cover'), thm){
  
  typ <- match.arg(typ)
  
  if(typ == 'fo'){
  
    dgval <- 0
    ylab <- 'Freq. Occ.'
    
    toplo <- vegdat %>% 
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
    
    toplo <- vegdat %>%
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

#' summarise tree plot data into species by zone or just by zone
treesum_fun <- function(treedat, site, byspecies = T){
  
  # summarize by plot in each zone first, then density of trees in the zone
  # this is used to get species densities in each zone
  zonedens <- treedat %>% 
    filter(site %in% !!site) %>% 
    group_by(site, sample, zone_name, zone, plot) %>%
    summarize(
      trees_m2 = 12 / pi / sum(dist_to_tree_m ^ 2, na.rm = T),
      cnt = n(),  
      .groups = 'drop'
    ) %>% 
    mutate(
      trees_m2 = case_when( # correction factor if four points were not sampled
        cnt == 4 ~ trees_m2, 
        cnt == 3 ~ trees_m2 * 0.58159, 
        cnt == 2 ~ trees_m2 * 0.3393,
        cnt == 1 ~ trees_m2 * 0.15351
      )
    ) %>% 
    group_by(site, sample, zone_name, zone) %>% 
    summarise(
      trees_m2 = mean(trees_m2, na.rm = T), 
      .groups = 'drop'
    )
  
  # get species density summaries by zone
  # uses results from above
  zonesppsum <- treedat %>% 
    filter(site %in% !!site) %>% 
    group_by(site, sample, zone_name, zone) %>%
    mutate(
      dbh_cm_gr0 = sum(dbh_cm > 0), 
      ba_cm2 = pi * (dbh_cm / 2) ^ 2, 
      ba_cm2sum = sum(ba_cm2, na.rm = T)
    ) %>% 
    group_by(site, sample, zone_name, zone, species) %>%
    inner_join(zonedens, by = c('site', 'sample', 'zone_name', 'zone')) %>% 
    summarise(
      trees_m2 = unique(trees_m2) * n() / unique(dbh_cm_gr0), 
      cm2_m2 = mean(ba_cm2), 
      relcov_per = 100 * sum(ba_cm2) / unique(ba_cm2sum),
      .groups = 'drop'
    ) %>% 
    mutate(
      trees_ha = trees_m2 * 1e4, 
      m2_ha = trees_ha * cm2_m2 / 1e4
    ) %>% 
    pivot_longer(names_to = 'var', values_to = 'val', -matches(c('site', 'sample', 'zone_name', 'zone', 'species'))) %>% 
    mutate(
      varlab = case_when(
        var == 'trees_m2' ~ 'Absolute species density (trees/m2)', 
        var == 'cm2_m2' ~ 'Species average basal area (cm2/m2)', 
        var == 'relcov_per' ~ 'Relative cover (%)', 
        var == 'trees_ha' ~ 'Absolute species density (trees/ha)', 
        var == 'm2_ha' ~ 'Species absolute cover (m2/ha)'  
      )
    )
  
  out <- zonesppsum
  
  # summarise the above across zone
  if(!byspecies){

    richdat <- out %>%  
      group_by(site, sample, zone_name, zone) %>%
      summarise(
        val = length(unique(species)), 
        .groups = 'drop'
      ) %>% 
      mutate(
        var = 'rich', 
        varlab = 'Species richness'
      )
    
    out <- out %>% 
      group_by(site, sample, zone_name, zone, var, varlab) %>% 
      summarise(
        val = sum(val, na.rm = T),
        .groups = 'drop'
      ) %>% 
      bind_rows(richdat) %>% 
      arrange(site, sample, zone)
    
  }
  
  return(out)
  
}