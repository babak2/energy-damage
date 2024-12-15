# A function for mapping valued damages

damage_map <- function(
    impact_type,
    stat = 'q95',
    timeperiod = 'today',
    aggregated = FALSE,
    region_level = NULL
){
  
  damages <- read.csv("data/national.csv")
  
    if (timeperiod == 'today') {
        period <- '2020'
    } else if (timeperiod == 'midc') {
        period <- '2040'
    } else if(timeperiod == 'endc') {
        period <- '2080'
    } else {
        stop("Invalid timeperiod. Choose 'today', 'midc', or 'endc'.")
    }

  damages = damages %>% 
    filter(impact == impact_type,
           stat == stat,
           period_start == period)
  
  pop_data = read.csv("data/population.csv") %>% 
    mutate(ssp = substr(SCENARIO, 1, 4)) %>% 
    filter(MODEL == 'IIASA-WiC POP' & ssp == 'SSP3') %>% 
    select(-SCENARIO, - UNIT, -VAR1, -VAR2, -VAR3, -VAR4) %>% 
    relocate(ssp, .after = REGION) %>% 
    pivot_longer(4:44, names_to = "year_raw", values_to = "pop_in_mil") %>% 
    mutate(year = str_remove(year_raw, 'X'),
           pop = pop_in_mil*1000000) %>% 
    filter(year == period) %>% 
    select(REGION, pop)
  
  world_map <- st_read("data/ne_50m_admin_0_countries_lakes/ne_50m_admin_0_countries_lakes.shp")
  
  damage_map = world_map %>% 
    left_join(damages, by=c('ADM0_A3'='iso')) %>% 
    left_join(pop_data, by=c('ADM0_A3'='REGION'))
  
  if(aggregated){
    macro_regions = read.csv('data/macro-regions_v1.csv')
    
    damage_map = damage_map %>% 
      left_join(macro_regions, by = c('ADM0_A3' = 'region.key'))
    
    na_map = damage_map %>% 
      filter(is.na(damages) | is.na(!!sym(region_level)) | is.na(pop))
    
    damage_map = damage_map %>% 
      group_by(FUND) %>% 
      mutate(damages = mean(damages/pop, na.rm = TRUE)/1e9) %>% 
      ungroup() %>% 
      {if(region_level == 'FUND') mutate(., label = glue("{str_to_upper(get(region_level))}: {round(damages, 2)}"))
        else mutate(., label = glue("{str_to_title(get(region_level))}: {round(damages, 2)}"))} %>% 
      mutate(damages = factor(damages, levels = sort(unique(damages)), ordered = TRUE)) %>% 
      arrange(damages)
    
	
    damage_map_data_cln <- damages %>%
        filter(impact == impact_type, stat == stat, period_start == period)

		
    labels <- as.vector(unique(damage_map_data_cln$iso))
    #labels = as.vector(unique(damage_map_data_cln$label))
    
    color.values = colorRampPalette(rev(c("#c92116", "#ec603f", "#fd9b64",
                                          "#fdc370", "#fee69b","#fef7d1", "#f0f7d9")))(length(labels))
    

    if(region_level == 'worldbank') {
      legend = "Damages by Worldbank Income Level (billion USD 2005):"

      
    } else if(region_level == 'FUND') {
      legend = "Average Damages per Capita by FUND Region:"
      
    }
    
    map_name = glue('{impact_type}_val_map_{region_level}.pdf')
    
  } else { # if not aggregated
    
    na_map <- damage_map %>% 
      filter(is.na(damages) | is.na(pop))
    
    damage_map <- damage_map %>% 
      filter(!is.na(damages) & !is.na(pop)) %>% 
      mutate(damages = damages/pop)
    
    color.values = rev(c("#c92116", "#ec603f", "#fd9b64",
                         "#fdc370", "#fee69b","#fef7d1", "#f0f7d9"))
    
    legend = "Country-wide damages per capita:"
    
    map_name = glue('{impact_type}_val_map.pdf')
  }
  
  na.color = "grey85"

  damage_map <- damage_map %>%
	  filter(!is.na(damages))

   #print("summary(damage_map$damages:")
   #print(summary(damage_map$damages))
  
  elec_damage_map <- ggplot(damage_map) +
    geom_sf(aes(fill=damages), color = NA) +
    coord_sf(datum=NA) +
    geom_sf(data = na_map, fill = na.color, color = 'black') +
    theme(legend.position = "bottom") 

  #print("elec_damage_map:")
  #print(elec_damage_map)

  
  if(aggregated){
    print("legend:")
    print(legend)
    print("color.values:")
    print(color.values)
    print("labels:")
    print(labels)

   if (length(labels) != length(color.values)) {
        stop("Mismatch between number of labels and color values.")
    }
	
    elec_damage_map = elec_damage_map +
      scale_fill_manual(name = legend,
                        values = color.values,
                        labels = labels)
  	  
  } else {
    elec_damage_map = elec_damage_map +
      scale_fill_gradientn(name = legend,
                           colors = color.values)
  }

  #print("elec_damage_map:")
  #print(elec_damage_map)
  
  #print("summary(damage_map$damages) Before ggsave : ")
  #print(summary(damage_map$damages))
	  
  ggsave(map_name,
         plot = elec_damage_map,
         width=7,
         height=7)
  
}



