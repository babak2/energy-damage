library(sf)
library(glue)
library(tidyverse)

source("graph_functions.R")

# Call damage_map function with desired parameters
damage_map(
  impact_type = 'electricity',
  stat = 'mean', 
  timeperiod = 'endc',
  aggregated = TRUE,
  region_level = 'worldbank'
)
