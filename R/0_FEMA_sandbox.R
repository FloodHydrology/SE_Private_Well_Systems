#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: FEMA Data Exploration
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 5/29/2023
#Purpose: Develop pipeline to estiamte max inundation for HUC6
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Data Sources: 
#   National Flood Hazard Layer: https://msc.fema.gov/portal/advanceSearch#searchresultsanchor
#   County Shapes: TIGER (via tirgris package download)
#   Private well locations: https://doi.org/10.5066/P9FSLU3B
#   US Census Data via tidycensus package

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 1: Setup workspace -------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory
remove(list=ls())

#Load packages of interest
library(tidyverse)
library(sf)
library(raster)
library(tigris)
library(tidycensus)
library(fasterize)
library(parallel)

#Install API Key
census_api_key(key = Sys.getenv("CENSUS_API_KEY"))

#Load Data
flood    <- st_read("data//II_scratch//NFHL_AL.shp")
wells    <- raster("data//I_data//USGS//REM_map_2010.tif")
state    <- states() %>% filter(NAME == "Alabama")
counties <- counties(state = "AL")

#Tidy data
state_proj <-st_transform(state, crs = st_crs(wells))
wells <- mask(wells, state_proj)
wells <- projectRaster(wells, crs = 4269)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 2: Create function to aggregate data at block level ----------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
flood_fun <- function(county_name = "Tuscaloosa"){
  
    #Install API Key
    census_api_key(key = Sys.getenv("CENSUS_API_KEY"))
    
    #load packages
    library(tidyverse)
    library(sf)
    library(raster)
    library(tigris)
    library(tidycensus)
    library(fasterize)
    
    #identify county shape
    county_shp <- counties(state="AL") %>% filter(NAME == county_name)
    
    #retrieve census block shapes
    blocks <- get_decennial(
      geography   = "block", 
      state       = "AL", 
      county      = county_name,
      variables   = c(
        tot_pop   = 'P010001', 
        white_pop = 'P010003'),
      year = 2010, 
      output = 'wide', 
      geometry = T) %>% 
      mutate(bipoc_pop = tot_pop - white_pop) %>% 
      mutate(GEOID = as.numeric(paste(GEOID)))
    
    #mask well user data to county  and resample to higher resolution
    wells_crop <- crop(wells, county_shp)
    wells_mask <- mask(wells_crop, county_shp)
    wells_resample <- disaggregate(wells_mask, fact=10)/100
    
    #mask flooding to county and convert to points
    flood_county <- flood[county_shp,]
    flood_county <- flood_county %>% 
      dplyr::mutate(flood = 1) %>% 
      dplyr::select(flood)
    flood_county <- fasterize(flood_county, wells_resample, fun = 'max')
    flood_points <- rasterToPoints(flood_county) %>% 
      as_tibble() %>% 
      st_as_sf(., coords = c("x","y"), crs = wells_resample@crs) %>% 
      dplyr::rename(flood=layer)
      
    #Convert well data to points
    well_points <- rasterToPoints(wells_resample) %>% 
      as_tibble() %>% 
      filter(REM_map_2010>0) %>% 
      st_as_sf(., coords = c("x", "y"), crs = wells_resample@crs) 
    
    #create master point file
    pnts <- st_join(well_points, blocks %>% dplyr::select(GEOID)) %>% drop_na()
    pnts <- st_join(pnts, flood_points)
    pnts <- pnts %>% mutate(flood = ifelse(is.na(flood), 0, flood))
  
    #Aggregate pnts to block level
    pnts <- pnts %>% 
      st_drop_geometry() %>% 
      group_by(GEOID, flood) %>% 
      summarise(wells = sum(REM_map_2010, na.rm=T)) %>% 
      mutate(flood = ifelse(flood==0, "wells_dry", "wells_flood")) %>% 
      pivot_wider(
        names_from = flood,
        values_from = wells, 
        values_fill = 0) %>% 
      mutate(wells_tot = wells_dry + wells_flood)
    
    #Export block data
    blocks <- blocks %>% 
      st_drop_geometry() %>% 
      left_join(., pnts) %>% 
      mutate(wells_bipoc = bipoc_pop/tot_pop*wells_tot) %>% 
      mutate(wells_flooded_bipoc = bipoc_pop/tot_pop*wells_flood) %>% 
      mutate(county_name = county_name)
    
    #Export blocks
    blocks
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Step 3: Apply function --------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Prep Wrapper Function ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create list of county names
county_names <- counties %>% st_drop_geometry() %>%  dplyr::select(NAME) %>% pull()

#Create wrapper function (with error catching)
error_fun<-function(n){
  tryCatch(
    expr = flood_fun(county_names[n]), 
    error = function(e)
      tibble(
        GEOID = -9999,
        NAME  = NA,
        tot_pop = -9999,
        white_pop = -9999,
        bipoc_pop = -9999,
        wells_dry = -9999,
        wells_flood = -9999,
        wells_tot = -9999,
        wells_bipoc = -9999,
        wells_flooded_bipoc = -9999,
        county_name = county_names[n]))
}  

#3.2 Prep parallel environs ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# #Determine number of processing cores available on your machine
# n.cores<-detectCores()-1
# 
# #Create clusters
# cl<-makeCluster(n.cores)
# 
# #Send libraries to cluster
# clusterEvalQ(cl, {
#   library(tidyverse)
#   library(sf)
#   library(raster)
#   library(tigris)
#   library(tidycensus)
#   library(fasterize)
# })
# 
# #Export data to cluter environments
# clusterExport(cl, c("flood_fun", "county_names","flood","wells", "counties", "state"))
# #stop cluster
# stopCluster(cl)

#3.3 Apply function in parallel ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#parLapply baby!
output<-lapply(
  seq(1, length(county_names)),
  error_fun)

#bind rows
output <- output %>%  bind_rows()


