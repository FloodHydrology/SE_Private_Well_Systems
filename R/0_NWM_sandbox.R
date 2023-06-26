# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#Title: Sandbox
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 5/29/2023
#Purpose: Develop pipeline to estiamte max inundation for HUC6
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Broad steps: 
# -download HAND data
# -get max Q value for each flowline
# -estimate stage from Q
# -estimate inundation from stage using HAND
# -overlap well users with inundation

#Important Links:
#  Notes Doc: https://docs.google.com/document/d/1CYuf0_NXsuMGG25Sl5jfN3IoKLuPp-NYv30Iuuynl2M/edit
#  Data Doc: https://cfim.ornl.gov/data/
#  Tutorials: https://hydrology.usu.edu/dtarb/giswr/2018/

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 1: Download and unzip data ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Set timeout to five minutes
options(timeout=300)

#download HUC06 data
download.file(
  url = "https://cfim.ornl.gov/data/HAND/20200301/120401.zip", 
  destfile = "data//120401.zip")

#unzip folders
unzip(
  zipfile = "data//120401.zip", 
  exdir= "data"
  )

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Step 2: Get max Q value for each flowline ----------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#load required packages
library(tidyverse)

#load comids
comid <- read_csv("data//120401//hydrogeo-fulltable-120401.csv") %>% 
  select(CatchId) %>% 
  unique()

# -