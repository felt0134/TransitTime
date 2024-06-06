#test
#clear work environment and set up libraries and functions
rm(list=ls())

#load core packages 
project_packages <- c('data.table','rstudioapi','tidyverse','sp','rgdal','raster',
                      'scico', 'cowplot','FNN')
lapply(project_packages, library, character.only = TRUE) 

#set working directory to local directory: file paths will be relative
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

#load custom functions for project
source('02_functions.R')

#import annual transit time and storage data
source('04_annual_turnover_storage_import.r')

#import minimum transit time data
source('05_minimum_turnover_import.r')

# todo:

# 1) Need to go through the generate data script
# 2) need to go throught he pythons cripts and add them to this project
# 3) then need to add this project to zenodo as a zip file
# 4) submit manuscript.

#hello