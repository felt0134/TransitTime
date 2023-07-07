
#clear work environment and set up libraries and functions
rm(list=ls())

#load packages
pkgs <- c('data.table',
          'rstudioapi','tidyverse','sp','rgdal','raster')
lapply(pkgs, library, character.only = TRUE) 

# Set working directory to local directory
current_path <- getActiveDocumentContext()$path
setwd(dirname(current_path))

# Load in-house functions
source('02_Functions2.R')

#small change