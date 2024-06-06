
# R code used to generate datasets used for ground-based comparisons of water storage.
# Running this code is not necessary and is primarily here for documentation purposes.

# Aggregate leaf water content data from TRY database for herbaceous plants ---------

library(taxize)

# get TRY leaf water content data and merge it with a taxonomy data
# base that can let us know families of the species

# The code below is a multi-step process. We first filter the TRY plant trait 
# dataset to only rows with leaf water content data, and then we extract taxonomic 
# information from these data using the taxize package. From there, we only
# retain observations from herbaceous (e.g., grasses, sedges, and forbs) species and 
# exclude locations (x-y coordinates) with less than 10 observations (replicates)
# We want non-woody plants because we are leveraging these leaf water content for 
# ecosystems with a significant (though obviously not exclusive) herbaceous biomass 
# component, specifically croplands, grasslands, savannas, and open shrublands.

#original data downloaded from TRY plant trait database 
#(https://www.try-db.org/TryWeb/Home.php)

#import data
water_content_try <- read.delim('data/supporting_data/14187.txt')

#trim down number of columns
water_content_try <- water_content_try %>% 
  dplyr::select(SpeciesName,DatasetID,ObservationID, DataName,OrigValueStr,
                OrigUnitStr)

#housekeeping: sort data IDs and convert to long form

#isolate lwc,reduce and rename column names,change water content to numeric 
#and round to two decimal places
water_content_try_id <- water_content_try %>%
  dplyr::filter(DataName == 'Leaf water content per leaf dry mass') %>%
  dplyr::select(SpeciesName,DatasetID,ObservationID,DataName,OrigValueStr,
                OrigUnitStr) %>%
  dplyr::rename('water_content' = 'OrigValueStr',
                'units' = 'OrigUnitStr') %>%
  drop_na() %>%
  dplyr::filter(water_content != 'NULL') %>%
  dplyr::mutate_at('water_content',as.numeric) %>%
  dplyr::mutate(water_content = round(water_content,2)) 

#isolate latitude
water_content_try_lat <- water_content_try %>%
  dplyr::filter(DataName == 'Latitude') %>%
  dplyr::select(SpeciesName, DatasetID, ObservationID,OrigValueStr) %>%
  dplyr::rename('Latitude' = 'OrigValueStr') %>%
  dplyr::filter(!DatasetID =='212') #remove 212 which is from 2005

#isolate longitude
water_content_try_lon <- water_content_try %>%
  dplyr::filter(DataName == 'Longitude') %>%
  dplyr::select(SpeciesName, DatasetID, ObservationID,OrigValueStr) %>%
  dplyr::rename('Longitude' = 'OrigValueStr') %>%
  dplyr::filter(!DatasetID =='212') #remove 212 which is from 2005

#merge the lat and lon
lat_lon_try <- merge(water_content_try_lat,water_content_try_lon,
                     by=c('DatasetID','ObservationID','SpeciesName'))

#merge with water content
lat_lon_try_water_content <- merge(lat_lon_try,water_content_try_id,
                                   by=c('DatasetID','ObservationID','SpeciesName'))

#reorder and rename columns
lat_lon_try_water_content <- lat_lon_try_water_content %>%
  dplyr::select("Longitude", "Latitude",'SpeciesName', "water_content",
                "units", "DatasetID",'ObservationID') %>%
  dplyr::rename("x" = "Longitude", "y" = "Latitude")

#cleanup
rm(water_content_try_lat,water_content_try_lon,water_content_try_id,
   lat_lon_try,water_content_try)

#get family names using taxize package (you need to pay attention to this loop)

#create vector of all the unique species in the dataset
species <- unique(lat_lon_try_water_content$SpeciesName)

#loop through each species and get higher-order taxonomic information
species_list<-list()
for(i in species){
  
  try_taxonomy <- tax_name(i,get = c("genus","family","order"))
  species_list[[i]] <- data.frame(try_taxonomy)
  
}

#remove
rm(i,try_taxonomy)

#bind stored taxonomic information for each species into one dataframe
species_df <- do.call("rbind", species_list)
colnames(species_df) <- c('db','SpeciesName','genus','family', 'order')

#merge species dataframe and water content dataframe
species_water_content_merge <- merge(species_df,lat_lon_try_water_content,
                                     by=c('SpeciesName'))

#cleanup
rm(species_list)

#now create a dataframe with list of herbaceous families to be double checked
#outside of R

# get names of all herbaceous families from a separately compiled dataset
herb_family <- read.csv('data/supporting_data/family.list.2.csv')
herb_family <- subset(herb_family,Herb.=='Yes')
family_list <- unique(herb_family$family)

#loop through and get all herb families
herbs_list <- list()

for(i in family_list){
  
  herbs <- subset(species_water_content_merge,family == i)
  herbs_list[[i]] <- herbs
  
}

#convert list to a dataframe
herbs_list_df <- do.call("rbind",herbs_list)

#cleanup
rm(herb_family,i,herbs,herbs_list,species_df)

#create and save a list of mostly 'herb species' to later X2 check which species 
#are herbaceous outside of R

mostly_herb_list <- data.frame(unique(herbs_list_df$SpeciesName))
colnames(mostly_herb_list) <- 'Species'

#running the code below will overwrite the file in the project, which has information
#as to whether each species was truly herbaceous, after double checking.
#write.csv(data/supporting_data/mostly.herb.list,'mostly_herb_X2_check.csv')

#cleanup
rm(mostly_herb_list,herbs_list_df)

#after checking which species are truly herbaceous, you can load in the 
#X2 checked data frame, with an added 'Herbaceous' column to filter
#to just species (and measurements of leaf water content) that are from
#herbaceous plants

#import
herbx2 <- read.csv('data/supporting_data/mostly_herb_X2_check.csv')
herbx2 <- subset(herbx2,Herbaceous. == 'Yes')
herbx2 <- herbx2[c(2)]

#now loop through to get X2 checked herb species
herbx2 <- unique(herbx2$Species)

herbsx2_list <- list()

for(i in herbx2){
  
  herbs <- dplyr::filter(species_water_content_merge,SpeciesName == i)
  herbsx2_list[[i]] <- herbs
  
}

#convert list into datadframe
herbsx2_list_df <- do.call("rbind",herbsx2_list)

#cleanup
rm(i,herbs,herbsx2_list)

#take the means of each coordinate and turn into raster 

#first just do poa coordinate means
try_wc_poa <- species_water_content_merge %>% 
  dplyr::filter(family == 'Poaceae') %>%
  dplyr::mutate(Latitude = round(as.numeric(y),2),
                Longitude = round(as.numeric(x),2)) %>%
  dplyr::select(Latitude,Longitude,SpeciesName,water_content)

# Then do all other herb species coordinate means, binding this with try_wc_poa
# and only retaining sites (coordinates) with least ten measurements (reps)
try_herb_water_content <- herbsx2_list_df %>%
  dplyr::mutate(Latitude = round(as.numeric(y),2),
                Longitude = round(as.numeric(x),2)) %>%
  dplyr::select(Latitude,Longitude,SpeciesName,water_content) %>%
  rbind(.,try_wc_poa) %>%
  dplyr::group_by(Latitude,Longitude) %>%
  dplyr::mutate(sample_size = length(water_content)) %>%
  dplyr::filter(sample_size >= 10) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Latitude,Longitude) %>%
  dplyr::summarise(mean_lwc_g_g = round(mean(water_content),2),
                   min_lwc_g_g = min(water_content,2),
                   max_lwc_g_g = max(water_content,2),
                   sd_lwc_g_g = round(sd(water_content),2),
                   sample_size = mean(sample_size)) %>%
  dplyr::mutate(land_cover = 'grassland/cropland/shrubland',
                site_name = 'not_provided',
                source = "TRY") %>%
  dplyr::select(Latitude,Longitude,site_name,land_cover,mean_lwc_g_g,
                min_lwc_g_g,max_lwc_g_g,sd_lwc_g_g,sample_size,source)

#save to file for later use
write.csv(try_herb_water_content,"data/supporting_data/try_filtered.csv")

#-------------------------------------------------------------------------------
# Import and aggregate Aboveground Biomass Density for year 2010 --------

# this code simply aggregated to the 300 m aboveground dry biomass raster
# 30 x to 9km to match the 9km VOD-based storage data we are using. It will 
# serve as another 'check' on which ground-based coordinates to keep. If a 
# 9km aggregated location is very different (50%) than the native 300 m, then
# we do not retain that ground-based location. 

#original file source of aboveground dry biomass:
#https://uwmadison.app.box.com/s/xj3fnde17yazlogbiq740da2mrv2ma61

#based on this workflow described in this paper:

# Spawn, Seth A., et al. "Harmonized global maps of above and belowground biomass 
# carbon density in the year 2010." Scientific Data 7.1 (2020): 112.

#upload native resolution aboveground biomass density data
aboveground_biomass_density <- 
  raster('data/supporting_data/AFELTON_agbDW_Mgha_x10_300m.tif')

#aggregate 30X to match (approximately) the resolution of the 9 km data
aboveground_biomass_density_30x_aggregate <- 
  raster::aggregate(aboveground_biomass_density,fact=30)

#save to file
writeRaster(aboveground_biomass_density_30x_aggregate,
            'data/supporting_data/aboveground_dry_biomass_density_aggregate_30X.tif')

#cleanup
rm(aboveground_biomass_density,aboveground_biomass_density_30x_aggregate)

#-------------------------------------------------------------------------------
# Aggregate leaf dry matter content data from tundra trait team database ------

# leaf dry matter content are from the tundra trait team dataset, obtained from 
# and described in this paper:

# Bjorkman, Anne D., et al. 
# "Tundra Trait Team: A database of plant traits spanning the tundra biome." 
# Global Ecology and Biogeography 27.12 (2018): 1402-1411.

#import dat
tundra_data <- read.csv('data/supporting_data/TTT_cleaned_dataset.csv')

#data wrangling steps:

# First filter to just leaf dry matter content data, which we can convert to leaf 
# water content (in g h20/g dry biomass), then filter to data beginning in 2016 
# to align, as best we can, with the timescale of the VOD-based storage data 
# (2016-2020). Then convert LDMC (dry/fresh weight) data into leaf water content 
# (water weight/dry weight). We then first average by each species and then for 
# each aggregated lat-lon coordinate. Coordinates with less than ten samples were removed

tundra_lwc_averaged <- tundra_data %>%
  dplyr::filter(Trait == "Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)") %>%
  dplyr::filter(Year > 2015) %>% #filter to latest data when possible
  dplyr::mutate(lwc = ((1 - Value)/Value),
                Latitude = round(Latitude,2),
                Longitude = round(Longitude,2)) %>%
  group_by(Latitude,Longitude,SiteName) %>%
  mutate(sample_size = length(lwc)) %>%
  ungroup() %>%
  dplyr::filter(sample_size >= 10) %>%
  group_by(Latitude,Longitude,SiteName,AccSpeciesName) %>%
  dplyr::summarise(mean_lwc = round(mean(lwc),2),
                   sample_size = mean(sample_size)) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Latitude,Longitude,SiteName) %>%
  dplyr::summarise(mean_lwc_g_g = round(mean(mean_lwc),2),
                   min_lwc_g_g = min(mean_lwc,2),
                   max_lwc_g_g = max(mean_lwc,2),
                   sd_lwc_g_g = round(sd(mean_lwc),2),
                   sample_size = mean(sample_size)) %>%
  dplyr::mutate(land_cover = 'tundra/shrubland') %>%
  dplyr::rename(site_name = SiteName) %>%
  dplyr::mutate(source = "tundra_trait_team") %>%
  dplyr::select(Latitude,Longitude,site_name,land_cover,mean_lwc_g_g,
                min_lwc_g_g,max_lwc_g_g,sd_lwc_g_g,sample_size,source)

#save to file
write.csv(tundra_lwc_averaged,'data/supporting_data/ttt_filtered.csv')

#cleanup
rm(tundra_data,tundra_lwc_averaged)

#-------------------------------------------------------------------------------
# Aggregate live fuel moisture content data from globe-lfmc database -----------

globe_lfmc <- read.csv("data/supporting_data/Globe-LFMC-2.0 final.csv")

# Data wrangling steps:
# first filter rows that match the time-range of VOD data (2016-2020)
# then filter to land cover classifications and plant functional types where leaf
# water content is most likely to reflect vegetation water storage. Then calculate
# avergae LFMC (converted to decimals, akin to g/g LWC), removing coordinates with
# less then 10 measurements (replicates). Note that here and elsewhere, coordinates
# are aggregated/coarsened to two decimal places, making the lat-lons inclusive of
# more potential observations (increasing the potential # of replicates)

globe_lfmc <- globe_lfmc %>%
  dplyr::filter(Sampling.date..YYYYMMDD. > 20151201 & 
                  Sampling.date..YYYYMMDD. < 20201201) %>%
  dplyr::filter(Isolated.data.point  == "FALSE") %>%
  dplyr::filter(IGBP.Land.Cover %in% c("Savannas","Croplands","Grasslands")) %>%
  dplyr::filter(Species.functional.type %in% 
                  c('Sedge','Grass','Graminoid',"Subshrub","Geophyte")) %>%
  dplyr::mutate(Latitude = round(Latitude..WGS84..EPSG.4326.,2),
                Longitude = round(Longitude..WGS84..EPSG.4326.,2),
                lwc = LFMC.value....*.01) %>%
  dplyr::group_by(Latitude,Longitude,State.Region,IGBP.Land.Cover) %>%
  dplyr::mutate(sample_size = length(lwc)) %>%
  dplyr::filter(sample_size >= 10) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(Latitude,Longitude,State.Region,IGBP.Land.Cover) %>%
  dplyr::summarise(mean_lwc_g_g = round(mean(lwc),2),
                   min_lwc_g_g = round(min(lwc),2),
                   max_lwc_g_g = round(max(lwc),2),
                   sd_lwc_g_g = round(sd(lwc),2),
                   sample_size = mean(sample_size)) %>%
  dplyr::rename(land_cover = IGBP.Land.Cover,
                site_name = State.Region) %>%
  dplyr::mutate(source = 'globe-lfmc') %>%
  dplyr::select(Latitude,Longitude,site_name,land_cover,mean_lwc_g_g,
                min_lwc_g_g,max_lwc_g_g,sd_lwc_g_g,sample_size,source)

#save
write.csv(globe_lfmc,"data/supporting_data/globe_lfmc_filtered.csv")

#cleanup
rm(globe_lfmc)

#-------------------------------------------------------------------------------
# Combine all leaf water content data  ----

wc_try <- read.csv('data/supporting_data/try_filtered.csv')
wc_ttt <- read.csv('data/supporting_data/ttt_filtered.csv')
wc_lfmc <- read.csv('data/supporting_data/globe_lfmc_filtered.csv')

wc_binded <- rbind(wc_try,wc_ttt,wc_lfmc)
hist(wc_binded$mean_lwc_g_g)

#save to file
write.csv(wc_binded,'data/supporting_data/leaf_wc_data.csv')

# These data then get cross-checked for land cover type (x-y coordinates) to
# see which sites are retained. For example, if labeled 'grassland' was sorrounded 
# by urban, cropland, or forest land cover types, then this would not be retained
# given the large area - 9km - we hope to compare the ground-based estimates to.
# These data ultimately get joined with a larger dataset of water content. Most 
# recent file: water_content_data_May2024.csv


#-------------------------------------------------------------------------------