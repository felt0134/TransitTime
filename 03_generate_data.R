
# R code used to generate datasets used for ground-based comparisons of water storage.
# Running this code is not necessary and is primarily here for documentation purposes.

library(taxize)

# Get leaf water content data for grassland land cover ---------

# get TRY leaf water content data and merge it with a taxonomy data
# base that can let us know families of the species

#load original data downloaded from TRY plant trait database 
#(https://www.try-db.org/TryWeb/Home.php)

water_content_try <- read.delim('data/supporting_data/14187.txt')

#trim down number of columns
water_content_try <- water_content_try %>% 
  dplyr::select(SpeciesName,DatasetID,ObservationID, DataName,OrigValueStr,
                OrigUnitStr)

#sort data IDs and convert to long form

#isolate ldmc,reduce and rename column names,change water content to numeric 
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
  dplyr::filter(!DatasetID =='212') #remove 212

#isolate longitude
water_content_try_lon <- water_content_try %>%
  dplyr::filter(DataName == 'Longitude') %>%
  dplyr::select(SpeciesName, DatasetID, ObservationID,OrigValueStr) %>%
  dplyr::rename('Longitude' = 'OrigValueStr') %>%
  dplyr::filter(!DatasetID =='212') #remove 212

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

species <- unique(lat_lon_try_water_content$SpeciesName)

species_list<-list()
for(i in species){
  
  try_taxonomy <- tax_name(i,get = c("genus","family","order"))
  species_list[[i]] <- data.frame(try_taxonomy)
  
}

rm(i,try_taxonomy)

species_df <- do.call("rbind", species_list)

colnames(species_df) <- c('db','SpeciesName','genus','family', 'order')

#merge species dataframe and water content dataframe
species_water_content_merge <- merge(species_df,lat_lon_try_water_content,
                                   by=c('SpeciesName'))

#cleanup
rm(species_list)


#Now create a dataframe with list of herbaceous families to be double checked

#set projection
projection <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

# get names of all herbaceous families from a separately compiled dataset
herb_family <- read.csv('data/supporting_data/family.list.2.csv')
herb_family <- subset(herb_family,Herb.=='Yes')
family_list <- unique(herb_family$family)


#loop through and get all herb families
herbs_list <- list()

for(i in family.list){
  
  herbs <- subset(species_water_content_merge,family == i)
  herbs_list[[i]] <- herbs
  
}

#convert list to a dataframe
herbs_list_df <- do.call("rbind",herbs_list)

#cleanup
rm(herb_family,i,herbs,herbs_list,species_df)

#create and save a list of mostly 'herb species' to later X2 check which species 
#are herbaceous

mostly_herb_list <- data.frame(unique(herbs_list_df$SpeciesName))
colnames(mostly_herb_list) <- 'Species'

#running the code below will overwrite the file in the project, which has information
#as to whether each species was truly herbaceous, after double checking.
#write.csv(data/supporting_data/mostly.herb.list,'mostly_herb_X2_check.csv')

#cleanup
rm(mostly_herb_list,herbs_list_df)

#after checking which species are truly herbaceous, you can load in the 
#X2 checked data frame, with an added 'Herbaceous' column.
herbx2 <- read.csv('data/supporting_data/mostly_herb_X2_check.csv')
herbx2 <- subset(herbx2,Herbaceous. == 'Yes')
herbx2 <- herbx2[c(2)]

#now loop through to get X2 checked herb species
herbx2 <- unique(herbx2$Species)

herbsx2_list <- list()

for(i in herbx2){
  
  herbs <- subset(species_water_content_merge,SpeciesName == i)
  herbsx2_list[[i]] <- herbs
  
}

#convert list into datadframe
herbsx2_list_df <- do.call("rbind",herbsx2_list)

#cleanup
rm(i,herbs,herbsx2_list)

#take the means of each coordinate and turn into raster 

#just poa coordinate means
derived_wc_poa_mean <- species_water_content_merge %>% 
  filter(family == 'Poaceae') %>%
  group_by(x,y) %>%
  summarise(water_content = mean(water_content))

#all other herb coordinate means
derived_wc_mean_herbs <- aggregate(water_content~x+y,mean,data=herbsx2_list_df)

#bind Poa and all other herbaceous species (pixel averages) to have all herb species
derived_wc_mean <- rbind(derived_wc_poa_mean,derived_wc_mean_herbs)

# do a final average because of overlapping pixels
derived_wc_mean_2 <- aggregate(water_content~x+y,mean,data=derived_wc_mean)

#cleanup
rm(derived_wc_mean,derived_wc_mean_herbs,derived_wc_poa_mean,herbsx2_list_df)

#use nearest spatial points to filter to actual grassland land cover type

#import storage data (if you have not already)
source('04_annual_turnover_storage_import.r')

#ensure all variables are numeric
derived_wc_mean_2 <- derived_wc_mean_2 %>%
  dplyr::mutate_at('x',as.numeric) %>%
  dplyr::mutate_at('y',as.numeric)
  
#turn ground data to spdf
coords_ground <- derived_wc_mean_2[ , c("x", "y")]   # coordinates
data_ground   <- data.frame(derived_wc_mean_2[c(3)])          # data
crs <- CRS('+proj=longlat +datum=WGS84 +no_defs') # proj4string of coords

spdf_ground <- SpatialPointsDataFrame(coords = coords_ground,
                                      data = data_ground, 
                                      proj4string = crs)

#turn vod raster data to spatial points DF
annual_strorage_2 <- annual_turnover_lc %>%
  dplyr::select(lon,lat,annual_storage,group,group_2)

rownames(annual_strorage_2) <- NULL
rm(annual_turnover_lc)

# prepare coordinates, data, and proj4string
coords_vod <- annual_strorage_2[ , c("lon", "lat")]   # coordinates
data_vod   <- annual_strorage_2[ , 3:5]          # data

# make the SpatialPointsDataFrame object
spdf_vod <- SpatialPointsDataFrame(coords = coords_vod,
                                   data = data_vod, 
                                   proj4string = crs)

#link ground-based coordinates to vod coordinates for storage
nn1 = get.knnx(coordinates(spdf_vod), coordinates(spdf_ground), 1)
vector <- data.frame(nn1[1])
vector <- vector[c(1:nrow(vector)),]
spdf_vod_df <- data.frame(spdf_vod)
new_df <- spdf_vod_df[c(vector),]
new_df <- new_df %>%
  dplyr::select(annual_storage,group,lon,lat)

#check that it worked
#points(lat~lon,data=new_df,add=T,col='red')

#bind and filter to crop and grassland
cbind_ground_vod_herb <- cbind(new_df,derived_wc_mean_2)

cbind_ground_vod_herb <- cbind_ground_vod_herb %>%
  dplyr::filter(group == c('Cropland','Grassland')) %>%
  dplyr::select(x,y,water_content,group)

rownames(cbind_ground_vod_herb) <- NULL

#below creates a file for double checking that sites are intact grasslands and then 
#incorporate those sites into main water content dataframe. The code is commented
#out because it is primarily for documentation purposes.

# write.csv(cbind_ground_vod_herb,
#           'Data/supporting_data/water.content.try/crop_grassland_mean_water_content.csv')

#cleanup
rm(annual_strorage_2,cbind_ground_vod_herb,coords_ground,coords_vod,crs,
   data_ground,data_vod,derived_wc_mean_2,lc_id,new_df,nn1,spdf_ground,
   spdf_vod,spdf_vod_df,species_water_content_merge)

#-------------------------------------------------------------------------------
# Import and aggregate Aboveground Biomass Density for year 2010 --------

#file source:
#https://uwmadison.app.box.com/s/xj3fnde17yazlogbiq740da2mrv2ma61

#upload native resolution aboveground biomass density data
aboveground_biomass_density <- raster('./../../../Data/Biomass/Biomass_Density_Spawn/AFELTON_agbDW_Mgha_x10_300m.tif')

#aggregate 30X to match (approximately) the resolution of the 9 km data
aboveground_biomass_density_30x_aggregate <- raster::aggregate(aboveground_biomass_density,fact=30)

#save to file
writeRaster(aboveground_biomass_density_30x_aggregate,'aboveground_dry_biomass_density_aggregate_30X.tif')

#cleanup
rm(aboveground_biomass_density,aboveground_biomass_density_30x_aggregate)

#-------------------------------------------------------------------------------
# Get leaf water content data for tundra land cover ------

#import tundra trait team dataset:

# Bjorkman, Anne D., et al. 
# "Tundra Trait Team: A database of plant traits spanning the tundra biome." 
# Global Ecology and Biogeography 27.12 (2018): 1402-1411.

tundra_data <- read.csv('TTT_cleaned_dataset.csv')

#filter to and summarise ldmc by site name. 9km pixel sizes are sufficiently
#large such that we can summarise to the site level as opposed to specific
#coordinates within a site.

tundra_ldmc_averaged <- tundra_data %>%
  dplyr::filter(Trait == "Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)") %>%
  group_by(SiteName) %>%
  dplyr::summarise(Value = mean(Value))

# these sites then get converted to relative water content, put into main water 
# content dataframe,and double checked using google earth engine to ensure its
# an intact tundra system, which gets classified as a shrubland.

#cleanup
rm(tundra_data,tundra_ldmc_averaged)

#-------------------------------------------------------------------------------
