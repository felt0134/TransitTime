# generate key derived datasets needed for ground-based comparisons 

#-------------------------------------------------------------------------------
# Load in and format TRY leaf water content data -----

#get TRY leaf water content data and merge it with a taxonomy data
# base that can let us know families of the species

#load original data downloaded from TRY
water_content_try<-read.delim('./../../Data/water.content.try/14187.txt')
#head(water.content.try)

#trim down columns
water_content_try <- water_content_try %>% 
  select(SpeciesName,DatasetID,ObservationID, DataName,OrigValueStr,OrigUnitStr)

#sort data IDs and convert to long form

#isolate ldmc
water_content_try_id <- water_content_try %>%
  dplyr::filter(DataName=='Leaf water content per leaf dry mass')
#head(water_content_try_id)

#reduce and rename column names
water_content_try_id <- water_content_try_id %>% 
  select(SpeciesName,DatasetID,ObservationID,DataName,OrigValueStr,OrigUnitStr) %>%
  rename('water_content' = 'OrigValueStr',
         'units' = 'OrigUnitStr')

#change water content to numeric and round to two decimal places
water_content_try_id$water_content <- 
  as.numeric(as.character(water_content_try_id$water_content)) #make numeric

water_content_try_id$water_content <- 
  round(water_content_try_id$water_content,2) #round 2 decimals
#summary(water_content_try_id)

#isolate latitude
water_content_try_lat <-  water_content_try %>%
  dplyr::filter(DataName=='Latitude') %>%
  select(SpeciesName, DatasetID, ObservationID,OrigValueStr) %>%
  rename('Latitude' = 'OrigValueStr') %>%
  dplyr::filter(!DatasetID =='212') #remove 212

#isolate longitude
water_content_try_lon <-  water_content_try %>%
  dplyr::filter(DataName=='Longitude') %>%
  select(SpeciesName, DatasetID, ObservationID,OrigValueStr) %>%
  rename('Longitude' = 'OrigValueStr') %>%
  dplyr::filter(!DatasetID =='212') #remove 212

#bind the lat and lon
lat_lon_try <- merge(water_content_try_lat,water_content_try_lon,
                     by=c('DatasetID','ObservationID','SpeciesName'))
#head(lat_lon_try,1)

#merge with water content
lat_lon_try_water_content <- merge(lat_lon_try,water_content_try_id,
                                   by=c('DatasetID','ObservationID','SpeciesName'))
#head(lat_lon_try_water_content,1)

#reorder and rename columns
lat_lon_try_water_content <- lat_lon_try_water_content %>%
  select("Longitude", "Latitude",'SpeciesName', "water_content",
         "units", "DatasetID",'ObservationID') %>%
  rename("x" = "Longitude",
         "y" = "Latitude")


#head(lat_lon_try_water_content,1)

#get family names

species <- unique(lat_lon_try_water_content$SpeciesName)
# length(species)

library(taxize)

species_list<-list()
for(i in species){
  
  try_taxonomy <- tax_name(i,get = c("genus","family","order"))
  species_list[[i]] <- data.frame(try_taxonomy)
  
}

species_df <- do.call("rbind", species_list)

colnames(species_df) <- c('db','SpeciesName','genus','family', 'order')
#head(species_df)

#merge them
species_water_content_merge<-merge(species_df,lat_lon_try_water_content,by=c('SpeciesName'))

#save this
write.csv(species_water_content_merge,'./../../../Data/water.content.try/water_content_taxonomy_global_dataset.csv')


#-------------------------------------------------------------------------------
# Get leaf water content data for grassland land cover (NEEDS WORK) ---------

# get leaf water content estimates for the grassland land cover

#set projection
projection <- '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0'

#step 1: create a dataframe with list of herbaceous families to be double checked

derived_wc <- read.csv('./../../Data/water.content.try/water_content_taxonomy_global_dataset.csv')

# get name of all herbs (x2 check where this came from)
herb_family<-read.csv('./../../Data/water.content.try/family.list.2.csv')
herb_family<-subset(herb_family,Herb.=='Yes')
family.list <- unique(herb_family$family)


#loop through and get all herb families
herbs.list <- list()

for(i in family.list){
  
  herbs<-subset(derived_wc,family==i)
  herbs.list[[i]] <- herbs
  
}

herbs.list.df <- do.call("rbind",herbs.list)
length(unique(herbs.list.df$SpeciesName))
#about 300 more observations than just poa

#create and save a list of mostly 'herb species' to X2 check which species are herb
mostly.herb.list <- data.frame(unique(herbs.list.df$SpeciesName))
colnames(mostly.herb.list) <- 'Species'
#write.csv(mostly.herb.list,'./../../../Data/Derived_Data/Land_Cover_Water_Content/mostly_herb_X2_check.csv')

#after checking which species are truly herbaceous in the mostly_herb_X2_check data frame,
#now you can load in the X2 checked data frame
herbx2.list <-read.csv('./../../Data/water.content.try/mostly_herb_X2_check.csv')
herbx2.list <- subset(herbx2.list,Herbaceous.=='Yes')
herbx2.list <- herbx2.list[c(2)]

#now loop through to get X2 checked herb species
herbx2.list <- unique(herbx2.list$Species)

herbsx2.list <- list()

for(i in herbx2.list){
  
  herbs<-subset(derived_wc,SpeciesName==i)
  herbsx2.list[[i]] <- herbs
  
}

herbsx2.list.df <- do.call("rbind",herbsx2.list)
head(herbsx2.list.df,1)

#take the means of each coordinate and turn into raster (do this once)
#derived_wc_mean<-aggregate(water.content~x+y,mean,data=derived_wc)

#just poa coordinate means
derived_wc_poa <- derived_wc %>% filter(family=='Poaceae')
derived_wc_mean_poa<-aggregate(water.content~x+y,mean,data=derived_wc_poa)
head(derived_wc_mean_poa,1)


#all other herb coordinate means
derived_wc_mean_herbs<-aggregate(water.content~x+y,mean,data=herbsx2.list.df)
head(derived_wc_mean_herbs,1)

#bind Poa and all other herbaceous species (pixel averages) to have all herb species
derived_wc_mean <- rbind(derived_wc_mean_poa,derived_wc_mean_herbs)

#final average in case there were overlapping pixels
derived_wc_mean_2 <- aggregate(water.content~x+y,mean,data=derived_wc_mean)
head(derived_wc_mean_2,1)
plot(y~x,data=derived_wc_mean_2)
#319 observations


#new approach using nearest spatial points to the land cover estimates

#import data
source('annual_turnover_storage_import.r')

#turn ground data to spdf
coords_ground <- derived_wc_mean_2[ , c("x", "y")]   # coordinates
data_ground   <- data.frame(derived_wc_mean_2[c(3)])          # data
crs    <- CRS('+proj=longlat +datum=WGS84 +no_defs') # proj4string of coords

spdf_ground <- SpatialPointsDataFrame(coords      = coords_ground,
                                      data        = data_ground, 
                                      proj4string = crs)

#turn vod raster data to spatial points DF
annual_strorage_2 <- annual_turnover_lc %>%
  select(lon,lat,annual_storage,group,group_2)
rownames(annual_strorage_2) <- NULL

# prepare coordinates, data, and proj4string
coords_vod <- annual_strorage_2[ , c("lon", "lat")]   # coordinates
data_vod   <- annual_strorage_2[ , 3:5]          # data

# make the SpatialPointsDataFrame object
spdf_vod <- SpatialPointsDataFrame(coords      = coords_vod,
                                   data        = data_vod, 
                                   proj4string = crs)

library(FNN)

#link ground-based coordinates to vod coordinates for storage
nn1 = get.knnx(coordinates(spdf_vod), coordinates(spdf_ground), 1)
vector <- data.frame(nn1[1])
vector <- vector[c(1:nrow(vector)),]
spdf_vod_df <- data.frame(spdf_vod)
new_df <- spdf_vod_df[c(vector),]
new_df <- new_df %>%
  select(annual_storage,group,lon,lat)

points(lat~lon,data=new_df,add=T,col='red')

cbind_ground_vod_herb <- cbind(new_df,derived_wc_mean_2)
cbind_ground_vod_herb <- cbind_ground_vod_herb %>%
  filter(group==c('Cropland','Grassland'))

cbind_ground_vod_herb <- cbind_ground_vod_herb %>%
  select(x,y,water.content,group)
rownames(cbind_ground_vod_herb) <- NULL

#create file for double checking
write.csv(cbind_ground_vod_herb,
          './../../Data/water.content.try/crop_grassland_mean_water_content.csv')


#stopped here. This same workflow can be implemented for the other water content data we have.


#olf approach to filter by making land cover a raster



#STOPPED HERE

#all mostly herbs

est_fix_wc_grid<-fix_grid(derived_wc_mean)
plot(est_fix_wc_grid)
proj4string(est_fix_wc_grid) <- CRS(projection)

grasslands<-raster('./../../../Data/Derived_Data/Land_Cover_Distributions/Grassland.tif')

#get into the same projection
proj4string(grasslands) <- CRS(projection)

#resample so can align
resample_test<-resample(grasslands,est_fix_wc_grid)

plot(resample_test)
plot(est_fix_wc_grid)

#turn into data frame and merge
biome_raster_df<-data.frame(rasterToPoints(resample_test))
est_fix_wc_grid_df<-data.frame(rasterToPoints(est_fix_wc_grid))

test_merge<-merge(biome_raster_df,est_fix_wc_grid_df,by=c('x','y'))
test_merge <-test_merge[c(1,2,4)]
colnames(test_merge) <- c('x','y','average.water.content')
test_merge$average.water.content <- round(test_merge$average.water.content,2)
hist(test_merge$average.water.content)
summary(test_merge)

#save as csv

# Poa and herb X2 checked
write.csv(test_merge,'./../../../Data/Derived_Data/Land_Cover_Water_Content/grassland_water_content_poa_herbX2.csv')

#-------------------------------------------------------------------------------
# Import and aggregate Aboveground Biomass Density for year 2010 --------

#file source:
#https://uwmadison.app.box.com/s/xj3fnde17yazlogbiq740da2mrv2ma61

#upload original
aboveground_biomass_density <- raster('./../../../Data/Biomass/Biomass_Density_Spawn/AFELTON_agbDW_Mgha_x10_300m.tif')
#plot(aboveground_biomass_density)

#aggregate 30X to match resolution of canopy transpiration data
aboveground_biomass_density_30x_aggregate <- raster::aggregate(aboveground_biomass_density,fact=30)
#plot(aaboveground_biomass_density_30x_aggregate)

#save to file
writeRaster(aboveground_biomass_density_30x_aggregate,'./../../../Data/Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')
#-------------------------------------------------------------------------------
# Get leaf water content data for tundra land cover ------

tundra_data<-read.csv('./../../../Data/tundra_trait_team/TTT_cleaned_dataset.csv')
head(tundra_data)
unique(tundra_data$Trait)
tundra_ldmc <- subset(tundra_data,
                      Trait="Leaf dry mass per leaf fresh mass (Leaf dry matter content, LDMC)")
head(tundra_ldmc)
hist(tundra_ldmc$Value)

tundra_ldmc_averaged <- aggregate(Value~Latitude + Longitude,mean,data=tundra_ldmc)
summary(tundra_ldmc_averaged)


#unclear to what is mean by fresh mass. How you can have more dry mass than 
#fresh mass (dry+water) or is fresh mass just water?

#assume LDMC = 1.2 (1.2 g dry mass/1g fresh mass)
#So it is either:
#1/1.2 = water divided by dry mass = 0.83 (assuming fresh mass=everything)
#((1.2+1) -1.2)/(1.2+1) = 0.45 (assuming fresh mass = water)
#((1.2+1) -1)/(1.2+1) = 0.55 (must be this one?)
#-------------------------------------------------------------------------------
