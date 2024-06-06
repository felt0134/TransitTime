
#import and wrangle annual storage and turnover. Run this 01_start.R.

#land cover ID
lc_id <- read.csv('data/land_cover_id.csv')

#set directories and land cover types to import
annual_turnover_filepath <- 'data/turnover_from_python/updated/annual/land_cover/csv/'
annual_turnover_dir <- dir(annual_turnover_filepath, full.names = T)
annual_turnover_dir <- annual_turnover_dir[-c(11,13,15,16,17)] #remove land classes with no data

#now loop through each land cover file and import

#annual turnover time and storage

#import each land cover type individually 
annual_turnover_list <- list()
for(i in annual_turnover_dir[1:12]){ 
  
  #load in
  lc <- read.csv(i)
  
  #filter out NA, inf, and zero values
  lc_filtered <- lc %>%
    dplyr::filter(lc01 != 'NA') %>%
    dplyr::filter(annual_turnover > 0) %>%
    dplyr::filter(annual_turnover != 'Inf')
  
  #get land cover ID
  name <-
    gsub(
      'data/turnover_from_python/updated/annual/land_cover/csv//landclass.','',i)
  name <- gsub('.3856x1624.bin.nc.csv','', name)
  lc_filtered$class_number <- as.integer(name)
  lc_filtered <- merge(lc_filtered, lc_id, by = c('class_number'))
  
  #store in list
  annual_turnover_list[[i]] <- lc_filtered
  
}

#bind all into one dataset
annual_turnover_lc <- do.call('rbind', annual_turnover_list)
rownames(annual_turnover_lc) <- NULL

#cleanup
rm(lc_filtered,lc,name,annual_turnover_filepath,annual_turnover_dir,
   annual_turnover_list,i)

#quick look
print("annual turnover quick look")
print(quantile(annual_turnover_lc$annual_turnover,probs = c(0.05,0.5,0.95)))

