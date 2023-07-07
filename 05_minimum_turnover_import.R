
#import and wrangle minimum storage and turnover

#set directories
minimum_turnover_filepath <- 'data/turnover_from_python/minimum/land_cover_csvs/'
minimum_turnover_dir <- dir(minimum_turnover_filepath, full.names = T)
minimum_turnover_dir <- minimum_turnover_dir[-c(11,13,15,16,17)] #remove land classes with no data

#now loop through each land cover file and import

#annual turnover time and storage

#import each land cover type individually 
minimum_turnover_list <- list()
for(i in minimum_turnover_dir[1:12]){
  
  #load in
  lc <- read.csv(i)
  
  #filter out NA, inf, and zero values
  lc_filtered <- lc %>%
    dplyr::filter(lc01 != 'NA') %>%
    dplyr::filter(minimum_turnover > 0) %>%
    dplyr::filter(minimum_turnover != 'Inf')
  
  #get land cover ID
  name <-
    gsub(
      'data/turnover_from_python/minimum/land_cover_csvs//landclass.','',i)
  name <- gsub('.3856x1624.bin.nc.csv','', name)
  lc_filtered$class_number <- as.integer(name)
  lc_filtered <- merge(lc_filtered, lc_id, by = c('class_number'))
  
  #store in list
  minimum_turnover_list[[i]] <- lc_filtered
  
}

#bind all into one dataset
minimum_turnover_lc <- do.call('rbind',minimum_turnover_list)
rownames(minimum_turnover_lc) <- NULL

#cleanup
rm(lc_filtered,lc,name,minimum_turnover_filepath,minimum_turnover_dir,
   minimum_turnover_list,i)

#quick look
print("minimum turnover quick look")
print(quantile(minimum_turnover_lc$minimum_turnover,probs = c(0.05,0.5,0.95)))
