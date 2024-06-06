
# custom functions used in this analysis

# seasonal transit imports ----

#seasonal import that summarizes transit by land cover type
seasonal_turnover_lc <- function(season,winter){
  
  lc_id <- read.csv('data/land_cover_id.csv')  
  
  #set directories
  # season = 'winter'
  # winter = T
  filepath <- 
    paste0('data/turnover_from_python/updated/seasonal/land_cover/csv/',
           season,'/')
  dir <- dir(filepath, full.names = T)
  dir <- dir[-c(11,13,15,16,17)] #remove land classes with no data
  
  #step 1 
  seasonal_turnover_summary_list <- list()
  for(i in dir[1:12]){
    
    #load in
    lc <- read.csv(i)
    
    if(winter == T){lc_filtered <- lc %>% #remove these in post, otherwise it screws things up for winter
      dplyr::filter(turnover > 0)}else{
        
        #filter out NA, inf, and zero values
        lc_filtered <- lc %>%
          dplyr::filter(lc01 != 'NA') %>% #only NAs for deciduous needleleaf...
          dplyr::filter(turnover > 0) %>%
          dplyr::filter(turnover != 'Inf')}
    
    
    #get land cover ID
    name <- 
      gsub(paste0('data/turnover_from_python/updated/seasonal/land_cover/csv/',
                  season,'//',season,'landclass.'),'', i)
    name <- gsub('.3856x1624.bin.nc.csv',
                 '', name)
    lc_filtered$class_number <- as.integer(name)
    lc_filtered <- merge(lc_filtered,lc_id,by=c('class_number'))
    
    #store in list
    seasonal_turnover_summary_list[[i]] <- lc_filtered
    
  }
  
  
  seasonal_turnover <- do.call('rbind',seasonal_turnover_summary_list)
  rm(seasonal_turnover_summary_list)
  
  #merge with annual turnover dataframe so we are working with pixels
  #with at least four months of data
  
  seasonal_turnover <- merge(seasonal_turnover,annual_turnover_lc[c(2,3)],by=c('lat','lon'))
  
  
  group_2_list <- unique(seasonal_turnover$group_2)
  
  #step 2
  seasonal_turnover_summary_list_2 <- list()
  for(j in group_2_list){
    
    #load in
    group_2_lc <- subset(seasonal_turnover,group_2==j)
    
    #5th quantile
    quantile_5 = round(quantile(group_2_lc$turnover,probs=0.05),2)
    
    #05th quantile
    quantile_50 = round(quantile(group_2_lc$turnover,probs=0.5),2)
    
    #95th quantile
    quantile_95 = round(quantile(group_2_lc$turnover,probs=0.95),2)
    
    quantile_df <- data.frame(quantile_50,quantile_5,quantile_95)
    
    quantile_df$cover <- j
    
    quantile_df$time <- season
    
    quantile_df <- quantile_df %>%
      dplyr::select(cover,quantile_50,quantile_5,quantile_95,time)
    
    
    #store in list
    seasonal_turnover_summary_list_2[[j]] <- quantile_df 
    
    
  }
  
  
  seasonal_turnover_summary <- do.call('rbind',seasonal_turnover_summary_list_2)
  rm(seasonal_turnover_summary_list_2)
  rownames(seasonal_turnover_summary) <- NULL
  
  return(seasonal_turnover_summary)
  
  
}

#seasonal import without any summaries of transit
seasonal_turnover_import <- function(season){
  
  lc_id <- read.csv('data/land_cover_id.csv')  
  
  #set directories
  season = 'spring'
  filepath <- paste0('data/turnover_from_python/updated/seasonal/land_cover/csv/',season,
                     '/')
  dir <- dir(filepath, full.names = T)
  dir <- dir[-c(11,13,15,16,17)]
  
  #step 1 
  seasonal_turnover_summary_list <- list()
  for(i in dir[1:12]){
    
    #load in
    lc <- read.csv(i)
    
    if(season == 'winter'){lc_filtered <- lc %>% #remove these in post, otherwise it screws things up for winter
      dplyr::filter(turnover > 0)}else{
        
        #filter out NA, inf, and zero values
        lc_filtered <- lc %>%
          dplyr::filter(lc01 != 'NA') %>% #only NAs for decidious needleleaf...
          dplyr::filter(turnover > 0) %>%
          dplyr::filter(turnover != 'Inf')}
    
    
    #get land cover ID
    name <- 
      gsub(paste0('data/turnover_from_python/updated/seasonal/land_cover/csv/',
                  season,'//',season,'landclass.'),'', i)
    name <- gsub('.3856x1624.bin.nc.csv',
                 '', name)
    lc_filtered$class_number <- as.integer(name)
    lc_filtered <- merge(lc_filtered,lc_id,by = c('class_number'))
    
    #store in list
    seasonal_turnover_summary_list[[i]] <- lc_filtered
    
  }
  
  
  seasonal_turnover <- do.call('rbind',seasonal_turnover_summary_list)
  rm(seasonal_turnover_summary_list)
  
  
  return(seasonal_turnover)
  
  
}


#-------------------------------------------------------------------------------
#convert 9 km mm storage data to cubic km of water for each pixel ----

get_km_cubed_3 <- function(x){
  
  x <- x/1000 #convert mm/m^2 to m/m^2
  x <- x*(9000^2) #convert to cubic m (multiply by length and width of each pixel: 9 by 9 km)
  x <- x*1e-9 # multiply by this value to get cubic km from cubic m.
  
  
  return(x)
  
}