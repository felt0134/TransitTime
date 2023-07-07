
#figures and summary stats. It is recommended you run each section on its own
#instead of running entire script. However, some sections use data produced
#by other sections. Thus, it is recommended to run each section in sequence from
#top-to-bottom.

#merge together by pixel so working from same set of pixels for analysis
#we do this for consistency but also because the annual turnover is filtered
#to pixels with at least four months of data.
minimum_turnover_lc <- merge(minimum_turnover_lc,annual_turnover_lc[c(2,3,7)],
                             by = c('lat','lon'))

#unique vegetation types
group_2_names <- unique(annual_turnover_lc$group_2)

#order by median transit
veg_order <- c("Savanna", "Cropland", "Deciduous broadleaf forest",
                 'Evergreen broadleaf forest','Grassland',
                 'Mixed forest','Evergreen needleleaf forest',
                 'Shrubland','Deciduous needleleaf forest')

#-------------------------------------------------------------------------------

# annual and minimum transit time by land cover types-------

#list to store each vegetation type
annual_turnover_list_2 <- list()

# right tail truncate for the figures. There is strong right skew that obscures
# interpretation/visualization of the distribution.

# truncate for annual transit
for(j in group_2_names){
  
  group_2_lc <- subset(annual_turnover_lc,group_2 == j)
  
  quantile_95 = quantile(group_2_lc$annual_turnover,probs = 0.95) #by 95th percentile
  quantile_95 = as.numeric(quantile_95)
  
  #truncate by 95th percentile
  lc_filtered_2 <- group_2_lc %>%
    dplyr::filter(annual_turnover < quantile_95) 
  
  #store in list
  annual_turnover_list_2[[j]] <- lc_filtered_2
  
  rm(lc_filtered_2)
  
}

annual_turnover_lc_95 <- do.call('rbind',annual_turnover_list_2)
rm(annual_turnover_list_2)

# truncate for minimum transit
minimum_turnover_list_2 <- list()
for(j in group_2_names){
  
  group_2_lc <- subset(minimum_turnover_lc,group_2 == j)
  
  quantile_95 = quantile(group_2_lc$minimum_turnover,probs = 0.95)
  quantile_95 = as.numeric(quantile_95)
  
  #truncate by 95th percentile
  lc_filtered_2 <- group_2_lc %>%
    dplyr::filter(minimum_turnover < quantile_95) 
  
  #store in list
  minimum_turnover_list_2[[j]] <- lc_filtered_2
  
  rm(lc_filtered_2)
  
}

minimum_turnover_lc_95 <- do.call('rbind',minimum_turnover_list_2)
rm(minimum_turnover_list_2)

# calculate median annual transit for each latitude 
annual_turnover_by_latitude <- 
  aggregate(annual_turnover ~ lat + group,median,data = annual_turnover_lc_95)

#median transit by latitude
annual_turnover_by_lat_plot <- 
  ggplot(annual_turnover_by_latitude,aes(lat,annual_turnover,col=group)) +
  stat_smooth(data=annual_turnover_lc_95,aes(lat,annual_turnover),col='black',size=1,se=F) +
  geom_line(size=0.5,alpha=0.6) +
  coord_flip() +
  scale_colour_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                               'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                      labels=c('Grassland'='Grassland','Forest'='Forest',
                               'Shrubland'='Shrubland','Savanna'='Savanna')) +
  ylab('Annual transit time (days)') +
  xlab('Latitude (degrees)') +
  geom_vline(xintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=10), #angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = c(0.85,0.25),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#break up land covers into facets
annual_turnover_by_lat_facet <- 
  ggplot(annual_turnover_by_latitude,aes(lat,annual_turnover,col=group)) +
  stat_smooth(data=annual_turnover_lc_95,aes(lat,annual_turnover),col='black',size=0.5,se=F) +
  facet_wrap(.~group) + 
  facet_wrap(ncol=1,~factor(group,levels=c('Savanna','Cropland','Grassland',
                                           'Forest','Shrubland'))) +
  geom_line(size=0.5,alpha=0.5) +
  coord_flip() +
  scale_colour_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                               'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                      labels=c('Grassland'='Grassland','Forest'='Forest',
                               'Shrubland'='Shrubland','Savanna'='Savanna')) +
  ylab('Annual transit time (days)') +
  xlab('Latitude (degrees)') +
  geom_vline(xintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=10), 
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


# calculate median minimum transit by latitude
minimum_turnover_by_latitude <- aggregate(minimum_turnover ~ lat + group,
                                          median,data = minimum_turnover_lc_95)

#minimum turnover by latitude
minimum_turnover_by_lat_plot <- 
  ggplot(minimum_turnover_by_latitude,aes(lat,minimum_turnover,col=group)) +
  stat_smooth(data=minimum_turnover_lc_95,aes(lat,minimum_turnover),col='black',size=1) +
  geom_line(size=0.4,alpha=0.5) +
  coord_flip() +
  scale_colour_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                               'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                      labels=c('Grassland'='Grassland','Forest'='Forest',
                               'Shrubland'='Shrubland','Savanna'='Savanna')) +
  ylab('Minimum transit time (days)') +
  xlab('Latitude (degrees)') +
  geom_vline(xintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=10), 
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#break up land covers into facets
minimum_turnover_by_lat_facet <- 
  ggplot(minimum_turnover_by_latitude,aes(lat,minimum_turnover,col=group)) +
  stat_smooth(data=minimum_turnover_lc_95,aes(lat,minimum_turnover),col='black',size=0.5) +
  facet_wrap(.~group) + 
  facet_wrap(ncol=1,~factor(group,levels=c('Savanna','Cropland','Grassland',
                                           'Forest','Shrubland'))) +
  geom_line(size=0.25,alpha=0.6) +
  coord_flip() +
  scale_colour_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                               'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                      labels=c('Grassland'='Grassland','Forest'='Forest',
                               'Shrubland'='Shrubland','Savanna'='Savanna')) +
  ylab('Minimum transit time (days)') +
  xlab('Latitude (degrees)') +
  geom_vline(xintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=10), 
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=12),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

png(height = 3000,width=3000,res=300,
    'manuscript_figures/transit_latitude_full_multipanel.png')

print(plot_grid(annual_turnover_by_lat_plot, annual_turnover_by_lat_facet,
                minimum_turnover_by_lat_plot, minimum_turnover_by_lat_facet,
                labels = c('a', 'b','c','d'),ncol = 2, nrow=2,
                rel_widths = c(1.25,1,1.25,1), 
                rel_heights = c(1,1,1,1),label_size = 20))

dev.off()

#cleanup
rm(annual_turnover_by_latitude,minimum_turnover_by_latitude,
   annual_turnover_by_lat_plot, annual_turnover_by_lat_facet,
   minimum_turnover_by_lat_plot, minimum_turnover_by_lat_facet,
   group_2_lc,j,quantile_95)

#-------------------------------------------------------------------------------




# turnover boxplots -------

#annual transit by landcover type

#plot
annual_turnover_boxplot <- 
  ggplot(annual_turnover_lc_95,aes(x=factor(group_2,level=veg_order),
                                   y=annual_turnover,
                                   color=annual_turnover)) +
  geom_hline(yintercept = 5.4) + #global median
  scale_color_scico('Annual transit time (days)',palette = 'batlow',direction=-1) +
  geom_jitter(size=.25,width = 0.25,height=0.2,alpha=0.1) +
  geom_violin(width=1,color='black') +
  geom_boxplot(width=.1,color='black') +
  ylab('Annual transit time (days)') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=9, angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=12),
    axis.title.y = element_text(color='black',size=16),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=10),
    legend.position = c(0.2,0.80),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#minimum transit by land cover boxplot

#plot
minimum_turnover_boxplot <- 
  ggplot(minimum_turnover_lc_95,aes(x=factor(group_2,level=veg_order),
                                    y=minimum_turnover,
                                    color=minimum_turnover)) +
  geom_hline(yintercept = 2.4) + #global median
  scale_color_scico('Minimum transit time (days)',palette = 'batlow',direction=-1) +
  geom_jitter(size=.25,width = 0.25,height=0.2,alpha=0.1) +
  geom_violin(width=1,color='black') +
  geom_boxplot(width=.1,color='black') +
  ylab('Minimum transit time (days)') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=9, angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=12),
    axis.title.y = element_text(color='black',size=16),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=10),
    legend.position = c(0.2,0.80),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save plot
png(height = 2500,width=2000,res=300,
    'manuscript_figures/transit_latitude_boxplot.png')

print(plot_grid(annual_turnover_boxplot, minimum_turnover_boxplot,
                labels = c('', ''),ncol = 1, nrow=2,
                rel_widths = c(1,1), 
                rel_heights = c(1,1),label_size = 20))

dev.off()

#cleanup
rm(annual_turnover_lc_95,minimum_turnover_lc_95,annual_turnover_boxplot, 
   minimum_turnover_boxplot)

#-------------------------------------------------------------------------------

# storage by land cover types-------

storage_list_2 <- list()
for(j in group_2_names){
  
  group_2_lc <- subset(annual_turnover_lc,group_2 == j)
  
  quantile_95 = quantile(group_2_lc$annual_storage,probs = 0.95)
  quantile_95 = as.numeric(quantile_95)
  
  #truncate by 95th percentile
  lc_filtered_2 <- group_2_lc %>%
    dplyr::filter(annual_storage < quantile_95) 
  
  #store in list
  storage_list_2[[j]] <- lc_filtered_2
  
  rm(lc_filtered_2,group_2_lc,j)
  
}

#store in list
annual_storage_lc_95 <- do.call('rbind',storage_list_2)
rm(storage_list_2)

#calculate median storage by latitude
storage_by_latitude <- aggregate(annual_storage ~ lat + group,median,data = annual_storage_lc_95)

#storage by latitude
storage_by_lat <- ggplot(storage_by_latitude,aes(lat,annual_storage,col=group)) +
  stat_smooth(data=annual_storage_lc_95,aes(lat,annual_storage),col='black',size=1) +
  geom_line(size=0.25,alpha=0.75) +
  coord_flip() +
  scale_colour_manual(values=c('Savanna'='purple','Cropland'='darkblue',
                               'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                      labels=c('Grassland'='Grassland','Forest'='Forest',
                               'Shrubland'='Shrubland','Savanna'='Savanna')) +
  ylab('Aboveground water storage (mm)') +
  xlab('Latitude (degrees)') +
  geom_vline(xintercept = 0) +
  theme(
    axis.text.x = element_text(color='black',size=10), 
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=15),
    axis.title.y = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = c(0.8,0.8),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#transit by land cover boxplot

#plot
boxplot_annual_storage <- ggplot(annual_storage_lc_95,aes(x=factor(group_2,level=veg_order),
                                                          y=annual_storage,
                                                          color=annual_storage)) +
  geom_hline(yintercept = 3.37) + #global median
  scale_color_scico('Water storage (mm)', palette = 'batlow',direction=-1) +
  geom_jitter(size=.25,width = 0.25,height=0.2,alpha=0.1) +
  geom_violin(width=1,color='black') +
  geom_boxplot(width=.1,color='black') +
  ylab('Aboveground water storage (mm)') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=10, angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=10),
    axis.title.x = element_text(color='black',size=12),
    axis.title.y = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_text(size=10),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=10),
    legend.position = c(0.15,0.80),
    strip.background = element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save plot
png(height = 2000,width=3500,res=300,
    'manuscript_figures/storage_multipanel.png')

print(plot_grid(boxplot_annual_storage,storage_by_lat,
                labels = c('a', 'b'),ncol = 2, nrow=1,
                rel_widths = c(1.5,1), 
                rel_heights = c(1,1),label_size = 25))

dev.off()

rm(storage_by_lat,boxplot_annual_storage,storage_by_latitude,
   annual_storage_lc_95)

#-------------------------------------------------------------------------------
# summary stats of transit: annual and minimum ------

#annual turnover summary
annual_turnover_summary_list <- list()
for(j in group_2_names){
  
  #load in
  group_2_lc <- subset(annual_turnover_lc,group_2 == j)
  
  #5th quantile
  quantile_5 = round(quantile(group_2_lc$annual_turnover,probs = 0.05),2)
  
  #05th quantile
  quantile_50 = round(quantile(group_2_lc$annual_turnover,probs = 0.5),2)
  
  #95th quantile
  quantile_95 = round(quantile(group_2_lc$annual_turnover,probs = 0.95),2)
  
  quantile_df <- data.frame(quantile_50,quantile_5,quantile_95)
  
  quantile_df$time <- 'annual'
  
  quantile_df$cover <- j
  
  quantile_df <- quantile_df %>%
    dplyr::select(cover,quantile_50,quantile_5,quantile_95,time)
  
  #store in list
  annual_turnover_summary_list[[j]] <- quantile_df 
  
}

#turn into dataframe
annual_turnover_summary <- do.call('rbind',annual_turnover_summary_list)
rownames(annual_turnover_summary) <- NULL

#write to
write.csv(annual_turnover_summary, 'manuscript_figures/annual_turnover_summaries.csv')

#cleanup
rm(group_2_lc,quantile_5,quantile_50,quantile_95,quantile_df,
   annual_turnover_summary_list,annual_turnover_summary)


#minimum turnover summary
minimum_turnover_summary_list <- list()
for(j in group_2_names){
  
  #load in
  group_2_lc <- subset(minimum_turnover_lc,group_2 == j)
  
  #5th quantile
  quantile_5 = round(quantile(group_2_lc$minimum_turnover,probs = 0.05),2)
  
  #50th quantile
  quantile_50 = round(quantile(group_2_lc$minimum_turnover,probs = 0.5),2)
  
  #95th quantile
  quantile_95 = round(quantile(group_2_lc$minimum_turnover,probs = 0.95),2)
  
  quantile_df <- data.frame(quantile_50,quantile_5,quantile_95)
  
  quantile_df$cover <- j
  
  quantile_df$time <- 'minimum'
  
  quantile_df <- quantile_df %>%
    dplyr::select(cover,quantile_50,quantile_5,quantile_95,time)
  
  
  #store in list
  minimum_turnover_summary_list[[j]] <- quantile_df 
  
  rm(quantile_df)
  
}

#turn to dataframe
minimum_turnover_summary <- do.call('rbind',minimum_turnover_summary_list)
rownames(minimum_turnover_summary) <- NULL

#write to file
write.csv(minimum_turnover_summary, 'manuscript_figures/minimum_turnover_summaries.csv')
rm(group_2_lc,quantile_5,quantile_50,quantile_95,
   minimum_turnover_summary,minimum_turnover_summary_list,j)


#-------------------------------------------------------------------------------
# transit estimates by each season -----

#create a function that can input the name of the season and output the dataframe

#winter
winter_summary <- seasonal_turnover_lc('winter',winter = T)
rownames(winter_summary) <- NULL

#spring
spring_summary <- seasonal_turnover_lc('spring',winter = F)
rownames(spring_summary) <- NULL

#summer
summer_summary <- seasonal_turnover_lc('summer',winter = F)
rownames(summer_summary) <- NULL

#fall
fall_summary <- seasonal_turnover_lc('fall',winter = F)
rownames(fall_summary) <- NULL

#combine and save to file
seasonal_summary <- rbind(winter_summary,spring_summary,
                          summer_summary,fall_summary)

write.csv(seasonal_summary, 'manuscript_figures/seasonal_summary.csv')

rm(winter_summary,spring_summary,
   summer_summary,fall_summary,seasonal_summary)

#-------------------------------------------------------------------------------
# summary stats of aboveground water storage -------

#run loop
annual_storage_summary_list <- list()
for(j in group_2_names){
  
  #load in
  group_2_lc <- subset(annual_turnover_lc,group_2 == j)
  
  #5th quantile
  quantile_5 = round(quantile(group_2_lc$annual_storage,probs = 0.05),2)
  
  #50th quantile
  quantile_50 = round(quantile(group_2_lc$annual_storage,probs = 0.5),2)
  
  #95th quantile
  quantile_95 = round(quantile(group_2_lc$annual_storage,probs = 0.95),2)
  
  quantile_df <- data.frame(quantile_50,quantile_5,quantile_95)
  
  quantile_df$time <- 'annual'
  
  quantile_df$cover <- j
  
  quantile_df <- quantile_df %>%
    dplyr::select(cover,quantile_50,quantile_5,quantile_95,time)
  
  #store in list
  annual_storage_summary_list[[j]] <- quantile_df 
  
  rm(quantile_df)
  
}

#turn to dataframe
annual_storage_summary <- do.call('rbind',annual_storage_summary_list)
rownames(annual_storage_summary) <- NULL

#save to file
write.csv(annual_storage_summary, 'manuscript_figures/annual_storage_summaries.csv')

#cleanup
rm(group_2_lc,quantile_5,quantile_50,
   quantile_95,annual_storage_summary,j,annual_storage_summary_list)

#-------------------------------------------------------------------------------
# correlate ground-based with vod-based water storage ------

#import ground-based water content
ground_estimates <- read.csv('data/site_WC_estimates-26Oct22.csv')

# filter out unusable sites
ground_estimates <- ground_estimates %>%
  dplyr::filter(Exclude == 'No')

coords_ground <- ground_estimates[,c("Long", "Lat")]   # coordinates
data <- ground_estimates[,c("Land.Cover.Type","mean.moisture")]      # data
crs  <- CRS('+proj=longlat +datum=WGS84 +no_defs') # proj4string of coords

# make the SpatialPointsDataFrame object
spdf_ground_measurement <- SpatialPointsDataFrame(coords = coords_ground,
                                                  data = data, 
                                                  proj4string = crs)

#import aboveground biomass raster
dry_biomass_only <- 
  raster('./../../Derived_Data/Biomass/aboveground_dry_biomass_density_aggregate_30X.tif')

#correct for 0.1 scale factor to get to megagrams C pe hectare
dry_biomass_only <- dry_biomass_only/10

#1000000 grams = 1 megagram
dry_biomass_only <- dry_biomass_only*1000000

#1 hectare = 10000 square meters to get g/m^2
dry_biomass_only <- dry_biomass_only/10000

#extract biomass value from raster for each ground-based point coordinate
dry_biomass <- data.frame(raster::extract(dry_biomass_only,coords_ground))
dry_biomass$id <- rownames(dry_biomass)
colnames(dry_biomass) <- c('dry_biomass','id')

#make ID column and trim down columns
ground_estimates$id <- rownames(ground_estimates)
ground_estimates_2 <- ground_estimates %>%
  dplyr::select(mean.moisture,id)

#merge and calculate water storage
dry_biomass_ground_vwc <- merge(dry_biomass,ground_estimates,by = ('id'))
dry_biomass_ground_vwc$ground_vwc <- 
  dry_biomass_ground_vwc$dry_biomass*dry_biomass_ground_vwc$mean.moisture
dry_biomass_ground_vwc$ground_vwc <- dry_biomass_ground_vwc$ground_vwc*0.001

#trim down columns
dry_biomass_ground_vwc <- dry_biomass_ground_vwc %>%
  dplyr::select(Long,Lat,ground_vwc,id)

#match up with VOD-based estimates

#turn ground data to spdf
coords_ground <- dry_biomass_ground_vwc[,c("Long", "Lat")]   # coordinates
data_ground   <- data.frame(dry_biomass_ground_vwc[c(3:4)])          # data
crs <- CRS('+proj=longlat +datum=WGS84 +no_defs') # proj4string of coords

spdf_ground <- SpatialPointsDataFrame(coords = coords_ground,
                                      data = data_ground, 
                                      proj4string = crs)

#turn vod raster data to spatial points DF
annual_strorage_2 <- annual_turnover_lc %>%
  dplyr::select(lon,lat,annual_storage,group,group_2)
rownames(annual_strorage_2) <- NULL

# prepare coordinates, data, and proj4string
coords_vod <- annual_strorage_2[,c("lon", "lat")]   # coordinates
data_vod <- annual_strorage_2[,3:5]          # data

# make the SpatialPointsDataFrame object
spdf_vod <- SpatialPointsDataFrame(coords = coords_vod,
                                   data = data_vod, 
                                   proj4string = crs)

#link ground-based coordinates to vod coordinates for storage
nn1 = get.knnx(coordinates(spdf_vod), coordinates(spdf_ground), 1)
vector <- data.frame(nn1[1])
vector <- vector[c(1:44),]
spdf_vod_df <- data.frame(spdf_vod)
new_df <- spdf_vod_df[c(vector),]
new_df <- new_df %>%
  dplyr::select(annual_storage,group,lon,lat)

#combine ground based and vod-based water storage
spdf_ground_df <- data.frame(spdf_ground)
cbind_ground_vod <- cbind(new_df,dry_biomass_ground_vwc)
cbind_ground_vod <- cbind_ground_vod %>%
  dplyr::filter(!group == c('Water'),
                !group == c('Urban')) #don't want these two

#fix the one missclassified vegetation type from shrubland to forest
cbind_ground_vod[38, 2] = 'Forest'

#metric of relationship between ground and VOD VWC
cor.test(cbind_ground_vod$ground_vwc,cbind_ground_vod$annual_storage,method='spearman')
#r=0.72
summary(lm(ground_vwc ~ annual_storage,data=cbind_ground_vod))
#slope = 1.036, R-squared = 0.39
mean((cbind_ground_vod$annual_storage-cbind_ground_vod$ground_vwc))
#bias = 0.22
vod_ground_lm <- lm(annual_storage~ground_vwc,data=cbind_ground_vod)
sqrt(mean(vod_ground_lm$residuals^2))
#RMSE = 2.35

#plot it
vod_vwc_plot <- ggplot(cbind_ground_vod,
                       aes(annual_storage,ground_vwc,fill=group)) +
  #scale_x_continuous(limits=c(0,12.9)) +
  #scale_y_continuous(limits=c(0,12.9)) +
  scale_x_continuous(limits=c(0,22)) +
  scale_y_continuous(limits=c(0,22)) +
  geom_point(size=7,pch=21,alpha = 0.60) +
  scale_fill_manual(values=c('Cropland'='purple','Savanna'='darkblue',
                             'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                    labels=c('Grassland'='Grassland','Forest'='Forest',
                             'Shrubland'='Shrubland','Savanna'='Savanna')) +
  # annotate("text", x=8.75, y=9.8, label= "1:1 Line",size=5) +
  # annotate("text", x=2.25, y=10, label= "r = 0.72",size=8) +
   annotate("text", x=15.2, y=18, label= "1:1 Line",size=8) +
  annotate(geom = "text",
           label = as.character(expression(paste(rho, " = 0.72"))),
           parse = TRUE, x=2.25, y=17,size=8) +
  geom_abline(slope=1) +
  xlab('Satellite-based vegetation water storage (mm)') +
  ylab('Ground-based vegetation water storage (mm)') +
  theme(
    axis.text.x = element_text(color='black',size=15), 
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=20),
    axis.title.y = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=20),
    #legend.position = c(0.85,0.2),
    legend.position = c(0.77,0.2),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#you can derive an estimate of the beta parameter from these data for each
#land cover type:

annual_turnover_lc %>%
  dplyr::select(lat,lon,beta) %>%
  dplyr::right_join(cbind_ground_vod,by = c("lat","lon")) %>%
  dplyr::mutate(vod = annual_storage*beta) %>%
  dplyr::group_by(group) %>%
  group_modify(~ broom::tidy(lm(vod ~ ground_vwc, data = .x)))

#compare to other pools/estimates
pools <- read.csv('./../../Pools/H2OPoolSizeEstimates.csv')
pools <- pools %>%
  dplyr::filter(Citation != 'Bar-On 2018')

pools$size <- as.numeric(as.character(pools$Size..km3.))

#pool means
pool_means <- aggregate(Size..km3. ~ Pool,mean,data = pools)

#contextualize our veg water storage estimate
16500.000/379 #compare our VOD-based estimate to soil water estimate

#create two subsets for plotting
vegetation_pools <- subset(pools,Pool == 'Vegetation')

this_study <- vegetation_pools %>%
  dplyr::filter(Citation == 'This study')

pool_size <- ggplot(pool_means, aes(y = reorder(Pool,Size..km3.), x = Size..km3.))  +
  geom_col(fill='grey70',color='black')+
  ylab(NULL) +
  scale_x_continuous(expand=c(0,0),
                     trans = "log", breaks = 10^c(1,3,5,8), labels = scales::comma) +
  xlab(expression('Freshwater pool size'~(km^3))) +
  geom_errorbar(data=vegetation_pools,mapping=aes(y=Pool,x=size),
                xmin=5.98,xmax=7.8,size=0.5,width=.25) +
  geom_point(data=this_study,mapping=aes(y=Pool,x=size),size=8,pch=21,
             fill='white') + 
  ylab('') +
  theme(
    axis.text.x = element_text(color='black',size=15), 
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=20),
    axis.title.y = element_text(color='black',size=20),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = c(0.6,0.15),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))


#print and save figure
png(height = 2000,width=4000,res=300,
    'manuscript_figures/storage_multipanel.png')

print(plot_grid(pool_size,vod_vwc_plot,labels = c('', ''),ncol = 2, nrow=1,
                                   rel_widths = c(2.5,2.75),
                                   rel_heights = c(1,1),label_size = 25))

dev.off()

#cleanup
rm(ground_estimates,coords_ground,data,crs,spdf_ground_measurement,dry_biomass_only,
   ground_estimates_2,dry_biomass_ground_vwc,spdf_ground,annual_strorage_2,
   coords_vod,data_vod,spdf_vod,nn1,vector,spdf_vod_df,new_df,spdf_ground_df,
   cbind_ground_vod,vod_vwc_plot,pools,pool_means,vegetation_pools,this_study,
   pool_size,dry_biomass,data_ground,vod_ground_lm)

#-------------------------------------------------------------------------------
# calculate total amount of water in aboveground vegetation ------

# calculate cubic km of water per 9 km pixel
km_cubed_by_pixel <- aggregate(annual_storage ~
                                 lon + lat + group_2,get_km_cubed_3,
                               data = annual_turnover_lc)

#get total amount of storage by land cover type
lc_total_pools <- aggregate(annual_storage~group_2,sum,data = km_cubed_by_pixel)
sum(lc_total_pools$annual_storage)

# % of total water stored in evergreen broadleaf forests
142.16/379.1
# about 37%, over a third, of water is stored in evergreen broadleaf forests

# % of total water stored in shrublands
71.01/379.1
# almost 19% of water is stored in  shrublands

#quick plot of how total storage varies by land cover type
lc_pool_size <- ggplot(lc_total_pools, aes(y = reorder(group_2,annual_storage), x = annual_storage))  +
  stat_summary(fun='mean',geom='bar',fill='grey70',color='black') +
  scale_x_continuous(expand=c(0,0),limits=c(0,154)) +
  ylab('') +
  xlab(bquote('Freshwater pool size'~(km^3))) +
  theme(
    axis.text.x = element_text(color='black',size=13), 
    axis.text.y = element_text(color='black',size=13),
    axis.title.x = element_text(color='black',size=19),
    axis.title.y = element_text(color='black',size=19),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(size=14),
    legend.position = c(0.6,0.15),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

png(height = 2000,width=3000,res=300,
    'manuscript_figures/total_storage_land_cover.png')

print(lc_pool_size)

dev.off()

rm(lc_total_pools,km_cubed_by_pixel,lc_pool_size)

#-------------------------------------------------------------------------------
# CV of storage and transit ------

#CV of storage by LC type

#import very large dataframe
cv_data <- read.csv('data/turnover_from_python/minimum/all_months.csv')
cv_data <- na.omit(cv_data)

#Create a sample size df
sample_size <- annual_turnover_lc %>%
  dplyr::select(lat,lon,sample_size,group_2) 

cv_data <- merge(cv_data,sample_size,by = c('lat','lon'))

cv_storage_lc <- cv_data %>%
  dplyr::select(group_2,transp_Wm2,VWC) %>%
  dplyr::group_by(group_2) %>%
  dplyr::summarise_all(cv) %>%
  dplyr::rename('Land cover' = 'group_2',
                'Canopy transpiration' = 'transp_Wm2',
                'Water storage' = 'VWC')

cv_storage_lc$`Canopy transpiration` <- round(cv_storage_lc$`Canopy transpiration`,2)
cv_storage_lc$`Water storage` <- round(cv_storage_lc$`Water storage`,2)

write.csv(cv_storage_lc,'manuscript_figures/storage_transp_lc_cv.csv')

#cleanup
rm(cv_data,sample_size,cv_storage_lc)

#-------------------------------------------------------------------------------
# ground-based transit compared to remote sensing transit broken up by season ----

#import ground-based estimate
isotope <- read.csv('data/isotope_data.csv')

# filter out unusable studies
isotope <- isotope %>% 
  dplyr::filter(Drop. == 'No',
                !season == 'none',
                !Latitude == 'NA')

#run loop
seasons <- unique(isotope$season)
seasonal_turnover_comp <- list()
for(i in seasons){
  
  isotope_2 <- subset(isotope,season == i)
  
  #turn ground data to spdf
  coords_transit_ground <- isotope_2[ , c("Longitude", "Latitude"),]  # coordinates
  data_transit_ground   <- data.frame(isotope_2[c(7)])    # data
  crs    <- CRS('+proj=longlat +datum=WGS84 +no_defs') # proj4string of coords
  
  spdf_transit_ground <- SpatialPointsDataFrame(coords = coords_transit_ground,
                                                data = data_transit_ground, 
                                                proj4string = crs)
  
  seasonal_turnover <- seasonal_turnover_import(i)
  
  #turn vod raster data to spatial points DF
  turnover_2 <- seasonal_turnover %>%
    dplyr::select(lon,lat,turnover,group,group_2)
  rownames(turnover_2) <- NULL
  
  # prepare coordinates, data, and proj4string
  coords_transit_vod <- turnover_2[ , c("lon", "lat")]   # coordinates
  data_transit_vod   <- turnover_2[ , 3:5]          # data
  crs    <- CRS('+proj=longlat +datum=WGS84 +no_defs')  # proj4string of coords
  
  # make the SpatialPointsDataFrame object
  spdf_transit_vod <- SpatialPointsDataFrame(coords = coords_transit_vod,
                                             data = data_transit_vod, 
                                             proj4string = crs)
  
  #link ground-based coordinates to vod coordinates for storage
  nn1_transit = get.knnx(coordinates(spdf_transit_vod), coordinates(spdf_transit_ground), k=1)
  vector_transit <- data.frame(nn1_transit[1])
  vector_length <- as.numeric(nrow(vector_transit))
  spdf_vod_transit_df <- data.frame(spdf_transit_vod)
  
  column <- seq(1,1,1) #can change depending on length of k
  new_transit_df_list <- list()
  for(j in column){
    
    vector_transit_2 <- vector_transit[j]
    vector_transit_2 <- vector_transit_2[c(1:vector_length),]
    new_transit_df <- spdf_vod_transit_df[c(vector_transit_2),]
    new_transit_df$ID <- 1:nrow(new_transit_df)
    
    new_transit_df_list[[j]] <- new_transit_df
    
  }
  
  new_transit_df_list_df <- do.call('rbind',new_transit_df_list)
  new_transit_df_list_df <- aggregate(turnover~ ID + group,median,data=new_transit_df_list_df)
  
  new_transit_df_list_df <- new_transit_df_list_df %>%
    dplyr::select(turnover,group,ID) %>%
    dplyr::filter(!group=='Urban') #in case this pops up
  
  #combine ground=based and vod-based water storage
  spdf_transit_ground_df <- data.frame(spdf_transit_ground)
  cbind_transit_ground_vod <- cbind(new_transit_df_list_df,spdf_transit_ground_df)
  cbind_transit_ground_vod <- cbind_transit_ground_vod %>%
    dplyr::filter(!group == c('Water'),
                  !group == c('Urban')) #don't want these two
  
  cbind_transit_ground_vod$season <- i
  
  seasonal_turnover_comp[[i]] <- cbind_transit_ground_vod
  
  
}

seasonal_turnover_comp_df <- do.call('rbind',seasonal_turnover_comp)

#plot it and save
compare_transit <- ggplot(seasonal_turnover_comp_df,aes(x = turnover,
                                                        y = Mean.transit.time..days.,
                                                        color = group,shape=season)) +
  scale_color_manual(values=c('Savanna'='purple','Forest'='orange','Shrubland'='red'),
                     labels=c('Grassland'='Grassland','Forest'='Forest',
                              'Shrubland'='Shrubland','Savanna'='Savanna')) +
  geom_abline(slope=1,size=1,color='black') +
  geom_point(size=7,alpha=0.6) +
  xlab('Satellite-based transit (days)') +
  ylab('Istotope-based transit (days)') +
  annotate("text", x=11, y=9.5, label= "1:1 Line") +
  theme(
    axis.text.x = element_text(color='black',size=15),
    axis.text.y = element_text(color='black',size=15),
    axis.title.x = element_text(color='black',size=22),
    axis.title.y = element_text(color='black',size=22),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=10),
    legend.position = c(0.75,0.70),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save to file
png(height = 2000,width=2500,res=300,
    'manuscript_figures/ground_satellite_transit_comparison.png')

print(compare_transit)

dev.off()

#cleanup
rm(isotope,isotope_2,seasons,seasonal_turnover_comp_df,
   coords_transit_ground,coords_transit_vod,data_transit_ground,data_transit_vod,
   turnover_2,crs,spdf_transit_ground,spdf_transit_ground_df,spdf_transit_vod,
   spdf_vod_transit_df,column,new_transit_df,new_transit_df_list,new_transit_df_list_df,
   cbind_transit_ground_vod,compare_transit,vector_transit,vector_transit_2,
   nn1_transit,i,j,vector_length,seasonal_turnover_comp)

#-------------------------------------------------------------------------------
# spatial correlations of annual transit with climate ----

#import climate data
climate_data <- fread('data/climate.csv')
climate_data <- na.omit(climate_data)
climate_data$mean_precipitation <- climate_data$mean_precipitation*365.25

#loop through each land cover to get correlations with mean climate
climate_cor_lc_list <- list()
for(j in group_2_names){
  
  group_2_lc <- subset(annual_turnover_lc,group_2 == j)
  
  climate_data_lc <- merge(group_2_lc,climate_data,by=c('lat','lon'))
  
  #cor with ppt
  cor_ppt <- cor.test(climate_data_lc$annual_turnover,climate_data_lc$mean_precipitation,
                      method = 'spearman',exact=FALSE)
  ppt_pval <- cor_ppt$p.value
  ppt_cor <- cor_ppt$estimate
  
  #cor with temp
  cor_temp <- cor.test(climate_data_lc$annual_turnover,climate_data_lc$mean_temp,
                       method = 'spearman',exact=FALSE)
  temp_pval <- cor_temp$p.value
  temp_cor <- cor_temp$estimate
  
  climate_val <- c('Precipitation','Temperature')
  cor <- c(ppt_cor,temp_cor)
  pval <- c(ppt_pval,temp_pval)
  
  climate_cor_df <- data.frame(climate_val,cor,pval)
  
  climate_cor_df$land_cover <- j
  
  #store in list
  climate_cor_lc_list[[j]] <- climate_cor_df
  
  rm(j,climate_cor_df,cor,pval,climate_val,group_2_lc,climate_data_lc,cor_ppt,
     ppt_cor)
  
}


climate_cor_lc_df <- do.call('rbind',climate_cor_lc_list)

#plot
climate_corrlations <- ggplot(climate_cor_lc_df,aes(x=factor(land_cover,level=veg_order),
                                                    y=cor)) +
  facet_wrap(~climate_val,ncol=1) + 
  geom_hline(yintercept = 0) +
  stat_summary(fun='mean',geom='bar',fill='grey70',color='black') +
  ylab('Correlation with annual transit time') +
  xlab('') +
  theme(
    axis.text.x = element_text(color='black',size=12,angle=25,hjust=1),
    axis.text.y = element_text(color='black',size=12),
    axis.title.x = element_text(color='black',size=22),
    axis.title.y = element_text(color='black',size=22),
    axis.ticks = element_line(color='black'),
    legend.key = element_blank(),
    legend.title = element_blank(),
    legend.key.size = unit(.50, 'cm'),
    legend.text = element_text(size=10),
    legend.position = 'none',
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), #make the borders clear in prep for just have two axes
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

# print and save
png(height = 2000,width=2000,res=300,
    'manuscript_figures/climate_correlations_annual_transit.png')

print(climate_corrlations)

dev.off()

#cleanup
rm(climate_cor_lc_df,climate_cor_lc_list,cor_temp,climate_data)



#-------------------------------------------------------------------------------