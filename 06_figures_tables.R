
# This script produces many of the figures, tables, summary stats. It is recommended 
# that you run each section on its own instead of running entire script. Some sections 
# use data produced by other sections within this script and R project and in other
# programming environments (python). 

# First, merge the two transit dataframes together by pixel so we are  working from 
# same set of pixels for the analysis 
minimum_turnover_lc <- merge(annual_turnover_lc[c(2,3)],minimum_turnover_lc,
                             by = c('lat','lon'))

#unique vegetation types
group_2_names <- unique(annual_turnover_lc$group_2)

# This is to find the axis order land cover on plots, which order by low to high
# median annualized transit time

# annual_turnover_lc %>%
#   dplyr::group_by(group_2) %>%
#   dplyr::summarise(med = median(annual_turnover)) %>%
#   dplyr::arrange(med)

#resulting order
veg_order <- c("Savanna", "Cropland",'Grassland',"Deciduous broadleaf forest",
               'Mixed forest','Evergreen broadleaf forest',
               'Deciduous needleleaf forest','Shrubland',
               'Evergreen needleleaf forest')

#-------------------------------------------------------------------------------

# annual and minimum transit time by land cover types-------

# list to store each vegetation type
annual_turnover_list_2 <- list()

# right tail truncate for the figures. There is strong right skew that obscures
# interpretation/visualization of the distribution.

# truncate for annual transit, looping through each land cover type.
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

# truncate for minimum transit, looping through for each land cover type
minimum_turnover_list_2 <- list()
for(j in group_2_names){
  
  group_2_lc <- subset(minimum_turnover_lc,group_2 == j)
  
  quantile_95 = quantile(group_2_lc$Turnover,probs = 0.95)
  quantile_95 = as.numeric(quantile_95)
  
  #truncate by 95th percentile
  lc_filtered_2 <- group_2_lc %>%
    dplyr::filter(Turnover < quantile_95) 
  
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
  stat_smooth(data=annual_turnover_lc_95,aes(lat,annual_turnover),col='black',
              linewidth=1,se=F) +
  geom_line(linewidth=0.5,alpha=0.5) +
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

#break up land covers into facets to show each individually
annual_turnover_by_lat_facet <- 
  ggplot(annual_turnover_by_latitude,aes(lat,annual_turnover,col=group)) +
  stat_smooth(data=annual_turnover_lc_95,aes(lat,annual_turnover),col='black',
              linewidth=0.5,se=F) +
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
minimum_turnover_by_latitude <- aggregate(Turnover ~ lat + group,
                                          median,data = minimum_turnover_lc_95)
#minimum turnover by latitude
minimum_turnover_by_lat_plot <- 
  ggplot(minimum_turnover_by_latitude,aes(lat,Turnover,col=group)) +
  stat_smooth(data=minimum_turnover_lc_95,aes(lat,Turnover),col='black',
              linewidth=1) +
  geom_line(linewidth=0.4,alpha=0.5) +
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
  ggplot(minimum_turnover_by_latitude,aes(lat,Turnover,col=group)) +
  stat_smooth(data=minimum_turnover_lc_95,aes(lat,Turnover),col='black',
              linewidth=0.5) +
  facet_wrap(.~group) + 
  facet_wrap(ncol=1,~factor(group,levels=c('Savanna','Cropland','Grassland',
                                           'Forest','Shrubland'))) +
  geom_line(size=0.25,alpha=0.5) +
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
    'manuscript_figures/updated/transit_latitude_full_multipanel_2016-2020.png')

print(plot_grid(annual_turnover_by_lat_plot, annual_turnover_by_lat_facet,
                minimum_turnover_by_lat_plot, minimum_turnover_by_lat_facet,
                labels = c('A)', 'B)','C)','D)'),ncol = 2, nrow=2,
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

#annual transit by landcover type. These figures also use the right-truncated
#data produced in the previous section

#plot
annual_turnover_boxplot <- 
  ggplot(annual_turnover_lc_95,aes(x=factor(group_2,level=veg_order),
                                   y=annual_turnover,
                                   color=annual_turnover)) +
  geom_hline(yintercept = 9) + #global 5-yr median
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
                                    y=Turnover,
                                    color=Turnover)) +
  geom_hline(yintercept = 3.5) + #global 5-yr median
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
    'manuscript_figures/updated/transit_latitude_boxplot_2016-2020.png')

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
  stat_smooth(data=annual_storage_lc_95,aes(lat,annual_storage),col='black',
              linewidth=1) +
  geom_line(linewidth=0.25,alpha=0.75) +
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


#storage by land cover boxplot

#plot
boxplot_annual_storage <- ggplot(annual_storage_lc_95,aes(x=factor(group_2,level=veg_order),
                                                          y=annual_storage,
                                                          color=annual_storage)) +
  geom_hline(yintercept = 3.6) + #global median
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
    'manuscript_figures/updated/storage_multipanel_2016-2020.png')

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
write.csv(annual_turnover_summary, 
          'manuscript_figures/updated/annual_turnover_summaries_2016-2020.csv')

#cleanup
rm(group_2_lc,quantile_5,quantile_50,quantile_95,quantile_df,
   annual_turnover_summary_list,annual_turnover_summary)


#minimum turnover summary
minimum_turnover_summary_list <- list()
for(j in group_2_names){
  
  #load in
  group_2_lc <- subset(minimum_turnover_lc,group_2 == j)
  
  #5th quantile
  quantile_5 = round(quantile(group_2_lc$Turnover,probs = 0.05),2)
  
  #50th quantile
  quantile_50 = round(quantile(group_2_lc$Turnover,probs = 0.5),2)
  
  #95th quantile
  quantile_95 = round(quantile(group_2_lc$Turnover,probs = 0.95),2)
  
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
write.csv(minimum_turnover_summary, 
          'manuscript_figures/updated/minimum_turnover_summaries_2016-2020.csv')
rm(group_2_lc,quantile_5,quantile_50,quantile_95,
   minimum_turnover_summary,minimum_turnover_summary_list,j)


#-------------------------------------------------------------------------------
# transit estimates by each season  -----

#create a function that can input the name of the season and output the dataframe

#winter
winter_summary <- seasonal_turnover_lc('winter',winter = T)

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
write.csv(annual_storage_summary, 
          'manuscript_figures/updated/annual_storage_summaries_2016-2020.csv')

#cleanup
rm(group_2_lc,quantile_5,quantile_50,
   quantile_95,annual_storage_summary,j,annual_storage_summary_list)

#-------------------------------------------------------------------------------
# correlate ground-based aboveground water storage with vod-based water storage ------

#set projection
crs  <- CRS('+proj=longlat +datum=WGS84 +no_defs') # proj4string of coords

#import ground-based water content data
ground_estimates <- read.csv('data/water_content_data_May2024.csv')

# filter out rows with unusable sites and trim down columns
ground_estimates <- ground_estimates %>%
  dplyr::filter(exclude == 'No') %>%
  dplyr::select(Latitude,Longitude,land_cover,mean_wc_g_g) %>%
  dplyr::mutate(mean_wc_g_g = round(mean_wc_g_g,2),
                id = rownames(.))

# import aboveground dry biomass rasters. We will import both native 300 m and aggregated
# 9km spatial resolutions to compare them. If there are big discrepancies in biomass between the
# two spatial scales, then we remove that location from our analysis.

dry_biomass_9km <-
  raster('data/supporting_data/aboveground_dry_biomass_density_aggregate_30X.tif')

dry_biomass_300m <- 
  raster('data/supporting_data/AFELTON_agbDW_Mgha_x10_300m.tif')

#extract biomass value from raster for each ground-based point coordinate
dry_biomass_9km <- data.frame(
  raster::extract(dry_biomass_9km,
                  ground_estimates[,c("Longitude", "Latitude")]))

#need to convert to g/m^2
dry_biomass_9km <- dry_biomass_9km/10

#1000000 grams = 1 megagram
dry_biomass_9km <- dry_biomass_9km*1000000

#1 hectare = 10000 square meters to get g/m^2
dry_biomass_9km <- round(dry_biomass_9km/10000,2)

dry_biomass_300m <- data.frame(
  raster::extract(dry_biomass_300m,
                  ground_estimates[,c("Longitude", "Latitude")]))

#need to convert to g/m^2
dry_biomass_300m <- dry_biomass_300m/10

#1000000 grams = 1 megagram
dry_biomass_300m <- dry_biomass_300m*1000000

#1 hectare = 10000 square meters to get g/m^2
dry_biomass_300m <- round(dry_biomass_300m/10000,2)

#quick look at the correlation between the native and aggregated data
# plot(dry_biomass_9km$raster..extract.dry_biomass_9km..ground_estimates...c..Longitude...,
#      dry_biomass_300m$raster..extract.dry_biomass_300m..ground_estimates...c..Longitude...)

#filter down to locations in which dry biomass is not over 100% different (smaller or larger)
#between 9km and 300 m spatial scales. This is a quantitative check to
#ensure our ground-based estimates can be scaled-up across the 9km VOD footprint
#we then convert dry biomass to water content via multiplying grams of dry biomass
#by water content, which is grams of water per grams of dry mass. We then convert to g
#kg as kg/m^2 of water is equivalent to mm of water.

dry_biomass_ground_vwc <- cbind(dry_biomass_9km,dry_biomass_300m) %>%
  dplyr::rename(dry_biomass_9km_g = raster..extract.dry_biomass_9km..ground_estimates...c..Longitude...,
                dry_biomass_300m_g = raster..extract.dry_biomass_300m..ground_estimates...c..Longitude...) %>%
  dplyr::mutate(biomass_difference = dry_biomass_9km_g/dry_biomass_300m_g) %>%
  dplyr::filter(biomass_difference > 0.5, biomass_difference < 1.5) %>%
  dplyr::mutate(id = rownames(.)) %>%
  dplyr::left_join(ground_estimates,join_by(id)) %>%
  dplyr::mutate(ground_based_vwc_9km_mm = round(((dry_biomass_9km_g*mean_wc_g_g)*.001),2),
                ground_based_vwc_300m_mm = round(((dry_biomass_300m_g*mean_wc_g_g)*.001),2)) %>%
  dplyr::select(Latitude,Longitude,land_cover,id,mean_wc_g_g,dry_biomass_9km_g,dry_biomass_300m_g,
                ground_based_vwc_9km_mm, ground_based_vwc_300m_mm,biomass_difference)

#quick look again to see correlation between native and aggregated biomass
# plot(dry_biomass_ground_vwc$dry_biomass_9km_g,
#      dry_biomass_ground_vwc$dry_biomass_300m_g)

rm(dry_biomass_300m,dry_biomass_9km)

#make the SpatialPointsDataFrame object for ground-based veg storage
spdf_ground_vwc <- 
  SpatialPointsDataFrame(
    coords = dry_biomass_ground_vwc[,c("Longitude", "Latitude")],
    data = data.frame(dry_biomass_ground_vwc[,c("ground_based_vwc_300m_mm","id")]),
    proj4string = crs)

#make the SpatialPointsDataFrame object for vod-based veg storage
spdf_vod_vwc <- SpatialPointsDataFrame(
  coords = annual_turnover_lc[,c("lon", "lat")],
  data = data.frame(annual_turnover_lc[,c("group","annual_storage")]), 
  proj4string = crs)

#link ground-based coordinates to vod coordinates for storage using nearest neighbor approach

#get an index of the nearest neighbor (coordinate) for VOD-based coordinates 
#and ground-based coordinates
nn1 = get.knnx(coordinates(spdf_vod_vwc), coordinates(spdf_ground_vwc), 1)
vector <- data.frame(nn1[1]) 
vector <- vector[1:nrow(vector),] #extract row-based index values for later extraction 

#extract VOD-based VWC values according to the near-neighbor index in the vector object
new_df <- (data.frame(spdf_vod_vwc))[c(vector),]

#there are repeats in terms of correlating nearby ground-based VWC with
#the same 9km VOD-based VWC. We need to further aggregated so that we are comparing
#one unique ground-based observation to one unique VOD-based observation. We do this
#by performing our aggregation (averaging) of ground-based VWC across unique values 
#of VOD-based VOD. A comparison shows that the coordinates line up correctly. 

#aggregate for duplicate VOD-based VWC values caused (nearby) ground-based coordinates
vod_ground_vwc_compare <- cbind(new_df,dry_biomass_ground_vwc) %>%
  dplyr::mutate(biomass_difference = dry_biomass_9km_g/dry_biomass_300m_g) %>%
  dplyr::filter(biomass_difference > 0.5, biomass_difference < 1.50) %>% #for good measure
  dplyr::group_by(lat,lon,group) %>%
  dplyr::summarise(mean_wc_g_g = round(mean(mean_wc_g_g),2),
                   dry_biomass_9km_g = round(mean(dry_biomass_9km_g),2),
                   dry_biomass_300m_g = round(mean(dry_biomass_300m_g),2),
                   ground_based_vwc_9km_mm = round(mean(ground_based_vwc_9km_mm),2),
                   ground_based_vwc_300m_mm = round(mean(ground_based_vwc_300m_mm),2),
                   vod_based_vwc_9km_mm = round(mean(annual_storage),2)) %>%
  dplyr::ungroup() %>%
  dplyr::rename(land_cover = group) 

#now to look at some correlations between the two

#check to see if anomalously high shrubland value is an outlier from relationship

plot(ground_based_vwc_9km_mm ~ vod_based_vwc_9km_mm,data=vod_ground_vwc_compare)

#load car to package to help with outlier test
library(car)

outlierTest(lm(
  ground_based_vwc_9km_mm ~ vod_based_vwc_9km_mm,data=vod_ground_vwc_compare))

#confirmed that this (obs. 4) is an outlier. Remove that specific location
vod_ground_vwc_compare_no_outlier <- vod_ground_vwc_compare[-4,]

#correlate ground-based storage with VOD-based storage
cor.test(vod_ground_vwc_compare_no_outlier$ground_based_vwc_9km_mm,
         vod_ground_vwc_compare_no_outlier$vod_based_vwc_9km_mm,method='spearman')

#correlate abovegorund biomass with VOD-based storage
cor.test(vod_ground_vwc_compare_no_outlier$dry_biomass_9km_g,
         vod_ground_vwc_compare_no_outlier$vod_based_vwc_9km_mm,method='spearman')

#look at mean difference between ground-based and vod-based storage
mean(vod_ground_vwc_compare$ground_based_vwc_9km_mm) - 
  mean(vod_ground_vwc_compare$vod_based_vwc_9km_mm)

#look at correlation without agricultural land cover types
vod_ground_vwc_compare_no_outlier_or_ag <- vod_ground_vwc_compare_no_outlier %>%
  dplyr::filter(land_cover != "Cropland")

cor.test(vod_ground_vwc_compare_no_outlier_or_ag$ground_based_vwc_9km_mm,
         vod_ground_vwc_compare_no_outlier_or_ag$vod_based_vwc_9km_mm,method='spearman')

#plot out the relationship
vod_vwc_plot <- ggplot(vod_ground_vwc_compare_no_outlier,
                       aes(vod_based_vwc_9km_mm,ground_based_vwc_9km_mm,fill=land_cover)) +
  scale_x_continuous(limits=c(0,18)) +
  scale_y_continuous(limits=c(0,18)) +
  geom_point(size=7,pch=21,alpha = 0.75) +
  scale_fill_manual(values=c('Cropland'='purple','Savanna'='darkblue',
                             'Grassland'='lightblue','Forest'='orange','Shrubland'='red'),
                    labels=c('Grassland'='Grassland','Forest'='Forest',
                             'Shrubland'='Shrubland','Savanna'='Savanna')) +
  annotate(geom = "text",
           label = as.character(expression(paste(rho, " = 0.36"))),
           parse = TRUE, x=14, y=17,size=10) +
  annotate("text", x=15.2, y=13, label= "1:1 Line",size=8) +
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
    legend.position = c(0.77,0.25),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#get the range of total global storage 2016-2020
years <- 2016:2020
storage_vec <- vector()
transit_vec <- vector()
for(i in 1:length(years)){
  
  print(years[i])
  
  temp <- 
    read.csv(paste0('data/turnover_from_python/updated/annual/annual_turnover',
                    years[i],'.csv'))
  
  km_cubed_by_pixel <- aggregate(annual_storage ~
                                   lon + lat,get_km_cubed_3,
                                 data = temp)
  
  #filter to pixels with at least an average of 4 months of data
  km_cubed_by_pixel <- 
    merge(km_cubed_by_pixel,annual_turnover_lc[,c(2,3)],by = c('lat','lon'))
  
  print(length(km_cubed_by_pixel$annual_storage))
  
  storage_vec[i] <- sum(km_cubed_by_pixel$annual_storage)
  transit_vec[i] <- quantile(temp$annual_turnover,probs = 0.5,na.rm=T)
  
  rm(km_cubed_by_pixel,temp)
  
  
}

#quick look at 5-yr standard deviation of global storage and transit
#round(sd(storage_vec),2) #1.9 cubed km
#round(sd(transit_vec),2) #0.31 days annual variation around global median transit

#compare out estimate of total aboveground vegetationw ater storage to to other 
# freshwater pools abd previous estimates
pools <- read.csv('data/H2OPoolSizeEstimates.csv')
pools <- pools %>%
  dplyr::filter(Citation != 'Bar-On 2018')

pools$size <- as.numeric(as.character(pools$Size..km3.))

#pool means
pool_means <- pools %>%
  dplyr::group_by(Pool) %>%
  dplyr::summarise(mean_size = mean(Size..km3.),
                   max_size = max(Size..km3.),
                   min_size = min(Size..km3.))

#contextualize our veg water storage estimate
16500.000/381 #compare our VOD-based estimate to soil water estimate

pool_size <- ggplot(pool_means, aes(y = reorder(Pool,mean_size), x = mean_size))  +
  geom_col(fill='grey70',color='black')+
  ylab(NULL) +
  scale_x_continuous(expand=c(0,0),
                     trans = "log", breaks = 10^c(1,3,5,8), labels = scales::comma) +
  xlab(expression('Freshwater pool size'~(km^3))) +
  geom_errorbar(data = pool_means, mapping=aes(xmin=min_size,xmax=max_size),
                size=0.5,width=.25) +
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
    'manuscript_figures/updated/storage_multipanel_may2024.png')

print(plot_grid(pool_size,vod_vwc_plot,labels = c('', ''),ncol = 2, nrow=1,
                rel_widths = c(2.5,2.75),
                rel_heights = c(1,1),label_size = 25))

dev.off()

#cleanup
rm(crs,ground_estimates,nn1,pool_means,pool_size,pools,spdf_ground_vwc,
   spdf_vod_vwc,vod_ground_vwc_compare_no_outlier,
   vod_ground_vwc_compare,vod_vwc_plot,new_df,dry_biomass_ground_vwc,
   vod_ground_vwc_compare_no_outlier_or_ag)

#-------------------------------------------------------------------------------
# calculate total (global) amount of water in aboveground vegetation ------

# calculate cubic km of water per 9 km pixel
km_cubed_by_pixel <- aggregate(annual_storage ~
                                 lon + lat + group_2,get_km_cubed_3,
                               data = annual_turnover_lc)

#get total amount of storage by land cover type
lc_total_pools <- aggregate(annual_storage~group_2,sum,data = km_cubed_by_pixel)
sum(lc_total_pools$annual_storage)

# % of total water stored in evergreen broadleaf forests
141.8/sum(lc_total_pools$annual_storage)
# about 37%, over a third, of (biological) water is stored in evergreen broadleaf forests

# % of total water stored in shrublands
71/sum(lc_total_pools$annual_storage)
# almost 19% of (biological) water is stored in  shrublands

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
    'manuscript_figures/updated/total_storage_land_cover_2016-2020.png')

print(lc_pool_size)

dev.off()

rm(lc_total_pools,km_cubed_by_pixel,lc_pool_size)

#-------------------------------------------------------------------------------
# monthly-based CV of storage and transit  ------

#CV of storage by LC type

#spatial variability of five-year average transp and storage
cv_storage_lc_spatial <- annual_turnover_lc %>%
  dplyr::group_by(group_2) %>%
  dplyr::summarise(cv_transp = cv(daily_transp_annual),
                   cv_storage = cv(annual_storage))

#save to file
write.csv(cv_storage_lc_spatial,
          'manuscript_figures/updated/storage_transp_lc_temporal_cv.csv')

#import very large dataframe (originally used spatial.csv)
cv_storage_lc_temporal <- 
  read.csv('data/turnover_from_python/updated/annual/multi_year_variability/monthly_scale_cv.csv')

cv_storage_lc_temporal <- merge(cv_storage_lc_temporal,annual_turnover_lc[c(2,3,11)],
                                by = c('lat','lon'))

cv_storage_lc_temporal <- cv_storage_lc_temporal %>%
  dplyr::group_by(group_2) %>%
  dplyr::summarise(transp_50 = round(quantile(transp_Wm2,probs=0.5),2),
                   transp_5 = round(quantile(transp_Wm2,probs=0.05),2),
                   transp_95 = round(quantile(transp_Wm2,probs=0.95),2),
                   storage_50 = round(quantile(VWC,probs=0.50),2),
                   storage_05 = round(quantile(VWC,probs=0.05),2),
                   storage_95 = round(quantile(VWC,probs=0.95),2))

# save to file
write.csv(cv_storage_lc_temporal,
          'manuscript_figures/updated/storage_transp_lc_temporal_cv.csv')

#cleanup
rm(cv_storage_lc_spatial,cv_storage_lc_temporal)

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
  
  #link ground-based coordinates to vod coordinates for storage using nearest neighbor
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
  geom_point(size=8) +
  xlab('Satellite-based transit (days)') +
  ylab('Istotope-based transit (days)') +
  annotate("text", x=10.5, y=9.5, label= "1:1 Line") +
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
    legend.position = c(0.13,0.77),
    strip.background =element_rect(fill="white"),
    strip.text = element_text(size=10),
    panel.background = element_rect(fill=NA),
    panel.border = element_blank(), 
    axis.line.x = element_line(colour = "black"),
    axis.line.y = element_line(colour = "black"))

#save to file
png(height = 2000,width=2500,res=300,
    'manuscript_figures/updated/ground_satellite_transit_comparison.png')

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
    'manuscript_figures/updated/climate_correlations_annual_transit_2016-2020.png')

print(climate_corrlations)

dev.off()

#cleanup
rm(climate_cor_lc_df,climate_cor_lc_list,cor_temp,climate_data)



#-------------------------------------------------------------------------------
