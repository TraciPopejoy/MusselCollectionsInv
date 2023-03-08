# APPENDIX 1.3: R script to create ecological attributes (lines 23559-23731)
##############################################################################
# Based on code from the National Fishes Vulnerability Assessment - Revised by Sam Silknetter, 03April2020
library(tidyverse);library(sf)
library(cowplot); library(maps)

# bring in the occurrence data
occs_sp<-read.csv('3d_species_records.csv')
PATH_WBD <- '/Users/PfeifferJ/Desktop/GitHub/MusselCollectionsInv/spatial_data/WBD_National_GDB.gdb'
huc8<-read_sf(dsn=PATH_WBD, layer='WBDHU8')
crs.geo <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84")
crs.albers <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")

# function to calculate mode for non-normal data
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# Find the traits of each species ----
geo_traits<-NULL
YearAOO<-NULL
stream_char<-NULL

# only when georeffed lots > 10
robust.spp<-occs_sp %>%
	# remove flagged occs and those without georeferences
  filter(!is.na(standardized_latitude),
         state_mismatch == F,
         outside_NHD == F,
         InRange == T,
         dupe_flag == F) %>%
  count(species_update) %>%
  filter(n > 10)

for(i in robust.spp$species_update){
  # load in georeferenced occurence for each species
  occ.df <- occs_sp %>%
	# remove flagged occs and those without georeferences
    filter(!is.na(standardized_latitude),
           species_update == i, 
           state_mismatch == F,
           outside_NHD == F,
           InRange == T,
           dupe_flag == F)
  # count the number of unique watersheds at important scales
  huc_count<-occ.df %>% as_tibble() %>%
    summarize(nhuc6=n_distinct(huc6),
              nhuc8=n_distinct(huc8), 
              nhuc10=n_distinct(huc10)) 
  # calculate watershed area
  watershed_poly <- occ.df %>% as_tibble() %>%
    # only keep one record for each huc
    distinct(species_update, huc8, .keep_all=T) %>%
    mutate(huc8=ifelse(nchar(huc8)==7, paste0('0',huc8), as.character(huc8))) %>%
    left_join(huc8, by='huc8') # join with WBD
  print(nrow(watershed_poly)==huc_count$nhuc8) #check filtering worked correctly
  wsarea <- sum(watershed_poly$areasqkm, na.rm=T)  # Sum the total area (square kilometer) for each unique HUC12.
  # calculate total watershed area, for occs with year info
  watershed_polywy <- occ.df %>% as_tibble() %>%
    filter(!is.na(Year)) %>%
    # only keep one record for each huc
    distinct(species_update, huc8, .keep_all=T) %>%
    mutate(huc8=ifelse(nchar(huc8)==7, paste0('0',huc8), as.character(huc8))) %>%
    left_join(huc8, by='huc8') # join with WBD
  wsarea_wy<-sum(watershed_polywy$areasqkm, na.rm=T)  # Sum the total area (square kilometer) for each unique HUC12.
  
  # Look at area shift through time
  # if less than two points or year not presence in < 2 points, skip this
  if(sum(!is.na(occ.df$Year)) > 2){ 
    # identify median year
    year_cdf<-summary(occ.df$Year)
    med_year<-year_cdf["Median"]
   
     year_info<-occ.df %>% as_tibble() %>%
      filter(!is.na(Year)) %>% # only lots that have years associated with them
      # group as before or after & fix character strings
      mutate(year_group=ifelse(Year <= med_year, 'before', 'after'),
             huc8=ifelse(nchar(huc8)==7, paste0('0',huc8), as.character(huc8))) %>%
      left_join(huc8, by='huc8') %>%
      distinct(year_group, huc8, .keep_all=T) %>% #only keep one row for each year_group x huc8 intersection 
      group_by(year_group) %>% # for each year group
      summarize(area=sum(areasqkm, na.rm=T)) %>%
      pivot_wider(names_from = year_group, values_from=area) %>% 
      bind_cols(median_year=med_year, taxa = i)
  }else{
    year_info<-data.frame(note='too few', taxa = i)
  }
   
  # calculate buffered polygon area
  # Generate 1km point buffers. You must use a projected CRS. For CRS, we use: USA Contiguous albers equal area. 
  occ.df <- occ.df %>% 
    st_as_sf(coords=c("standardized_longitude","standardized_latitude"), crs=crs.geo) %>%
    st_transform(crs.albers) #overwrites to the projection 
  # Create 1 km buffer and calculate the total area occupied per species.
  buff_poly <- st_union(st_buffer(occ.df, dist = 5))
  bfarea<-st_area(buff_poly)
  
  # Figure out other geographic traits
  ex<-buff_poly %>%  st_transform(crs.geo) %>% st_bbox()
  eoo<-buff_poly %>% st_union() %>% st_convex_hull() 
  #ggplot()+geom_sf(data=states) + geom_sf(data=eoo, fill='purple', alpha=0.5)
  mid_pt<-st_centroid(eoo) %>% st_transform(crs.geo) %>% st_coordinates()
  
  # create geo traits dataframe to output
  geo_onesp <- data.frame(taxa = i, 
                     trait = c('aoo 5km Buffer sqkm','aoo HUC8 sqkm','total_AOO_with_year',
                               'nhuc6','nhuc8','nhuc10',
                               'min longitude','min latitude',
                               'max longitude', 'max latitude',
                               'eoo area sqkm', 'mid longitude', 'mid latitude',
                               'noccs'),
                     value = c(as.numeric(bfarea), wsarea, wsarea_wy,
                               t(huc_count),
                               as.matrix(ex), as.numeric(st_area(eoo)), t(mid_pt),
                               nrow(occ.df)))
  # stream characteristic information pulled from NHDPlusV2
  sp_stc<-occ.df %>% as_tibble() %>%
    filter(VE_MA >= 0, QE_MA >= 0, SLOPE >= 0) %>%
    group_by(species_update) %>%
    summarize(modeStreamOrder=getmode(StreamOrde),
              iqrStreamOrder=IQR(StreamOrde),
              medianSlope=median(SLOPE),
              iqrSlope=IQR(SLOPE),
              medianQA_MA=median(QE_MA),
              iqrQA_MA=IQR(QE_MA),
              medianVA_MA=median(VE_MA),
              iqrVA_MA=IQR(VE_MA),
              n_sc_lots=n())

  # output all data
  YearAOO<-bind_rows(YearAOO, year_info)
  geo_traits<-bind_rows(geo_traits, geo_onesp)
  stream_char<-bind_rows(stream_char, sp_stc)
  print(paste('done with', i, '; number', which(i==robust.spp$species_update), 'of', nrow(robust.spp)))
}

View(geo_traits)
View(YearAOO)

# check median year working
test.spp<-occs_sp %>% 
  filter(!is.na(standardized_latitude),
         species_update == "Alasmidonta triangulata", 
         state_mismatch == F,
         outside_NHD == F,
         InRange == T,
         dupe_flag == F)
test.spp %>% 
  ggplot()+geom_histogram(aes(x=Year))+
  geom_vline(aes(xintercept=median(Year, na.rm=T)))+
  geom_text(data=. %>% summarize(m_year=median(Year, na.rm=T)),
            aes(x=m_year, y=-1, label=round(m_year,1)))

# Table AOO ====
s3<-geo_traits %>%
  filter(trait %in% c("aoo HUC8 sqkm", "total_AOO_with_year", "nhuc8","noccs")|grepl('tude',trait)) %>%
  pivot_wider(names_from = trait, values_from=value) %>%
  left_join(YearAOO) %>%
  mutate(after_per=round((after/total_AOO_with_year)*100,1),
         before_per=round((before/total_AOO_with_year)*100,1)) %>%
  select(taxa, noccs, nhuc8, `aoo HUC8 sqkm`, total_AOO_with_year, median_year, after, after_per, before, before_per, ends_with('tude')) %>%
  mutate(per.change=after_per-before_per) %>%
  left_join(stream_char, by=c('taxa'='species_update'))
s3 %>% filter(is.na(after_per)|is.na(n_sc_lots)) %>% pull(taxa)
s3 %>% filter(after_per==max(after_per))
s3 %>% filter(per.change==max(per.change)|per.change==min(per.change))
write.csv(s3, 'species_attributes.csv', row.names = F)

# missing taxa from this analysis -----
unique(occs_sp$species_update)[which(!c(unique(occs_sp$species_update)%in%robust.spp$species))]
s3 %>%
  filter(is.na(after_per)|is.na(n_sc_lots)) %>% pull(taxa)

##############################################################################
# APPENDIX 1.4: R script to create HUC attribute (lines 23734-23921)
##############################################################################

# calculate basic huc8 statistics
sec4.huc8<-occs_sp %>% 
  filter(InRange==T, state_mismatch==F | is.na(state_mismatch),
         dupe_flag==F) %>%
  group_by(huc8, huc8_name) %>% 
  summarize(n = n(), #numer of rows
            n_species = n_distinct(species_update), # number of unique species
            med_year = ifelse(sum(!is.na(Year))>=1, # median year, this part sums T where year is not NA
                              summary(Year)[3], NA)) # if more than one year, finds median, if not NA
# use the above frame to calculate info at occ level and then summarize to huc8 level
huc8.space<-occs_sp %>%
	# remove flagged occs
  filter(InRange==T, state_mismatch==F | is.na(state_mismatch),
         dupe_flag==F) %>%
  group_by(huc8) %>%
    # at each huc8, add whether the occ is before or after the median huc8 year
  mutate(year_group=ifelse(Year <= median(Year, na.rm=T), 'before','after'))%>%
  filter(!is.na(year_group)) %>% # remove lots where year is NA
  group_by(huc8_name, huc8, year_group) %>%
    # count number of distinct years and species (aka species richness) at the huc8-timeperiod level
  summarise(n_sp_time = n_distinct(species_update),
            n_year=n_distinct(Year)) %>% 
  pivot_wider(names_from=year_group, values_from=c(n_sp_time, n_year), values_fill=0) %>% 
  left_join(sec4.huc8) %>% 
	# quantify percentages
  mutate(per_before=n_sp_time_before/n_species*100,
         per_after=n_sp_time_after/n_species*100,
         per_change=per_after-per_before,
         huc8_real=as.character(ifelse(nchar(huc8)==7,paste0('0',huc8),huc8))) %>% 
  select(huc8_name, huc8, huc8_real, n, n_species, med_year, n_year_before, n_year_after, everything()) %>%
  filter(!is.na(huc8)) %>% # remove non georeferenced lots
  arrange(desc(n)) %>%
  left_join(huc8, by=c("huc8_real"="huc8")) %>% select(-huc8_real, -shape, -tnmid)

write.csv(huc8.space,
          'huc8_attributes.csv', row.names=F)

# Map Figures ----------------------
#download state map
tmp_dl <- tempfile()
download.file("http://www2.census.gov/geo/tiger/GENZ2013/cb_2013_us_state_20m.zip", tmp_dl)
unzip(tmp_dl, exdir='.')


huc8<-read_sf(dsn=PATH_WBD, layer='WBDHU8') %>%
  # code to remove aleutian islands and other E hemisphere hucs
  mutate(huc6=substr(huc8,1,6)) %>% 
  filter(huc6 < 190000, huc8 != 19030103 )
states <- read_sf('.', "cb_2013_us_state_20m") %>% # map of the states in conterminous US
  st_transform(st_crs(huc8))
ratioAlaska <- (2500000 - 200000) / (1600000 - (-2400000)) #used for plotting Alaska
# to plot alaska state
alaska<-states %>% 
  filter(NAME == 'Alaska') %>% 
  st_crop(xmin=-180, xmax=0, ymin=0, ymax=90)
usa_l48<-states %>% 
  filter(!(NAME %in% c('Alaska','Hawaii')),
         !grepl('Rico', NAME))
# identify alaska huc8
alas<-read_sf(dsn=PATH_WBD, layer='WBDHU8') %>%
  # code to remove aleutian islands and other E hemisphere hucs
  mutate(huc6=substr(huc8,1,6)) %>%
  filter(huc6>= 19000, huc6 < 191000,  huc8 != 19030103)
# Figure 6 -------------
# n occurrence plots ---
max(huc8.space$n)
# build alaska to add in the bottom left corner of the map
ak_plot_n <- alas %>%
  right_join(huc8.space %>%
               mutate(huc8_real=as.character(ifelse(nchar(huc8)==7,paste0('0',huc8),huc8))), 
             by=c('huc8'='huc8_real')) %>%
  select(huc8.y, huc8, shape, n) %>%
  mutate(log1pn=log1p(n)) %>%
  ggplot()+
  geom_sf(aes(fill=log1pn, color=log1pn))+
  geom_sf(data=alaska, fill=NA, color='lightgrey', size=0.2)+
  # make sure scale aligns with usa plot
  scale_fill_viridis_c('n occurrences',
                       breaks=log1p(c(1,25,150,1750,7940)),
                       labels=function(x){round(expm1(x),3)},
                       aesthetics=c('color','fill'))+
  theme_void()+
  theme(legend.position='none')
# build conterminous usa plot
usa_n<-huc8 %>%
  right_join(huc8.space %>%
               mutate(huc8_real=as.character(ifelse(nchar(huc8)==7,paste0('0',huc8),huc8))), 
             by=c('huc8'='huc8_real')) %>%
  select(huc8.y, huc8, shape, n) %>%
  mutate(log1pn=log1p(n)) %>%
  ggplot()+
  geom_sf(aes(fill=log1pn, color=log1pn))+
  geom_sf(data=usa_l48, fill=NA, color='lightgrey', size=0.2)+
  scale_fill_viridis_c('n occurrences',
                       breaks=log1p(c(1,25,150,1750,7940)),
                       labels=function(x){round(expm1(x),3)},
                       aesthetics=c('color','fill'))+
  theme_void()

# species richness plots ---
max(huc8.space$n_species)
ak_plot_rich <- alas %>%
  right_join(huc8.space %>%
               mutate(huc8_real=as.character(ifelse(nchar(huc8)==7,paste0('0',huc8),huc8))), 
             by=c('huc8'='huc8_real')) %>%
  select(huc8.y, huc8, shape, n_species) %>%
  ggplot()+
  geom_sf(aes(fill=n_species, color=n_species))+
  geom_sf(data=alaska, fill=NA, color='lightgrey', size=0.2)+
  # make sure scale aligns with usa plot
  scale_fill_viridis_c('n species',
                       breaks=c(1,25,50,75,92),
                       aesthetics=c('color','fill'))+
  theme_void()+
  theme(legend.position='none')

usa_rich<-huc8 %>%
  right_join(huc8.space %>%
               mutate(huc8_real=as.character(ifelse(nchar(huc8)==7,paste0('0',huc8),huc8))), 
             by=c('huc8'='huc8_real')) %>%
  select(huc8.y, huc8, shape, n_species) %>%
  ggplot()+
  geom_sf(aes(fill=n_species, color=n_species))+
  geom_sf(data=usa_l48, fill=NA, color='lightgrey', size=0.2)+
  scale_fill_viridis_c('n species',
                       breaks=c(1,25,50,75,92),
                       aesthetics=c('color','fill'))+
  theme_void()

ggsave('Fig6.pdf',
       plot_grid(ggdraw(usa_n)+
                   draw_plot(ak_plot_n, width = 0.23, height = 0.23 * 10/6 * ratioAlaska, 
                             x = 0.03, y = 0.06),
                 ggdraw(usa_rich)+
                   draw_plot(ak_plot_rich, width = 0.23, height = 0.23 * 10/6 * ratioAlaska, 
                             x = 0.03, y = 0.06),
                 labels='AUTO'),
       width=6.5, height=2.1)

# Figure 7A Species loss ------
per_bins<-hist(huc8.space$per_change, breaks=7)
per_bins$breaks
huc8.sp.plot.df<-huc8.space %>%
  mutate(per_bin=cut(per_change, breaks = per_bins$breaks)) 	

# alaska
ak_plot_sp <- alas %>%
  right_join(huc8.sp.plot.df %>%
               filter(n_year_after!=0, n_year_before !=0) %>%
               mutate(huc8_real=as.character(ifelse(nchar(huc8)==7,paste0('0',huc8),huc8))), 
             by=c('huc8'='huc8_real')) %>%
  select(huc8.y, huc8, shape, med_year, per_bin) %>%
  ggplot()+
  geom_sf(aes(fill=per_bin, color=per_bin))+
  geom_sf(data=alaska, fill=NA, color='lightgrey', size=0.2)+
  scale_fill_viridis_d('Percent change', na.value=NA, begin = 1, end=0,
                    labels=function(x){ifelse(is.na(x),'',x)},
                    aesthetics = c('fill','color'))+
  theme_void()+
  theme(legend.position='none')

# conterminous US
usa_sp<-huc8 %>%
  left_join(huc8.sp.plot.df %>%
              filter(n_year_after!=0, n_year_before !=0) %>%
              mutate(huc8_real=as.character(ifelse(nchar(huc8)==7,paste0('0',huc8),huc8))),
            by=c('huc8'='huc8_real')) %>%
  select(huc8.y, huc8, shape, med_year, per_bin) %>%
  ggplot()+
  geom_sf(aes(fill=per_bin, color=per_bin))+
  geom_sf(data=usa_l48, fill=NA, color='lightgrey', size=0.2)+
  scale_fill_viridis_d('Percent change', begin = 1, end=0,
                    labels=function(x){ifelse(is.na(x),'',x)},
                    na.value=NA, aesthetics = c('fill','color'))+
  theme_void()+ theme(legend.position='none')

ggsave('Fig7A.pdf',
       ggdraw(usa_sp)+
         draw_plot(ak_plot_sp, width = 0.23, height = 0.23 * 10/6 * ratioAlaska, 
                   x = 0.03, y = 0.06),
       width=3, height=2.1)  
