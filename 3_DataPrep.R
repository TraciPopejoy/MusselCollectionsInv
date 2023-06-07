##############################################################################
#APPENDIX 1.2: R script to prepare refined data for analysis 
##############################################################################
getwd()
# load necessary libraries
library(sf); library(tidyverse); library(scales) # load necessary libraries.
library(rgbif) # for taxonomic backbone
library(cowplot) #for plotting maps later
# for building shapefiles/creating 'species_update' & 'InRange' filters
library(maps); library(rgdal); library(gridExtra); library(maptools); library(raster); library(rgeos); library(grid); library(mapplots); library(mapproj); library(ggspatial); library(ggrepel); library(rnaturalearth); library(CoordinateCleaner) 
# to download spatial dataframes (NHDPlusv2 and Watershed Boundary Dataset, respectively)
library(nhdplusTools) #install.packages("devtools"); devtools::install_github("USGS-R/nhdplusTools")

# code to download spatial databases (WBD and the NHDPlusV2)
# run these once
download_nhdplusv2(
  outdir = "nhdPlusV2",
  url = paste0("https://edap-ow-data-commons.s3.amazonaws.com/NHDPlusV21/",
               "Data/NationalData/NHDPlusV21_NationalData_Seamless", "_Geodatabase_Lower48_07.7z"),
  progress = TRUE
)

download_wbd(
  outdir = "spatial_data",
  url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/",
               "Hydrography/WBD/National/GDB/WBD_National_GDB.zip"),
  progress = TRUE
)

# path locations
outdirNHD<-'nhdPlusV2/'
outdirWBD<-'spatial_data/'
PATH_OpenRefineOutput <- '3a_refined.csv'
PATH_NHDv2 <- paste0(outdirNHD,'NHDPlusNationalData/NHDPlusV21_National_Seamless_Flattened_Lower48.gdb')
PATH_WBD <- paste0(outdirWBD,'WBD_National_GDB/')
wd <-"/Users/PfeifferJ/Desktop/GitHub/MusselCollectionsInv/" # PUT YOUR WORKING DIRECTORY HERE
PATH_sppranges <- paste0(wd,'species ranges/')

#load the data
occ <- read_csv(PATH_OpenRefineOutput, trim_ws = TRUE, col_types = 'ccccccccccdccc') %>%
  rowid_to_column() # add a unique number to each column as a check at the end/throughout

# Flag any badly geolocated lots ----
# Set spatial coordinate reference system (CRS).
crs.geo <- st_crs("+proj=longlat +ellps=WGS84 +datum=WGS84")
crs.albers <- st_crs("+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=37.5 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=km +no_defs")#2.12kb

# pull out records that have degrees, minutes, seconds
mo.nondec<-occ %>%
  filter(grepl(':', Latitude) | grepl('//"', Latitude) | grepl("°", Latitude)) %>%
  mutate(converted=TRUE)

# function to convert other forms of latitude/longitude ----
conv_dec<-function(x, char=':'){
  # remove letters from the numbers
  y<-gsub(' N','', x) %>% gsub(' W','', .) %>% 
    gsub('W','', .)  %>% gsub(' S','', .) %>% strsplit(char)
  # determine if number should be negative
  sign<-grepl('W', x) | grepl('S', x)
  
  if(length(y[[1]]) != 3){y[[1]][3]<-0}  # add seconds if missing from number
  c<-abs(as.numeric(y[[1]][1]))+as.numeric(y[[1]][2])/60+as.numeric(y[[1]][3])/3600
  c<-ifelse(y[[1]][1]<0 | sign ,-c,c)
  return(c)
}
# check it works
conv_dec('34:35:30 S')
conv_dec('34"35"30 S', '"')

# convert those with :
tmp1<-mo.nondec %>% filter(grepl(':', Latitude)) %>%
  rename(Long_orig=Longitude, Lat_orig=Latitude) %>%
  rowwise() %>%
  mutate(Latitude=conv_dec(Lat_orig),
         Longitude=conv_dec(Long_orig))
tmp1 %>% dplyr::select(Longitude) %>% filter(is.na(Longitude))

# convert those wih a degree symbol
tmp2<-mo.nondec %>% filter(!grepl(':', Latitude), !grepl("'", Latitude))%>%
  rename(Long_orig=Longitude, Lat_orig=Latitude) %>%
  mutate(Latitude2=case_when(grepl('S', Lat_orig)~paste0('-',gsub('S','',Lat_orig)), #remove S from South American lats
                             grepl('N', Lat_orig)~gsub('/\\s','',gsub('N','', Lat_orig)), #remove N from NA lats
                             T~Lat_orig),
         Longitude2=case_when(grepl('W', Long_orig)~paste0('-',gsub(' W','',Long_orig)), #remove S from South American lats
                              grepl('E', Long_orig)~gsub('/\\s','',gsub(' E','', Long_orig)), #remove N from NA lats
                              T~Long_orig),
         Latitude=as.numeric(gsub('°','',Latitude2)),
         Longitude=as.numeric(gsub('°','',Longitude2))) %>%
  dplyr::select(-Latitude2, -Longitude2)
tmp2 %>% dplyr::select(Latitude) %>% filter(is.na(Latitude))

# convert those with a appostrophe
tmp3<-mo.nondec %>% filter(grepl("'", Latitude)) %>%
  rename(Long_orig=Longitude, Lat_orig=Latitude) %>%
  mutate(tlat=gsub('°',"'", Lat_orig),
         tlong=gsub('°',"'", Long_orig)) %>% 
  rowwise() %>%
  mutate(Latitude=conv_dec(tlat, char="'"),
         Longitude=conv_dec(tlong, char="'")) %>%
  dplyr::select(-tlat, -tlong)
tmp3 %>% dplyr::select(Latitude) %>% filter(is.na(Latitude))

# bring all the non decimal stuff back together
converted.dec<-bind_rows(tmp1, tmp2, tmp3) 
which(duplicated(converted.dec$rowid)) #make sure no duplicated rows
which(converted.dec %>% arrange(rowid) %>% pull(rowid) != mo.nondec$rowid) #make sure in same order
rm(tmp1, tmp2, tmp3)

# pull out records that have decimal degree ----
mo.dec<-occ %>%
  filter(!(rowid %in% mo.nondec$rowid), #those that are in deg/min/sec form
         !is.na(Longitude), !is.na(Latitude)) %>% #those that aren't georeffed
  rename(Long_orig=Longitude, Lat_orig=Latitude) %>% #preserve original longitude rowid
  #fix the latitudes
  mutate(Latitude2=case_when(grepl('S', Lat_orig)~paste0('-',gsub('S','',Lat_orig)), #remove S from South American lats
                             grepl('N', Lat_orig)~gsub('/\\s','',gsub('N','', Lat_orig)), #remove N from NA lats
                             T~Lat_orig),
         Latitude=as.numeric(gsub('[^0-9\\.\\-]', "", Latitude2))) %>% #convert to a numeric
  mutate(Longitude2=case_when(grepl('W', Long_orig)~paste0('-',gsub('W','',Long_orig)), #remove W from Western Hem long
                              grepl('E', Long_orig)~gsub('/\\s','',gsub('E','', Long_orig)), #remove E from Eastern Hem long
                              T~Long_orig),
         Longitude=as.numeric(gsub('[^0-9\\.\\-]', "", Longitude2))) %>% #convert to a numeric
  dplyr::select(-Longitude2, -Latitude2)
names(mo.dec)
View(mo.dec %>% filter(is.na(Longitude)))

# build a dataframe with all georeferenced occurrences
mo.in.NW<-bind_rows(mo.dec, converted.dec) %>%
  rename(Lat3=Latitude, Long3=Longitude) %>% # keep the second step lat/longs
  rowwise() %>%
  # if longitude > 0 & latitude < 0, flip the coordinates 
  mutate(Latitude=ifelse(Lat3 < 0 & Long3 > 0,  Long3, Lat3),
         Longitude=ifelse(Lat3 < 0 & Long3 > 0,  Lat3, Long3),
         converted=ifelse(Lat3 < 0 & Long3 > 0,  T, converted)) %>%
  #if longitude > 0, multiply by -1
  mutate(Longitude=ifelse(Longitude > 0, Longitude*-1, Longitude),
         converted=ifelse(Longitude > 0,  T, converted)) %>%
  dplyr::select(-Lat3, -Long3)

#mismatch between state and location ----
# bring in state polygons
library(maps)
states <- st_as_sf(map("state", plot = FALSE, fill = TRUE)) %>%
  st_transform(crs.albers)

mo.decs <- mo.in.NW %>%
  filter(!is.na(Longitude), !is.na(Latitude)) %>% #only those with a lat & long
  st_as_sf(coords=c("Longitude","Latitude"), crs=crs.geo) %>%  #convert to shapefile
  st_transform(crs.albers) %>% #transform to equal area projection
  st_join(states %>% st_buffer(10)) %>% #add a buffer to the states and then spatial join
  group_by(rowid) %>% #for each unique record
  # add a flag if the state polygon does not match the lot's indicated state
  # only works for the conterminous US, not Alaska or CN states
  mutate(state_mismatch=ifelse(tolower(State)==ID | State =='Alaska', FALSE, TRUE)) %>%
  arrange(state_mismatch)%>% ungroup() %>%
  distinct(rowid, .keep_all=T) 

# check not a ton of extra lots outside of states
mo.decs %>% as_tibble() %>% count(state_mismatch) 
## NAs from point that weren't joined to states (coast or great lakes)
## or had NA as the verbatim state

# huc10 is the dataframe that matches occurrences with hucs -- very large
PATH_WBD<- 'spatial_data/WBD_National_GDB/WBD_National_GDB.gdb'
huc10<-read_sf(dsn=PATH_WBD, layer='WBDHU10') %>%
  mutate(huc2=substr(huc10,1,2),
         huc4=substr(huc10,1,4),
         huc6=substr(huc10,1,6),
         huc8=substr(huc10,1,8))

## SMK needs this workaround to prevent ERROR: Loop 0 is not valid: Edge x has duplicate vertex with edge y
# see https://github.com/r-spatial/sf/issues/1762
sf_use_s2(FALSE)

# Join the occurrence data with the WBD  -----
# create a spatial dataframe and join it with the huc10 dataset
mo.decs.hu<-mo.decs %>% 
  st_as_sf(coords=c("Longitude","Latitude"), crs=crs.geo) %>%
  st_transform(st_crs(huc10)) %>% #transform to match huc8
  st_join(huc10) #spatial join with the huc10 polygon

# Join the occurrence data with the NHDPlusV2 -----
# using nhdplusTools to pull make the NHDPlusV2 easily accessible
st_layers(PATH_NHDv2)
nhdplus_path(PATH_NHDv2)
staged_data <- stage_national_data(output_path = '.')
flowline <- readRDS(staged_data$flowline) %>%
  filter(StreamOrde > 1)

all_occs<-NULL # starts the dataframe to save occurrences to
cuts<-seq(1,nrow(mo.decs.hu), 1000) # subset occs to avoid a computational quit from st_join
for(s in 1:length(cuts)){
  # pull out all the occs for one subset
  occ_s<-mo.decs.hu %>% slice(cuts[s]:ifelse(!is.na(cuts[s+1]), cuts[s+1]-1,nrow(mo.decs.hu))) %>%
    filter(!is.na(huc10)) %>% # do not join if not within NHD boundaries
    st_transform(st_crs(flowline)) # change projection to match the NHDPlusV2
  flowline_row<-st_nearest_feature(occ_s, flowline) # snaps the point to the closest flowline
  
  # pull out and keep the stream characteristic info in flowline
  stream.i.occ<-flowline[flowline_row,] %>% 
    as_tibble() %>%
    dplyr::select(COMID, GNIS_NAME, #identifiers of each stream reach
                  StreamOrde, SLOPE, QE_MA, VE_MA) %>%
    bind_cols(occ_s %>% as_tibble()) 

  # keeping the occurrence information used to build these plots
  all_occs<-bind_rows(all_occs, stream.i.occ)
  print(paste(cuts[s], ';', s, 'of', 
              length(cuts)))
}
nrow(all_occs)==nrow(mo.decs.hu)
nrow(all_occs)+nrow(mo.decs.hu %>% filter(is.na(huc10))) == nrow(mo.decs.hu)

all_occs1<-mo.decs.hu %>% left_join(all_occs %>% as_tibble() %>% dplyr::select(-geometry))
nrow(all_occs1)==nrow(mo.decs.hu)
all_occs1 %>% as_tibble() %>% count(rowid) %>% filter(n!=1)
sum(is.na(all_occs$COMID));sum(is.na(all_occs1$VE_MA)); sum(is.na(mo.decs.hu$huc10))

# writing out the georefferenced occurrence information in raw form ---
# pulling in huc names as dataframes instead of spatial files
huc2_names <- read_sf(dsn = PATH_WBD, layer='WBDHU2')%>%
  as_tibble() %>% dplyr::select(huc2, name) %>% rename(huc2_name=name)
huc4_names<-read_sf(dsn = PATH_WBD, layer='WBDHU4') %>%
  as_tibble() %>% dplyr::select(huc4, name) %>% rename(huc4_name=name)
huc6_names<-read_sf(dsn = PATH_WBD, layer='WBDHU6') %>%
  as_tibble() %>% dplyr::select(huc6, name) %>% rename(huc6_name=name)
huc8_names<-read_sf(dsn = PATH_WBD, layer='WBDHU8') %>%
  as_tibble() %>% dplyr::select(huc8, name) %>% rename(huc8_name=name)

# improving dataframe for output
temp_geo<-all_occs1 %>% 
  # removing columns not needed
  dplyr::select(-shape_Area, -shape_Length, -globalid, -humod, 
                -areasqkm, -areaacres, -referencegnis_ids, -states,
                -sourceoriginator, -sourcefeatureid, -sourcedatadesc, -tnmid, 
                -metasourceid, -loaddate, -ID) %>%
  dplyr::select(rowid, Institution, Cat_No, orig_ident, everything()) %>%
  rename(huc10_name=name) %>%
  left_join(huc2_names, by='huc2') %>% 
  left_join(huc4_names, by='huc4') %>%
  left_join(huc6_names, by='huc6') %>%
  left_join(huc8_names, by='huc8')
nrow(temp_geo)
st_write(temp_geo, 
         paste0(wd, 'georeffed_occ_raw_20220628.csv'),
         layer_options = "GEOMETRY=AS_XY",
         append=F)

# Clean Up the Data Columns ----
geo_occ<-read_csv(paste0(wd, 'georeffed_occ_raw_20220628.csv')) %>%
  rename(Latitude=Lat_orig, Longitude=Long_orig,
         standardized_longitude=X, standardized_latitude=Y)

bind_rows(mo.dec, converted.dec) %>%
  filter(is.na(Longitude)|is.na(Latitude)) %>% nrow()+ nrow(mo.decs)
nrow(occ)
nrow(geo_occ) # lost 5 that have 'no data' in localities

not_georeffed<-bind_rows(mo.dec, converted.dec) %>%
  filter(is.na(Longitude)|is.na(Latitude)|grepl('No', Latitude)|grepl('Unknown', Latitude)) %>%
  bind_rows(occ %>% filter(is.na(Longitude)|is.na(Latitude)) %>%
              mutate(Longitude=as.numeric(Longitude), Latitude=as.numeric(Latitude))) %>%
  distinct(rowid, .keep_all=T) %>% #remove any duplicates
  dplyr::select(-Longitude,-Latitude) %>%
  dplyr::select(rowid, Institution, Cat_No, orig_ident, everything()) %>%
  rename(Latitude=Lat_orig, Longitude=Long_orig)

occ_fin <- not_georeffed %>%
  bind_rows(geo_occ %>% 
              # converting to character to preserve original georeferencing
              mutate(Latitude=as.character(Latitude), Longitude=as.character(Longitude),
                     Day=as.character(Day))) %>%
  relocate(.after='Preparation', standardized_longitude, standardized_latitude,
           huc2_name, huc2, huc4_name, huc4, huc6_name, huc6,
           huc8_name, huc8, huc10_name, huc10, hutype,
           converted, state_mismatch) %>%
  mutate(state_mismatch=case_when(state_mismatch==1~T,
                                  state_mismatch==0~F), 
         converted=ifelse(is.na(converted), F, converted),
         outside_NHD=dplyr::case_when(is.na(standardized_latitude)~NA,
                                      is.na(huc2)~T,
                                      !is.na(huc2)~F)) %>%
  #select(state_mismatch, converted, outside_NHD) %>% summary()
  #grouping flags at end of dataframe
  relocate(.after='VE_MA', converted, state_mismatch, outside_NHD)

nrow(occ_fin)==nrow(occ)
which(duplicated(occ_fin$rowid))

write_csv(occ_fin,"occ_fin.csv")

##### TAXONOMY SECTION ######
### Running all the records through the GBIF Taxonomy Backbone ####

# identify unique species names in the data to minimize calls to name_backbone
# Had to remove NA's as it broke the for loop below
unique_orig_ident <- occ_fin %>% drop_na(orig_ident)
unique_orig_ident <- unique(unique_orig_ident$orig_ident)

#start the for loop - take each unique name, search gbif backbone, return and keep the results
mussel_taxa_key <- NULL #initialize an empty dataframe to be filled
for(u in unique_orig_ident){
  temp_df<-name_backbone(name = u) %>% #pull down data from the gbif backbone # add strict=TRUE to prevent names from being added to higher rank
    mutate(orig_ident=u) # keep track of the original name
  mussel_taxa_key<-bind_rows(temp_df, mussel_taxa_key) #build the dataframe
}

# join the key with the museum data using the column orig_ident
occ_fin<-left_join(occ_fin, mussel_taxa_key, 
                   by="orig_ident") %>%
  dplyr::select(-ends_with("Key")) #don't want to keep gbif keys, so select columns that do not end with "Key"

### UPDATE GBIF TAXONOMY TO FMCS CHECKLIST ####

#update genus taxonomy 
#Change Magnonaias to Megalonaias, etc.
occ_fin["genus"][occ_fin["genus"]== "Magnonaias"] <- "Megalonaias"
occ_fin["genus"][occ_fin["genus"]== "Ortmanniana"] <- "Actinonaias"

#update species taxonomy
occ_fin["species"][occ_fin["species"]== "Anodonta impura"] <- "Anodonta nuttalliana"
occ_fin["species"][occ_fin["species"]== "Anodontoides argenteus"] <- "Anodontoides denigratus"
occ_fin["species"][occ_fin["species"]== "Elliptio nasutidus"] <- "Elliptio producta"
occ_fin["species"][occ_fin["species"]== "Obovaria jacksoniana"] <- "Obovaria arkansasensis"
occ_fin["species"][occ_fin["species"]== "Ortmanniana ligamentina"] <- "Actinonaias ligamentina"
occ_fin["species"][occ_fin["species"]== "Ortmanniana pectorosa"] <- "Actinonaias pectorosa"
occ_fin["species"][occ_fin["species"]== "Pleurobema altum"] <- "Pleurobema fibuloides"
occ_fin["species"][occ_fin["species"]== "Toxolasma cylindrellum"] <- "Toxolasma cylindrellus"
occ_fin["species"][occ_fin["species"]== "Toxolasma pullum"] <- "Toxolasma pullus"

#V. pleasii is considered a synonym of V. ellipsiformis in GBIF backbone (not sure why). Using the orig_ident col I pulled the pleasi samples out of ellipsifromis.
occ_fin["species"][occ_fin["orig_ident"]== c("Venustaconcha  pleasii", "Venustaconcha pleasi", "Venustaconcha pleasi (Marsh, 1891)", "Venustaconcha pleasii", "Venustaconcha pleasii (Marsh 1891)", "Venustaconcha pleasii (Marsh, 1891)")] <- "Venustaconcha pleasii" 

# add tribe and subfamily assignments
#read in fmcs checklist doc 
fmcs_checklist <- read_csv("3b_fmcs_checklist.csv", trim_ws = TRUE, col_types = cols(.default = "c"))

#create key to assign genera to a taxon not included in GBIF backbone (e.g. tribe subfamily)
higher_taxon_key <- fmcs_checklist[c("genus","tribe", "subfamily")]

#remove duplicates
higher_taxon_key <- higher_taxon_key[!duplicated(higher_taxon_key$genus),]

#join to add tribe/subfamily col
occ_fin <- left_join(occ_fin, higher_taxon_key, by ="genus")

#FILTERING OUT RECORDS OF TAXA NOT IN US CHECKLIST
#create list of US CHECKLIST genera and species
checklist_genera <- fmcs_checklist$genus
checklist_species <- fmcs_checklist$fmcs_species

#grab records that are in Unionidae or Margartiferidae 
occ_fin <- occ_fin %>% filter(family == "Margaritiferidae" | family == "Unionidae")

# grabs records that are identified as either fmcs genera AND are not ID'd to species OR fmcs checklist species  
occ_fin <- occ_fin %>% filter(genus %in% checklist_genera & is.na(species) | species %in% checklist_species)


### OUTPUT ALL RECORDS
write.csv(occ_fin, file = paste0(wd,"3c_all_records.csv"), row.names = F)

###  CREATE DATA SET OF ONLY SPECIES-LEVEL OCCURENCES
#Filter to 303 fmcs checklist species 
occ_species <- occ_fin %>% filter(species %in% checklist_species)

### OUTPUT ALL RECORDS WITH SPECIES-LEVEL ID
write.csv(occ_species, file = paste0(wd, "3d_species_records.csv"), row.names = F)

####
# The following code was run by Sean Keogh - keogh026@umn.edu
####

# read in both csvs
all = readr::read_csv(paste0(wd,"3c_all_records.csv")) 
occur = readr::read_csv(paste0(wd,"3d_species_records.csv")) 


## flag duplicate collections
# first flag duplicates for records that do not have species ID's
no_species<-all %>% 
  filter(is.na(species))
# filter based on orig_ident, institution, date, locality, lat, long,
library(campfin)
na_vals<-no_species[c("rowid","Institution","orig_ident","Country","State","Locality","standardized_latitude","standardized_longitude","Month","Day","Year")]
# one duplicate will remain 'FALSE' others flagged as 'TRUE'
na_flag<-flag_dupes(na_vals, -rowid, .both = FALSE)
na_flag<-na_flag[c("rowid","dupe_flag")]
no_species<-inner_join(no_species,na_flag, by='rowid')
# now flag duplicates that do have species ID's (5b_species_records.csv)
oc_dupes<-occur[c("rowid","Institution","species","Country","State","Locality","standardized_latitude","standardized_longitude","Month","Day","Year")]
# one duplicate will remain 'FALSE' others flagged as 'TRUE'
oc_flag<-flag_dupes(oc_dupes, -rowid, .both = FALSE)
oc_flag<-oc_flag[c("rowid","dupe_flag")]
occur<-inner_join(occur,oc_flag, by='rowid')

#Row bind occur and no_species to compile 3c_all_records.csv
all<-bind_rows(occur,no_species)


### FLAG & CHANGE SPECIES NAME FOR RECENTLY DESCRIBED, ALLOPATRIC SPECIES
# species updated in 'species_update' column
library(parallel)
library(readxl)

sppup_df <- read_xlsx('species_updated_citations.xlsx',
                      sheet='species_update',na=c('NA',''))
dir.create('sppup_csvs')

# For loop to flag (True/False in InRange col) old taxonomy & provide path to update to new taxonomy
# see 'species_updated_citations.xlsx' for details
mclapply(1:nrow(sppup_df),mc.cores=2,function(ii){
  # ii=1
  cat(ii, '\n')
  old_taxon <- sppup_df[[ii,'Change_this_taxon']]
  new_taxon <- sppup_df[[ii,'To_this_taxon']]
  range_shp = sf::st_read(paste0(wd,"taxonomy_ranges/",new_taxon,".shp")) 
  
  spdf = range_shp %>% as("Spatial")
  spdf$species<-old_taxon
  clean<-occur %>%
    filter(species==old_taxon)
  clean$standardized_longitude<-as.numeric(clean$standardized_longitude)
  clean$standardized_latitude<-as.numeric(clean$standardized_latitude)
  clean<- clean %>%
    drop_na(standardized_longitude) %>%
    drop_na(standardized_latitude)
  clean$InRange<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  #### write your new file
  nrow(clean %>% filter(InRange=='TRUE'))
  write_csv(clean,file = paste0('sppup_csvs/',old_taxon,'to',new_taxon,'.csv'))
})


# Aggregate updated taxonomy
mussel_newtaxa<-NULL
for (i in 1:nrow(sppup_df)) {
  # ii=1
  cat(i, '\n')
  old_taxon <- sppup_df[[i,'Change_this_taxon']]
  new_taxon <- sppup_df[[i,'To_this_taxon']]
  sppup_sp<-read.csv(paste0("sppup_csvs/",old_taxon,"to",new_taxon,".csv"),header=TRUE)
  sppup_sp<-sppup_sp %>% mutate(species_update = ifelse(InRange == TRUE,new_taxon, old_taxon))
  sppup_sp<-sppup_sp %>% filter(InRange==TRUE) %>%  dplyr::select(rowid, species_update)
  mussel_newtaxa<-bind_rows(sppup_sp, mussel_newtaxa) #build the dataframe
}
occur$species_update<-occur$species
occur<-occur %>%
  left_join(mussel_newtaxa, by = "rowid") %>%
  mutate(species_update = coalesce(species_update.y, species_update.x)) %>%
  dplyr::select(-species_update.y, -species_update.x)

# manual check
num_flagged<-NULL
for (i in 1:nrow(sppup_df)) {
  # ii=1
  cat(i, '\n')
  old_taxon <- sppup_df[[i,'Change_this_taxon']]
  new_taxon <- sppup_df[[i,'To_this_taxon']]
temp_df<-nrow(occur %>% filter(species==old_taxon & species_update==new_taxon))
num_flagged<-rbind(temp_df, num_flagged) #build the dataframe
}

# Another check
del<-occur %>% 
  mutate(tf = species == species_update) %>% 
  filter(tf =='FALSE')
nrow(del)
# ^ This filter updated 1135 records 
# check number of species = 302
length(unique(occur$species))
length(unique(occur$species_update))

### FLAG RECORDS THAT OCCUR OUTSIDE SPECIES KNOWN RANGES

# Read in FMCS names
mussel.names <- read_csv(paste0(wd,"3b_fmcs_checklist.csv"), trim_ws = TRUE, col_types = cols(.default = "c"))

# Remove Disconaias fimbriata as it as no occurrences in our dataset and therefore breaks the for loop
mussel.names<-mussel.names %>% 
  subset(fmcs_species !='Disconaias fimbriata')

#species list
mussel_sp<-mussel.names$fmcs_species
# run for loop for all species aside from Disconaias to flag records
species1='Actinonaias ligamentina'

# Create new directory for csv files
dir.create('spp_csvs')

# Flagging step - occurrences outside known ranges of species
for(species1 in mussel_sp){
  range_shp = sf::st_read(dsn = paste0("species_ranges/shapefiles/",species1,".shp")) 
  spdf = range_shp %>% as("Spatial")
  spdf$species<-paste(species1)
  clean<-occur %>%
    filter(species_update==species1)
  clean$species<-clean$species_update # for some reason cc_iucn needs column names to be 'species'
  ## make all lat-long records numeric and drop NA's
  clean$standardized_longitude<-as.numeric(clean$standardized_longitude)
  clean$standardized_latitude<-as.numeric(clean$standardized_latitude)
  clean<- clean %>%
    drop_na(standardized_longitude) %>%
    drop_na(standardized_latitude)
  #now flag records that occur outside species range
  clean$InRange<-clean %>%
    cc_iucn(
      range = spdf,
      lon = "standardized_longitude",
      lat = "standardized_latitude",
      species = "species",
      buffer = 0, # buffer in decimal degrees
      value = "flagged")
  ### only save 'TRUE' flags
  truth<- clean %>%
    filter(InRange=="TRUE")
  #### write your new file, only keep 'TRUE' records
  write_csv(truth,file = paste0('spp_csvs/',species1,".csv"))
}

## combine all records to master spreadsheet
temp <- list.files(path=paste0(wd,"spp_csvs/"),pattern="*.csv")
df <- data.frame(rowid=numeric(0), InRange=numeric(0))
for (i in 1:length(temp)) {
  tmp <- read.csv(paste0('spp_csvs/',temp[i]))
  df <- rbind(df, tmp[, c("rowid", "InRange")])
}

#### Add TRUE records to InRange field of master spreadsheet
library(naniar)
occur<- full_join(occur,df, by='rowid')

# Change all NA values to FALSE
occur<- occur %>%
  replace_na(list(InRange = FALSE)) 
# now change all false values without georef info to NA
InRange<-ifelse(is.na(occur$standardized_latitude) == TRUE, occur$InRange == NA, occur$InRange)
occur<-occur %>% dplyr::select(-InRange)
occur<-cbind.data.frame(occur,InRange)
### add InRange and species_update columns to all dataset

all<-left_join(all,occur[,c("rowid","species_update","InRange")],by="rowid")
all<-arrange(all,rowid)
occur<-arrange(occur,rowid)

# check InRange flags
dim(occur %>% filter(InRange=='FALSE'))
# 9752 records flagged

#write csvs
write.csv(all, file=paste0(wd,"3c_all_records.csv"), row.names = F)
write.csv(occur,file=paste0(wd,"3d_species_records.csv"), row.names = F)

