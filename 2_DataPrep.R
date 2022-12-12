##############################################################################
#APPENDIX 1.2: R script to prepare refined data for analysis (lines 15707-23556)
##############################################################################

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
outdirNHD<-'nhdPlusV2/'
download_nhdplusv2(
  outdir,
  url = paste0("https://edap-ow-data-commons.s3.amazonaws.com/NHDPlusV21/",
    "Data/NationalData/NHDPlusV21_NationalData_Seamless", "_Geodatabase_Lower48_07.7z"),
  progress = TRUE
)
outdirWBD<-'spatial data/'
download_wbd(
  outdirWBD,
  url = paste0("https://prd-tnm.s3.amazonaws.com/StagedProducts/",
    "Hydrography/WBD/National/GDB/WBD_National_GDB.zip"),
  progress = TRUE
)

# path locations
PATH_OpenRefineOutput <- '3_refined.csv'
PATH_NHDv2 <- paste0(outdirNHD,'NHDPlusV21_National_Seamless_Flattened_Lower48.gdb')
PATH_WBD <- paste0(outdirWBD,'WBDNational.gdb/')
wd <-"~/Desktop/IZ_NMNH/MusselMuseum/" # PUT YOUR WORKING DIRECTORY HERE

#load the data
occ <- read_csv(PATH_OpenRefineOutput, trim_ws = TRUE, col_types = 'cccccccccccdccc') %>%
  rename(Latitude=Lattitude) %>% # change the names of some columns
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
huc10<-read_sf(dsn=PATH_WBD, layer='WBDHU10') %>%
  mutate(huc2=substr(huc10,1,2),
         huc4=substr(huc10,1,4),
         huc6=substr(huc10,1,6),
         huc8=substr(huc10,1,8))

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

all_occs1<-mo.decs.hu %>% left_join(all_occs) 
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
  dplyr::select(rowid, Institution, Cat_No, 
         verbatim_name, refined_name, 
         everything()) %>%
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
  dplyr::select(rowid, Institution, Cat_No,  
         verbatim_name, refined_name, 
         everything()) %>%
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

##### TAXONOMY SECTION ######
### Running all the records through the GBIF Taxonomy Backbone ####

#identify unique species names in the data to minimize calls to name_backbone
unique_refined_name <- unique(occ_fin$refined_name)

#start the for loop - take each unique name, search gbif backbone, return and keep the results
mussel_taxa_key <- NULL #initialize an empty dataframe to be filled
for(u in unique_refined_name){
  temp_df<-name_backbone(name = u) %>% #pull down data from the gbif backbone # add strict=TRUE to prevent names from being added to higher rank
    mutate(refined_name=u) # keep track of the original name
  mussel_taxa_key<-bind_rows(temp_df, mussel_taxa_key) #build the dataframe
}

# join the key with the museum data using the column verbatim_name
occ_fin<-left_join(occ_fin, mussel_taxa_key, 
                   by="refined_name") %>%
  dplyr::select(-ends_with("Key")) #don't want to keep gbif keys, so select coulumns that do not end with "Key"

### UPDATE GBIF TAXONOMY TO US CHECKLIST ####

#update genus taxonomy 
#Change Beringiana to Sinanodonta,Magnonaias to Megalonaias, etc.
occ_fin["genus"][occ_fin["genus"]== "Beringiana"] <- "Sinanodonta"
occ_fin["genus"][occ_fin["genus"]== "Magnonaias"] <- "Megalonaias"
occ_fin["genus"][occ_fin["genus"]== "Ortmanniana"] <- "Actinonaias"

#update species taxonomy
occ_fin["species"][occ_fin["species"]== "Anodonta impura"] <- "Anodonta nuttalliana"
occ_fin["species"][occ_fin["species"]== "Anodontoides argenteus"] <- "Anodontoides denigratus"
occ_fin["species"][occ_fin["species"]== "Beringiana beringiana"] <- "Sinanodonta beringiana"
occ_fin["species"][occ_fin["species"]== "Elliptio nasutidus"] <- "Elliptio producta"
occ_fin["species"][occ_fin["species"]== "Obovaria jacksoniana"] <- "Obovaria arkansasensis"
occ_fin["species"][occ_fin["species"]== "Ortmanniana ligamentina"] <- "Actinonaias ligamentina"
occ_fin["species"][occ_fin["species"]== "Ortmanniana pectorosa"] <- "Actinonaias pectorosa"
occ_fin["species"][occ_fin["species"]== "Pleurobema altum"] <- "Pleurobema fibuloides"
occ_fin["species"][occ_fin["species"]== "Toxolasma cylindrellum"] <- "Toxolasma cylindrellus"
occ_fin["species"][occ_fin["species"]== "Toxolasma pullum"] <- "Toxolasma pullus"

#V. pleasii is considered a synonym of V. ellipsiformis in GBIF backbone (not sure why). Using the verbatim names col I pulled the pleasi samples out of ellipsifromis.
occ_fin["species"][occ_fin["verbatim_name"]== c("Venustaconcha  pleasii", "Venustaconcha pleasi", "Venustaconcha pleasi (Marsh, 1891)", "Venustaconcha pleasii", "Venustaconcha pleasii (Marsh 1891)", "Venustaconcha pleasii (Marsh, 1891)")] <- "Venustaconcha pleasii" 

# add tribe and subfamily assignments
#read in US CHECKLIST 
US_checklist <- read_csv("4b_US_checklist.csv", trim_ws = TRUE, col_types = cols(.default = "c"))

#create key to assign genera to a taxon not included in GBIF backbone (e.g. tribe subfamily)
higher_taxon_key <- US_checklist[c("genus","tribe", "subfamily")]
#remove duplicates
higher_taxon_key <- higher_taxon_key[!duplicated(higher_taxon_key$genus),]

#join to add tribe/subfamily col
occ_fin <- left_join(occ_fin, higher_taxon_key, by ="genus")

#FILTERING OUT RECORDS OF TAXA NOT IN US CHECKLIST

#create list of US CHECKLIST genera and species
checklist_genera <- US_checklist$genus
checklist_species <- US_checklist$species

#grab records that are in Unionidae or Margartiferidae 
occ_fin <- occ_fin %>% filter(family == "Margaritiferidae" | family == "Unionidae")

# grabs records that are identified to family-level [is.na(genus)] OR are US CHECKLIST genera AND are not ID to species [is.na(species)] OR are US CHECKLIST species  
occ_fin <- occ_fin %>% filter(is.na(genus) | genus %in% checklist_genera & is.na(species) | species %in% checklist_species)

### OUTPUT ALL RECORDS
write.csv(occ_fin, file = paste0(wd,"5a_all_records.csv"), row.names = F)

###  CREATE DATA SET OF ONLY SPECIES-LEVEL OCCURENCES
#Filter to 303 US checklist species 
occ_species <- occ_fin %>% filter(species %in% checklist_species)

#output all species-level records
write.csv(occ_species, file = paste0(wd, "5b_species_records.csv"), row.names = F)
