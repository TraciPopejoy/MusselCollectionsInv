# -----------------------------------------------------
# BEGIN: Make species shapefiles
# -----------------------------------------------------
  # load necessary libraries
  library(sf); library(tidyverse); library(scales) # load necessary libraries.
  library(rgbif) # for taxonomic backbone
  library(cowplot) #for plotting maps later
  # for building shapefiles/creating 'species_update' & 'InRange' filters
  library(maps); library(rgdal); library(gridExtra); library(maptools); library(raster); 
  library(rgeos); library(grid); library(mapplots); library(mapproj); library(ggspatial); 
  library(ggrepel); library(rnaturalearth); library(CoordinateCleaner); library(readxl);
  library(leaflet)
  # to download spatial dataframes (NHDPlusv2 and Watershed Boundary Dataset, respectively)
  library(nhdplusTools) #install.packages("devtools"); devtools::install_github("USGS-R/nhdplusTools")
  
  wd <-"~/Desktop/IZ_NMNH/MusselMuseum/" # PUT YOUR WORKING DIRECTORY HERE
  
  # see https://github.com/r-spatial/sf/issues/1762
  sf_use_s2(FALSE)
  
  #WBD shapefile https://drive.google.com/drive/folders/146eSx7yVRu55uqsbu7uBb2VunNqqZX6q
  # Read in all Watershed Boundary Datasets - here it is all HUC10's
  watersheds <- readOGR(paste(wd, "old/ranges_SK/shapefiles/watersheds/WBD_HU10_watersheds.shp", sep=""))
  watersheds <- spTransform(watersheds, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  watersheds <- gBuffer(watersheds, byid = T, width = 0)
  
 
  
  ########## DRAW GREAT LAKES
  ## This code takes a long time to run.
  ## Therefore, for sp. in Great Lakes I made one GL shapefile then made a union with other parts of their range
  ## Rather than run the GL code for each GL species
  Great_lakes<- c("0401","0402","0403","0404","0405","0406","0407","0408","0409","0410","0411","0412","0413","0414","0415")
  HUCs<- (Great_lakes)
  river_name <- "Great_lakes_04"
  huc10s <- as.character(watersheds$huc10)
  target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
  huc10s <- as.character(target_watershed$huc10)
  target_range <- aggregate(target_watershed, dissolve=T)
  target_range <- as(target_range, "SpatialPolygonsDataFrame")
  #write shapefile
  writeOGR(target_range, dsn = '.', layer = paste("old/ranges_SK/shapefiles/Great_lakes_04"), driver = "ESRI Shapefile", overwrite_layer=TRUE)
  
  #Read in all Great Lakes HUCs (04)
  lakes <- readOGR(paste(wd, "old/ranges_SK/shapefiles/Great_lakes_04.shp", sep=""))
  lakes <- spTransform(lakes, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
  lakes <- gBuffer(lakes, byid = T, width = 0)
  
  
WKST<-read_xlsx(paste0(wd,'InRange_citations:hucs.xlsx'),
                       sheet='polygon_checklist',na=c('NA',''))
PATH_SpeciesRanges<-"species_ranges/"

mclapply(1:nrow(WKST),mc.cores=2,function(ii){
  # ii=7
  cat(ii, '\n')
  sp <- WKST[[ii,'FMCS_NAME']]
  HUCs<- WKST[[ii,'HUCs']]
  GL<-WKST[[ii,'Greatlakes']]

  tryCatch({

  if(!file.exists(paste0(wd,PATH_SpeciesRanges,'shapefiles/', sp,'.shp'))) {
   HUCs<-gsub('HUCs<-', '',HUCs)
   HUCs<-gsub('c','',HUCs)
   HUCs<-gsub('[^[:alnum:] ]', ',',HUCs)
   HUCs<-gsub(',,', '',HUCs)
   HUCs<-gsub(',', ' ',HUCs)
   HUCs<-scan(text=HUCs, what=character(), qui=T)
   
   
   huc10s <- as.character(watersheds$huc10)
   target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
   
   huc10s <- as.character(target_watershed$huc10)
   target_range <- aggregate(target_watershed, dissolve=T)
   target_range <- as(target_range, "SpatialPolygonsDataFrame")
   target_range <- spTransform(target_range, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
   target_range <- gBuffer(target_range, byid = T, width = 0)
   
   
   #if statement
   if(!is.na(GL)) {
   target_range <- spTransform(target_range, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
   target_range<-raster::union(lakes,target_range)
   }
   
   plot3 <- leaflet(target_range) %>%
     addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
                 color="black")
   plot3 %>% addTiles()
   
   #write shapefile
   writeOGR(target_range, dsn = '.', layer = paste0(wd,PATH_SpeciesRanges,'shapefiles/',sp), driver = "ESRI Shapefile", overwrite_layer=TRUE)
    
  }
  },error=function(e){
    save(out,file=str_c(PATH_SpeciesRanges,sp,'_FAILED.data'))
  })
})

# -----------------------------------------------------
# END: Make species shapefiles
# -----------------------------------------------------


# NOTE: In some instances taxonomy shapefiles made below are slightly smaller than their
# species range shapefiles made above. This was done as we wanted to be more conservative
# in respect to making taxonomic changes

# -----------------------------------------------------
# BEGIN: Make taxonomy shapefiles
# -----------------------------------------------------


sppup_df <- read_xlsx(paste0(wd,'species_updated_citations.xlsx'),
                      sheet='species_update',na=c('NA',''))

mclapply(1:nrow(WKST),mc.cores=2,function(ii){
  # ii=3
  cat(ii, '\n')
  sp <- sppup_df[[ii,'To_this_taxon']]
  HUCs<- sppup_df[[ii,'HUCs']]
  
  tryCatch({
    
    if(!file.exists(paste0(wd,'taxonomy_ranges/', sp,'.shp'))) {
      HUCs<-gsub('HUCs<-', '',HUCs)
      HUCs<-gsub('c','',HUCs)
      HUCs<-gsub('[^[:alnum:] ]', ',',HUCs)
      HUCs<-gsub(',,', '',HUCs)
      HUCs<-gsub(',', ' ',HUCs)
      HUCs<-scan(text=HUCs, what=character(), qui=T)
      
      huc10s <- as.character(watersheds$huc10)
      target_watershed <- watersheds[which(as.logical(rowSums(sapply(HUCs, startsWith, x=huc10s)))),]
      huc10s <- as.character(target_watershed$huc10)
      target_range <- aggregate(target_watershed, dissolve=T)
      target_range <- as(target_range, "SpatialPolygonsDataFrame")
      target_range <- spTransform(target_range, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
      target_range <- gBuffer(target_range, byid = T, width = 0)
     
      # plot3 <- leaflet(target_range) %>%
      #   addPolygons(stroke = TRUE,opacity = 1,fillOpacity = 0.5, smoothFactor = 0.5,
      #               color="black")
      # plot3 %>% addTiles()
      
      #write shapefile
      writeOGR(target_range, dsn = '.', layer = paste0(wd,'taxonomy_ranges/',sp), driver = "ESRI Shapefile", overwrite_layer=TRUE)
      
    }
  },error=function(e){
    save(out,file=str_c(wd,sp,'_tax_FAILED.data'))
  })
})

# -----------------------------------------------------
# END: Make taxonomy shapefiles
# -----------------------------------------------------
