##############################################################################
# APPENDIX 1.5: R script to report results and figures (lines 23926-24359)
##############################################################################

library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(ggExtra)
library(viridis)


# read in all records 
records <- read_csv("5a_all_records.csv", trim_ws = TRUE, col_types = cols(.default = "c"))

#add col to show georef'd or not
records <- records %>% mutate(georeferenced = ifelse(is.na(standardized_latitude) | is.na(standardized_longitude), FALSE, TRUE))

#Get rid of Conservation col it got messed up in data prep script, needs to be done after Sean creates species_update col. This is a temp solution
records <- records %>%
  select(-conservation_status_verbatim)

#read in US CHECKLIST 
US_checklist <- read_csv("4b_US_checklist.csv", trim_ws = TRUE, col_types = cols(.default = "c"))

#create key to assign species species their federal conservation status
conservation_key <- US_checklist[c("species_update","con_status_simp")]

#add conservation status
records <- left_join(records, conservation_key, by = "species_update")

#Filter down to only unique occurnece
occ <- records %>% filter(dupe_flag == FALSE)


### INSTITUTIONAL RESULTS ###
#create summary statistics for each collection (Supplemental Information 2)
t1main <- occ %>% as_tibble() %>%
  group_by(Institution) %>%
  summarize(N_unique_occs=n(),
            N_genera=n_distinct(genus, na.rm = TRUE),
            N_species=n_distinct(species_update, na.rm = TRUE),
            N_huc8=n_distinct(huc8, na.rm = TRUE),
            Percent_species_ID = round(sum(rank %in% c('SPECIES', 'SUBSPECIES', 'VARIETY'))/N_unique_occs*100,2),
            Percent_with_year=100-round(sum(is.na(Year))/N_unique_occs*100,2),
            Percent_dry=round(sum(Preparation=='dry' & !is.na(Preparation))/N_unique_occs*100,2),
            Percent_geolocated=round((sum(georeferenced==T))/N_unique_occs*100,2),
            N_state_mismatch=sum(state_mismatch==T & !is.na(state_mismatch)),
            N_outside_NHD=sum(outside_NHD==T & !is.na(outside_NHD)),
            N_outside_range=sum(InRange ==F & !is.na(InRange))) %>%
  arrange(desc(N_unique_occs))

# summary statistics for each collection (Supplemental Information 2)
t1main %>% write.csv('institutuion_stats.csv')

# Median stats
median(t1main$N_unique_occs)
median(t1main$N_species)
median(t1main$N_huc8)

# summary statistics for all the occurrences
t1main_total <- occ %>% as_tibble() %>%
  summarize(N_unique_occs=n(),
            N_genera=n_distinct(genus, na.rm = TRUE),
            N_species=n_distinct(species_update, na.rm = TRUE),
            N_huc8=n_distinct(huc8, na.rm = TRUE),
            Percent_species_ID = round(sum(rank %in% c('SPECIES', 'SUBSPECIES', 'VARIETY'))/N_unique_occs*100,2),
            Percent_with_year=100-round(sum(is.na(Year))/N_unique_occs*100,2),
            Percent_dry=round(sum(Preparation=='dry' & !is.na(Preparation))/N_unique_occs*100,2),
            Percent_geolocated=round((sum(georeferenced==T))/ N_unique_occs*100,2),
            N_state_mismatch=sum(state_mismatch==T & !is.na(state_mismatch)),
            N_outside_NHD=sum(outside_NHD==T & !is.na(outside_NHD)),
            N_outside_range=sum(InRange ==F & !is.na(InRange)))

#### FIGURE 1 ####
#list of ten collections with the most lots
top_10_count <- head(t1main,10)

#bubble plot of species X huc with dots relative to collection size
f1a <- ggplot(t1main, aes(x=N_species, y=N_huc8, label = Institution)) +
  geom_point(aes(size = N_unique_occs, color = N_unique_occs),alpha = .65, show.legend = FALSE) +
  scale_size(range = c(1, 10),  name="n occs", breaks = c(1,100,1000,10000,20000,30000,40000,50000,60000,70000), labels = c("1","100","1000","10000","20000","30000","40000","50000","60000","70000")) +
  scale_color_viridis_c(name="n occs", guide="legend",breaks = c(1,100,1000,10000,20000,30000,40000,50000,60000,70000), labels = c("1","100","1000","10000","20000","30000","40000","50000","60000","70000"))+
  geom_text_repel(size = 2, data = top_10_count, min.segment.length = 0, color = "black", show.legend = FALSE) +
  xlab("n species") +
  scale_x_continuous(breaks = c(50,100,150,200,250,300))+
  ylab("n HUC8")+
  theme_bw()


#bubble plot of Percent_with_year X Percent_geolocated with dots relative to collection size
f1b <- ggplot(t1main, aes(x=Percent_with_year, y=Percent_geolocated, label = Institution)) +
  geom_point(aes(size = N_unique_occs, color = N_unique_occs),alpha = .65) +
  scale_size(range = c(1, 10),  name="n occurrences", breaks = c(1,100,1000,10000,20000,30000,40000,50000,60000,70000), labels = c("1","100","1000","10000","20000","30000","40000","50000","60000","70000")) +
  scale_color_viridis_c(name="n occurrences", guide="legend",breaks = c(1,100,1000,10000,20000,30000,40000,50000,60000,70000), labels = c("1","100","1000","10000","20000","30000","40000","50000","60000","70000"))+
  geom_text_repel(size = 2, data = top_10_count, min.segment.length = 0, color = "black", show.legend = FALSE) +
  xlab("% occurrences with year data") +
  ylab("% occurrences georefferenced")+
  theme_bw()

figure1 <- ggarrange(f1a, f1b, ncol = 2, nrow = 1, labels="AUTO")
#export as 8 X 3.5
figure1

### TEMPORAL RESULTS ###

#PER YEAR RESULTS
#occurrences per year summary stat
occ_per_year <- occ %>% 
  filter(!is.na(Year), Year >=1900 & Year <=2020) %>%
  group_by(Year) %>%
  count()

max(occ_per_year$n)
mean(occ_per_year$n)
min(occ_per_year$n)

#species per year summary stats
sp_per_year <- occ %>% 
  filter(!is.na(Year), !is.na(species_update), Year >=1900 & Year <=2020) %>%
  group_by(Year) %>%
  summarize(N_species=n_distinct(species_update))

max(sp_per_year$N_species)
mean(sp_per_year$N_species)
min(sp_per_year$N_species)
  
# huc per year summary stats
hucs_per_year <- occ %>% 
  filter(!is.na(Year), !is.na(huc8), Year >=1900 & Year <=2020) %>%
  group_by(Year) %>%
  summarize(N_hucs=n_distinct(huc8))

max(hucs_per_year$N_hucs)
mean(hucs_per_year$N_hucs)
min(hucs_per_year$N_hucs)

### FIGURE 2 ###
### A - occurrences per year
 #grab records since 1900, add conservation status, group by year and conservation status
occ_per_year <- occ %>% 
  filter(!is.na(Year), Year >=1900 & Year <=2020) %>%
  group_by(Year, con_status_simp) %>%
  count() 

occ_per_year$con_status_simp <- factor(occ_per_year$con_status_simp, levels = c("Listed", "Not Listed")) 

# plot it
f2a <- ggplot(occ_per_year) +
  aes(x=Year, y=n, fill=con_status_simp)+
  geom_bar(stat='identity', width=.75)+
  xlab("year") +
  ylab("n occurrences")+
  scale_x_discrete(breaks=seq(1900, 2020, 5))+
  scale_fill_viridis_d(begin = 0, end = 1, direction = -1, na.value = "grey50")+
  theme_bw()+ 
  theme(legend.position='bottom', legend.title=element_blank(), axis.text.x = element_text(size = 6, angle=45,hjust=1))


### B - species per year
sp_per_year <- occ %>% 
  filter(!is.na(Year), !is.na(species_update), Year >=1900 & Year <=2020) %>%
  mutate(tribe_simp=case_when(tribe=='Gonideini'~'Other',
                         tribe=='Margaritiferinae'~'Other',
                         tribe=='Amblemini'~'Amblemini',
                         tribe=='Lampsilini'~'Lampsilini',
                         tribe=='Pleurobemini'~'Pleurobemini',
                         tribe=='Quadrulini'~'Quadrulini',
                         tribe=='Anodontini'~'Anodontini',
                         tribe=='Popenaiadini'~'Other')) %>%
  group_by(Year, tribe_simp) %>%
  summarize(N_species=n_distinct(species_update))

sp_per_year$tribe_simp <- factor(sp_per_year$tribe_simp, levels = c("Anodontini","Amblemini","Lampsilini","Pleurobemini","Quadrulini","Other")) 

#plot it
f2b <- ggplot(sp_per_year) +
  aes(x=Year, y=N_species, fill=tribe_simp)+
  geom_bar(stat='identity', width=.75)+
  xlab("year") +
  ylab("n species")+
  scale_x_discrete(breaks=seq(1900, 2020, 5))+
  theme_bw()+ 
  scale_fill_viridis_d( begin = 0, end = 1, direction = -1)+
  theme(legend.position='bottom', legend.title=element_blank(), axis.text.x = element_text(size = 6, angle=45,hjust=1))+
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


### C - hucs per year
hucs_per_year <- occ %>% 
  filter(!is.na(Year), !is.na(huc8), Year >=1900 & Year <=2020) %>%
  mutate(Region=case_when(huc2_name == 'Alaska Region' ~ 'Pacific',
                          huc2_name == 'Arkansas-White-Red Region' ~ 'Mississippian',
                          huc2_name == 'California Region' ~ 'Pacific',
                          huc2_name == 'Great Basin Region' ~ 'Pacific',
                          huc2_name == 'Great Lakes Region' ~ 'North Atlantic',
                          huc2_name == 'Lower Colorado Region' ~ 'Pacific',
                          huc2_name == 'Lower Mississippi Region' ~ 'Mississippian',
                          huc2_name == 'Mid Atlantic Region' ~ 'North Atlantic',
                          huc2_name == 'Missouri Region' ~ 'Mississippian',
                          huc2_name == 'New England Region' ~ 'North Atlantic',
                          huc2_name == 'Ohio Region' ~ 'Mississippian',
                          huc2_name == 'Pacific Northwest Region' ~ 'Pacific',
                          huc2_name == 'Rio Grande Region' ~ 'Western Gulf',
                          huc2_name == 'Souris-Red-Rainy Region' ~ 'North Atlantic',
                          huc2_name == 'South Atlantic-Gulf Region' ~ 'South Atlantic and Eastern Gulf',
                          huc2_name == 'Tennessee Region' ~ 'Mississippian',
                          huc2_name == 'Texas-Gulf Region' ~ 'Western Gulf',
                          huc2_name == 'Upper Mississippi Region' ~ 'Mississippian')) %>%
  group_by(Year, Region) %>%
  summarize(N_hucs=n_distinct(huc8))

hucs_per_year$Region <- factor(hucs_per_year$Region, levels = c("Pacific","Western Gulf","Mississippian","South Atlantic and Eastern Gulf","North Atlantic")) 


#plot it
f2c <- ggplot(hucs_per_year) +
  aes(x=Year, y=N_hucs, fill=Region)+
  geom_bar(stat='identity', width=.75)+
  xlab("year") +
  ylab("n HUC8")+
  scale_x_discrete(breaks=seq(1900, 2020, 5))+
  theme_bw()+
  scale_fill_viridis_d( begin = 0, end = 1, direction = -1)+
  theme(legend.position='bottom', legend.title=element_blank(), axis.text.x = element_text(size = 6, angle=45,hjust=1))+
  guides(fill = guide_legend(nrow = 1, byrow = TRUE))


figure2 <- ggarrange(f2a, f2b, f2c, ncol = 1, nrow = 3, labels="AUTO")
#Export as 8 X 8
figure2


### TAXONOMIC RESULTS ###

#backbone results
verbatim_names <- records %>%
  group_by(verbatim_name) %>%
  count()

standardized_names <- records %>%
  group_by(scientificName) %>%
  count()

status_count <- records %>%
  group_by(status) %>%
  count()

match_count <- records %>%
  group_by(matchType) %>%
  count()

mean(as.numeric(records$confidence))
min(as.numeric(records$confidence))
max(as.numeric(records$confidence))


#Number of occurrences only identified to genus
high_rank_genera <- occ %>%
  filter(rank == "GENUS") %>%
  group_by(genus) %>%
  count()

#taxonomic accuracy summary
tax_sum <- occ %>%
  filter(!is.na(InRange)) %>%
  summarise(total=n(),
    in_range=sum(InRange==TRUE),
    out_range=sum(InRange==FALSE),
    percent=round(in_range/total*100,2))


### FIGURE 3 ### 
options(scipen=10000)
sp_trait <- read_csv("Supplement3_mussel_traits.csv", trim_ws = TRUE)

#add conservation status
sp_trait <- left_join(sp_trait, conservation_key, by = c("species" = "species_update"))

sp_trait$con_status_simp <- factor(sp_trait$con_status_simp, levels = c("Listed", "Not Listed")) 

f3 <- ggplot(sp_trait, aes(x=noccs, y=aoo_HUC8_sqkm, label = species)) +
  geom_point(aes(size = noccs, color = con_status_simp,),alpha = .7) +
  geom_point(aes(size = noccs), shape = 21, alpha = .7) +
  labs(color = "conservation status") +
  scale_y_log10("AOO (sq km)") +
  scale_x_log10('n occurrences') +
  scale_size(range = c(1, 7),  name="n occurrences", breaks = c(1,10,50,100,500,1000,5000,10000), labels = c("1","10","50","100","500","1000","5000","10000")) +
  geom_text_repel(aes(size = 400, segment.alpha = .5), data = sp_trait, color = "black", show.legend = FALSE, max.overlaps = 7, min.segment.length = 0) +
  theme_bw()+
  theme(legend.position='bottom', legend.justification = "left")+
  guides(size = guide_legend(title.position = "top", nrow = 1), colour = guide_legend(title = "conservation status", override.aes = list(size=3), title.position = "top"))+
  scale_color_viridis_d(begin = 0, end = 1, direction = -1)


f3 <- ggMarginal(f3, type="boxplot",  margins = "both", size=10, groupFill = TRUE)
f3

# identify species with largest and smalles AOO
sp_trait %>% 
  filter(aoo_HUC8_sqkm %in% range(sp_trait$aoo_HUC8_sqkm)) %>%
  dplyr::select(species, aoo_HUC8_sqkm)
  
shapiro.test(sp_trait$noccs) #not normal
shapiro.test(sp_trait$aoo_HUC8_sqkm) #not normal

wilcox.test(noccs~con_status_simp, data=sp_trait)
wilcox.test(aoo_HUC8_sqkm~con_status_simp, data=sp_trait)

f4 <- ggplot(sp_trait, aes(x= noccs, y=per.change, label = species)) +
  geom_point(aes(color = con_status_simp, size = noccs),alpha = .7) +
  geom_point(aes(size = noccs), shape = 21, alpha = .7) +
  scale_size(range = c(1, 7),  name="n occurrences", breaks = c(10,50,100, 500, 1000, 5000, 10000), labels = c("10","50","100","500","1000", "5000", "10000")) +
  geom_text_repel(aes(size = 100, segment.alpha = .5), data = sp_trait , color = "black", show.legend = FALSE, max.overlaps = 5, min.segment.length = 0) +
  guides(colour = guide_legend(title = "conservation status", override.aes = list(size=3), title.position = "top"),size = guide_legend(title.position = "top"))+
  scale_x_log10('n occurrences') +
  scale_y_continuous(breaks = seq(-80,80,20)) +
  theme_bw()+
  theme(legend.position='bottom', legend.justification = "left")+
  guides(size = guide_legend(title.position = "top", nrow = 1), colour = guide_legend(title = "conservation status", override.aes = list(size=3), title.position = "top"))+
  scale_color_viridis_d(begin = 0, end = 1, direction = -1) +
  xlab("occurrences") +
  ylab("change in AOO (%)")


f4 <- ggMarginal(f4, type="boxplot",  margins = "both", size=10, groupFill = TRUE)
f4

shapiro.test(sp_trait$per.change) #not normal
wilcox.test(per.change~con_status_simp, data=sp_trait)

sp_trait %>% 
  filter(per.change %in% range(sp_trait$per.change)) %>%
  dplyr::select(species, per.change)

#Do test on species with >50 records to remove volitile occurrence poor taxa
sp_trait_50 <- sp_trait %>% 
  filter(noccs >= 50)

shapiro.test(sp_trait_50$per.change) #not normal
wilcox.test(per.change~con_status_simp, data=sp_trait_50)


### Figure 5 ###
pos <- position_jitter(width = 0.25, height = 0.1, seed = 2)

f5 <- ggplot(sp_trait, aes(x=modeStreamOrder, y=medianQA_MA, label = species)) +
  geom_point(data = sp_trait, aes(color = con_status_simp, size = n_sc_lots,), alpha = .7, position = pos) +
  geom_point(aes(size = noccs), shape = 21, alpha = .7, position = pos) +
  scale_size(range = c(1, 7),  name="n occurrences", breaks = c(10,50,100, 500, 1000, 5000, 10000), labels = c("10","50","100","500","1000", "5000", "10000")) +
  scale_y_log10('discharge (cfs)') +
  scale_x_continuous(breaks = seq(2,8,1)) +
  geom_text_repel(aes(size = 100, segment.alpha = .5), data = sp_trait, color = "black", show.legend = FALSE, max.overlaps = 5, min.segment.length = 0, position = pos) +
  theme_bw()+
  guides(colour = guide_legend(title = "conservation status", override.aes = list(size=3), title.position = "top"),size = guide_legend(title.position = "top", nrow = 1))+
  scale_color_viridis_d(begin = 0, end = 1, direction = -1) +
  theme(legend.position='bottom', legend.justification = "left")+
  xlab("mode stream order")


f5 <- ggMarginal(f5, type="boxplot",  margins = "both", size=10, groupFill = TRUE)
f5

# estimates of mode stream order
sp_trait %>% 
  filter(modeStreamOrder %in% range(sp_trait$modeStreamOrder)) %>%
  dplyr::select(species, modeStreamOrder)
  
# estimates of discharge
sp_trait %>% 
  filter(medianQA_MA %in% range(sp_trait$medianQA_MA)) %>%
  dplyr::select(species, medianQA_MA)

shapiro.test(sp_trait$modeStreamOrder) #not normal
shapiro.test(sp_trait$medianQA_MA) #not normal

wilcox.test(modeStreamOrder~con_status_simp, data=sp_trait)
wilcox.test(medianQA_MA~con_status_simp, data=sp_trait)


#FIGURE 6A+B begins on line 23803

#Figure 7A begins on line 23879


###Spatial Results
#non flagged records assigned to hucs
huc8_occs <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T) %>%
  group_by(huc8) %>%
  count()

huc8_occs %>%
  filter(n == 1)

median(huc8_occs$n)
max(huc8_occs$n)

huc8_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T) %>%
  group_by(huc8) %>%
  summarize(n_species = n_distinct(species_update))

huc8_sp %>%
  filter(n_species == 1)

median(huc8_sp$n_species)

#Figure 7B
huc_attr <- read_csv("Supplement4_huc8_summarization.csv", trim_ws = TRUE)

#remove hucs without years sampled after median year
huc_attr_full <-huc_attr %>%
  filter(per_change > -100)

#median percent chnage per huc8
median(huc_attr_full$per_change)
huc_attr_full %>% 
  filter(per_change %in% range(huc_attr_full$per_change)) %>%
  dplyr::select(huc8_name, per_change))

Figure7B <- ggplot(huc_attr_full, aes(x=n, y=per_change, label = huc8_name)) +
  geom_point(aes(color = per_change, size = n),alpha = .7) +
  scale_size(range = c(1, 10),  name="n occurrences", breaks = c(1,10, 50, 100, 500, 1000, 5000), labels = c("1","10","50", "100", "500", "1000","5000")) +
  geom_text_repel(aes(size = 100, segment.alpha = .5), data = huc_attr_full, color = "black", show.legend = FALSE, max.overlaps = 5, min.segment.length = 0) +
  scale_x_log10("n occurrences") +
  scale_y_continuous(breaks = seq(-100,80,20)) +
  theme_bw()+
  scale_color_viridis(begin = 0, end = 1, direction = -1) +
  theme(legend.position='bottom')+
  guides(size = guide_legend(nrow = 1))+
  ylab("species richness change (%)")
Figure7B
