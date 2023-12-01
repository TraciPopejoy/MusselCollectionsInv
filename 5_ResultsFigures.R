##############################################################################
# APPENDIX 1.5: R script to report results and figures (lines 23926-24359)
##############################################################################
library(tidyverse)
library(ggplot2)
library(ggrepel)
library(ggpubr)
library(ggExtra)
library(viridis)
library(effsize)
library(jpeg)
options(scipen=10000)

# read in all records 
records <- read_csv("3c_all_records.csv", trim_ws = TRUE, col_types = cols(.default = "c"))

#add col to show georef'd or not
records <- records %>% mutate(georeferenced = ifelse(is.na(standardized_latitude) | is.na(standardized_longitude), FALSE, TRUE))

#read in US CHECKLIST 
fmcs_checklist <- read_csv("3b_fmcs_checklist.csv", trim_ws = TRUE, col_types = cols(.default = "c"))

#create conservation key to assign species species their conservation status
conservation_key <- fmcs_checklist[c("fmcs_species","con_status_simp")]

#add conservation status
records <- left_join(records, conservation_key, by = c("species_update"="fmcs_species"))

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
            Percent_with_year=round(sum(!is.na(Year))/N_unique_occs*100,2),
            Percent_dry=round(sum(Preparation=='dry' & !is.na(Preparation))/N_unique_occs*100,2),
            Percent_geolocated=round((sum(georeferenced==T))/N_unique_occs*100,2),
            N_state_mismatch=sum(state_mismatch==T & !is.na(state_mismatch)),
            N_outside_NHD=sum(outside_NHD==T & !is.na(outside_NHD)),
            N_outside_range=sum(InRange ==F & !is.na(InRange))) %>%
  arrange(desc(N_unique_occs))

# summary statistics for each collection (Supplemental Information 2)
t1main %>% write.csv('institutuion_stats.csv')

# summary statistics for all the occurrences
t1main_total <- occ %>% as_tibble() %>%
  summarize(N_unique_occs=n(),
            N_genera=n_distinct(genus, na.rm = TRUE),
            N_species=n_distinct(species_update, na.rm = TRUE),
            N_huc8=n_distinct(huc8, na.rm = TRUE),
            Percent_species_ID = round(sum(rank %in% c('SPECIES', 'SUBSPECIES', 'VARIETY'))/N_unique_occs*100,2),
            Percent_with_year=round(sum(!is.na(Year))/N_unique_occs*100,2),
            Percent_dry=round(sum(Preparation=='dry' & !is.na(Preparation))/N_unique_occs*100,2),
            Percent_geolocated=round((sum(georeferenced==T))/ N_unique_occs*100,2),
            N_state_mismatch=sum(state_mismatch==T & !is.na(state_mismatch)),
            N_outside_NHD=sum(outside_NHD==T & !is.na(outside_NHD)),
            N_outside_range=sum(InRange ==F & !is.na(InRange)))

# Median institution-level stats
median(t1main$N_unique_occs)
median(t1main$N_species)
median(t1main$N_huc8)

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

#total occ w year
occ_w_year <- occ %>% 
  filter(!is.na(Year))

#PER YEAR RESULTS
#occurrences per year summary stat
occ_per_year <- occ %>% 
  filter(!is.na(Year), Year >=1900 & Year <=2020) %>%
  group_by(Year) %>%
  count()

min(occ_per_year$n)
max(occ_per_year$n)
mean(occ_per_year$n)


#species per year summary stats
sp_per_year <- occ %>% 
  filter(!is.na(Year), !is.na(species_update), Year >=1900 & Year <=2020) %>%
  group_by(Year) %>%
  summarize(N_species=n_distinct(species_update))

min(sp_per_year$N_species)
max(sp_per_year$N_species)
mean(sp_per_year$N_species)

  
# huc per year summary stats
hucs_per_year <- occ %>% 
  filter(!is.na(Year), !is.na(huc8), Year >=1900 & Year <=2020) %>%
  group_by(Year) %>%
  summarize(N_hucs=n_distinct(huc8))

min(hucs_per_year$N_hucs)
max(hucs_per_year$N_hucs)
mean(hucs_per_year$N_hucs)


### FIGURE 2 ###
### A - occurrences per year
 #grab records since 1900, add conservation status, group by year and conservation status
occ_per_year <- occ %>% 
  filter(!is.na(Year), Year >=1900 & Year <=2020) %>%
  group_by(Year, con_status_simp) %>%
  count() 

occ_per_year$con_status_simp <- factor(occ_per_year$con_status_simp, levels = c("Imperiled", "Non-imperiled")) 

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
orig_identifications <- records %>%
  group_by(orig_ident) %>%
  count()

standardized_names <- records %>%
  group_by(scientificName) %>%
  count()

match_count <- records %>%
  group_by(matchType) %>%
  count() %>%
  mutate(percent = n/nrow(records)*100)

mean(as.numeric(records$confidence))
min(as.numeric(records$confidence))
max(as.numeric(records$confidence))

status_count <- records %>%
  group_by(status) %>%
  count() %>%
  mutate(percent = n/nrow(records)*100)

taxon_rank <- occ %>%
  group_by(rank) %>%
  count() %>%
  mutate(percent = n/nrow(occ)*100)

#Number of occurrences of taxa only identified to genus
high_rank_genera <- occ %>%
  filter(rank == "GENUS") %>%
  group_by(genus) %>%
  count()

#identification accuracy summary
tax_sum <- occ %>%
  filter(!is.na(InRange)) %>%
  summarise(total=n(),
    in_range=sum(InRange==TRUE),
    out_range=sum(InRange==FALSE),
    percent=round(in_range/total*100,2))

#Number of occurrences per species
sp_occ <- occ %>%
  group_by(species_update) %>%
  count()

#median occ per species
median(sp_occ$n)


### FIGURE 3 ### 
sp_trait <- read_csv("species_attributes.csv", trim_ws = TRUE)
#rename some columns
sp_trait <- sp_trait %>% 
  rename("species" = "taxa",
         "aoo_HUC8_sqkm" = "aoo HUC8 sqkm")

#add conservation status
sp_trait <- left_join(sp_trait, conservation_key, by = c("species" = "fmcs_species"))
sp_trait$con_status_simp <- factor(sp_trait$con_status_simp, levels = c("Imperiled", "Non-imperiled"))

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
f3 #10x7

# identify species with largest and smallest AOO
sp_trait %>% 
  filter(aoo_HUC8_sqkm %in% range(sp_trait$aoo_HUC8_sqkm)) %>%
  dplyr::select(species, aoo_HUC8_sqkm)

#test for normality, significance, effect size
shapiro.test(sp_trait$aoo_HUC8_sqkm) #not normal
wilcox.test(aoo_HUC8_sqkm~con_status_simp, data=sp_trait) #sig diff
VD.A(aoo_HUC8_sqkm~con_status_simp, data=sp_trait) #large effect size

#test for normality, significance, effect size
shapiro.test(sp_trait$noccs) #not normal
wilcox.test(noccs~con_status_simp, data=sp_trait) #sig diff
VD.A(noccs~con_status_simp, data=sp_trait) #medium effect size

### FIGURE 4
f4 <- ggplot(sp_trait, aes(x= noccs, y=per.change, label = species)) +
  geom_point(aes(color = con_status_simp, size = noccs),alpha = .7) +
  geom_point(aes(size = noccs), shape = 21, alpha = .7) +
  scale_size(range = c(1, 7),  name="n occurrences", breaks = c(10,50,100, 500, 1000, 5000, 10000), labels = c("10","50","100","500","1000", "5000", "10000")) +
  geom_text_repel(aes(size = 400, segment.alpha = .5), data = sp_trait , color = "black", show.legend = FALSE, max.overlaps = 5, min.segment.length = 0) +
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
f4 #10x7

# identify species with largest and smallest per.change
sp_trait %>% 
  filter(per.change %in% range(sp_trait$per.change)) %>%
  dplyr::select(species, per.change)

#test for normality, significance, effect size
shapiro.test(sp_trait$per.change) #not normal
wilcox.test(per.change~con_status_simp, data=sp_trait) # sig diff (but marginal?)
VD.A(per.change~con_status_simp, data=sp_trait) # small

### Figure 5 ###
pos <- position_jitter(width = 0.25, height = 0.1, seed = 2)

f5 <- ggplot(sp_trait, aes(x=modeStreamOrder, y=medianQA_MA, label = species)) +
  geom_point(data = sp_trait, aes(color = con_status_simp, size = n_sc_lots,), alpha = .7, position = pos) +
  geom_point(aes(size = noccs), shape = 21, alpha = .7, position = pos) +
  scale_size(range = c(1, 7),  name="n occurrences", breaks = c(10,50,100, 500, 1000, 5000, 10000), labels = c("10","50","100","500","1000", "5000", "10000")) +
  scale_y_log10('discharge (cfs)') +
  scale_x_continuous(breaks = seq(2,8,1)) +
  geom_text_repel(aes(size = 400, segment.alpha = .5), data = sp_trait, color = "black", show.legend = FALSE, max.overlaps = 5, min.segment.length = 0, position = pos) +
  theme_bw()+
  guides(colour = guide_legend(title = "conservation status", override.aes = list(size=3), title.position = "top"),size = guide_legend(title.position = "top", nrow = 1))+
  scale_color_viridis_d(begin = 0, end = 1, direction = -1) +
  theme(legend.position='bottom', legend.justification = "left")+
  xlab("mode stream order")

f5 <- ggMarginal(f5, type="boxplot",  margins = "y", size=10, groupFill = TRUE)
f5 #10x7

#test for normality, significance, and effect size of mode stream order
shapiro.test(sp_trait$modeStreamOrder) #not normal
wilcox.test(modeStreamOrder~con_status_simp, data=sp_trait) #sig diff
VD.A(modeStreamOrder~con_status_simp, data=sp_trait) #small

# identify species with largest and smallest discharge
sp_trait_qa_range <- sp_trait %>%
  filter(species != "Beringiana beringiana")
sp_trait_qa_range %>%
  filter(medianQA_MA %in% range(sp_trait_qa_range$medianQA_MA)) %>%
  dplyr::select(species, medianQA_MA)

#test for normality, significance, effect size
shapiro.test(sp_trait$medianQA_MA) #not normal
wilcox.test(medianQA_MA~con_status_simp, data=sp_trait) #sig diff
VD.A(medianQA_MA~con_status_simp, data=sp_trait) #small


###SPATIAL RESULTS
#non flagged occs assigned to hucs
huc8_occs <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T) %>%
  group_by(huc8) %>%
  count()

#huc8 occ summary stats
huc8_occs %>%
  filter(n == 1)
median(huc8_occs$n)
max(huc8_occs$n)

#non flagged occs and species diversity per huc
huc8_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T) %>%
  group_by(huc8) %>%
  summarize(n_species = n_distinct(species_update))

#huc8 species summary stats
huc8_sp %>%
  filter(n_species == 1)

median(huc8_sp$n_species)
max(huc8_sp$n_species)

### FIGURE 6 code is in 4_EcolAttributSum.R

### FIGURE 7
huc_attr <- read_csv("huc8_attributes.csv", trim_ws = TRUE)

#remove hucs without years sampled after median year
huc_attr_full <-huc_attr %>%
  filter(per_change > -100)

#median percent change per huc8
median(huc_attr_full$per_change)
huc_attr_full %>% 
  filter(per_change %in% range(huc_attr_full$per_change)) %>%
  dplyr::select(huc8_name, per_change)

#FIGURE 7A code is in 4_EcolAttributSum.R

#FIGURE 7B
Figure7B <- ggplot(huc_attr_full, aes(x=n, y=per_change, label = huc8_name)) +
  geom_point(aes(color = per_change, size = n),alpha = .7) +
  scale_size(range = c(1, 10),  name="n occurrences", breaks = c(1,10, 50, 100, 500, 1000, 5000), labels = c("1","10","50", "100", "500", "1000","5000")) +
  geom_text_repel(aes(size = 400, segment.alpha = .5), data = huc_attr_full, color = "black", show.legend = FALSE, max.overlaps = 5, min.segment.length = 0) +
  scale_x_log10("n occurrences") +
  scale_y_continuous(breaks = seq(-100,80,20)) +
  theme_bw()+
  scale_color_viridis(begin = 0, end = 1, direction = -1) +
  theme(legend.position='bottom')+
  guides(size = guide_legend(nrow = 1))+
  ylab("species richness change (%)")

#export 8x6
Figure7B

### TABLE 1 hackjob

tenn_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc2 == "06") %>%
  group_by(species_update) %>%
  count()

tenn_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc2 == "06")

cumberland_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0513") %>%
  group_by(species_update) %>%
  count()

cumberland_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0513")

wabash_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0512") %>%
  group_by(species_update) %>%
  count()

wabash_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0512")

mobile_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0316" | huc4 == "0315") %>%
  group_by(species_update) %>%
  count()

mobile_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0316" | huc4 == "0315")

green_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0511") %>%
  group_by(species_update) %>%
  count()

green_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0511")

scioto_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0506") %>%
  group_by(species_update) %>%
  count()

scioto_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0506")

muskingum_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0504") %>%
  group_by(species_update) %>%
  count()

muskingum_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0504")

kentuckey_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc6 == "051002") %>%
  group_by(species_update) %>%
  count()

kentuckey_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc6 == "051002")

white_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "1101") %>%
  group_by(species_update) %>%
  count()

white_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "1101")

licking_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc6 == "051001") %>%
  group_by(species_update) %>%
  count()

licking_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc6 == "051001")

kanawha_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0505") %>%
  group_by(species_update) %>%
  count()

kanawha_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0505")

salt_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc8 == "05140102") %>%
  group_by(species_update) %>%
  count()

salt_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc8 == "05140102")

ouawatch_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc6 == "080401" | huc6 == "080402") %>%
  group_by(species_update) %>%
  count()

ouawatch_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc6 == "080401" | huc6 == "080402")

stfran_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc6 == "080202") %>%
  group_by(species_update) %>%
  count()

stfran_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc6 == "080202")

illinois_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0712" | huc4 == "0713") %>%
  group_by(species_update) %>%
  count()

illinois_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0712" | huc4 == "0713")

rock_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0709") %>%
  group_by(species_update) %>%
  count()

rock_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc4 == "0709")

meramec_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc8 == "07140102") %>%
  group_by(species_update) %>%
  count()

meramec_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc8 == "07140102")

kaskaskia_sp <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc6 == "071402") %>%
  group_by(species_update) %>%
  count()

kaskaskia_occ <- occ %>%
  filter(!is.na(species_update),  state_mismatch == F | is.na(state_mismatch), outside_NHD == F, InRange == T, huc6 == "071402")

write_csv(records,"3c_all_records_end.csv")

### convert column names to DarwinCore
colnames(records)[which(names(records) == "Institution")] <- "institutionCode"
colnames(records)[which(names(records) == "Cat_No")] <- "catalogNumber"
colnames(records)[which(names(records) == "orig_ident")] <- "identification"
colnames(records)[which(names(records) == "Country")] <- "country"
colnames(records)[which(names(records) == "State")] <- "stateProvince"
colnames(records)[which(names(records) == "Locality")] <- "locality"
colnames(records)[which(names(records) == "standardized_latitude")] <- "decimalLatitude"
colnames(records)[which(names(records) == "standardized_longitude")] <- "decimalLongitude"
colnames(records)[which(names(records) == "Month")] <- "month"
colnames(records)[which(names(records) == "Day")] <- "day"
colnames(records)[which(names(records) == "Year")] <- "year"
colnames(records)[which(names(records) == "Collector")] <- "recordedBy"
colnames(records)[which(names(records) == "Remarks")] <- "eventRemarks"
colnames(records)[which(names(records) == "Preparation")] <- "preparations"

records <- records %>%
  mutate(converted=case_when(converted == 1 ~ TRUE, converted == 0 ~ F))


write_csv(records,"3c_all_records_converted.csv")
