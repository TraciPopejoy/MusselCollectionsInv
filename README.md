# SYNTHESIS OF NATURAL HISTORY COLLECTIONS DATA REVEALS PATTERNS OF US FRESHWATER MUSSEL DIVERSITY AND DECLINE.
### by John Pfeiffer, Traci P DuBose and Sean Keogh
Natural history collections are uniquely positioned to chronicle biodiversity changes across time and space and are a fundamental data source in taxon-based research and conservation, especially for imperiled freshwater mussels. We used the following code to aggregate specimen records from 45 US natural history collections and enriched these records by programmatically standardizing taxonomic information, flagging potentially problematic records, and joining records with freshwater-specific spatial frameworks (e.g., hydrological units and stream segments) and their associated metadata (e.g., area, stream order, discharge,
velocity). 

The assembled dataset can be found at [this Dryad Repository](https://datadryad.org/stash/dataset/doi:10.5061/dryad.c2fqz61cg).
The manuscript is currently available as [a pre-print on bioArxiv](https://www.biorxiv.org/content/10.1101/2022.09.22.509037v1).
Explore the synthesized freshwater mussel collections data at our interactive web app -- [MusselMapR](https://musselmapr.shinyapps.io/hic_sunt_naiades/)

## Data Processing and Analysis Scripts
1_OpenRefine_operation_history.txt: edits to refine synthesized collections data (JSON)
2_Build_species_shapefiles.R: an R script to build shapefiles of species ranges  
3_DataPrep.R: an R script to prepare refined collections data for analysis  
4_EcolAttributSum.R: an R script to estimate ecological attributes  
5_ResultsFigures.R: an R script to report results and generate figures  

### Intended Uses
Code is provided for transparency and for other users to leverage in their own collections-based research. It is provided without warranty and the authors welcome any suggestions.