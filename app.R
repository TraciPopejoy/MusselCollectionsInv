# load libraries
library(tidyverse)
library(ggplot2)
library(leaflet)
library(sf)
library(shiny)
library(shinythemes)
library(raster)
options(scipen=10000)


# load all occurrence data
all <- readRDS(file = "all_data.rds")
occ <- readRDS(file = "occ.rds")

# get species list for pull-down options and sort alphabetically
species_list <- sort(unique(occ$species_update))

#read inn trait data
traits <- read_csv("mussel_traits.csv", trim_ws = TRUE, col_types = cols(.default = "c"))


# This info is important for the second (HUC) map
state_bord_simp<-readRDS("state_borders.rds") # state boarders 
huc_simp<-readRDS("huc6.rds") # HUC 6's simplified geometry
extents <- extent(huc_simp) # adjust the extends so the ggplot is centered on HUC map
ymax <- 53
extents@ymax <- ymax
ymin <- 24
extents@ymin <- ymin
xmax <- -65
extents@xmax <-  xmax
xmin <- -126
extents@xmin <- xmin
# Colors for second map
cols <- c('1-2 records' = "#FFD377", '3-4 records' = "#CE6814",'>4 records' = "#740000")


########## FLUID PAGE ###########
#this section is to set up User Interface - dropdown menus, sliders, boxes, etc

ui <- fluidPage(
  theme = shinythemes::shinytheme("readable"),
  
  # App Title
  h1("MusselMapR"),
  br(),
  
  # Introduction text
  h4("MusselMapR is a web app designed to help folks navigate US freshwater mussel occurrence data! This resource includes >410k voucher-based occurrences synthesized from 45 US natural history museums."),
  br(),

  
  # Radio buttons to select search type
  radioButtons(inputId = "search_type", 
               label = "Explore by species or watershed?",
               choices = c("Species Search" = "species", "Watershed (HUC6) Search" = "watershed")),
  br(),
  
  # UI elements for species search (conditional)
  conditionalPanel(
    condition = "input.search_type == 'species'",
    selectInput(inputId = "species",
                label = "Pick a species",
                choices = species_list),
    
    leafletOutput("mymap"),
    textOutput("map_desc"),
    br(),
    plotOutput("hucmap"),
    textOutput("hucmap_desc"),
    br(),
    plotOutput("year_bar"),
    textOutput("year_desc"),
    br(),
    plotOutput("stream_order"),
    textOutput("stream_desc"),
    br(),
    plotOutput("stream_char"),
    textOutput("stream_char_desc"),
    br(),
    br(),
    textOutput("table_desc"),
    tableOutput("species_attributes"),
    br(),
    downloadButton("download", "Download Occurrences"),
    br(),
    br(),
    br()
  ),
  
  # UI elements for watershed search (conditional)
  conditionalPanel(
    condition = "input.search_type == 'watershed'",
    label = "click on the map to select a watershed",
    
    # Add the watershed selection map
    leafletOutput("watershed_selection_map"),
    
    # Display the selected watershed name
    textOutput("selected_watershed_name"),
    br(),
    
    leafletOutput("watershed_map"),
    textOutput("watershed_desc"),
    br(),
    
    plotOutput("huc_sp_bar"),
    textOutput("huc_sp_desc"),
    br(),
    
    plotOutput("huc_year_bar"),
    textOutput("huc_year_desc"),
    br(),
  ),
  
  br(),
  textOutput("contact"),
  br(),
  HTML("<p>Want to learn more about this resource? Please read our <a href='https://www.sciencedirect.com/science/article/pii/S0006320724000235?via%3Dihub'> paper</a>.</p>"),
  br(), 
)



server <- function(input, output, session) {
  
  # Species search logic
  output$mymap <- renderLeaflet({
    if(input$search_type == "species") {
      sp_map <- occ %>% filter(!is.na(year) & species_update == input$species)
      leaflet(sp_map) %>%
        addTiles() %>%
        addCircleMarkers(lat = ~as.numeric(decimalLatitude), lng = ~as.numeric(decimalLongitude), color = "orange",
                         radius = 4, stroke = FALSE, opacity = 1, fillOpacity = 1,
                         popup = ~paste("<h5>Record Info</h5>",
                                        "<b>Institution:</b>", catalogNumber, "<br>",
                                        "<b>Waterbody:</b>", GNIS_NAME, "<br>",
                                        "<b>Date:</b>", day, month, year, "<br>",
                                        "<b>Stream order:</b>", StreamOrde, "<br>",
                                        "<b>Flow rate (cfs):</b>", flow_rate, "<br>",
                                        "<b>Slope:</b>", slope, "<br>",
                                        "<b>Water velocity (f/s):</b>", velocity, "<br>",
                                        "<b>HUC4:</b>", huc4_name, "<br>",
                                        "<b>HUC6:</b>", huc6_name, "<br>",
                                        "<b>HUC8:</b>", huc8_name, "<br>",
                                        "<b>COMID:</b>", COMID))
    }
  })
  
  output$map_desc <- renderText({
    if(input$search_type == "species") {
      paste("Figure 1: Interactive dot distribution map of", input$species, "(click on individual records for more info)")
    }
  })
  
  # MAKE HUC MAP
  output$hucmap <- renderPlot({
    huc_occ<-occ %>% filter(!is.na(year) & species_update == input$species)%>% 
      as_tibble() %>%
      group_by(huc6) %>%
      count()  
    
    hucs<-huc_simp %>%
      right_join(huc_occ %>% as_tibble(), by='huc6')
    p2<-hucs  %>%
      mutate(nc=factor(case_when(between(n, 1,2) ~ '1-2 records',
                                 between(n, 3,4) ~ '3-4 records',
                                 n > 4 ~ '>4 records'),
                       levels=rev(c('1-2 records', '3-4 records', '>4 records'))))
    
    # plot it
    ggplot()+
      geom_sf(data = huc_simp,aes(fill=NULL,geometry = geometry),size=0.1,color='gray50',fill='white')+
      geom_sf(data=p2,aes(fill=nc),size=0.15, color='gray50')+
      scale_fill_manual(values=cols,aesthetics = "fill")+
      geom_sf(data=state_bord_simp, size=0.25, fill=NA, color="black", )+
      coord_equal() +
      coord_sf(xlim = c(xmin, xmax), ylim = c(ymin, ymax), expand = FALSE) +
      guides(fill=guide_legend(title=paste0('Number of<br>*',input$species,"*<br>records in HUC6s")))+
      theme(legend.title.align=0.5,
            legend.title = ggtext::element_markdown(size=14),
            legend.text = element_text(size=12))
  })
  
  # LABEL HUC MAP 
  output$hucmap_desc <-renderText({
    paste("Figure 2: Map of", input$species, "and its occupancy of hydrological units (HUC6)")
  }) 
  
  
  #MAKE THE year BAR
  output$year_bar <- renderPlot({
    year_count <- all %>% 
      filter(!is.na(year) & species_update == input$species) %>%
      group_by(year) %>%
      count() %>%
      mutate(year=as.numeric(year))
    
    # plot it
    ggplot(year_count) +
      aes(x=year, y=n)+
      geom_bar(stat='identity', width=1,fill="orange",color='black')+
      scale_x_continuous("year", limits=c(1800,2020), breaks=seq(1800,2020,10)) +
      ylab("occurrences")+
      theme_bw()+ 
      theme(legend.position='bottom', axis.text.x = element_text(size = 9, angle=45,hjust=1))
  })
  
  # LABEL THE year BAR 
  output$year_desc <-renderText({
    paste("Figure 3: Bar graph of ", input$species, "occurrences per year")
  }) 
  
  #MAKE THE STREAM ORDER BAR
  output$stream_order <- renderPlot({
    SO_count <- occ %>% 
      filter(!is.na(StreamOrde) & !is.na(year) & species_update == input$species) %>%
      group_by(StreamOrde) %>%
      count() %>%
      mutate(StreamOrde=as.numeric(StreamOrde))
    
    # plot it
    ggplot(SO_count) +
      aes(x=StreamOrde, y=n)+
      geom_col(width=.75,fill="orange",color='black')+
      scale_x_continuous('Stream Order', limits=c(2,10), breaks=2:10)+
      ylab("occurrences")+
      theme_bw()
  })
  
  # LABEL THE STREAM BAR 
  output$stream_desc <-renderText({
    paste("Figure 4: Bar graph of ", input$species, "occurrences per stream order")
  })
  
  #MAKE THE STREAM CHAR DENSITIES 
  output$stream_char <- renderPlot({
    sc_df <- occ %>% 
      filter(!is.na(StreamOrde) & !is.na(year) & species_update == input$species) %>%
      dplyr::select(flow_rate, velocity, slope) %>%
      mutate(across(everything(), as.numeric)) %>%
      pivot_longer(everything())
    
    # plot it
    ggplot(sc_df) +
      aes(x=value)+
      geom_density(fill='orange',color='black')+
      facet_wrap(~name, scales='free', nrow=1)+
      scale_x_log10()+
      ylab('Probability Density')+
      theme_bw()
  })
  
  # LABEL THE STREAM BAR 
  output$stream_char_desc <-renderText({
    paste("Figure 5: Density plots of stream flow rate (log; cfs), stream slope (log), and stream velocity (log; f/s) of", input$species, "occurrences")
  })
  
  # LABEL THE STREAM BAR 
  output$table_desc <-renderText({
    paste("Table 1: Summary statistics for", input$species )
  })
  #table 
  output$species_attributes <- renderTable({
    sp_trait <- traits %>%
      filter(taxa == input$species) %>%
      dplyr::select(c("noccs", "nhuc8", "aoo_HUC8_sqkm", "median_year", "per.change", "modeStreamOrder", "medianSlope", "medianQA_MA", "medianVA_MA")) %>%
      rename("Number of georeferenced occurrences" = "noccs", "Number of HUC8s" = "nhuc8", "Area of occupancy (sqkm)" = "aoo_HUC8_sqkm", "Median collection year" = "median_year", "% change in area of occupancy since median year" = "per.change", "Stream order (mode)" = "modeStreamOrder",  "Flow rate (median; cfs)" = "medianQA_MA","Stream slope (median)" = "medianSlope", "Flow velocity (median; f/s)" = "medianVA_MA") %>% 
      pivot_longer(cols = everything())
  },colnames = FALSE)
  
  # Contact info
  output$contact <-renderText({
    paste("Thanks for using MusselMapR! Please contact John Pfeiffer, Traci DuBose, and Sean Keogh with questions - pfeifferj@si.edu, tracipdubose@gmail.com, skeogh@fieldmuseum.org")
  })
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("MusselMapR_", input$species, "_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      #filter the species of interest from the all dataframe
      write_csv(all %>% filter(species_update == input$species), file)
    })  
  

  
  
  # watershed search logic
  # Create a reactive value to store the selected watershed
  selected_watershed <- reactiveVal(NULL)
  
  # Render the watershed selection map
  output$watershed_selection_map <- renderLeaflet({
    leaflet(huc_simp) %>%
      addTiles() %>%
      addPolygons(layerId = ~name, # Use the HUC6 name as the layer ID
                  color = "blue", weight = 2, opacity = 1,
                  fillColor = "transparent", fillOpacity = 0.5,
                  highlight = highlightOptions(color = "red", weight = 2,
                                               bringToFront = TRUE))
  })
  
  # Observe map clicks and update the selected watershed
  observeEvent(input$watershed_selection_map_shape_click, {
    click <- input$watershed_selection_map_shape_click
    selected_watershed(click$id) # Save the selected watershed (HUC6 name)
    print(paste("Selected Watershed:", click$id))  # Debugging line to print selected ID
  })
  
  # Display the selected watershed's name
  output$selected_watershed_name <- renderText({
    req(selected_watershed()) # Ensure that a watershed has been selected
    paste("You selected the", selected_watershed(),"HUC6. Scroll down for more!")
  })
  
  # Use the selected watershed in your app logic
  output$watershed_map <- renderLeaflet({
    req(selected_watershed()) # Ensure that a watershed has been selected
    
    # Filter the data based on the selected watershed
    filtered_data <- occ %>% filter(!is.na(species_update), huc6_name == selected_watershed())
    
    # Check if any rows were returned
    if(nrow(filtered_data) == 0) {
      return(NULL) # If no data is found, don't render the map
    }
    
    leaflet(filtered_data) %>%
      addTiles() %>%
      addCircleMarkers(lat = ~as.numeric(decimalLatitude), lng = ~as.numeric(decimalLongitude), color = "orange",
                       radius = 4, stroke = FALSE, opacity = 1, fillOpacity = 1,
                       popup = ~paste("<h5>Record Info</h5>",
                                      "<b>Species:</b>", species_update, "<br>",
                                      "<b>Institution:</b>", catalogNumber, "<br>",
                                      "<b>Waterbody:</b>", GNIS_NAME, "<br>",
                                      "<b>Date:</b>", day, month, year, "<br>",
                                      "<b>Stream order:</b>", StreamOrde, "<br>",
                                      "<b>Flow rate (cfs):</b>", flow_rate, "<br>",
                                      "<b>Slope:</b>", slope, "<br>",
                                      "<b>Water velocity (f/s):</b>", velocity, "<br>",
                                      "<b>HUC4:</b>", huc4_name, "<br>",
                                      "<b>HUC6:</b>", huc6_name, "<br>",
                                      "<b>HUC8:</b>", huc8_name, "<br>",
                                      "<b>COMID:</b>", COMID))
  })
  
  output$watershed_desc <- renderText({
    req(selected_watershed()) # Ensure that a watershed has been selected
    
    filtered_data <- occ %>% filter(!is.na(species_update), huc6_name == selected_watershed())
    
    if(nrow(filtered_data) == 0) {
      return("No data available for the selected watershed.") # Message if no data found
    }
    
    paste("Figure 1: Interactive dot distribution map of", selected_watershed(), "HUC6. Click on individual records for more info. ")
  })
  
  output$huc_sp_bar <- renderPlot({
    req(selected_watershed())  # Ensure that a watershed is selected
    
    huc_sp_count <- all %>% 
      filter(!is.na(species_update) & huc6_name == selected_watershed()) %>%
      group_by(species_update) %>%
      count()
    
    if(nrow(huc_sp_count) == 0) {
      return(NULL)  # Don't render the plot if no data is found
    }
    
    # Plot it
    ggplot(huc_sp_count) +
      aes(x = reorder(species_update, -n), y = n) +  # Reordering species by count
      geom_bar(stat = 'identity', width = 0.8, fill = "orange", color = 'black') +
      theme_bw() +
      theme(legend.position = 'bottom', 
            axis.text.x = element_text(size = 9, angle = 90, hjust = 1),
            axis.title.x = element_blank())
  })
  
  output$huc_sp_desc <- renderText({
    req(selected_watershed())  # Ensure that a watershed is selected
    
    filtered_data <- all %>% filter(!is.na(species_update) & huc6_name == selected_watershed())
    
    if(nrow(filtered_data) == 0) {
      return("No species data available for the selected HUC6.")  # Message if no data found
    }
    
    paste("Figure 1: Species recorded from the ", selected_watershed(), " HUC6 and their total number of occurrences")
  })
  
  output$huc_year_bar <- renderPlot({
    req(selected_watershed())  # Ensure that a watershed is selected
    
    huc_year_count <- all %>% 
      filter(!is.na(year) & huc6_name == selected_watershed()) %>%
      group_by(year) %>%
      count() %>%
      mutate(year = as.numeric(year))
    
    if(nrow(huc_year_count) == 0) {
      return(NULL)  # Don't render the plot if no data is found
    }
    
    # Plot it
    ggplot(huc_year_count) +
      aes(x = year, y = n) +
      geom_bar(stat = 'identity', width = 1, fill = "orange", color = 'black') +
      scale_x_continuous("year", limits = c(1800, 2020), breaks = seq(1800, 2020, 10)) +
      ylab("occurrences") +
      theme_bw() + 
      theme(legend.position = 'bottom', 
            axis.text.x = element_text(size = 9, angle = 45, hjust = 1))
  })
  
  output$huc_year_desc <- renderText({
    req(selected_watershed())  # Ensure that a watershed is selected
    
    filtered_data <- all %>% filter(!is.na(year) & huc6_name == selected_watershed())
    
    if(nrow(filtered_data) == 0) {
      return("No year data available for the selected HUC6.")  # Message if no data found
    }
    
    paste("Figure 2: Collecting efforts in the ", selected_watershed(), " HUC6 over time")
  })
  
}

shinyApp(ui = ui, server = server)











# remove all flagged, non-georef, and non-species level records
#occ<-all %>% 
#  filter(!is.na(species_update)) %>%
#  filter(InRange=='TRUE') %>% 
#  filter(state_mismatch=='FALSE') %>% 
#  filter(outside_NHD=='FALSE') %>% 
#  filter(dupe_flag=='FALSE') 

# occ<-occ %>% 
#  rename("flow_rate" = "QE_MA", "velocity" = "VE_MA", "slope" = "SLOPE")


# NHDPlus assumes the sixth digit is a zero but is not always included
# This causes an issue when joining the occ spreadsheet to the spatial framework
# occ$huc6<-str_pad(occ$huc6, 6, side ='left', pad = 0)

