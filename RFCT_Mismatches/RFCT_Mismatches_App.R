## ----knitOptions-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

knitr::opts_chunk$set(echo = TRUE)
#knitr::opts_chunk$set(root.dir = normalizePath("C:\\Users\\Tyler Marino\\Documents\\GitHub\\RuhiBusiness\\DeltaShiny"))



## ----Upload Packages-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

library(patchwork)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(readxl)
library(sf)
library(tidyverse)
library(raster)
library(sp)
library(DT)
library(RColorBrewer)
library(rsconnect)



getwd()

#setwd("C:\\Users\\Tyler Marino\\Documents\\GitHub\\RuhiBusiness\\DeltaShiny")


## ----Deploy App------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------




## ----data analysis---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------



  #### Gathering Overall sataion data ###
      # Read in Station Summary Data
if (file.exists("StationSummary.csv")) {
  StationSummaryData <- read.csv("StationSummary.csv")
  cat("Successfully read StationSummary.csv\n")
} else {
  cat("File StationSummary.csv does not exist.\n")
}

      # reading risk data file
if (file.exists("Fish_risk.csv")) {
  RiskData <- read.csv("Fish_risk.csv")
  cat("Successfully read Fish_risk.csv\n")
} else {
  cat("File Fish_risk_5-15.csv does not exist.\n")
}

      # Read in 1 Year Prediction Data
if (file.exists("fishandzoop8-24.csv")) {
  FishZoopComparison_data <- read.csv("fishandzoop8-24.csv")
  cat("Successfully read fishandzoop8-24.csv")
} else {
  cat("File fishandzoop8-24.csv does not exist.\n")
}

Predict10yrData <- FishZoopComparison_data

#### For indexing ####

        #Create list of unique station regions
            #Used in checkboxInput for Selecting Regions
station_regions <- unique(StationSummaryData$Region)

        #Remove Center Points
StationSummaryData <- StationSummaryData %>% filter(Group != 'Mean')


        #Reference names scientific = common
SpeciesNamesSciNorm <- c("Engraulis mordax" = 'Northern anchovy', "Atherinopsis californiensis" = 'Jack silverside', "Clupea pallasii" = 'Pacific herring', "Alosa sapidissima" = 'American shad', "Spirinchus thaleichthys" = 'Longfin smelt', "Morone saxatilis" = 'Striped Bass', "Dorosoma petenense" = 'Threadfin shad')


        #Get all Taxa Options
            #Used for checkboxInput for Selecting Taxa
TaxaOptions <- unique(RiskData$Taxa)



##### Preparing data for Phenology of Risk plot ######
            #renmaing to simpler month identifiers.
phenology_of_risk_data <- RiskData %>% mutate(MonthName = factor(MonthName, levels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")))
            #region clarification
phenology_of_risk_data$Region<-dplyr::recode(phenology_of_risk_data$Region, "Sanpablo" = "San Pablo Bay", "Suisun" = "Suisun Bay", "Delta" = "Delta", "Confluence" = "Confluence", "Central" = "Central Bay", "South" = "South Bay")
            #Refactoring regions
phenology_of_risk_data$Region<-factor(phenology_of_risk_data$Region, levels = c("Delta", "Confluence" , "Suisun Bay", "San Pablo Bay", "Central Bay", "South Bay"))
            #Filter for valuable regions
phenology_of_risk_data<-phenology_of_risk_data %>% filter(Region %in% c("Delta", "Confluence", "Suisun Bay", "San Pablo Bay"))

#Data Manipulation For 10yr plot
# Predict10yrData <- Predict10yrData  %>% filter(Region %in% c("San Pablo Bay", "Suisun Bay", "Confluence", "Delta"))
# Predict10yrData$Region <- factor(Predict10yrData$Region, levels = c("Delta", "Confluence" , "Suisun Bay", "San Pablo Bay"))

#Necessary filler information for plots
pd <- position_dodge(1)
alphas<- c("Key" = 1, "Off" = .3)
SHAPES <- c("San Pablo Bay" = 17, "Suisun Bay" = 16, "Delta" = 18, "Confluence" = 15)
LINES <- c("Fish" = 1, "Zoop" = 2)
LINES2 <- c("Fish" = 1, "Zoop" = 3)
COLORS  <- c("Delta" = "#1B9E77", 
             "Confluence" = "#D95F02",
             "Suisun Bay" = "#7570B3",
             "San Pablo Bay" = "#E7298A")

legend_spec_POR <- list(
  list(label = "Delta", color = "#1B9E77"),
  list(label = "Confluence", color = "#D95F02"),
  list(label = "Suisun Bay", color = "#7570B3"),
  list(label = "San Pablo Bay", color = "#E7298A")
  
)


#Information on each plot

POR_blurb <- "Monthly risk that an age-0 fish species would experience a 90% catch decline for that month in each region (hereafter, critical decline risk). Points are scaled to represent monthly percentage of annual catch (based on long-term averages). Months that collectively contain 80% of the mean annual catch are in saturated tones, while off-window months are desaturated. Gaps indicate that a species often had zero abundance for that month and region thus risk for that month was not assessed."

SingleYearPredicitons_blurb <- "Mean critical decline risk of fish predators during their high-abundance windows, paired with critical decline risk for their potential suite of zooplankton prey within that same window. Points represent probabilities calculated from maximum likelihood parameter estimates. Lower risk bounds represent “best case scenarios”, calculated with the most positive population trend and the lowest process error variance possible (based on estimated uncertainty for these parameters). Conversely, upper risk bounds represent “worst case scenarios” calculated with the most negative population trend and highest  process error variance."

TenYearPredictions_blurb <- "Critical decline risk projected out 10 years from present for fish predators (solid line) in their high-abundance window, and mean risk of their zooplankton prey assemblage (dashed line) during that same window. Bands represent the range between best case and worst case scenarios. Lower risk bounds represent “best case scenarios”, calculated with the most positive population trend and the lowest process error variance (based on estimated uncertainty for these parameters). Conversely, upper risk bounds represent “worst case scenario”, calculated with the most negative population trend and highest process error variance. Asterisks denote significant differences in mean critical decline risk between fishes and zooplankton."

StationData_Blurb <- "Metadata summarizing the datasets included in our modeling exercise. The table summarizes information available for each station (each shown as a unique row). The search bar on the top right allows for filtering  species (Taxa) or stations (Station). 
The fields are: Group (Fish or Zooplankton), Taxa (species surveyed up at each station), Region (South Bay, Central Bay, San Pablo Bay, Suisun Bay, Confluence, Delta), Start.Collection.Year (first year each taxa was collected, in order of taxa appearance), End.Collection.Year (most recent year each taxa was collected, in order of taxa appearance), Total.Years.of.Collection (Number of years collection occurred, in order of taxa appearance), Mean.CPUE (Mean Catch Per Unit Effort), Occurrences (number of non-zero records).
"

Map_blurb <- "Locations of the stations used in our models. Collection locations for fish data are represented in black circles, and data were generated by the California Department of Fish and Wildlife Bay Study. Collection locations for zooplankton are represented in maroon circles, and data were generated by the Interagency Ecological Program’s Environmental Monitoring Program. Hovering over data points reveals the station number and assigned region."

Data_resource_blurb <- "Please find the raw data and the code for these analysis here: https\\ADDREPOLINK.com "

Manuscript_blurb <- "Fournier, R.J., Carlson, S.M., Marino, T.C. & A. Ruhi. Phenology-informed decline risk of estuarine fishes and their prey suggests potential for future trophic mismatches. Conservation Biology, in review."
Cite_blurb <- "Marino, T.C., Fournier, R.J., Carlson, S.M. & A. Ruhi. Data explorer: Phenology-informed decline risk of estuarine fishes and their prey suggests potential for future trophic mismatches. https://12022001delta.shinyapps.io/RFCT_Mismatches_2/"

Bug_reporting_blurb <- "If you encounter a bug or inconsitency in the app, please report it to Tyler Marino: tcmarino8@berkeley.edu "
  
?titlePanel

## ----ShinyApp--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
ui <- fluidPage(
  titlePanel(div(h3("Data Explorer"), h1("Phenology-informed decline risk of estuarine fishes and their prey suggests potential for future trophic mismatches"))),
  
  hr(),
  
  fluidRow(                                                        
    div( style = "width: 30%; float: left; box-sizing: border-box;",                                   #Left hand side Tab Bar
      sidebarPanel(
        conditionalPanel(
          condition = " input.Tabs =='Map' || input.Tabs == 'Station Data' ",
          checkboxInput(                                                                                 #Check box for Fish Stations
          "fish",
          "Display Fish Stations",
          TRUE
          ),
          checkboxInput(                                                                                 #Check Box for Zooplankton Stations
            "zooplankton",
            "Display Zooplankton Stations",
            TRUE
          ),
          hr(),                                                                                          #Horizontal Line
        ),
        conditionalPanel(                                                                              #Inputs for Species
          condition = " input.Tabs == 'Phenology of Risk' 
          || input.Tabs == '1 Year Risk Predictions'
          || input.Tabs == '10 Year Risk Predictions' 
          || input.Tabs == 'Station Data' 
          || input.Tabs == 'Map' ",
          checkboxGroupInput(                                                                            #Check boxes for regions
            'region',
            'Select Regions',
            choices = station_regions,
            selected = station_regions
          ),
          checkboxGroupInput(
            'species',
            'Select Taxa',
            choices = TaxaOptions,
            selected = TaxaOptions
          )
        ),
        conditionalPanel(                                                                              #Inputs for basemap
          condition = "input.Tabs == 'Map'",
          selectInput(
            "basemap",
            "Select Basemap",
            choices = c("Esri.WorldStreetMap", "OpenStreetMap", "CartoDB", "OpenStreetMap.HOT"),
            selected = "Esri.WorldStreetMap"
          )
        )
      )
    ),
    tags$head(
      tags$style(HTML("
      .custom-text {
        font-size: 20px;
        font-family: arial, sans-serif;
        color: black;
        font-weight: semi-bold
      }
      .custom-text-bold {
        font-size: 20px;
        font-family: arial, sans-serif;
        color: black;
        font-weight: bold
      }
    "))
    ),
    
    div( style = "width: 70%; float: left; box-sizing: border-box;",                                          
      mainPanel(
        tabsetPanel( id = 'Tabs',
                     tabPanel('Map', 
                              fluidRow(leafletOutput('DeltaMap'), hr(), div(textOutput('Map_blurb'), class = 'custom-text'))),
                     tabPanel('Phenology of Risk', 
                              fluidRow(div(textOutput('POR_blurb'), class = 'custom-text'), hr(), plotOutput("PORplot", width = "1000px", height = "90vh"))),
                     tabPanel('1 Year Risk Predictions',
                              fluidRow(div(textOutput('SYP_blurb'), class = 'custom-text'), hr(), plotOutput("SingleYearPlot", width = "100%", height = "90vh"))),
                     tabPanel('10 Year Risk Predictions',
                              fluidRow(div(textOutput('TYP_blurb'), class = 'custom-text'), hr(), plotOutput("TenYearPlot", width = "100%", height = "90vh"))),
                     tabPanel('Station Data', 
                              fluidRow(div(textOutput('StationData_blurb'), class = 'custom-text'), hr(), DTOutput("Station_data"))),
                     tabPanel('Additional Information',
                              fluidRow(div(textOutput('LinktoDataBlurb'), class = 'custom-text-bold'), 
                                       hr(),
                                       div("Analysis and model outputs are associated with the following manuscript:", class = 'custom-text-bold'), div(textOutput('Manuscript_blurb'), class = 'custom-text'),
                                       hr(),
                                       div('Cite App:', class = 'custom-text-bold'), div(textOutput('Cite_blurb'), class = 'custom-text'), 
                                       hr(), 
                                       div(textOutput('BugReportingBlurb'), class = 'custom-text'))
                       
                     )
          
        )
      )
    )
    
  )
  
  
  
)



server <- function(input, output, session) {
  
  
############################ REACTIVE FUNCTIONS ###########################################
  
     ### Ensure the legend covers the most ###
get_legend_species <- reactive ({
  req(input$species)
  req(input$region)
  taxaSelected <- input$species
  to_check <- phenology_of_risk_data %>% filter(Region %in% input$region)
  to_check <- to_check %>% filter(Taxa %in% taxaSelected)
  to_check <- to_check %>% group_by(Taxa) %>% count() 
  to_check <- to_check[order(to_check$n, decreasing = TRUE), ]
  species_legend <- to_check$Taxa[1]
  return (species_legend)
})


  #### PLOTS #####

### Phenology of Risk ###
      #Input: TaxaInput (list), RegionInput (list)  
      #Ouptput: Phenology of Risk Plots in a grid
  
make_phenologyOR_plots <- reactive ({
  req(input$region)
  req(input$species)
  plots_phenology_of_risk <- list()
  taxaSelected <- input$species
  if (length(taxaSelected) == 0) {
    return (plot(1, 1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10)))               #Blank Plot
  }
  #Ensure the legend covers the most
  to_check <- phenology_of_risk_data %>% filter(Region %in% input$region)
  to_check <- to_check %>% filter(Taxa %in% taxaSelected)
  to_check <- to_check %>% group_by()
  

  i <- 0
  for (taxa in taxaSelected) {
    taxa_specific <- phenology_of_risk_data %>% filter(Taxa == taxa)
    taxa_region_spec_data <- taxa_specific %>% filter(Region %in% input$region)
      if (length(taxa_region_spec_data$Region) < 1) {
        next
      }
    plot <- taxa_region_spec_data %>% ggplot()+
      geom_line(aes(x=MonthName, y=Probability, color=Region, group=interaction(Taxa, Region), alpha=Window), linewidth=1.5)+
      geom_point(aes(x=MonthName, y=Probability, color=Region, group=interaction(Taxa, Region), alpha=Window, size=Percenttot))+
      scale_alpha_manual(values = alphas) +
      guides(scale = TRUE, alpha = FALSE) +
      labs(title = SpeciesNamesSciNorm[taxa])+
      labs(subtitle = taxa)+
      theme_classic() +ylab("Critical Decline Risk (%)") +xlab("Month") +ylim(0,100)+
      scale_x_discrete(guide = guide_axis(angle = 50))+
      theme(text = element_text(size = 25))+
      scale_color_manual(values = COLORS, name = ~Region)+
      scale_size_continuous(name = "Percent of Annual Catch")
    
    if (i >= 1) {
      plot <- plot + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
    }
    if (taxa != get_legend_species()) {
        plot <- plot + guides(color = "none", shape = "none", size = "none", linetype = "none") 
    }
    i <- i + 1
  
    plots_phenology_of_risk[[taxa]] <- plot
    
  }
  
  patchwork <- wrap_plots(plots_phenology_of_risk, ncol = 3, guides = 'collect')+
  plot_annotation(title = "Phenology of Risk Plots", theme = theme(plot.title = element_text(size = 35)))&
  theme(legend.position = "right")
  return (patchwork)
})
  
  
### Single Year Risk Prediction plots ###
      #Input: taxaInputs, regionInputs
      #Output: patchwork of plots.
make_1year_risk_plot <- reactive ({
  req(input$species)
  req(input$region)
  plots <- list()
  
  taxaSelected <- input$species
  if (length(taxaSelected) == 0) {
    return (plot(1, 1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10)))               #Blank Plot
  }
  
  regionSelected <- input$region
  if (length(regionSelected) == 0) {
    return (plot(1, 1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10)))               #Blank Plot
  }
  i <- 0

  for (taxa in taxaSelected) {
    taxa_spec_data <- FishZoopComparison_data %>% filter(Predator == SpeciesNamesSciNorm[taxa]) %>% filter(Timesteps == 1)  %>% filter(Region %in% regionSelected)
    taxa_spec_data$Region<-factor(taxa_spec_data$Region, levels = c("Delta", "Confluence" , "Suisun Bay", "San Pablo Bay"))
    taxa_spec_data$Taxa<-as.factor(taxa_spec_data$Taxa)
    taxa_spec_data$Taxa<-relevel(taxa_spec_data$Taxa, taxa)
    
    Oneyrplotdata <- taxa_spec_data
    #Abreviate names to fit in plot display
    Oneyrplotdata <- Oneyrplotdata %>% mutate(Taxa = recode(Taxa, "Spirinchus thaleichthys" = "S. Thaleichthys", "Engraulis mordax" = "E. Mordax", "Atherinopsis californiensis" = "A. Californiensis",  "Clupea pallasii" = "C. Pallasii", "Alosa sapidissima" = "A. sapidissima",  "Morone saxatilis" = "M. saxatilis", "Dorosoma petenense" = "D. petenense")
)
    
    j <- 0
    
    for (region in unique(taxa_spec_data$Region)) {
      taxa_region_spec_data <- Oneyrplotdata %>% filter(Region == region)
      if (length(taxa_region_spec_data$Region) < 1) {
        next
      }
      plot <- taxa_region_spec_data%>% ggplot(aes(x=Taxa, y=Probability))+
        geom_point(aes(color=Region), size=3, position = pd)+
        geom_linerange(aes(ymin=Best, ymax=Worst, group=Region, color=Region, linetype=Group),size=1, position = pd)+
        ylab("Key Window Probability")+
        theme_classic()+
        scale_x_discrete(guide = guide_axis(angle = 47))+
        ylim(0,100)+
        labs(title = paste0(SpeciesNamesSciNorm[taxa], ', ', region))+
        ylab("Critical Decline Risk (%)")+
        xlab("Fish and Potential Zooplankton Prey")+
        scale_color_manual(values = COLORS)+
        scale_linetype_manual(values = LINES2,  'Group') +
        theme(text = element_text(size = 15))+ 
        theme(plot.title = element_text(size=15))+ 
        geom_vline(xintercept = 1.5, linetype="dotted", color = "black", size=1.5)
      
      if (i >= 0 ) {
        plot <- plot + theme(axis.title.x=element_blank(), axis.title.y=element_blank())
      }
      
      if (taxa != get_legend_species() && j >= 1) {
        plot <- plot + guides(color = "none", shape = "none", size = "none", linetype = "none") 
      }
      j <- j + 1
      i <- i + 1
    
      plots[[paste0(SpeciesNamesSciNorm[taxa], ', ', region)]] <- plot
    }
  }
  patchwork <- wrap_plots(plots, ncol = 4, guides = 'collect')+
  plot_annotation(title = "1 Year Predicted Risk Plots", theme = theme(plot.title = element_text(size = 35)))&
  theme(legend.position = 'right')
  return (patchwork)
})
  
  
  

### 10 Year Prediction Risk Plots ###
        #Input: Taxas Selected(list), Regions Selected (list)
        #Output: All plots in a grid to show the Region Taxa Pairing graphs for
make_10year_prediction_plot <- reactive( {
  
  req(input$species)
  req(input$region)
  
  taxaSelected <- input$species
  if (length(taxaSelected) == 0) {
    return (plot(1, 1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10)))               #Blank Plot
  }
  
  regionSelected <- input$region
  if (length(regionSelected) == 0) {
    return (plot(1, 1, type = "n", xlab = "", ylab = "", xlim = c(0, 10), ylim = c(0, 10)))               #Blank Plot
  }
  
  plots <- list()
  i <- 0
  for (taxa in taxaSelected) {
    taxa_spec_data <-Predict10yrData %>% filter(Predator== SpeciesNamesSciNorm[taxa]) %>% filter(Region %in% regionSelected)
    Predict10yrData$Region <- factor(Predict10yrData$Region, levels = c("Delta", "Confluence" , "Suisun Bay", "San Pablo Bay"))
    taxa_spec_data<-taxa_spec_data %>%
      group_by(Region, Timesteps, Group) %>% unique() %>%
      mutate(Meanprob=mean(Probability))%>% 
      mutate(Meanbest=mean(Best))%>%
      mutate(Meanworst=mean(Worst))
    
    for (region in unique(taxa_spec_data$Region)) {
      taxa_region_spec_data <- taxa_spec_data %>% filter(Region == region)
      if (length(taxa_region_spec_data$Region) < 1) {
        next
      }
      plot <- taxa_region_spec_data %>% ggplot()+
        geom_line(aes(x=Timesteps, y=Meanprob, color=Region, linetype=Group), linewidth=2)+
        geom_ribbon(aes(x=Timesteps, ymin=Meanbest, ymax=Meanworst, fill=Region, group=(interaction(Group, Region))), alpha=0.05, size=2)+
        theme_classic() +ylim(0,100) + scale_x_continuous(breaks=seq(1,10,1))+
        theme(text = element_text(size = 20))+
        scale_color_manual(values = COLORS)+
        scale_fill_manual(values = COLORS)+
        ylab("Critical Decline Risk (%)") +
        xlab("Years From Present")+
        scale_linetype_manual(values = LINES2, name = 'Group') + 
        theme(plot.title = element_text(size = 15)) +
        labs(title = paste0(SpeciesNamesSciNorm[taxa], ', ', region))
      
             # Conditionally remove axis labels for all plots except one
      if (i >= 1) {
        plot <- plot + theme(axis.title.x = element_blank(), axis.title.y = element_blank())
      }
      if (taxa != get_legend_species()) {
        plot <- plot + guides(color = "none", shape = "none", size = "none", linetype = 'none', fill = 'none') 
      }
      
      i <- i + 1
    
    
      plots[[paste0(SpeciesNamesSciNorm[taxa], ', ', region)]] <- plot
    }
  }
  patchwork <- wrap_plots(plots, ncol = 4, guides = 'collect')+
  plot_annotation(title = "10 Year Predicted Risk Plots", theme = theme(plot.title = element_text(size = 35)))&
  theme(legend.position = 'right')
  return (patchwork)
})


  FetchStationSummaryData <- reactive({

    filtered_data <- StationSummaryData
    
    filtered_data <- filtered_data |> dplyr::filter(Region %in% input$region)                                               # Filter by region
    
                                                                                         
    species <- c(input$species, 'Acartiella', 'Limnoithona', 'Pseudodiaptomus', 'Bosmina', 'Sinocalanus',                   # Name desired species, hard include zooplankton???
                 'Acartia', 'Oithona', 'Tortanus', 'Eurytemora', 'Daphnia')
    species_pattern <- paste(species, collapse = "|")
    filtered_data <- filtered_data %>%                                                                                      # Filter based on species input, matching species in Taxa column
      dplyr::filter(str_detect(Taxa, species_pattern))
                                                                                                                            # Filter based on group input (fish or zooplankton)
    if (input$fish && input$zooplankton) {                                                                                  # Both fish and zooplankton are selected, no further filtering needed
      return(filtered_data)
    } else if (input$fish) {                                                                                                # Only fish selected
      filtered_data <- filtered_data |> dplyr::filter(Group == 'Fish')
    } else if (input$zooplankton) {                                                                                         # Only Zooplankton Selected
      filtered_data <- filtered_data |> dplyr::filter(Group == 'Zooplankton')
    } else {                                                                                                                # Neither fish nor zooplankton selected, return an empty data frame
      return(filtered_data[0, ])
    }
    
    return(filtered_data)
  })
  
    
  
  
  
  ######################### BEFORE ANY ACTIVITY ON THE APP, DISPLAY THE BELOW ###########################
  # # Initialize reactive values for zoom and center
  map_state <- reactiveValues(
    zoom = 8,  # Default zoom level
    center = list(lat = 37.48549685534591163, lng = -122.1670034591194991)  # Default center
  )
  
  
  ######################## UPON ACTIVITY ON THE APP ##########################
  
  observeEvent(
    c(input$region, input$fish, input$zooplankton, input$Tabs, input$species), {                               #Upon clicking the sidebar, MANIPULATE UI
    
      
      # Update zoom and center from input
      if (!is.null(input$DeltaMap_zoom)) {
        map_state$zoom <- input$DeltaMap_zoom  # Update zoom
      }
      if (!is.null(input$DeltaMap_center)) {
        map_state$center <- input$DeltaMap_center  # Update center as list
      }
      
      
      ### Dynamic Rendition of the Map and Its static blurb###
      output$DeltaMap <- renderLeaflet({
        # Fetch data
        summary_data <- FetchStationSummaryData()
        
        
        # Create base map with tiles
        map <- leaflet::leaflet() %>%
          addProviderTiles(input$basemap) %>%
          setView(lng = map_state$center$lng, lat = map_state$center$lat, zoom = map_state$zoom)
        
        # Check if data is not empty

        if (nrow(summary_data) > 0) {
          # Add circle markers if there is data
          map <- map %>%
            addCircleMarkers(data = summary_data,
              lng = ~Longitude,
              lat = ~Latitude,
              label = ~paste0(Group, " Station ", Station, " || Region: ", Region), labelOptions = labelOptions(textsize = "18px"),
              color = ~ifelse(Group == "Fish", "black", "maroon"),
              radius = 7,
              fillOpacity = 1,
              stroke = FALSE
            ) %>%
            addLegend(
              position = "bottomright", 
              colors = c("black", "maroon"), 
              labels = c("Fish Stations", "Zooplankton Stations"), 
              opacity = 1 
            )
        }

        return(map)
      })
      
      
      
      output$Map_blurb <- renderText(Map_blurb)
      
      #output$Station_Counts <- renderText(FetchStationSummaryData())
      
      
      ### Phenology of Risk plot and blurb ###
      output$PORplot <- renderPlot({
        make_phenologyOR_plots()
      }, width = 1200, height = 1200)  
      
      output$POR_blurb <- renderText(POR_blurb)
      
      
      ### Single Year Critical Decline Probability plot and blurb ###
      output$SingleYearPlot <- renderPlot({
        make_1year_risk_plot()
      }, width = 1200, height = 1200)  
      
      output$SYP_blurb <- renderText(SingleYearPredicitons_blurb)
      
      
      ### Ten Year Prediction of Critical Decline Probability plot and blurb ###
      output$TenYearPlot <- renderPlot({
        make_10year_prediction_plot()
      }, width = 1200, height = 1200)
      
      output$TYP_blurb <- renderText(TenYearPredictions_blurb)
      
      
      ### Station Meta Data and blurb ###
      output$Station_data <- renderDT({FetchStationSummaryData()})                             #Dynamic Table for Metadata
      output$StationData_blurb <- renderText(StationData_Blurb)
      
      
      ### Additional Information blurbs + links ###
      output$LinktoDataBlurb <- renderText(Data_resource_blurb)
      output$Manuscript_blurb <- renderText(Manuscript_blurb)
      output$Cite_blurb <- renderText(Cite_blurb)
      output$BugReportingBlurb <- renderText(Bug_reporting_blurb)
      
      
      
      
    }
  ) 
  
}
  

#Call the shiny app
shinyApp(ui, server)



