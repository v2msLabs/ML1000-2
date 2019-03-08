library(shiny)
library(shinyBS)
library(leaflet)
library(dplyr)
library(tidyr)
library(cluster)
library(RColorBrewer) # color palettes
library(jcolors)

# read data
victimData = read.csv("./www/victims-stats.csv", header = TRUE, sep=",")
numVicitms = victimData[c("n_victims")] %>% scale()
nRows =  nrow(numVicitms)
clusters = rep(1,nRows)
metrics = c("euclidean", "manhattan")
colors =  colorNumeric(jcolors('rainbow'), c(1,2,3,4,5,6,7,8,9,10), n = 10)
#colors =  colorNumeric("Paired", c(1,2,3,4,5,6,7,8,9,10), n = 10)

placeDecoder = function(code){
  place = ""
  if(code ==1 ){
    place = "School/university/college"
  } else if (code == 2){
    place = "Community center/shopping center/hospital/church"
  } else if (code == 3){
    place = "Home invasion"
  } else if (code == 4){
    place = "Street/drive by"
  }else if (code == 5){
    place = "Public"
  }
  place
}

# UI code
ui <- bootstrapPage( theme = "styles.css",
    div( class = "outer",
               
      # map in the background
      leafletOutput("map", width="100%", height="100%"),
      absolutePanel( id = "controls", class = "control-panel", 
      h3("CLARA Clustering"),
      h4("Data clustering by number of victims of the incident"),
      #1 Input: Simple Humidity3pm ----
      sliderInput("k", "Number of Clusters:",min = 2, max = 10,
      value = 8, step = 1),
      bsTooltip("k", "Specify number of clusters",  placement = "right"),
                                         
      #2 Input: Metrics ----
      selectInput('m', 'Metric', metrics),
      bsTooltip("m", "Select the metric to be used for calculating dissimilarities between observations",
                placement = "right"),
      # action
      actionButton("cluster", "Clusters Data", class = "btn")
    )
))

# Server Code
server <- function(input, output) {

  observeEvent(input$cluster, {
    clara = clara(numVicitms, k = as.numeric(input$k), metric = as.character(input$m),stand = F, samples = 80, sampsize =200)
    clusters <<- clara[["clustering"]]
    map <- leafletProxy("map")
    map %>%  clearMarkers() %>%
      addCircleMarkers(data = victimData, lng = ~longitude, lat = ~latitude,
                       label =paste0("Cluster: ",clusters," Date: ",victimData$date," Vicitms: ",victimData$n_victims,
                       " Suspects: ",victimData$n_suspects," Place: ",lapply(victimData$place_type,placeDecoder)), 
                       color = ~colors(clusters), fillColor = ~colors(clusters), radius = ~n_victims+2, weight = 4,
                       opacity = 1, fillOpacity = .7,
                       labelOptions = labelOptions(noHide = F, textOnly = F, className = "map-label"))

  })

  # draw a map
  output$map <- renderLeaflet({
    map = leaflet() %>% setView(lng = -100.957969, lat = 41.116280, zoom = 5 ) %>% addTiles() %>% 
    addCircleMarkers(data = victimData, lng = ~longitude, lat = ~latitude,
    label =paste0("Cluster: ",clusters," Date: ",victimData$date," Vicitms: ",victimData$n_victims,
      " Suspects: ",victimData$n_suspects," Place: ",lapply(victimData$place_type,placeDecoder)), 
      color = ~colors(clusters), fillColor = ~colors(clusters), radius = ~n_victims+2, weight = 4,
      opacity = 1, fillOpacity = .4,
      labelOptions = labelOptions(noHide = F, textOnly = F, className = "map-label")) 
  })
  
}

# Create Shiny app ----
shinyApp(ui, server)