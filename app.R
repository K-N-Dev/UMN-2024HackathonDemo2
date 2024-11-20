
library(shiny)
library(leaflet)
library(tidyverse)
library(sf)
library(RColorBrewer)

#Load CSV
data <- vroom::vroom("RecreationVisits.csv")

#Group data by state and year
groupdata <- data %>% 
  group_by(State, Year) %>% 
  summarise(sum = sum(RecreationVisits))

#Add column to grouped data with state name instead of abbrevation
groupdata$name <- state.name[match(groupdata$State, state.abb)]

#Drop NAs from data frame (States not included in polygon source)
NAFiltered <- drop_na(groupdata)

#Load outside table with polygon information and state names
geojsontable <- sf::read_sf("https://rstudio.github.io/leaflet/json/us-states.geojson")


# Define UI
ui <- fluidPage(
        fluidRow(
          #Year slider
          column(6, tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
          sliderInput("year", "Year",
                      min(data$Year), max(data$Year),
                      value = c(2020),
                      sep = ""))
        ),
        fluidRow(
          #Map Output
           leafletOutput("statemap")
        )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

#Filter Data by Input Year
InputFilteredData <- reactive({
  NAFiltered %>% 
    filter(Year == input$year)
}) 

#Merge Filtered Data table with geojson table with polygons and state names
MapMergeData <- reactive({
  merge(InputFilteredData(), geojsontable1, by="name", all=F)
})


#Map Rendering
output$statemap <- renderLeaflet({
    #Objects for colot pallete legend
    bins <- c(0, 10000, 20000, 50000, 100000, 200000, 500000, 1000000, Inf)
    pal <-  colorBin("Reds", domain = MapMergeData()$sum, bins = bins)
      
      #Label object to display info on hover over
      labels <- sprintf(
        "<strong>%s</strong><br/>%s Visitors<sup>",
        MapMergeData()$name, prettyNum(MapMergeData()$sum, big.mark = ",") 
      ) %>% lapply(htmltools::HTML)
      
      leaflet(MapMergeData()) %>%
        setView(-96, 37.8, 4) %>%
        addProviderTiles(provider = providers$Thunderforest.Transport) %>%
        addPolygons(
          data = MapMergeData()$geometry,
          fillColor = pal(MapMergeData()$sum),
          weight = 2,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto")) %>%
        addLegend(pal = pal, values = sum, opacity = 0.7, title = NULL,
                  position = "bottomright")
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
