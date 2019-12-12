# rsconnect::deployApp('~/Dropbox/currentprojects/RoadSalt/Shiny/ChlorideObservations/')

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(sf)
library(leaflet)
library(leafgl) #devtools::install_github("r-spatial/leafgl")
library(colourvalues)
library(viridisLite)

allObs = read_csv('Data/allstatesWQP.csv') %>% 
  st_as_sf(coords = c("Long", "Lat"), crs = 4326) %>% 
      mutate(label = paste0("<b>",MonitoringLocationName,"</b>", '<br>',
                         '# Observations = ', n,'<br>',
                         'Years Observed = ',minYear,' - ',maxYear, '<br>',
                         'Observed Chloride = ',round(MinCl,2), ' - ',round(MaxCl,2),' mg/L')) %>% 
  mutate(group = case_when(MeanCl <= 20 ~ '< 20',
                           MeanCl > 20 & MeanCl <= 50 ~ '20-50',
                           MeanCl > 50 & MeanCl <= 230 ~ '50-230',
                           MeanCl > 230 & MeanCl <= 860 ~ '230-860',
                           MeanCl > 860 ~ '> 860')) %>% 
  mutate(group = factor(group, levels =  c('< 20', '20-50', '50-230','230-860','> 860'))) %>% 
  arrange(MeanCl)

streams = allObs %>% filter(MonitoringLocationTypeName == 'Stream')  
lakes = allObs %>% filter(MonitoringLocationTypeName != 'Stream')  

lake.cols = colour_values_rgb(lakes$group, include_alpha = FALSE) / 255  
stream.cols = colour_values_rgb(streams$group, include_alpha = FALSE) / 255  

# Define UI for application 
ui <- fluidPage(

    headerPanel("Water quality observations: Chloride"),

    mainPanel(
      HTML(paste(
          h5("Shiny app by Hilary Dugan @hildug"),#'<br/>',
          h5("All data is from the Water Quality Portal: ", a('https://www.waterqualitydata.us/', href = 'https://www.waterqualitydata.us/')),
          hr()
        ))),
    
    # Lake Map
    mainPanel(h2("Lakes")),
    leafletOutput('lakemap'),
    
    # Stream Map
    mainPanel(h2("Streams")),
    leafletOutput('streammap'),
    hr()

)

# Define server logic required to map
server <- function(input, output) {
    
    output$streammap <- renderLeaflet(({
        leaflet() %>% 
            addProviderTiles(providers$OpenStreetMap,
                             options = providerTileOptions(noWrap = TRUE)) %>% 
            
            # addGlPoints(data = obs) %>%
            addGlPoints(data = streams, group = 'Observed', color = stream.cols,
                        popup = 'label') %>%

            setView(lng = -80.37, lat = 44.16, zoom = 4) %>% 
     
              addLegend("topright", colors = viridis(5),
                      labels = c('< 20', '20-50', '50-230','230-860','> 860'),
                      # labFormat = labelFormat(digits = 1, transform = function(x) exp(x)),
                      title = "Average Stream Chloride (mg/L)")
    }))
    
    output$lakemap <- renderLeaflet(({
      leaflet() %>% 
        addProviderTiles(providers$OpenStreetMap,
                         options = providerTileOptions(noWrap = TRUE)) %>% 
        
        # addGlPoints(data = obs) %>%
        addGlPoints(data = lakes, group = 'Observed', color = lake.cols,
                    popup = 'label') %>%
        
        setView(lng = -80.37, lat = 44.16, zoom = 4) %>% 
        
        addLegend("topright", colors = viridis(5),
                  labels = c('< 20', '20-50', '50-230','230-860','> 860'),
                  # labFormat = labelFormat(digits = 1, transform = function(x) exp(x)),
                  title = "Average Lake Chloride (mg/L)")
    }))
}

# Run the application 
shinyApp(ui = ui, server = server)
