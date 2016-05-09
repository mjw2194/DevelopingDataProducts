library(curl)
library(shiny)
library(jsonlite)
library(lubridate)
library(leaflet)
library(stringr)
library(ggplot2)
library(tidyr)
library(dplyr)



url_citibike <- "http://citibikenyc.com/stations/json"

citibike_stations <- fromJSON(txt = url_citibike,
                              simplifyVector = T,
                              simplifyDataFrame = T,
                              flatten = T)

citibike_stations_status_proceesed<- citibike_stations$stationBeanList


citibike_stations_status_proceesed<- citibike_stations_status_proceesed %>%
        rename(station_id = id) %>%
        mutate(availability_color = ifelse(availableBikes == 0, "#990000", NA),
               availability_color = ifelse(availableBikes > 0 & availableBikes <= 3, "#ffd700", availability_color),
               availability_color = ifelse(availableBikes > 3, "#008000", availability_color))




your.map <- leaflet() %>%
        addTiles() %>%
        addCircleMarkers(data = citibike_stations_status_proceesed,
                         lng = ~longitude,
                         lat = ~latitude,
                         color = ~availability_color,
                         weight = 5,
                         radius = c(15),
                         # fillColor = c("#004a8f","#fc846b"),
                         fillOpacity = 0.2,
                         popup = ~paste("Station ID: ", station_id, "<br>",
                                        "Station Name: ", stationName, "<br>",
                                        "Available Bikes: ", availableBikes, "<br>",
                                        "Total Bikes: ", totalDocks, "<br>",
                                        "Updated: ", lastCommunicationTime),
                         clusterOptions = markerClusterOptions()) %>%
        setView(lng = -73.9495532, lat = 40.7703832, zoom = 12)

citibike_stations_net_trips<- readRDS("./data/citibike_stations_net_trips_processed.rds")


app <- shinyApp(
        ui <- fluidPage(
                absolutePanel(h1("Citibike Availability in NYC"),
                              h4(p("Explore the current availability of ", a(href = "http://www.citibikenyc.com/","Citibikes "), "at stations across NYC as well as historical trends in availability.",
                                   "Current availability data is accessed via the Citibike API and ", a(href = "https://s3.amazonaws.com/tripdata/index.html","histoical data "), " (4/15 through 3/16) was collected from the website and processed.",
                                   "Users can explore the availability of bikes on the map and use the station id (displayed in the popup) to find view trends in availability for a specific future date."))),
                absolutePanel(h3("Station"),
                              h5("Enter Station ID & Date"),
                              top = 150, left = 600, width = 300, height = 300, draggable = T, fixed = F, style = "opacity: 1.0",
                              wellPanel(numericInput(inputId = "station_id", label = "Enter Station ID", value = 518, min = 0, max = 5000),
                                        dateInput(inputId = "date", label = "Select Date",value = today(), min = today()),
                                        actionButton(inputId = "estimate_availability", label = "View Station Trend")
                                        )
                              ),
                absolutePanel(h3("Citibike Stations (New York City)"),
                              h5(p("Find a Citibike station on the map. ",
                                   "A red circle around an individual station means there are no bikes available. A yellow cirle means there are between 1 and 3 bikes.", 
                                   "A green circle means there are more than 3 bikes available, but you should explore the station's trend to see how likely it is that a bike will be available.")),
                              top = 150,left = 50,width=500,height = 500,draggable = T,fixed = F,style = "opacity: 1.0",
                              wellPanel(leafletOutput('myMap',width = "100%", height = 500))
                              ),
                absolutePanel(h3("Trends in Availability"),
                              h5(p("The average flow of bikes (bikes ariving minus bikes leaving) is displayed for the station and day of the week selected.", 
                                   "The upper and lower bounds represent +/- 1 standard deviation, respectively.", 
                                   "A negative value represents more bikes leaving from the station than bikes arriving at the station for the 30 min window.")),
                              top = 150, left = 950, width = 1200, height = 1000, draggable = T, fixed = F, style = "opacity: 1.0", 
                              plotOutput(outputId = "trend", width = 1200, height = 500))),
        
        server <- function(input, output) {
                map <- your.map
                output$myMap <- renderLeaflet(map)
                
                
                
                update_station_id<- eventReactive(input$estimate_availability, {
                        My_Station<- citibike_stations_net_trips[citibike_stations_net_trips[,2]==input$station_id,]
                        
                        
#                         My_Station<- citibike_stations_net_trips %>%
#                                 filter(station_id == intput$station_id)
                        
                        start_y<- year(min(My_Station$date))
                        start_m<- month(min(My_Station$date))
                        min_day<- "01"
                        min_time<- "00:00"
                        
                        start_date<- ymd_hm(paste(paste(start_y,start_m,min_day, sep = "-"), min_time, sep = " "),tz = "America/New_York" )
                        
                        
                        end_y<- year(max(My_Station$date))
                        end_m<- month(max(My_Station$date))
                        end_d_1<- ymd_hm(paste(paste(end_y,end_m,min_day, sep = "-"), min_time, sep = " "), tz = "America/New_York" )
                        end_date<- (end_d_1+months(1))-minutes(30)
                        
                        seq_dates<- seq(from = start_date, to = end_date, by = "30 mins")
                        
                        
                        My_Station_Range<- data.frame(date = seq_dates, row.names = NULL)
                        
                        
                        My_Station_Trend<- My_Station_Range %>%
                                left_join(My_Station, by = c("date")) %>%
                                mutate(station_id = max(station_id,na.rm = T),
                                       trips_started = ifelse(is.na(trips_started),0,trips_started),
                                       trips_stopped = ifelse(is.na(trips_stopped),0,trips_stopped),
                                       net_trips = trips_stopped - trips_started,
                                       day = wday(date, label = T, abbr = T),
                                       hour = str_pad(hour(date), width = 2, side = "left", pad = 0),
                                       min = str_pad(minute(date), width = 2, side = "left", pad = 0),
                                       date_time = paste(day,paste(hour,min, sep = ":"), sep = "-")) %>%
                                group_by(date_time) %>%
                                summarise(mean_net_trips = round(mean(net_trips),digits = 1),
                                          upper_net_trips = round(mean(net_trips) + sd(net_trips),digits = 1),
                                          lower_net_trips = round(mean(net_trips) - sd(net_trips),digits = 1)) %>%
                                separate(col = "date_time",into = c("Day","Time"), sep = "-") 
                        
                        Trend<- ggplot(data = My_Station_Trend[My_Station_Trend[,1]== as.character(wday(input$date, label = T, abbr = T)),],
                                       aes(x = Time, y = mean_net_trips,
                                           ymin = lower_net_trips,
                                           ymax = upper_net_trips))
                        Trend<- Trend + geom_pointrange()
                        Trend<- Trend + theme(axis.text.x = element_text(angle = 90, hjust = 1))
                        Trend<- Trend + geom_hline(aes(yintercept = 0), size = 1, colour = "red", alpha = 0.5)
                        
                        Trend
                        
                        })
                
                
                
                output$trend<- renderPlot(update_station_id())
        }
)
