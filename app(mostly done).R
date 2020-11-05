library(shiny)
library(ggplot2)
library(jsonlite)
library(ggmap)
library(googleway)
library(leaflet)
library(sf)
library(shinycssloaders)
library(shinythemes)
library(tidyr)
library(dplyr)
library(DT)
ggmap::register_google(key = 'AIzaSyBtkpz6CUH-lwaRTLrfnBbGPpaj4pst6Z8')

key <- "AIzaSyBtkpz6CUH-lwaRTLrfnBbGPpaj4pst6Z8"
set_key(key = key)

# Outstanding tasks: 1. Add the views based on heatmap polygons 2. Decide what to show on popups

# Sample code for popups: popup = paste("Region", area$Region, "<br>",
#"Employed:", area$Employed, "<br>",
#"Retired:", area$Retired, "<br>",
#"Unemployed:", area$Unemployed))

bto <- read.csv("BTOPredict.csv")
bto[6,1] <- 6
bto[,5] <- NA
bto$X <- paste0("B",row_number(bto$X))
bto <- bto %>% rename(ID=X,`Month of Launch`=`Month.of.Launch`,`Town/Estate`=`Town.Estate`,
                      `Estimated Flats`=`Estimated.Flats`,`2-Room Flexi`=`X2.room.Flexi`,`3-Room`=`X3.room`,
                      `4-Room`=`X4.room`,`5-Room`=`X5.room`) 
resale <- read.csv("Resale_coords.csv") 
resale$X.1 <- paste0("R",resale$X.1)
resale <- resale %>% rename(ID=X.1) %>% subset(select = -c(X))
mop <- read.csv("MOP.csv")
mop$X <- paste0("M",mop$X)
mop <- mop %>% rename(ID=X,`Town/Name`=`Town.Name`,`Project Name`=`BTO.Project.Name`,
                      `Launch Date`=`Launch.Date`,`Year Completed`=`Year.of.Completion`,
                      `Studio Units`=`No.of.Studio.units`,`2-Room Units`=`No.of.2.room.units`,
                      `3-Room Units`=`No.of.3.room.units`,`4-Room Units`=`No.of.4.room.units`,
                      `5-Room Units`=`No.of.5.room.units`, `3Gen Units`=`No.of.3.gen.units`,`Total Units`=`Total.no.of.units`, 
                      `End of MOP`=`End_of_mop`) %>% subset(select=-c(idMOP))

mop <- mop[,c(1,2,3,4,14,5,6,7,8,9,10,11,12,13,15,16)] %>% select(-Type,Type)


# Primary schools
schools <- read.csv("Primary Schools.csv")

# Parks
parks <- file.path(getwd(), 'parks-kml.kml')   
parks <- read_sf(parks, layer = 'NATIONALPARKS')
parks$Description <- gsub(".*<th>NAME</th> <td>","", parks$Description)
parks$Description <- gsub("<.*", '', parks$Description)

# CCs
ccs <- file.path(getwd(), 'community-clubs-kml.kml')   
st_layers(ccs)
ccs <- read_sf(ccs, layer="COMMUNITYCLUBS")

ccs$Description <- gsub(".*<th>NAME</th> <td>","", ccs$Description)
ccs$Description <- gsub("<.*", '', ccs$Description)

# MRT Stations
mrt <- read.csv("mrt_df.csv")
mrt$final <- gsub(" Singapore","",mrt$final)
mrt$final <- ifelse(grepl("N/A",mrt$MRT.Alpha.numeric.code.s.....In.operation,fixed = T),
                    paste0(mrt$final," (U/C)"),mrt$final)

button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"

# first part: user chooses Resale, MOP and BTO

#3 room 4 room and 5 room

# still need to do the "setview" by area

# facilities: school, MRT and CC
ui <- fluidPage(
  navbarPage("Housing in Singapore", theme = shinytheme("lumen"),
             tabPanel("Tab 1", fluid = TRUE, tags$style(button_color_css),
                      sidebarLayout(
                        sidebarPanel(
                          
                          titlePanel("Choose Characteristics"),
                          fluidRow(column(12,
                                          
                      # Select which area to view
                                          radioButtons(inputId = "AreaView",
                                                       label = "Select Area to View",
                                                       choices = c("None Selected","A", "B")),
                  
                      
                      # Select which Housing Type(s) to plot
                                          checkboxGroupInput(inputId = "HousingType",
                                                             label = "Select Housing Type(s):",
                                                             choices = c("BTO" = "BTO", 
                                                                         "Resale" = "Resale",
                                                                         "BTO reaching MOP soon" = "MOP"),
                                                             selected = "BTO"),
                      # Select which Type of Room(s) to plot   
                                          checkboxGroupInput(inputId = "RoomType",
                                                             label = "Select Type of Room(s) for Resale:",
                                                             choices = c("2-Room", "3-Room", "4-Room",
                                                                         "5-Room"),
                                                             selected = "2-Room")
                          )),
                      
                      fluidRow(column(12,
                                      helpText("Fill in parents' address (Multiple addresses can be filled, showing
                                               2km each point)"))
                      ),
                          textInput(inputId = "parents_address", label = "Parents' Address")
                          ,textOutput(outputId = "parents_full_address")
                          ,HTML(paste0(" <script> 
                          function initAutocomplete() 
                          {
                            var autocomplete = new google.maps.places.Autocomplete(document.getElementById('parents_address'),{types: ['geocode']});
                             autocomplete.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
                             autocomplete.addListener('place_changed', function() 
                             {
                              var place = autocomplete.getPlace();
                              if (!place.geometry) 
                              {
                                return;
                              }

                              var addressPretty = place.formatted_address;
                              var address = '';
                              if (place.address_components) 
                                {
                                  address = [
                                  (place.address_components[0] && place.address_components[0].short_name || ''),
                                  (place.address_components[1] && place.address_components[1].short_name || ''),
                                  (place.address_components[2] && place.address_components[2].short_name || ''),
                                  (place.address_components[3] && place.address_components[3].short_name || ''),
                                  (place.address_components[4] && place.address_components[4].short_name || ''),
                                  (place.address_components[5] && place.address_components[5].short_name || ''),
                                   (place.address_components[6] && place.address_components[6].short_name || ''),
                                   (place.address_components[7] && place.address_components[7].short_name || '')
                                   ].join(' ');
                                 }
                                  var address_number =''
                                  address_number = [(place.address_components[0] && place.address_components[0].short_name || '')]
                                  var coords = place.geometry.location;
                                  //console.log(address);
                                  Shiny.onInputChange('jsValue', address);
                                  Shiny.onInputChange('jsValueAddressNumber', address_number);
                                  Shiny.onInputChange('jsValuePretty', addressPretty);
                                  Shiny.onInputChange('jsValueCoords', coords);
                              });
                          }
                 </script> 
                 <script src='https://maps.googleapis.com/maps/api/js?key=", key,
                                   "&libraries=places&callback=initAutocomplete' async defer></script>")
                                ),
                      
                      conditionalPanel(
                        condition = "output.parents_full_address",
                        actionButton(inputId = "FinderClear", label = "Clear Address(es)"))
                      
                      ),
                      mainPanel(
                        withSpinner(leafletOutput(outputId = "map")),
                        hr(),
                        fluidRow(column(7,
                                        helpText("Click on housing locations to 
                                                 populate table below with information on houses 
                                                 in that block")
                                        ), 
                                column(7,  
                                        helpText("Note that MOP houses will
                                                 only be on the resale market after certain number 
                                                 of years, with about 60% estimated to be available"))
                        ),
                        br(),
                        fluidRow(
                          withSpinner(dataTableOutput(outputId = "PropertyFinder")),
                        )
                      )
                    ),
             
             )
  ))


server <- function(input, output) 
{
  # Base map with layers
  output$map <- renderLeaflet(
  {
    leaflet() %>% 
      addTiles() %>%  
      setView(lng = 103.803214, lat = 1.368063, zoom = 11) %>% 
      addMarkers(data=schools, ~lon, ~lat,popup= ~Schools, label = ~Schools,
                 icon=makeIcon("School.png",iconWidth = 12, iconHeight =12), group="Primary Schools") %>% 
      addMarkers(data=parks, popup = ~Description, label = ~Description, 
                 icon=makeIcon("Tree.png",iconWidth = 12, iconHeight =12), group="Parks") %>%
      addMarkers(data=ccs, popup = ~Description, label = ~Description, 
                 icon=makeIcon("CCs.png",iconWidth = 12, iconHeight =12), group="Community Centres") %>%
      addMarkers(data=mrt, popup = ~final, label = ~final, 
                 icon=makeIcon("Train.png",iconWidth = 12, iconHeight =12), group="MRTs") %>%
      addLayersControl(overlayGroups=c("Primary Schools","Parks","Community Centres","MRTs")) 
  })
  
  # Set view to area
  observeEvent(input$AreaView,{
      leafletProxy("map") #%>%
        #setView()
  })
  
  
  # For predictive address
  parents_address <- reactive({
    if(!is.null(input$jsValueAddressNumber)){
      if(length(grep(pattern = input$jsValueAddressNumber, x = input$jsValuePretty ))==0){
        final_address<- c(input$jsValueAddressNumber, input$jsValuePretty)
      } else{
        final_address<- input$jsValuePretty
      }
      final_address
    }
  })
  
  
  output$parents_full_address <- renderText({
    if(!is.null(parents_address())){
      parents_address()
    }
  })
  
  # Parents' address plot
  observe({
    parents_address <- parents_address()
    if(length(parents_address>0))
    {
      address <- google_geocode(address = parents_address)
      coords <- geocode_coordinates(address)  
      leafletProxy("map") %>% 
        addCircles(data=coords,~lng,~lat,radius=2000,fillOpacity=0.1, layerId="x") %>% 
        addMarkers(data=coords,~lng,~lat,popup=parents_address, label="Your Parents' House",
                   icon=makeIcon("Parents' House.png",iconWidth=12, iconHeight=12),layerId="y")
    }
    else
    {
      leafletProxy("map")  
    }
  })
  
  # Parents' address clearer
  observeEvent(input$FinderClear,{
    if(input$FinderClear)
    {
      leafletProxy("map") %>%
        removeShape("x") %>%
        removeMarker("y")
    }  
  })
  
  # BTO reactive function
  BTO <- reactive({
    req(input$HousingType)
    filter(bto, Type %in% input$HousingType) 
  })
  
  observe({
    if(nrow(BTO())==0)
    {
      leafletProxy("map") %>%
        clearGroup("BTO")
    }
    else
    {
      leafletProxy("map") %>%
        clearGroup("BTO") %>%
        addMarkers(data=BTO(),~lon,~lat,popup = ~`Town/Estate`, group="BTO", layerId=~`Town/Estate`)  
    }
  })
  
  # Resale reactive function
  Resale <- reactive({
    req(input$HousingType)
    req(input$RoomType)
    filter(resale, Room %in% input$RoomType) %>%
      filter(Type %in% input$HousingType)
  })
  
  
  observe({
    if(nrow(Resale())==0)
    {
      leafletProxy("map") %>%
        clearGroup("Resale")
    }
    else
    {
      leafletProxy("map") %>%
        clearGroup("Resale") %>% 
        addMarkers(data=Resale(),~lon,~lat,group="Resale", layerId=~Address)
    }
  })
  
  
  # MOP reactive function
  MOP <- reactive({
    req(input$HousingType)
    filter(mop, Type %in% input$HousingType) 
  })
  
  
  observe({
    if(nrow(MOP())==0)
    {
      leafletProxy("map") %>%
        clearGroup("MOP")
    }
    else
    {
      leafletProxy("map") %>%
        clearGroup("MOP") %>%
        addMarkers(data=MOP(),~lon,~lat,group="MOP", layerId=~`Project Name`)
    }
  })
  
  # Datatables for the 3 types of properties 
  observe({
    data <- input$map_marker_click
    if (is.null(data) || is.null(data$id))
    {
      return()
    }
    else
    {
      output$PropertyFinder <- renderDataTable({
        if (data$id %in% bto$`Town/Estate`)
        {
          return(
            datatable(filter(BTO()[,1:8], `Town/Estate` == data$id))
          )
        }
        else if (data$id %in% resale$Address)
        {
          return(
            datatable(filter(Resale()[,1:7], Address == data$id))
          )
        }
        else if (data$id %in% mop$`Project Name`)
        {
          return(
            datatable(filter(MOP()[,1:13], `Project Name` == data$id))
          )
        }
      })
      
    }
  })

  
}

shinyApp(ui, server)
