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


ui <- fluidPage(
  radioButtons(inputId = "HousingType",
               label = "Select Housing Type:",
               choices = c("BTO" = "BTO", 
                           "Resale" = "Resale",
                           "BTO reaching MOP soon" = "MOP"),
               selected = "BTO"),
  conditionalPanel(
    condition = "input.HousingType == 'Resale'",
    radioButtons(inputId = "RoomType",
                 label = "Select Type of Room for Resale:",
                 choices = c("2-Room", "3-Room", "4-Room",
                             "5-Room"),
                 selected = "2-Room")),
  actionButton("goButton", "Go!"),
  
  mainPanel(
    withSpinner(leafletOutput(outputId = "map"))
  ))

server <- function(input, output) {
  html_legend <- "<img src='https://cdn.pixabay.com/photo/2018/02/18/20/34/locomotive-3163448_1280.png'style='width:10px;height:10px;'>MRT<br/>

<img src='https://cdn.pixabay.com/photo/2014/12/22/00/07/tree-576847_1280.png'style='width:10px;height:10px;'>Park<br/>

<img src='https://cdn.pixabay.com/photo/2017/01/31/00/09/book-2022464_1280.png'style='width:10px;height:10px;'>School<br/>

<img src='https://cdn.pixabay.com/photo/2016/08/31/11/54/user-1633249_1280.png'style='width:10px;height:10px;'>Community Centre<br/>

<img src='https://cdn.pixabay.com/photo/2020/06/22/10/55/house-5328786_1280.png'style='width:10px;height:10px;'>BTO<br/>

<img src='https://cdn.pixabay.com/photo/2020/07/19/18/23/real-estate-5420920_1280.png'style='width:10px;height:10px;'>Resale<br/>

<img src='https://cdn.pixabay.com/photo/2013/07/12/12/56/home-146585_1280.png 'style='width:10px;height:10px;'>MOP Soon<br/>"
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
        addLayersControl(overlayGroups=c("Primary Schools","Parks","Community Centres","MRTs")) %>%
        addControl(html=html_legend,position = "bottomright")
    })
  # MOP by types too?
  observeEvent(input$goButton, {
    type <- input$HousingType
    bto <- filter(bto, Type %in% type) 
    resale <- filter(resale, Type %in% type) %>% filter(Room %in% input$RoomType)
    mop <- filter(mop, Type %in% type) 
    
    leafletProxy("map") %>%
      clearGroup("BTO") %>%
      clearGroup("Resale") %>%
      clearGroup("MOP") %>%
      addMarkers(data=bto,~lon,~lat,popup = ~`Town/Estate`, group="BTO", 
                 icon=makeIcon("BTO.png",iconWidth=30, iconHeight=30),layerId=~`Town/Estate`) %>%
      addMarkers(data=resale,~lon,~lat,group="Resale", 
                 icon=makeIcon("Resale.png",iconWidth=30, iconHeight=30),layerId=~Address) %>%
      addMarkers(data=mop,~lon,~lat,group="MOP", 
                 icon=makeIcon("MOP.png",iconWidth=30, iconHeight=30),layerId=~`Project Name`)
      
      
      
  })
  
  
  
}

shinyApp(ui, server)

  
  
  
