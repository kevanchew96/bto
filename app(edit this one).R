library(shiny)
library(shinythemes)
library(leaflet)
library(rgdal)
library(raster)
library(XML)
library(tidyverse)
library(ggplot2)
library(plotly)
library(scales)
library(ggmap)
library(jsonlite)
library(googleway)
library(leaflet)
library(sf)
library(shinycssloaders)
library(tidyr)
library(dplyr)
library(DT)
library(geosphere)
library(htmlwidgets)
ggmap::register_google(key = 'AIzaSyBtkpz6CUH-lwaRTLrfnBbGPpaj4pst6Z8')

key <- "AIzaSyBtkpz6CUH-lwaRTLrfnBbGPpaj4pst6Z8"
set_key(key = key)
final_out <- read.csv("Resale_Region.csv")
resale_avail <- read.csv("Resale_Avail.csv")


##############################################################################################################################################################################
#DATA SOURCE & CLEANING ################################################################################################################################################################


bto <- read.csv("BTO_new.csv")
bto <- bto %>% rename(`Month of Launch`=`Month.of.Launch`,`Town/Estate`=`Town.Estate`,
                      `Estimated Flats`=`Estimated.Flats`,`Flat Type`=flat_type,
                      `Price (Estimated)`=price) %>% 
  select(ID, `Month of Launch`, `Town/Estate`, `Flat Type`, `Estimated Flats`, 
         `Price (Estimated)`, lon, lat, Type)

resale <- read.csv("Resale_coords.csv") 
resale <- distinct(resale %>% subset(select = -c(`X.1`,X)))
resale$ID<- paste0("R",seq(1:nrow(resale)))
resale$Price <- (gsub("[\\$,]", "", resale$Price))
resale <- resale %>% select(ID,everything())

mop <- read.csv("MOP_new.csv")
mop <- mop %>% rename(`Town/Name`=`Town.Name`,`Project Name`=`Project.Name`,
                      `Launch Date`=`Launch.Date`,`Year Completed/ Year to be Complete`=`Year.Completed`,
                      `No. of Units`=`Total.Units`, `Flat Type`=flat_type,
                      `End of MOP`=`End.of.MOP`, `Price (Predicted)`=price1) %>% 
  select(ID, `Town/Name`, `Project Name`, `End of MOP`, `Flat Type`, 
         `Year Completed/ Year to be Complete`, `Price (Predicted)`, `No. of Units`,lon, lat, Type)
mop$`Flat Type` <- gsub("EXECUTIVE", "Executive", mop$`Flat Type`)
mop <- mop[!is.na(mop$`Price (Predicted)`),]

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

######################################################################################################################################
### FUNCTIONS ########################################################################################################################


########### GRANT CALCULATION FUNCTIONS ##################################################################


#resale_grant: family grant
family_grant <- function(income, flat_type, is_married, citizenship, application){
  if(income <=14000 & citizenship == "SC,SC" & is_married == T & application == "FT,FT"){
    f_grant <- ifelse((flat_type == "2 ROOM" | flat_type == "3 ROOM"| flat_type == "4 ROOM"), yes = 50000, no = 40000)
  } else if (income <=14000 & citizenship == "SC,SPR" & is_married == T & application == "FT,FT") {
    f_grant <- ifelse((flat_type == "2 ROOM" | flat_type == "3 ROOM"| flat_type == "4 ROOM"), yes = 40000, no = 30000)
  } else{
    f_grant <- 0
  }
  return(f_grant)
}


#resale_grant : singles grant
singles_grant <- function(income, flat_type, is_married, citizenship, application){
  if(income <= 7000 & is_married == F & application == "FT" & citizenship == "SC" | is_married == T & income <=14000 & citizenship == "SC,F" & application == "FT,FT"){
    s_grant <- ifelse((flat_type == "2 ROOM" | flat_type == "3 ROOM"| flat_type == "4 ROOM"), yes = 25000, no = 20000)
  } else {
    s_grant <- 0
  }
  return(s_grant)
}


#resale_grant : half-housing grant

half_housing_grant <- function(income, flat_type, is_married, citizenship, application){
  
  if(income <= 14000 & is_married == T & citizenship != "SC,F" & application == "FT,ST") {
    hh_grant <- ifelse((flat_type == "2 ROOM" | flat_type == "3 ROOM"| flat_type == "4 ROOM"), yes = 25000, no = 20000)
    
  } else{
    hh_grant <- 0
  }
  
}



#resale_grant: Enhanced CPF Housing Grant (EHG)
enhanced_housing_grant <- function(income, is_married, citizenship, application){
  eh_grant <- 0 
  
  if(is_married == T & citizenship == "SC,SC" & application == "FT,FT"){
    if(income <= 1500 ){
      eh_grant <- 80000
    } else if(income <= 2000){
      eh_grant <- 75000
    } else if(income <= 2500){
      eh_grant <- 70000
    } else if(income <= 3000){
      eh_grant <- 65000
    } else if(income <= 3500){
      eh_grant <- 60000
    } else if(income <= 4000){
      eh_grant <- 55000 
    } else if(income <= 4500){
      eh_grant <- 50000
    } else if(income <= 5000){
      eh_grant <- 45000
    } else if(income <= 5500){
      eh_grant <- 40000
    } else if(income <= 6000){
      eh_grant <- 35000  
    } else if(income <= 6500){
      eh_grant <- 30000
    } else if(income <= 7000){
      eh_grant <- 25000
    } else if(income <= 7500){
      eh_grant <- 20000
    } else if(income <= 8000){
      eh_grant <- 15000  
    } else if(income <= 8500){
      eh_grant <- 10000
    } else if(income <= 9000){
      eh_grant <- 5000   
    } else {
      eh_grant <- 0
    }
  }else{
    eh_grant <- 0
  }
  return(eh_grant)
}





#resale_grant: Singles Enhanced CPF Housing Grant (EHG-SINGLES)
half_enhanced_housing_grant <- function(income, is_married, citizenship, application){
  half_eh_grant <- 0
  
  if((is_married == F  & citizenship == "SC") | (is_married == T & citizenship != "SC,SC") | (is_married == T & citizenship == "SC,SC" & application == "FT,ST" )){
    if(income <= 750 ){
      half_eh_grant <- 40000
    } else if(income <= 1000){
      half_eh_grant <- 37500
    } else if(income <= 1250){
      half_eh_grant <- 35000
    } else if(income <= 1500){
      half_eh_grant <- 32500
    } else if(income <= 1750){
      half_eh_grant <- 30000
    } else if(income <= 2000){
      half_eh_grant <- 27500 
    } else if(income <= 2250){
      half_eh_grant <- 25000
    } else if(income <= 2500){
      half_eh_grant <- 22500
    } else if(income <= 2750){
      half_eh_grant <- 20000
    } else if(income <= 3000){
      half_eh_grant <- 17500  
    } else if(income <= 3250){
      half_eh_grant <- 15000
    } else if(income <= 3500){
      half_eh_grant <- 10000
    } else if(income <= 3750){
      half_eh_grant <- 10000
    } else if(income <= 4000){
      half_eh_grant <- 7500  
    } else if(income <= 4250){
      half_eh_grant <- 5000
    } else if(income <= 4500){
      half_eh_grant <- 2500   
    } else {
      half_eh_grant <- 0
    }
  }else{
    half_eh_grant <- 0
  }
  return(half_eh_grant)
}  




#resale_grant: Proximity Housing Grant (PHG)

proximity_grant <- function(distance_from_p, with_parents, is_married, citizenship){
  
  if(with_parents == T){
    
    if (is_married == T & citizenship != "SC,F") {
      p_grant <- 30000
    } else if (is_married == F & citizenship == "SC"){
      p_grant <- 15000
    }else{
      p_grant <- 0
      
    }
    
  } else if (with_parents == F){
    
    if (distance_from_p < 4 & is_married == T & citizenship != "SC,F") {
      p_grant <- 20000
    } else if (distance_from_p < 4 & is_married == F & citizenship == "SC") {
      p_grant <- 10000
    } else{
      p_grant <- 0
    }
  }
  
  else{
    p_grant <- 0
  }
  return(p_grant)
}



#downpayment function

downpayment <- function (base_price, income, is_married, citizenship) {
  downpayment_amt <- 0
  
  if (is_married == T & income <= 14000 ) {
    downpayment_amt <- 0.1 * base_price
   
  }
  else if (is_married == F & income <= 7000){
    downpayment_amt <- 0.1 * base_price
    
  }
  else {
    downpayment_amt <- 0.2 * base_price
    
  }
  return (downpayment_amt)
}




#loantype function

loan_type <- function (base_price, income, is_married, citizenship) {
  loan_type <- "NA"
  
  if (is_married == T & income <= 14000 ) {
    loan_type <- "HDB"
  }
  else if (is_married == F & income <= 7000){
    loan_type <- "HDB"
  }
  else {
    loan_type <- "Bank Loan"
  }
  return (loan_type)
}




#total resale grants function
resale_breakdown<- function(base_price, income, flat_type, distance_from_p, with_parents, is_married, citizenship, application){
  
  f_grant <- family_grant(income, flat_type, is_married, citizenship, application)
  s_grant <- singles_grant(income, flat_type, is_married, citizenship, application)
  hh_grant <- half_housing_grant(income, flat_type, is_married, citizenship, application)
  eh_grant <- enhanced_housing_grant(income, is_married, citizenship, application) 
  p_grant <- proximity_grant(distance_from_p, with_parents, is_married, citizenship)
  single_eh_grant <- half_enhanced_housing_grant(income, is_married, citizenship, application)
  total_grant <- f_grant + s_grant + hh_grant + eh_grant + p_grant + single_eh_grant
  downpayment_amt <- downpayment(base_price, income, is_married, citizenship)
  loan_type <- loan_type(base_price, income, is_married, citizenship)
 
  sum_of_grant <- sum(c(f_grant, s_grant, hh_grant, eh_grant, single_eh_grant, p_grant))
  balance <- base_price - sum_of_grant - downpayment_amt
  
  cost_breakdown <- c(f_grant, s_grant, hh_grant, eh_grant, single_eh_grant, p_grant, downpayment_amt, balance)
  
  df <- data.frame(matrix(ncol=3,nrow=8, dimnames=list(NULL, c("total_price", "type", "amount") )))
  
  types <- c( "Family Grant", "Singles Grant", "Half housing Grant", "Enhanced Housing Grant", "Singles Enhanced Housing Grant", "Proximity Grant" , "Downpayment","Balance")
  
  title <- c(rep(base_price, 8))
  
  df[,1] <- title
  df[,2] <- types
  df[,3] <- cost_breakdown
  
  
  breakdown_plot <- ggplot(df, aes(fill=type, y=amount, x=total_price)) + 
    geom_bar(position="stack", stat="identity") + 
    geom_text(aes(label = stat(y), group = total_price), stat = 'summary', fun=sum) +  #ADD THE TOTAL SUM ABOVE
    ylab("Amount (SG$)") + xlab("") + 
    scale_fill_discrete(name = "Type") + 
    scale_y_continuous(label=comma) +
    theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  
  breakdown <- ggplotly(breakdown_plot)
  
  return(breakdown)
  

}


#Total BTO Breakdown

bto_breakdown<- function(base_price, income, is_married, citizenship, application){
  eh_grant <- 0
  single_eh_grant <- 0
  if(is_married == T & citizenship == "SC,SC" & application == "FT,FT"){
    eh_grant <- enhanced_housing_grant(income, is_married, citizenship, application)
  }
  
  else if (is_married == T & citizenship == "SC,SC" & application == "FT,ST"){
    single_eh_grant <- half_enhanced_housing_grant((income/2), is_married, citizenship, application)
    
  }
  
  else if(is_married == T & citizenship == "SC,SPR"){
    single_eh_grant <- half_enhanced_housing_grant((income/2), is_married, citizenship, application )
  }
  
  else if(is_married == F){
    single_eh_grant <- half_enhanced_housing_grant(income, is_married, citizenship, application)
  } 
  
  
  downpayment_amt <- downpayment(base_price, income, is_married, citizenship)
  loan_type <- loan_type(base_price, income, is_married, citizenship)
  sum_of_grant <- sum(c(eh_grant, single_eh_grant))
  balance <- base_price - sum_of_grant - downpayment_amt
  
  bto_cost_breakdown<- c(eh_grant, single_eh_grant, downpayment_amt, balance)
  
  df <- data.frame(matrix(ncol=3,nrow=4, dimnames=list(NULL, c("total_price", "type", "amount") )))
  
 types <- c("Enhanced Housing Grant", "Singles Enhanced Housing Grant", "Downpayment","Balance")
  
  title <- c(rep(base_price, 4))
  
  df[,1] <- title
  df[,2] <- types
  df[,3] <- bto_cost_breakdown
  
  
  breakdown_plot <- ggplot(df, aes(fill=type, y=amount, x=total_price)) + 
    geom_bar(position="stack", stat="identity") + 
    geom_text(aes(label = stat(y), group = total_price), stat = 'summary', fun=sum) +  #ADD THE TOTAL SUM ABOVE
    ylab("Amount (SG$)") + xlab("") + 
    scale_fill_discrete(name = "Type") + 
    scale_y_continuous(label=comma) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank())
  
  breakdown <- ggplotly(breakdown_plot)
  
  return(breakdown)
  
  

}


#use the relevant grant calculator

cost_calculator <- function (ID, base_price, income, flat_type, distance_from_p, with_parents, is_married, citizenship, application){
  

  if((substring(ID, 1, 1)) == 'B'|(substring(ID, 1, 1)) == 'b'){
    
  
    bto_breakdown (base_price, income, is_married, citizenship, application)
    
  }else {
    resale_breakdown(base_price, income, flat_type, distance_from_p, with_parents, is_married, citizenship, application)
    
  }
  
}



#get your coordinates from ID (lon,lat)

find_lonlat <- function(ID){
  ID_char <- as.character(ID)
  if((substring(ID, 1, 1)) == 'B'|(substring(ID, 1, 1)) == 'b'){
    
    row_index <- as.numeric(substring(ID, 2, 9))
    lon <- bto[row_index, 9]
    lat <- bto[row_index, 10]
  }else if((substring(ID, 1, 1)) == 'R'| (substring(ID, 1, 1)) == 'r'){
    
    row_index <- as.numeric(substring(ID, 2, 9))
    lon <- resale[row_index, 9]
    lat <- resale[row_index, 10]
  }else if((substring(ID, 1, 1)) == 'M'| (substring(ID, 1, 1)) == 'm'){
    
    row_index <- as.numeric(substring(ID, 2, 9))
    lon <- mop[row_index, 14]
    lat <- mop[row_index, 15]
  }
  return(c(lon,lat))
}



#find distance from parents postal code to selected (lon, lat)

measure_distance_from_p <- function(parent_postal_code, your_lon_lat){  #your_lon_lat must be a vector of (lon,lat) as generated from function find_lonlat
  parent_lat <- as.numeric(geocode(paste("Singapore", as.character(parent_postal_code)))[2])
  parent_lon <- as.numeric(geocode(paste("Singapore", as.character(parent_postal_code)))[1])
  your_lon<- as.numeric(your_lon_lat[1])
  your_lat <- as.numeric(your_lon_lat[2])
  dist <- as.numeric(distm(c(parent_lon, parent_lat), c(your_lon, your_lat), fun = distHaversine))/1000
  return(dist)
}




#find the price for the relevant property

find_price <- function(ID){
  ID_char <- as.character(ID)
  if((substring(ID, 1, 1)) == 'R'|(substring(ID, 1, 1)) == 'r'){
    
    row_index <- as.numeric(substring(ID, 2, 9))
    price <- as.numeric(format(resale[row_index, "Price"],scientific = F))
    
  }else if ((substring(ID, 1, 1)) == 'M'| (substring(ID, 1, 1)) == 'm'){
    
    price <- as.numeric(format(400000, scientific = F))
    
  }else{
    price <- as.numeric(format(400000, scientific = F))
    
  }
  return(price)
}


####################  UI FUNCTIONS #####################################################################################


financePlan <- function(){
  fluidPage(
    tagList(
      div(class = "container",style="margin-bottom:50px;",
          h1("Financial Planning", class = "title fit-h1"),
          p("You have already identified two apartments of interest, but cannot decide which one to buy? Let us help make your final decision.
          Enter your personal details to see what grants you can qualify for. 
          Then, enter the addresses and flat types of the apartments you have in mind and we will show you the price breakdown for each apartment."),
      )
    ),
    tags$h4("Enter your details"),
    fluidRow(
      
      column(3, wellPanel(
        selectInput("marital_status", "Marital Status",
                    c("Single","Married")
        )
      )),
      
      column(9, wellPanel(
        # This outputs the dynamic UI component
        uiOutput("ui")
      ))
    ),
    tags$h4("Enter the addresses"),
    wellPanel(
      fluidRow(
        column(width=6,numericInput("parent_address","Your parent's postal code",value="Enter Postal Code")),
        column(width=6,selectInput("with_parents","Are you intending to stay with your parents?",choices =c("Yes","No")))
      ),
      fluidRow(
        column(width=6,
               textInput("home_type_1","First Apartment ID",value="")
        ),
        column(width=6,
               textInput("home_type_2","Second Apartment ID",value="")
        ),
        fluidRow(
          column(width=6,
                 selectInput("flat_type_1","First Flat type",choices=c("2 ROOM","3 ROOM","4 ROOM","5 ROOM","EXECUTIVE","Multi-Generation"))
          ),
          column(width=6,
                 selectInput("flat_type_2","Second Flat type",choices=c("2 ROOM","3 ROOM","4 ROOM","5 ROOM","EXECUTIVE","Multi-Generation")))
        )
      )
    ),
    tags$h4("Your Selected Properties"),
    leafletOutput("leaflet_parents"),
    tags$h4("Financial Breakdown"),
    fluidRow(
  
    column(width = 6,plotlyOutput("price_grant_barchart_1")),
    column(width = 6,plotlyOutput("price_grant_barchart_2"))
  )
  )
  
}

plot_polygon <- function(data,room_type){
  data1 <- data %>% filter(Room==room_type)
  data1_reg<- data1 %>% group_by(Postal_District) %>% summarise(Average_Price=mean(Price))
  
  data_out <- data.frame(Postal_District = seq(1,28,1))
  data_out <- left_join(data_out,data1_reg)
  
  shapeData <- readOGR(dsn=".",layer="districts")
  data_plot <- merge(shapeData,data_out,by.x="PLN_AREA_N",by.y="Postal_District")

  popup <- paste0("<b>","District: ","</b>",data_plot$PLN_AREA_N,"<br/>",
                  "<b>","General Location: ","</b>",data_plot$Gnrl_Lc,"<br/>",
                  "<b>", "Average Price: ", "</b>", round(data_plot$Average_Price,digits=2))
  
  
  pal <- colorNumeric( palette = "PuRd", domain = data_plot$Average_Price)

  m <-leaflet(data = data_plot) %>% addTiles() %>% addPolygons(data=data_plot, weight = 1, stroke = TRUE, color="grey", smoothFactor = 0.5, fillOpacity = 0.8, fillColor = ~pal(data_plot$Average_Price),
                                                               popup = popup, dashArray = "") %>% addLegend("bottomright", pal = pal, values = ~Average_Price,labFormat = labelFormat(prefix = "$")) %>% setView(103.851245,1.3445821
,zoom = 10.5)
  m
}

plot_polygon2 <- function(data,room_type){
  data1 <- data %>% filter(Room==room_type)
  
  shapeData <- readOGR(dsn=".",layer="districts")
  data_plot <- merge(shapeData,data1,by.x="PLN_AREA_N",by.y="Postal_District")
  
  popup <- paste0("<b>","District: ","</b>",data_plot$PLN_AREA_N,"<br/>",
                  "<b>","General Location: ","</b>",data_plot$Gnrl_Lc,"<br/>",
                  "<b>", "Number Of Units Available: ", "</b>", data_plot$Availability)
  
  pal <- colorNumeric( palette = "YlGn", domain = data_plot$Availability)
  
  m <-leaflet(data = data_plot) %>% addTiles() %>% addPolygons(data=data_plot, weight = 1, stroke = TRUE, color="grey", smoothFactor = 0.5, fillOpacity = 0.8, fillColor = ~pal(data_plot$Availability),
                                                               popup = popup, dashArray = "") %>% addLegend("bottomright", pal = pal, values = ~Availability) %>% setView(103.851245,1.3445821
                                                                                                                                                                          ,zoom = 10.5)

  m <- m %>% htmlwidgets::prependContent(html_fix)                   # Insert into leaflet HTML code
  m
}

OverviewPrices <- function(){
  fluidPage(
    tagList(
      div(class = "container",style="margin-bottom:50px;",style="padding:50px;",
          h1("Overview of Prices & Flat Availability", class = "title fit-h1"),
          p("Use the maps below to find out how prices and availability of housing vary across districts. You can navigate through the tabs and use the control panel on the left to view data and flat types of your interest. Click on the specific district on the map for more information."),
      )),
    sidebarPanel(width=3,
                 radioButtons("leaflet_type","Data Shown",c("Average Price","Availability"))
    ),
    mainPanel(
      
      uiOutput("showleaflet"),
      tags$div(style="margin-top:20px;",
        p("Want to explore the housing options and facilities available in each district?")),
      actionButton(inputId = "bttn1",label= "Go to Housing View")
      
    )
  )
}


button_color_css <- "
#DivCompClear, #FinderClear, #EnterTimes{
/* Change the background color of the update button
to blue. */
background: DodgerBlue;
/* Change the text size to 15 pixels. */
font-size: 15px;
}"


###################################################################################################################################################################
############   UI STARTS HERE   ###################################################################################################################################




#To format legend for polygon leaflets 
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))

ui <- fluidPage(HTML(html_fix),theme = "style/style.css",
                navbarPage(title = "AppName", id="navbar",fluid = TRUE, 
                                                         collapsible = TRUE,
                                                         
                                                         # ----------------------------------
                                                         # tab panel 1 - Home
                                                         tabPanel("Home",
                                                                  includeHTML("home.html"),
                                                                  tags$script(src = "plugins/scripts.js"),
                                                                  tags$head(
                                                                    tags$link(rel = "stylesheet", 
                                                                              type = "text/css", 
                                                                              href = "plugins/font-awesome-4.7.0/css/font-awesome.min.css"),
                                                                    tags$link(rel = "icon", 
                                                                              type = "image/png", 
                                                                              href = "images/logo_icon.png")
                                                                  )),
                                                         # tab panel 2 
                                                         tabPanel("Overview",
                                                                  OverviewPrices() #function to display overview of prices on polygons
                                                                  ),
                                                         
                                                         # ----------------------------------
                                                         # tab panel 3 

                                                        
                                                         tabPanel("Housing View", value = "housingview",fluid = TRUE, tags$style(button_color_css),
                                                                  tagList(
                                                                    div(class = "container",style="margin-bottom:50px;",
                                                                        h1("Housing View", class = "title fit-h1"),
                                                                        p("Using the controls on the left, select your preferred housing district. 
                                                                          You may then proceed to select the housing and flat type of your interest. 
                                                                          Markers will be shown on the map for housing options that match your selections. 
                                                                          Facilities nearby will also be shown. 
                                                                          You can click on the markers for more information on the housing options and facilities."),
                                                                        p("If you are intending to find a flat near your parents' house, enter your parents' address on the left panel.
                                                                          Housing options within 2km radius of your parents' house will be highlighted.")
                                                                    )
                                                                  ),
                                                                  sidebarLayout(
                                                                    sidebarPanel(
                                                                      
                                                                      titlePanel("Choose Characteristics"),
                                                                      fluidRow(column(12,
                                                                                      
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
                                                                                      conditionalPanel(
                                                                                        condition = "input.HousingType == 'MOP'",
                                                                                        radioButtons(inputId = "RoomType2",
                                                                                                     label = "Select Type of Room for MOP:",
                                                                                                     choices = c("2-Room", "3-Room", "4-Room",
                                                                                                                 "5-Room", "Executive"),
                                                                                                     selected = "2-Room")),
                                                                                      actionButton("goButton", "Go!"),
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
                                                                      ),
                                                                      tags$div(style="margin-top:20px;",
                                                                               p("Want to compare costs and grants available for your preferred housing?")),
                                                                      actionButton(inputId = "bttn2",label= "Go to Finance Planning")
                                                                    ))),
                           
                                                         # ----------------------------------
                                                         # tab panel 4 
                                                         tabPanel("Financial Planning",value = "financeplanning",
                                                                  financePlan() #function to compare financial planning of two flats
                                                         )
))




###################################################################################################################################################################
############  SERVER STARTS HERE   ################################################################################################################################


server <- function(input, output,session){
  output$ui <- renderUI({
    if (is.null(input$marital_status))
      return()
    
    # Depending on input$marital_status, we'll generate a different
    # UI component and send it to the client.
    if (input$marital_status == "Single") {
      fluidPage(  
        fluidRow(
          column(width = 6,
                 radioButtons("FTST", "Are you a First-Timer Applicant?",
                              choices=c("Yes","No"))
          ),
          column(width=6,
                 radioButtons("Nationality", "Nationality Type",choices=c("SC", "Others"))
        ),
        fluidRow(
          numericInput("NetIncome","Income",value="Enter your income")
        )))} else {
          fluidPage(
            fluidRow(
              helpText("*Note: FT = First Timer, ST = Second Timer"),
              helpText("*Note: SC = Singapore Citizen, SPR = Singapore Permanent Resident, F = Foreigner"),
              column(width = 6,
                     radioButtons("FTST", "Applicant Type",choices=c("FT,FT","FT,ST","Others"))
              ),
              column(width=6,
                     radioButtons("Nationality", "Nationality Type",choices=c("SC,SC","SC,SPR","SC,F"))
              )
            ),
            fluidRow(
              numericInput("NetIncome","Household Income",value="Enter your income")
            )
          )
        }
  })
  
  output$showleaflet <- renderUI({
    if (input$leaflet_type == "Average Price"){
      fluidPage(
        tabsetPanel(type = "tabs",
                    tabPanel("2-Room", leafletOutput("leaflet1")),
                    tabPanel("3-Room", leafletOutput("leaflet2")),
                    tabPanel("4-Room", leafletOutput("leaflet3")),
                    tabPanel("5-Room", leafletOutput("leaflet4")),
                    tabPanel("EC", leafletOutput("leaflet5")),
                    tabPanel("Multi-Generation", leafletOutput("leaflet6"))
        )
      )
    } else if (input$leaflet_type == "Availability") {
      fluidPage(
        tabsetPanel(type = "tabs",
                    tabPanel("2-Room", leafletOutput("leaflet_avail_1")),
                    tabPanel("3-Room", leafletOutput("leaflet_avail_2")),
                    tabPanel("4-Room", leafletOutput("leaflet_avail_3")),
                    tabPanel("5-Room", leafletOutput("leaflet_avail_4")),
                    tabPanel("EC", leafletOutput("leaflet_avail_5")),
                    tabPanel("Multi-Generation", leafletOutput("leaflet_avail_6"))
        )
      )
    }
  })
  
  
  output$leaflet1 <- renderLeaflet({
    plot_polygon(final_out,"2-Room")
  })
  
  output$leaflet2 <- renderLeaflet({
    plot_polygon(final_out,"3-Room")
  })
  
  output$leaflet3 <- renderLeaflet({
    plot_polygon(final_out,"4-Room")
  })
  
  output$leaflet4 <- renderLeaflet({
    plot_polygon(final_out,"5-Room")
  })
  
  output$leaflet5 <- renderLeaflet({
    plot_polygon(final_out,"HDB Executive")
  })
  
  output$leaflet6 <- renderLeaflet({
    plot_polygon(final_out,"Multi-Gen/Jumbo")
  })
  
  output$leaflet_avail_1 <- renderLeaflet ({
    plot_polygon2(resale_avail,"2-Room")
  })
  
  output$leaflet_avail_2 <- renderLeaflet ({
    plot_polygon2(resale_avail,"3-Room")
  })
  
  output$leaflet_avail_3 <- renderLeaflet ({
    plot_polygon2(resale_avail,"4-Room")
  })
  
  output$leaflet_avail_4 <- renderLeaflet ({
    plot_polygon2(resale_avail,"5-Room")
  })
  
  output$leaflet_avail_5 <- renderLeaflet ({
    plot_polygon2(resale_avail,"HDB Executive")
  })
  
  output$leaflet_avail_6 <- renderLeaflet ({
    plot_polygon2(resale_avail,"Multi-Gen/Jumbo")
  })
  
  observeEvent(input$bttn1, {
    updateTabsetPanel(session, inputId = "navbar", selected = "housingview")
  })
  
  observeEvent(input$bttn2, {
    updateTabsetPanel(session, inputId = "navbar", selected = "financeplanning")
  })

  

  
  #Financial Planning Map
  
  
  output$leaflet_parents <- renderLeaflet(
    
    leaflet() %>%  setView(lat = 1.376875, lng = 103.822169,
                           zoom = 11) %>%
      addTiles() %>%
      
      addMarkers(lat = as.numeric((find_lonlat(input$home_type_1))[2]), lng = as.numeric((find_lonlat(input$home_type_1))[1]) ) %>%
      addMarkers(lat = as.numeric((find_lonlat(input$home_type_2))[2]), lng = as.numeric((find_lonlat(input$home_type_2))[1]) ) %>%
      addMarkers(lat = as.numeric(geocode(paste("Singapore", as.character(input$parent_address)))[2]), lng = as.numeric(geocode(paste("Singapore", as.character(input$parent_address)))[1]), popup = "Your Parents' Home") %>%
      addCircles(lat = as.numeric(geocode(paste("Singapore", as.character(input$parent_address)))[2]),lng = as.numeric(geocode(paste("Singapore", as.character(input$parent_address)))[1]), radius= 2000,fillOpacity=0.1, layerId="x") 
    
  
      )
  
  
  
  
  
  #Grant Breakdown
  
  output$price_grant_barchart_1 <- renderPlotly( {
    
    cost_calculator(input$home_type_1,
                find_price(input$home_type_1),
                  input$NetIncome, 
                 input$flat_type_1, 
                 measure_distance_from_p(input$parent_address, (find_lonlat(input$home_type_1))), 
                 (input$with_parents == "Yes"), 
                 (input$marital_status == "Married") ,
                 input$Nationality, 
                 input$FTST)
    
  }
    )
  

  
  output$price_grant_barchart_2 <- 
    renderPlotly( {
        cost_calculator(input$home_type_2,
                        find_price(input$home_type_2),
                         input$NetIncome, 
                         input$flat_type_2, 
                         measure_distance_from_p(input$parent_address, (find_lonlat(input$home_type_2))), 
                         (input$with_parents == "Yes"), 
                         (input$marital_status == "Married") ,
                         input$Nationality, 
                         input$FTST)
        
      }
      )
      
  

      


  # Base map with layers
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
  
  # Plot houses on leaflet output
  observeEvent(input$goButton, {
    type <- input$HousingType
    bto <- filter(bto, Type %in% type) 
    resale <- filter(resale, Type %in% type) %>% filter(Room %in% input$RoomType)
    mop <- filter(mop, Type %in% type) %>% filter(`Flat Type` %in% input$RoomType2)
    
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
            datatable(filter(bto[,1:6], `Town/Estate` == data$id))
          )
        }
        else if (data$id %in% resale$Address)
        {
          return(
            datatable(filter(resale[,1:7], Address == data$id))
          )
        }
        else if (data$id %in% mop$`Project Name`)
        {
          return(
            datatable(filter(mop[,1:8], `Project Name` == data$id))
          )
        }
      })
      
    }
  })
}



shinyApp(ui=ui, server=server)
