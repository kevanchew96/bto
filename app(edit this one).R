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
library(htmltools)
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
                      `Estimated Price`=price) %>% 
  select(ID, `Month of Launch`, `Town/Estate`, `Flat Type`, `Estimated Flats`, 
         `Estimated Price`, lon, lat, Type)

resale <- read.csv("Resale_coords.csv") 
resale <- distinct(resale %>% subset(select = -c(`X.1`,X)))
resale$ID<- paste0("R",seq(1:nrow(resale)))
resale$Price <- (gsub("[\\$,]", "", resale$Price))
resale <- resale %>% select(ID,everything())
resale$Year <- gsub("Built-", "", resale$Year)
resale <- resale %>% rename(`Flat Type`=Room, `Year Built`=Year)
resale$Area <- gsub("[(Built)]","",resale$Area)
resale$`Flat Type` <- gsub("Multi-Gen/Jumbo","Multi-Generation",resale$`Flat Type`)

mop <- read.csv("MOP_new.csv")
mop <- mop %>% rename(`Town/Name`=`Town.Name`,`Project Name`=`Project.Name`,
                      `Launch Date`=`Launch.Date`,`Year Completed/ Year to be Complete`=`Year.Completed`,
                      `No. of Units`=`Total.Units`, `Flat Type`=flat_type,
                      `End of MOP`=`End.of.MOP`, `Predicted Price`=price1) %>% 
  select(ID, `Town/Name`, `Project Name`, `End of MOP`, `Flat Type`, 
         `Year Completed/ Year to be Complete`, `Predicted Price`, `No. of Units`,lon, lat, Type)
mop$`Flat Type` <- gsub("EXECUTIVE", "Executive", mop$`Flat Type`)


mop <- mop[!is.na(mop$`Predicted Price`),]
mop$`Predicted Price` <- round(mop$`Predicted Price`)

districts <- read.csv("Districts.csv")
districts$Combined <- paste0(districts$Postal.District," - ", districts$General.Area)
districts$Combined <- gsub(", Singapore", "",districts$Combined)


# Primary schools
schools <- read.csv("Primary Schools.csv")
schools <- schools[order(schools$Schools),]

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
    f_grant <- ifelse((flat_type == "2-Room" | flat_type == "3-Room"| flat_type == "4-Room"), yes = 50000, no = 40000)
  } else if (income <=14000 & citizenship == "SC,SPR" & is_married == T & application == "FT,FT") {
    f_grant <- ifelse((flat_type == "2-Room" | flat_type == "3-Room"| flat_type == "4-Room"), yes = 40000, no = 30000)
  } else{
    f_grant <- 0
  }
  return(f_grant)
}


#resale_grant : singles grant
singles_grant <- function(income, flat_type, is_married, citizenship, application){
  if(income <= 7000 & is_married == F & application == "FT" & citizenship == "SC" | is_married == T & income <=14000 & citizenship == "SC,F" & application == "FT,FT"){
    s_grant <- ifelse((flat_type == "2-Room" | flat_type == "3-Room"| flat_type == "4-Room"), yes = 25000, no = 20000)
  } else {
    s_grant <- 0
  }
  return(s_grant)
}


#resale_grant : half-housing grant

half_housing_grant <- function(income, flat_type, is_married, citizenship, application){
  
  if(income <= 14000 & is_married == T & citizenship != "SC,F" & application == "FT,ST") {
    hh_grant <- ifelse((flat_type == "2-Room" | flat_type == "3-Room"| flat_type == "4-Room"), yes = 25000, no = 20000)
    
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
  
  if((is_married == F  & citizenship == "SC" ) | (is_married == T & citizenship != "SC,SC" & application == "FT,FT") | (is_married == T & citizenship == "SC,SC" & application == "FT,ST" )){
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
    geom_bar(position=position_stack(reverse = TRUE), stat="identity", width = 1) + 
    geom_text(aes(label = stat(y), group = total_price), stat = 'summary', fun=sum) +  #ADD THE TOTAL SUM ABOVE
    ylab("Amount (SG$)") + xlab("") + 
    scale_fill_discrete(name = "Type") +
    scale_fill_manual("legend", values = c("Balance" = "#ffada9", "Downpayment" = "#ff483f", "Family Grant" = "#3F704D", "Singles Grant" = "#4F7942", "Half housing Grant" = "#00A86B", "Enhanced Housing Grant" = "#9DC183", "Singles Enhanced Housing Grant" = "#98FB98", "Proximity Grant" = "#D0F0C0")) +
    scale_y_continuous(label=comma,limits= c(0,650000)) +
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
    geom_bar(position=position_stack(reverse = TRUE), stat="identity", width = 1) + 
    geom_text(aes(label = stat(y), group = total_price), stat = 'summary', fun=sum, position=position_dodge(width=0.9)) +  #ADD THE TOTAL SUM ABOVE
    ylab("Amount (SG$)") + xlab("") + 
    scale_fill_discrete(name = "Type") + 
    scale_fill_manual("legend", values = c("Balance" = "#ffada9", "Downpayment" = "#ff483f", "Enhanced Housing Grant" = "#9DC183", "Singles Enhanced Housing Grant" = "#98FB98")) +
    scale_y_continuous(label=comma,limits= c(0,650000)) +
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
    lon <- bto[row_index, "lon"]
    lat <- bto[row_index, "lat"]
  }else if((substring(ID, 1, 1)) == 'R'| (substring(ID, 1, 1)) == 'r'){
    
    row_index <- as.numeric(substring(ID, 2, 9))
    lon <- resale[row_index, "lon"]
    lat <- resale[row_index, "lat"]
  }else if((substring(ID, 1, 1)) == 'M'| (substring(ID, 1, 1)) == 'm'){
    
    row_index <- as.numeric(substring(ID, 2, 9))
    lon <- mop[row_index, "lon"]
    lat <- mop[row_index, "lat"]
  } else{
    
    lon <- NA
    lat <- NA
    
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

    if((substring(ID, 1, 1)) == 'R'|(substring(ID, 1, 1)) == 'r'){
    
    row_index <- as.numeric(substring(ID, 2, 9))
    price <- as.numeric(format(resale[row_index, "Price"],scientific = F))
    
  }else if ((substring(ID, 1, 1)) == 'M'| (substring(ID, 1, 1)) == 'm'){
    row_index <- as.numeric(substring(ID, 2, 9))
    price <- as.numeric(format(mop[row_index, 'Predicted Price'], scientific = F))
    
  }else if ((substring(ID, 1, 1)) == 'B'| (substring(ID, 1, 1)) == 'b'){
    row_index <- as.numeric(substring(ID, 2, 9))
    price <- as.numeric(format(bto[row_index, "Estimated Price"], scientific = F))
    
  }
  
  else{
    price <- 0
    
  }
  return(price)
}




#Find room-type for relevant property

find_room <- function(ID){
  
  if((substring(ID, 1, 1)) == 'R'|(substring(ID, 1, 1)) == 'r'){
    
    row_index <- as.numeric(substring(ID, 2, 9))
    room_type <- resale[row_index, "Flat Type"]
    
  }else if ((substring(ID, 1, 1)) == 'M'| (substring(ID, 1, 1)) == 'm'){
    row_index <- as.numeric(substring(ID, 2, 9))
    room_type <- mop[row_index, 'Flat Type']
    
  }else if ((substring(ID, 1, 1)) == 'B'| (substring(ID, 1, 1)) == 'b'){
    row_index <- as.numeric(substring(ID, 2, 9))
    room_type <- bto[row_index, "Flat Type"]
    
  }
  
  else{
    room_type <- NA
    
  }
  return(room_type)
}


#Find address for relevant property

find_address <- function(ID){
  
  if((substring(ID, 1, 1)) == 'R'|(substring(ID, 1, 1)) == 'r'){
    
    row_index <- as.numeric(substring(ID, 2, 9))
    address <- resale[row_index, "Address"]
    
  }else if ((substring(ID, 1, 1)) == 'M'| (substring(ID, 1, 1)) == 'm'){
    row_index <- as.numeric(substring(ID, 2, 9))
    address <- mop[row_index, 'Project Name']
    
  }else if ((substring(ID, 1, 1)) == 'B'| (substring(ID, 1, 1)) == 'b'){
    row_index <- as.numeric(substring(ID, 2, 9))
    address <- paste(bto[row_index, "Town/Estate"], "BTO", sep = " ")
    
  }
  
  else{
   address <- NA
    
  }
  return(address)
}


#Match correct icon for relevant property

find_icon <- function(ID){
  
  if((substring(ID, 1, 1)) == 'R'|(substring(ID, 1, 1)) == 'r'){
    
   icon_prop <- "Resale.png"
    
  }else if ((substring(ID, 1, 1)) == 'M'| (substring(ID, 1, 1)) == 'm'){
   icon_prop <- "MOP.png"
    
  }else if ((substring(ID, 1, 1)) == 'B'| (substring(ID, 1, 1)) == 'b'){
    icon_prop <- "BTO.png"
    
  }
  
  else{
    icon_prop <- NA
    
  }
  return(icon_prop)
}


####################  UI FUNCTIONS #####################################################################################


financePlan <- function(){
  fluidPage(
    tagList(
      div(class = "container",style="margin-bottom:50px;",
          h1("Financial Planning", class = "title fit-h1"),
          p("You have already identified some apartments of interest, but cannot decide which one to buy? Let us help make your final decision.
          Enter your personal details to see what grants you can qualify for. 
          Then, enter the Apartment IDs that you have noted down from the Housing View Page."),
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
        tags$div(style="margin-left:10px;",helpText("Note: You can get the Apartment IDs from the table in the Housing View Page.")),
        column(width=6,
               selectizeInput(inputId = "home_type_1",
                              label = "First Apartment ID",
                              choices = c(bto$ID,resale$ID,mop$ID),
                              selected = bto$ID[1])
        ),
        column(width=6,
               selectizeInput(inputId = "home_type_2",
                              label = "Second Apartment ID",
                              choices = c(bto$ID,resale$ID,mop$ID),
                              selected = bto$ID[1])
        ),
        fluidRow(
          tags$div(style="margin-left:15px; display: inline-block;",
          column(3,actionButton(inputId = "mapgen",label= "See Selected Properties")
        )),
          tags$div(style="display: inline-block;", 
                   column(4,actionButton(inputId = "mapreset",label= "Reset"))
          )
        )
      )
    ),
    tags$h4("Your Selected Properties"),
    br(),
    leafletOutput("leaflet_parents"),
    tags$h4("Financial Breakdown"),
    br(),
    fluidRow(
  
    column(width = 6,
           textOutput(outputId = "property_name_1"),
           plotlyOutput("price_grant_barchart_1")),
    column(width = 6,
           textOutput(outputId = "property_name_2"),
           plotlyOutput("price_grant_barchart_2"))
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
      div(class = "container",style="margin-bottom:30px;",style="padding:50px;",
          h1("Overview of Prices & Flat Availability", class = "title fit-h1"),
          p("Use the maps below to find out how prices and availability of resale housing vary across districts. You can navigate through the tabs and use the control panel on the left to view data and flat types of your interest. Click on the specific district on the map for more information."),
      )),
    tags$div(style="margin-left:55px;",
             sidebarPanel(width=3,
                 radioButtons("leaflet_type","Data Shown",c("Average Price","Availability"))
    )),
    mainPanel(
      
      uiOutput("showleaflet"),
      tags$div(style="margin-top:20px; margin-left:15px;",
        p("Want to explore the housing options and facilities available in each district?")),
      tags$div(style="margin-bottom:10px; margin-left:15px;",
               actionButton(inputId = "bttn1",label= "Go to Housing View"))

      
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


#To format legend for polygon leaflets 
css_fix <- "div.info.legend.leaflet-control br {clear: both;}"
html_fix <- as.character(htmltools::tags$style(type = "text/css", css_fix))


###################################################################################################################################################################
############   UI STARTS HERE   ###################################################################################################################################



ui <- fluidPage(HTML(html_fix),theme = "style/style.css",tags$head(
  tags$style(type = 'text/css',HTML("body{background-color: #f9f4ee;} 
                  .navbar { background-color: #392613;}
                  .color-me{color: #fefdfd;}
                  .navbar-default .navbar-nav > .active > a, 
                  .navbar-default .navbar-nav > .active > a:focus, 
                  .navbar-default .navbar-nav > .active > a:hover {color: #555;background-color: #654321;}
                  .img-id {float: left;}
                  .navbar-brand {padding-top: 0px;}"))),
                 
  
                navbarPage(
                           title = div(
                             div(
                               class = "img-id",
                               img(height = 50,
                                   width = 40,src = "images/logo.png")
                             )),

                           id="navbar",fluid = TRUE, 
                                                         collapsible = TRUE,
                                                        
                                                         
                                                         # ----------------------------------
                                                         # tab panel 1 - Home
                                                         tabPanel(tags$div(class="color-me","Home"),
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
                                                         tabPanel(tags$div(class="color-me","Overview"),
                                                                  OverviewPrices() #function to display overview of prices on polygons
                                                                  ),
                                                         
                                                         # ----------------------------------
                                                         # tab panel 3 

                                                        
                                                         tabPanel(tags$div(class="color-me","Housing View"), value = "housingview",fluid = TRUE, tags$style(button_color_css),
                                                                  tagList(
                                                                    div(class = "container",style="margin-bottom:50px;",
                                                                        h1("Housing View", class = "title fit-h1"),
                                                                        p(HTML(paste0("Find your desired HDB in 5 quick steps:", "<br/>",
                                                                          "1. Select your desired housing district", "<br/>",
                                                                          "2. Select the type of housing and the relevant flat type, then click 'Go!'", "<br/>",
                                                                          "3. Mouse over the icons on the map to see more details", "<br/>",
                                                                          "4. (Optional) Enter your parents' address/desired primary school for your children to view an area around them", "<br/>",
                                                                          "5. Take note of the IDs of any properties that catch your eye"
                                                                          ))),
                                                                        
                                                                    )
                                                                  ),
                                                                  sidebarLayout(
                                                                    sidebarPanel(
                                                                      
                                                                      titlePanel("Housing Type"),
                                                                      fluidRow(column(12,
                                                                                      selectizeInput(inputId = "AreaView",
                                                                                                  label = "Select District to View:",
                                                                                                  choices = districts$Combined,
                                                                                                  selected = districts$Combined[1]),
                                                                                      actionButton(inputId = "goButton", label = "View"),
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
                                                                                                     choices = c("1-Room", "2-Room", "3-Room", "4-Room",
                                                                                                                 "5-Room", "HDB Executive", "Multi-Generation"), 
                                                                                                     selected = "2-Room")),
                                                                                      conditionalPanel(
                                                                                        condition = "input.HousingType == 'MOP'",
                                                                                        radioButtons(inputId = "RoomType2",
                                                                                                     label = "Select Type of Room for MOP:",
                                                                                                     choices = c("2-Room", "3-Room", "4-Room",
                                                                                                                 "5-Room", "Executive"),
                                                                                                     selected = "2-Room")),
                                                                                      actionButton(inputId = "goButton2", label = "Go!"),
                                                                      )),
                                                                      
                                                                      fluidRow(column(12,
                                                                                      helpText("Fill in parents' address")),
                                                                               
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
                                                                      actionButton(inputId = "goButton3", label = "Show"),
                                                                      
                                                                      conditionalPanel(
                                                                        condition = "output.parents_full_address",
                                                                        actionButton(inputId = "FinderClear", label = "Clear Address")),
                                                                    
                                                                    fluidRow(column(12,
                                                                                    helpText("Fill in school you would like to view"))),
                                                                    
                                                                    selectizeInput(inputId = "SelectSchool",
                                                                                label = "Select school:",
                                                                                choices = schools$Schools,
                                                                                selected = schools$Schools[1]),
                                                                    fluidRow(column(12,
                                                                                    helpText("Which distance (1 or 2km) would you like to view?"))
                                                                    ),
                                                                    
                                                                    selectInput(inputId = "SchoolDist",
                                                                                label = "Select distance (km):",
                                                                                choices = c(1, 2),
                                                                                selected = 1),
                                                                             
                                                                    actionButton(inputId = "goButton4", label = "View")),
                                                                    
                                                                    mainPanel(
                                                                      withSpinner(leafletOutput(outputId = "map")),
                                                                      hr(),
                                                                      fluidRow(column(12,
                                                                                      helpText("Click on housing locations to 
                                                 see detailed information about them below")
                                                                      ), 
                                                                      column(12,
                                                                             helpText("Tip: You may toggle on/off non-housing markers using the interface at the top right")),
                                                                      column(12,  
                                                                             helpText("Note that MOP houses will
                                                 only be on the resale market after a certain number 
                                                 of years (indicated by End of MOP), with about 60% estimated to be available"))
                                                                      ),
                                                                      br(),
                                                                      fluidRow(
                                                                        withSpinner(dataTableOutput(outputId = "PropertyFinder")),
                                                                      ),
                                                                      tags$div(style="margin-top:20px;",
                                                                               p("Have you noted down the IDs of your desired properties? If yes, then you are ready to proceed to Finance Planning!")),
                                                                      tags$div(style="margin-bottom:10px;",
                                                                      actionButton(inputId = "bttn2",label= "Go to Finance Planning"))
                                                                      
                                                                      
                                                                    ))),
                           
                                                         # ----------------------------------
                                                         # tab panel 4 
                                                         tabPanel(tags$div(class="color-me","Financial Planning"),value = "financeplanning",
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
          helpText("Note: SC = Singapore Citizen"),
          column(width = 6,
                 radioButtons("FTST", "Are you a First-Timer Applicant?",
                              choices=c("Yes","No"))
          ),
          column(width=6,
                 radioButtons("Nationality", "Nationality Type",choices=c("SC", "Others"))
        ),
        fluidRow(
          tags$div(style="margin-left:10px;",column(width=10,numericInput("NetIncome","Income",value="Enter your income")))
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
                    tabPanel("HDB Executive", leafletOutput("leaflet5")),
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
                    tabPanel("HDB Executive", leafletOutput("leaflet_avail_5")),
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
    {
      leaflet() %>% 
      addTiles() %>%  
       setView(lng = 103.803214, lat = 1.368063, zoom = 11)
    })
  
  


  
  observeEvent(input$mapgen, {
    


    property1 <- find_lonlat(input$home_type_1)
    property2 <- find_lonlat(input$home_type_2)
    parents_add <- geocode(paste("Singapore", as.character(input$parent_address)))
    
    
    leafletProxy("leaflet_parents") %>%
  
 
    
    
    addMarkers(lat = as.numeric(property1[2]), lng = as.numeric(property1[1]), popup = find_address(input$home_type_1), icon=makeIcon(find_icon(input$home_type_1),iconWidth=25, iconHeight=25), layerId = "1") %>%
    addMarkers(lat = as.numeric(property2[2]), lng = as.numeric(property2[1]), popup = find_address(input$home_type_2), icon=makeIcon(find_icon(input$home_type_2),iconWidth=25, iconHeight=25), layerId = "2") %>%
    addMarkers(lat = as.numeric(parents_add[2]),lng = as.numeric(parents_add[1]), popup = "Your Parents' Home", icon=makeIcon("Parents' House.png",iconWidth=25, iconHeight=25), layerId = "3") %>%
    addCircles(lat = as.numeric(parents_add[2]),lng = as.numeric(parents_add[1]), radius= 4000,fillOpacity=0.1, layerId="c")
    
    
    
    output$property_name_1 <- renderText({
      paste(find_address(input$home_type_1),find_room(input$home_type_1), sep = " ")
    })
    
  
    
    
    output$price_grant_barchart_1 <- renderPlotly( {
      
      cost_calculator(input$home_type_1,
                      find_price(input$home_type_1),
                      input$NetIncome, 
                      find_room(input$home_type_1), 
                      measure_distance_from_p(input$parent_address, (find_lonlat(input$home_type_1))), 
                      (input$with_parents == "Yes"), 
                      (input$marital_status == "Married") ,
                      input$Nationality, 
                      input$FTST)
      
    }
    )
    
    output$property_name_2 <- renderText({
      paste(find_address(input$home_type_2),find_room(input$home_type_2), sep = " ")
    })
    

    output$price_grant_barchart_2 <- 
      renderPlotly( {
        cost_calculator(input$home_type_2,
                        find_price(input$home_type_2),
                        input$NetIncome, 
                        find_room(input$home_type_2), 
                        measure_distance_from_p(input$parent_address, (find_lonlat(input$home_type_2))), 
                        (input$with_parents == "Yes"), 
                        (input$marital_status == "Married") ,
                        input$Nationality, 
                        input$FTST)
        
      }
      )
     })
  
  
  
  observeEvent(input$mapreset,{
    if(input$mapreset)
    {
      leafletProxy("leaflet_parents") %>%
        removeShape("c") %>%
        removeMarker("1") %>%
        removeMarker("2") %>%
        removeMarker("3")
    }  
  })
  

  # Base map with layers
html_legend <- "<img src='https://www.flaticon.com/svg/static/icons/svg/2987/2987903.svg'style='width:10px;height:10px;'>&nbsp Primary School<br/>

<img src='https://www.flaticon.com/svg/static/icons/svg/3104/3104941.svg'style='width:10px;height:10px;'>&nbsp Park<br/>

<img src='https://www.flaticon.com/svg/static/icons/svg/1189/1189136.svg'style='width:10px;height:10px;'>&nbsp Community Club<br/>

<img src='https://www.flaticon.com/svg/static/icons/svg/821/821354.svg'style='width:10px;height:10px;'>&nbsp MRT<br/>

<img src='https://www.flaticon.com/svg/static/icons/svg/2451/2451622.svg'style='width:10px;height:10px;'>&nbsp BTO<br/>

<img src='https://www.flaticon.com/svg/static/icons/svg/2590/2590591.svg'style='width:10px;height:10px;'>&nbsp Resale<br/>

<img src='https://www.flaticon.com/svg/static/icons/svg/3523/3523064.svg'style='width:10px;height:10px;'>&nbsp MOP Soon<br/>

<img src='https://www.flaticon.com/svg/static/icons/svg/3014/3014764.svg'style='width:10px;height:10px;'>&nbsp Parents' House"
  output$map <- renderLeaflet(
    {
      leaflet() %>% 
        addTiles() %>%  
        setView(lng = 103.803214, lat = 1.368063, zoom = 11) %>% 
        addMarkers(data=schools, ~lon, ~lat,label = ~Schools,
                   icon=makeIcon("School.png",iconWidth=25, iconHeight=25), group="Primary Schools") %>% 
        addMarkers(data=parks, label = ~Description,  
                   icon=makeIcon("Park.png",iconWidth=25, iconHeight=25), group="Parks") %>%
        addMarkers(data=ccs, label = ~Description, 
                   icon=makeIcon("CCs.png",iconWidth=25, iconHeight=25), group="Community Clubs") %>%
        addMarkers(data=mrt, label = ~final,
                   icon=makeIcon("Train.png",iconWidth=25, iconHeight=25), group="MRTs") %>%
        addLayersControl(overlayGroups=c("Primary Schools","Parks","Community Clubs","MRTs")) %>%
        addControl(html=html_legend,position = "bottomright") %>%
        hideGroup(c("Parks","Community Clubs","MRTs"))
    })
  
  # Set view to area
  observeEvent(input$goButton,{
    districts <- districts %>% filter(Combined==input$AreaView)
    leafletProxy("map") %>%
      setView(lng=districts[1,4],lat=districts[1,5],zoom=15)
  })
  
  # Plot area around school
  observeEvent(input$goButton4,{
    schools <- schools %>% filter(Schools==input$SelectSchool)
    leafletProxy("map") %>%
      removeShape("sch") %>%
      setView(lng=schools[1,3],lat=schools[1,4],zoom=15) %>%
      addCircles(data=schools,lng=~lon,lat=~lat, color = "red",
                 radius=(as.integer(input$SchoolDist)*1000),fillOpacity=0.1, layerId="sch")
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
  observeEvent(input$goButton3,{
    parents_address <- parents_address()
    if(length(parents_address>0))
    {
      address <- google_geocode(address = parents_address)
      coords <- geocode_coordinates(address)  
      leafletProxy("map") %>% 
        addCircles(data=coords,~lng,~lat,radius=4000,fillOpacity=0.1, layerId="x") %>% 
        addMarkers(data=coords,~lng,~lat, label="Your Parents' House",
                   icon=makeIcon("Parents' House.png",iconWidth=30, iconHeight=30),layerId="y")
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
  observeEvent(input$goButton2, {
    type <- input$HousingType
    bto <- filter(bto, Type %in% type) 
    resale <- filter(resale, Type %in% type) %>% filter(`Flat Type` %in% input$RoomType)
    mop <- filter(mop, Type %in% type) %>% filter(`Flat Type` %in% input$RoomType2)
    
    labsbto <- lapply(seq(nrow(bto)), function(i) {
      paste0( "Type: ", bto[i, "Type"], "<br>",
              "ID: ", bto[i, "ID"], "<br>",
              "Month of Launch: ", bto[i, "Month of Launch"], "<br>",
              "Town/Estate: ", bto[i, "Town/Estate"], "<br>",
              "Estimated Flats: ", bto[i, "Estimated Flats"], "<br>",
              "Estimated Price: ", bto[i, "Estimated Price"], "<br>")})
    
    labsresale <- lapply(seq(nrow(resale)), function(i) {
      paste0( "Type: ", resale[i, "Type"], "<br>",
              "ID: ", resale[i, "ID"], "<br>",
              "Address: ", resale[i, "Address"], "<br>",
              "Flat Type: ", resale[i, "Flat Type"], "<br>",
              "Model: ", resale[i, "Model"], "<br>",
              "Year Built: ", resale[i, "Year Built"], "<br>",
              "Area: ", resale[i, "Area"], "<br>",
              "Price: ", resale[i, "Price"])})
    
    labsmop <- lapply(seq(nrow(mop)), function(i) {
      paste0( "Type: ", mop[i, "Type"], "<br>",
              "ID: ", mop[i, "ID"], "<br>",
              "Town/Name: ", mop[i, "Town/Name"], "<br>",
              "Project Name: ", mop[i, "Project Name"], "<br>",
              "End of MOP: ", mop[i, "End of MOP"], "<br>",
              "Flat Type: ", mop[i, "Flat Type"], "<br>",
              "Year Completed/ Year to be Complete: ", mop[i, "Year Completed/ Year to be Complete"], "<br>",
              "Predicted Price: ", mop[i, "Predicted Price"], "<br>",
              "No. of Units: ", mop[i, "No. of Units"])})
    
    leafletProxy("map") %>%
      clearGroup("BTO") %>%
      clearGroup("Resale") %>%
      clearGroup("MOP") %>%
      addMarkers(data=bto,~lon,~lat,label = lapply(labsbto, htmltools::HTML), group="BTO", 
                 icon=makeIcon("BTO.png",iconWidth=30, iconHeight=30),layerId=~`Town/Estate`) %>%
      addMarkers(data=resale,~lon,~lat,label = lapply(labsresale, htmltools::HTML),group="Resale", 
                 icon=makeIcon("Resale.png",iconWidth=30, iconHeight=30),layerId=~Address) %>%
      addMarkers(data=mop,~lon,~lat,label = lapply(labsmop, htmltools::HTML),group="MOP", 
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
