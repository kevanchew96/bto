library(shiny)
library(ggplot2)
library(shinyTime)
library(jsonlite)
library(ggmap)
library(googleway)
library(leaflet)
ggmap::register_google(key = 'AIzaSyBtkpz6CUH-lwaRTLrfnBbGPpaj4pst6Z8')

key <- "AIzaSyBtkpz6CUH-lwaRTLrfnBbGPpaj4pst6Z8"
set_key(key = key)

ui <- pageWithSidebar(
  headerPanel("BTO Map"),
  
  sidebarPanel(
    textInput(inputId = "parents_address", label = "Parents' Address")    
    ,textOutput(outputId = "parents_full_address")
    ,HTML(paste0(" <script> 
                function initAutocomplete() {

                 var autocomplete =   new google.maps.places.Autocomplete(document.getElementById('parents_address'),{types: ['geocode']});
                 autocomplete.setFields(['address_components', 'formatted_address',  'geometry', 'icon', 'name']);
                 autocomplete.addListener('place_changed', function() {
                 var place = autocomplete.getPlace();
                 if (!place.geometry) {
                 return;
                 }

                 var addressPretty = place.formatted_address;
                 var address = '';
                 if (place.address_components) {
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
                 Shiny.onInputChange('jsValueCoords', coords);});}
                 </script> 
                 <script src='https://maps.googleapis.com/maps/api/js?key=", key,"&libraries=places&callback=initAutocomplete' async defer></script>"))
    
  )
  ,
  
  mainPanel (
  (leafletOutput(outputId = "my_map"))
  ))
  

server <- function(input, output) {
  
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
  
  
  output$my_map <- renderLeaflet({
    parents_address <- parents_address() 
    shiny::validate(
      need(parents_address, "Address not available")
    )
    
    address <- google_geocode(address = parents_address)
    coords <- geocode_coordinates(address)
    
    leaflet(data=coords) %>% addTiles %>% addCircles(~lng,~lat,radius=2000,fillOpacity = 0.1) %>% addMarkers(~lng,~lat,popup=parents_address)
    
  })
  
}

shinyApp(ui, server)

