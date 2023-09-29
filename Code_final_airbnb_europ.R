#Chargement données
data_cities=read.table("Airbnb_Cities_Europ.csv",header=TRUE,sep=";",stringsAsFactors = TRUE,dec=",")
summary(data_cities)
#Chargement packages
library(shinydashboard)
library(shiny)
library(maps)
library(leaflet)
library(dplyr)

#Customiser icone de marquage
HouseIcon <- makeIcon(
  iconUrl = "maison1.png",
  iconWidth = 15, iconHeight = 15,
  iconAnchorX = 22, iconAnchorY = 94
  
)

ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = "Airbnb Europe"),
  dashboardSidebar(
    selectInput('Ville',
                'Choisir une ville',
                choices=unique(data_cities$City),selected ='Paris'),
    
    sliderInput("Prix", "Selectionner la gamme de prix",
                min=0, max=2000,value=c(0,200)),
    
    selectInput('Day',
                'Weekend ou Weekdays',
                choices=unique(data_cities$Weekdays_WE),selected='Weekend'),
    selectInput("Room",
                "Type de logement",
                choices=unique(data_cities$room_type, selected='Private room')),
   
     numericInput("Clean", "Propreté", value=1,min=0,max=10,step=1),
    
    numericInput("Capacity","Nombre de personne", value=2,min=2,max=6,step=1),
    numericInput("Satisfaction", "Satisfaction client", value=20,min=20,max=100,step=5)
    
    
    
  ),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(title=span(icon("map"),"Carte"),status="success" , width = 12,height = 490,solidHeader = TRUE,collapsible = TRUE,
          leafletOutput(outputId='map', height=400, width = 1250)#Crer un élément de l'interface qui indique qu'on doit adfficher une sortie carte leaflet ici
      )
    
      )
    )
  )


server <- function(input, output) {  #Création d'un outil réactif: ensemble donnée géographique, chq fois user modifie variable l'application exécutera des actions
  #On crée une variable réactive qui change quand l'user change la valeur de la variable
  city_filtered <- reactive({
    data_cities %>% 
      filter(City == input$Ville & Price >= input$Prix[1] & Price <= input$Prix[2] & Weekdays_WE==input$Day
             & cleanliness_rating>=input$Clean & room_type==input$Room & 
              person_capacity>=input$Capacity & guest_satisfaction_overall >= input$Satisfaction)
  })
  
  
  #Création de la carte 
  output$map = renderLeaflet({
    leaflet(city_filtered()) %>%
      addTiles() %>%
      addMarkers(lng=~lng,
                 lat=~lat,
                 popup=~as.character(Price), #Afficher le prix quand on passe le curseur
                 label=~as.character(Price),
                 icon=HouseIcon)
    
    
  })
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

