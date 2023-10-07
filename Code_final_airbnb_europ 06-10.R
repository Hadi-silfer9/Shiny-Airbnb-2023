#Chargement données
data_cities=read.table("Airbnb_Cities_Europ.csv",header=TRUE,sep=";",stringsAsFactors = TRUE,dec=",")
data_cities<-data_cities[,-1]
summary(data_cities)

#Chargement packages
library(shinydashboard)
#library(shinyjs)
library(shiny)
library(DT)
library(maps)
library(leaflet)
library(dplyr)
library(caret)
library(randomForest)

#Fonction pour la couleur des marqueurs
getColor <- function(data_cities) {
  sapply(data_cities$Price, function(Price) {
    if(Price <= 100) {
      "lightgreen"
    } else if(Price <= 200) {
      "green"
    } else if(Price <= 300) {
      "beige"
    } else if(Price <= 400) {
      "orange"
    } else if(Price <= 500) {
      "lightred"
    } else {
      "red"
    }})
}

#Chargement du modèle préalablement entrainé
trained_model <- readRDS("model_rf.rds")



auteurs <- c(" Manon Gaudin", "Mathilde Yim", "Hadirou Tamdamba")



ui <- dashboardPage(
 
 # useShinyjs(),  # Activer shinyjs
  skin="green",
  dashboardHeader(
    title = tags$h1("Airbnb Europe", style = "margin-bottom: 0;"),
    tags$li(
      class = "dropdown",
      tags$a(
        href = "#",
        class = "dropdown-toggle",
        "data-toggle" = "dropdown",
        tags$span("Welcome", icon("users", class = "fa-2x")),
        tags$span("Auteurs"),
        style = "color: #333 !important; font-size: 8.5px !important;",  # Ajustez la couleur et la taille de la police
        class = "caret"
      ),
      tags$ul(
        class = "dropdown-menu",
        lapply(auteurs, function(aut) {
          tags$li(
            aut
          )
        })
      )
    )
  ),
  
  
  
  
  

  dashboardSidebar(
    
    sidebarMenu(id = "sidebar",
                menuItem("Documentation", tabName = "Documentation", icon = icon("info-circle")),
                menuItem("Base de Données", tabName = "baseDeDonnees", icon = icon("database")),
                
                menuItem("Prix Moyen", tabName = "PrixMoyen", icon = icon("euro")),
                
                menuItem("Recherche", tabName = "RechercherunAirBNB", icon = icon("dashboard")),
                
                
                menuItem("Dépôt annonce", tabName = "Deposeruneannonce", icon = icon("th")),
                
                
                conditionalPanel(
                  
                  condition = "input.sidebar === 'RechercherunAirBNB'",
                  
                  tagList(
                    
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
                    
                  )
                  
                ),
                
                conditionalPanel(
                  
                  condition = "input.sidebar === 'Deposeruneannonce'",
                  
                  tagList(
                    selectInput('Ville2',
                                'Choisir une ville',
                                choices=unique(data_cities$City),selected ='Paris'),
                    selectInput('Day2',
                                'Weekend ou Weekdays',
                                choices=unique(data_cities$Weekdays_WE),selected='Weekend'),
                    selectInput("Room2",
                                "Type de logement",
                                choices=unique(data_cities$room_type, selected='Private room')),
                    numericInput("Capacity2","Nombre de personne", value=2,min=2,max=6,step=1)
                    
                  )
                  
                )
                
                
    )
    
  ),
  
  dashboardBody(
    tags$style(type = "text/css", "#map {height: calc(90vh - 80px) !important;}"),
    tags$style(type = "text/css", "#map2 {height: calc(90vh - 80px) !important;}"),
    
    tabItems(
      
      # First tab content
      
      tabItem(tabName = "Documentation",
              fluidRow(
                column(12,
                       tags$h1("Explorez et utilisez l'application Airbnb Europe avec facilité!" , icon("smile", class = "fa-2x")),
                       tags$h2("1. Base de Données"),
                       tags$p("La base de données fournit un aperçu complet des prix Airbnb dans certaines des villes européennes les plus populaires au cours de l’année 2021."), 
                       tags$p("Nous avons décidé pour le développement de notre application de nous focaliser uniquement sur les villes : Paris, Amsterdam, Barcelone, Berlin, Rome. 
                              L’application pouvant bien entendu être déclinée à toutes les autres."),
                       tags$p("Chaque annonce Airbnb est évaluée en fonction de divers attributs : 
types de chambres, les notes de propreté et de satisfaction, la capacité d’acceuil, la distance du centre-ville, les coordonnées géographiques (lat et long), la distance au métro ainsi que le prix du bien à louer. 
"),
                       tags$h2("2. Prix Moyen par Ville"),
                       
                       tags$p("Consultez l'onglet 'Prix Moyen' pour découvrir le graphique des prix moyens par ville"),
                       tags$h2("3. Carte Interactive"),
                       tags$p("L'onglet 'Recherche' vous permet d'explorer les offres Airbnb par ville. Utilisez les filtres sur le côté pour affiner vos critères de recherche. La carte affiche des marqueurs colorés en fonction des prix. Survolez les marqueurs pour voir les prix exacts et les détails."),
                       tags$h2("4. Prédiction du Prix"),
                       tags$p("L'onglet 'Dépôt annonce' vous permet de prédire le prix de location d'un bien Airbnb en fonction de certaines caractéristiques. Voici comment utiliser cette fonctionnalité :"),
                       tags$p("Choisir une Ville : Sélectionnez la ville où se trouve le bien que vous souhaitez louer."),
                       tags$p("Sélectionner le Jour : Choisissez entre 'Weekend' et 'Weekdays' en fonction de vos besoins."),
                       tags$p("Type de Logement : Sélectionnez le type de logement que vous souhaitez proposer, par exemple, Private room "),
                       tags$p("Capacité : Indiquez le nombre de personnes que votre bien peut accueillir."),
                       tags$p("Localisation : Cliquez sur la carte pour sélectionner l'emplacement exact de votre bien."),
                       tags$p("Ensuite, l'application utilisera un modèle de prédiction de type Random Forest préalablement entraîné pour prédire le prix de location en fonction de ces caractéristiques."), 
                       
                       
                ))
              
      ),
      
      # Second tab content
      
      tabItem(tabName = "baseDeDonnees",
              fluidRow(
                box(
                  title = "Base de Données",
                  status = "info",
                  width = 12,
                  solidHeader = TRUE,
                  collapsible = TRUE,
                  DT::dataTableOutput("data_table")
                )
              )
      ),
      
      #Third tab content
      
      tabItem(tabName = "PrixMoyen",
              fluidRow(
                box(
                  title = "Prix Moyen par Ville",
                  status = "success", 
                  width = 12, 
                  solidHeader = TRUE, 
                  collapsible = TRUE,
                  plotOutput("prixMoyenPlot")
                )
              )
      ),
      
      # 4th tab content
      
      tabItem(tabName = "RechercherunAirBNB",
              
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(title=span(icon("map"),"Carte"),status="success", width = 12, solidHeader = TRUE,collapsible = TRUE,
                    leafletOutput(outputId='map', height="100%", width = "100%")#Créer un élément de l'interface qui indique qu'on doit adfficher une sortie carte leaflet ici
                )
                
              )
      ),
      
      # 5th tab content
      
      tabItem(tabName = "Deposeruneannonce",
              fluidRow(
                box(title="Après avoir sélectionner les caractéristiques de votre bien à louer, veuillez cliquer sur sa localisation", status="success", width=12,solidHeader = TRUE,collapsible = TRUE),
                box(title=span(icon("map2"),"Carte"),status="success", width = 12, solidHeader = TRUE,collapsible = TRUE,
                    leafletOutput(outputId='map2', height="100%", width = "100%")#Crer un élément de l'interface qui indique qu'on doit adfficher une sortie carte leaflet ici
                ),
                
                box(title="Prix-pred",solidHeader=TRUE, collapsible = TRUE, status= "success",
                    textOutput("predicted_price")
                    
                )
                
              )
              
      )
      
     
      
      
 
    
    
    
    
  )
  )


)

server <- function(input, output, session) {
  
  # Afficher la base de données dans l'onglet "Base de Données"
  output$data_table <- DT::renderDataTable({
    data_cities
  })
  
  # Calcul du prix moyen par ville
  prix_moyen <- reactive({
    data_cities %>%
      group_by(City) %>%
      summarise(PrixMoyen = mean(Price))
  })
  
  # Graphique du prix moyen par ville
  output$prixMoyenPlot <- renderPlot({
    prix_data <- prix_moyen()
    
    ggplot(prix_data, aes(x = City, y = PrixMoyen, fill = City)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = sprintf("%.2f €", PrixMoyen)), vjust = -0.5) +
      labs(title = "Prix Moyen par Ville", x = "Ville", y = "Prix Moyen") +
      theme_minimal()
  })
  
  
  
  # Fonction de prédiction
  predict_price <- eventReactive(input$map2_click, {
    # Création d'un nouveau data frame avec les valeurs de l'utilisateur
    # browser()
    new_data <- data.frame(
      City = input$Ville2,
      Weekdays_WE = input$Day2,
      room_type = input$Room2,
      person_capacity = input$Capacity2,
      lng=input$map2_click$lng,
      lat=input$map2_click$lat
      
    )
    
    # # Prédiction du prix avec le modèle
    predict(trained_model, new_data)
    
  })
  
  
  # Afficher la prédiction
  output$predicted_price <- renderText({
    paste("Prix prédit :", round(predict_price(), 2), "€")
  })
  
  
  
  #Création d'un outil réactif: ensemble donnée géographique, chq fois user modifie variable l'application exécutera des actions
  #On crée une variable réactive qui change quand l'user change la valeur de la variable
  city_filtered <- reactive({
    data_cities %>% 
      filter(City == input$Ville & Price >= input$Prix[1] & Price <= input$Prix[2] & Weekdays_WE==input$Day
             & cleanliness_rating>=input$Clean & room_type==input$Room & 
               person_capacity>=input$Capacity & guest_satisfaction_overall >= input$Satisfaction)
  })
  
  
  #Création de la carte 
  output$map = renderLeaflet({
    data_cities <- city_filtered()
    
    # Définit les icones
    icons <- awesomeIcons(
      icon = 'house',
      iconColor = 'black',
      library = 'fa',
      markerColor = getColor(data_cities)
    )
    
    # Affichage de la carte
    leaflet(data_cities) %>%
      addTiles() %>%
      addAwesomeMarkers(lng=~lng,
                        lat=~lat,
                        icon=icons,
                        popup=~as.character(round(Price,2)),
                        label=~as.character(round(Price,2)),
                        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend("bottomright", 
                colors = c("lightgreen", "green", "beige", "orange", "#FF8778", "red"), 
                labels = c("< 100$", "101-200$", "201-300$", "301-400$", "401-500$", "> 500$"),
                title = "Prix par nuit",
                opacity = 1
      )
    
  })
  
  
  
  
  
  city_filtered2 <- reactive({
    data_cities %>% 
      filter(City == input$Ville2 & Weekdays_WE==input$Day2
             & room_type==input$Room2 & 
               person_capacity>=input$Capacity2)
  })
  
  #Création de la 2eme map 
  #Création de la carte 
  output$map2 = renderLeaflet({
    data_cities <- city_filtered2()
    
    #Définit les icones
    icons <- awesomeIcons(
      icon = 'house',
      iconColor = 'black',
      library = 'fa',
      markerColor = getColor(data_cities)
    )
    
    #Affichage de la carte
    leaflet(data_cities) %>%
      addTiles() %>%
      addAwesomeMarkers(lng=~lng,
                        lat=~lat,
                        icon=icons,
                        popup=~as.character(round(Price,2)),
                        label=~as.character(round(Price,2)),
                        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend("bottomright", 
                colors = c("lightgreen", "green", "beige", "orange", "#FF8778", "red"), 
                labels = c("<= 100$", "101-200$", "201-300$", "301-400$", "401-500$", "> 500$"),
                title = "Prix par nuit",
                labFormat = labelFormat(prefix = "$"),
                opacity = 1
      )
    
  })
  
  
}


# Run the application 
shinyApp(ui = ui, server = server)
