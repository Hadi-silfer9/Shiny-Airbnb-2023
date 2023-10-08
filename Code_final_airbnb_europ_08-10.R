# Chargement données
data_cities <- read.table("Airbnb_Cities_Europ.csv",header=TRUE,sep=";",stringsAsFactors = TRUE,dec=",")
data_cities<-data_cities[,-1]
summary(data_cities)

# Chargement packages
library(shinydashboard)
library(shiny)
library(maps)
library(leaflet)
library(dplyr)
library(caret)
library(randomForest)

# Fonction pour la couleur des marqueurs
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

# Chargement du modèle prélablement entrainé
trained_model <- readRDS("model_rf.rds")

ui <- dashboardPage(
  skin="green",
  dashboardHeader(title = "Airbnb Europe",
                  tags$li(a(href = "https://www.institut-agro-rennes-angers.fr/", 
                            target = "_blank",
                            tags$img(src="https://www.zupimages.net/up/23/40/g6qe.png", 
                                     height = "25px"
                            )
                  ),
                  class = "dropdown")
  ),
  dashboardSidebar(
    sidebarMenu(id = "sidebar",
                # Crée les différents onglets dans la sidebar
                menuItem("Documentation", 
                         tabName = "Documentation", 
                         icon = icon("info-circle")
                ),
                menuItem("Base de données", 
                         tabName = "BaseDonnees", 
                         icon = icon("database")
                ),
                menuItem("Prix moyen", 
                         tabName = "PrixMoyVill", 
                         icon = icon("chart-column")
                ),
                menuItem("Recherche", 
                         tabName = "RechercherunAirBNB", 
                         icon = icon("dashboard")
                ),
                menuItem("Dépôt annonce", 
                         tabName = "Deposeruneannonce", 
                         icon = icon("th")
                ),
                # Sidebar pour l'onglet "Prix moyen"
                conditionalPanel(
                  condition = "input.sidebar === 'PrixMoyVill'",
                  tagList(
                    selectInput("Ville3", 
                                "Choisissez une ou plusieurs villes", 
                                choices = unique(data_cities$City), 
                                selected = "Paris", 
                                multiple = TRUE
                    )
                  )
                ),
                # Sidebar pour l'onglet "Recherche"
                conditionalPanel(
                  condition = "input.sidebar === 'RechercherunAirBNB'",
                  tagList(
                    selectInput("Ville",
                                "Choisir une ville",
                                choices = unique(data_cities$City), 
                                selected = "Paris"
                    ),
                    sliderInput("Prix", 
                                "Selectionner la gamme de prix",
                                min = 0, 
                                max = 2000,
                                value = c(0,200)
                    ),
                    selectInput("Day",
                                "En semaine ou en week-end",
                                choices = unique(data_cities$Weekdays_WE),
                                selected = "Weekend"
                    ),
                    selectInput("Room",
                                "Type de logement",
                                choices = unique(data_cities$room_type), 
                                selected = "Private room"
                    ),
                    numericInput("Clean", 
                                 "Propreté", 
                                 value = 1,
                                 min = 0,
                                 max = 10,
                                 step = 1
                    ),
                    numericInput("Capacity",
                                 "Nombre de personne", 
                                 value = 2,
                                 min = 2,
                                 max = 6,
                                 step = 1
                    ),
                    numericInput("Satisfaction", 
                                 "Satisfaction client", 
                                 value = 20,
                                 min = 20,
                                 max = 100,
                                 step = 5
                    )
                  )
                ),
                # Sidebar pour l'onglet "Dépôt annonce"
                conditionalPanel(
                  condition = "input.sidebar === 'Deposeruneannonce'",
                  tagList(
                    selectInput("Ville2",
                                "Choisir une ville",
                                choices = unique(data_cities$City),
                                selected = "Paris"
                    ),
                    selectInput("Day2",
                                "En semaine ou en week-end",
                                choices = unique(data_cities$Weekdays_WE),
                                selected = "Weekend"
                    ),
                    selectInput("Room2",
                                "Type de logement",
                                choices = unique(data_cities$room_type), 
                                selected = "Private room"
                    ),
                    numericInput("Capacity2",
                                 "Nombre de personne", 
                                 value = 2,
                                 min = 2,
                                 max = 6,
                                 step = 1
                    )
                  )
                )
    )
  ),
  dashboardBody(
    # CSS pour l'apparence de l'application
    tags$style(type = "text/css", "#map {
                height: calc(90vh - 80px) !important;
               }
               #map2 {
                height: calc(77vh - 80px) !important;
               }
               #PrixMoyWDWE,  #PrixMoyTypLgmt, #PrixMoyPersCap, #PrixMoyPropr, #PrixSatisf {
                height: calc(85vh - 45px) !important;
               }
               .main-header .logo {
                font-family: Futura, 'Trebuchet MS', Arial, sans-serif;
                font-weight: bold;
                font-size: 25px;
                color: white !important;
                box-shadow: inset 0 0 0 0 white;
                transition: color 0.5s ease-in-out, box-shadow 0.4s ease-in-out;
                -webkit-transition: color 0.5s ease-in-out, box-shadow 0.4s ease-in-out;
               }
               .main-header .logo:hover{
                color: #008D4C !important;
                box-shadow: inset 100vh 0 0 0 white;
               }
               .leaflet-popup-content-wrapper {
                background-color: rgba(255, 255, 255, 0.9);
                box-shadow: rgba(0, 0, 0, 0.35) 0px 5px 15px;
                color: black;
                padding: 2px;
                border-radius: 0px;
                border: 1px dashed black;
               }
               html::-webkit-scrollbar {
                width: 12px;
               }
               html::-webkit-scrollbar-track {
                border-radius: 8px;
                background-color: #ECF0F5;
                border: 1px solid #cacaca;
               }
               html::-webkit-scrollbar-thumb {
                border-radius: 8px;
                background-color: #008D4C;
               }
               .icon-title {
                height: 20px;
               }
               h1 {
                font-family: Futura, 'Trebuchet MS', Arial, sans-serif;
                font-weight: bold;
                text-align: center;
                background-image: linear-gradient(
                                  to right,
                                  #008D4C,
                                  #008D4C 50%,
                                  #000 50%
                                  );
                background-size: 200% 100%;
                background-position: -100%;
                -webkit-background-clip: text;
                -webkit-text-fill-color: transparent;
                transition: all 0.3s ease-in-out;
                -webkit-transition: all 0.3s ease-in-out;
               }
               h1:before{
                content: '';
                background: #008D4C;
                display: block;
                position: absolute;
                bottom: -3px;
                left: 0;
                width: 0;
                height: 3px;
                transition: all 0.3s ease-in-out;
                -webkit-transition: all 0.3s ease-in-out;
               }
               h1:hover {
                background-position: 0;
               }
               h1:hover::before{
                width: 100%;
               }
               h2 {
                font-family: Futura, 'Trebuchet MS', Arial, sans-serif;
                margin-left: 25px;
               }
               p {
                background-color:rgba(0, 141, 76, 0.5);
                padding: 15px;
                border-radius: 10px;
                font-family: Avant Garde,Avantgarde,Century Gothic,CenturyGothic,AppleGothic,sans-serif; 
               }
               p span {
                color: #222D32;
                font-weight: bold;
               }"
    ),
    tabItems(
      # 1er Onglet : "Documentation"
      tabItem(tabName = "Documentation",
              fluidRow(
                div(style="display: flex; justify-content: center; align-items: center", # Aligne la boite
                    box(
                      title = h1("Explorez et utilisez l'application Airbnb Europe avec facilité !", icon("face-smile", class = "icon-title")),
                      width = 8,
                      status = "success",
                      h2("1. Base de Données"),
                      p("La base de données fournit un aperçu complet des prix Airbnb dans certaines des villes européennes les plus populaires au cours de l’année 2021.", br(),
                        "Nous avons décidé pour le développement de notre application de nous focaliser uniquement sur les villes :", em(" Paris, Amsterdam, Barcelone, Berlin, Rome."), br(), 
                        "L’application pouvant bien entendu être déclinée à toutes les autres.", br(), 
                        "Chaque annonce Airbnb est évaluée en fonction de divers attributs :", em(" le type de logement, les notes de propreté et de satisfaction, la capacité d’accueil, la distance du centre-ville, les coordonnées géographiques (latitude et longitude), la distance au métro ainsi que le prix du bien à louer."), br(),
                        "Nous avons décidé de ne garder que les variables ", br(), br(),
                        "La base de données brute est visible dans l'onglet", span(" 'Base de données'"),"."),
                      h2("2. Prix Moyen par Ville"),
                      p("Consultez l'onglet", span(" 'Prix moyen' "),"pour découvrir le graphique des prix moyens par ville, ainsi que le prix moyen en fonction des différentes caractéristiques du logement."),
                      h2("3. Carte Interactive"),
                      p("L'onglet", span(" 'Recherche' "),"vous permet d'explorer les offres Airbnb par ville. Utilisez les filtres sur le côté pour affiner vos critères de recherche. La carte affiche des marqueurs colorés en fonction des prix. Survolez les marqueurs pour voir les prix exacts et les détails."),
                      h2("4. Prédiction du Prix"),
                      p("L'onglet", span(" 'Dépôt annonce' "),"vous permet d'estimer le prix de location d'un bien Airbnb en fonction de certaines caractéristiques. Voici comment utiliser cette fonctionnalité : ", br(),
                        "• Choisir une ville : Sélectionnez la ville où se trouve le bien que vous souhaitez louer.", br(),
                        "• Sélectionner le jour : Choisissez entre 'Weekend' et 'Weekdays' en fonction de vos besoins.", br(),
                        "• Type de Logement : Sélectionnez le type de logement que vous souhaitez proposer, par exemple, Private room", br(),
                        "• Capacité : Indiquez le nombre de personnes que votre bien peut accueillir.", br(),
                        "• Localisation : Cliquez sur la carte pour sélectionner l'emplacement exact de votre bien.", br(),
                        "Ensuite, l'application utilisera un modèle de prédiction de type Random Forest préalablement entraîné pour prédire le prix de location en fonction de ses caractéristiques.")
                    )
                )
              ),
              br(),  
              fluidRow(
                box(title = span(icon("marker"), "Crédits"), 
                    width = NULL, 
                    solidHeader = TRUE, 
                    "Projet fait par Mathilde YIM, Hadirou TAMDAMBA et Manon GAUDIN", br(), "dans le cadre de la spécialisation Sciences de Donnée à l'Institut Agro Rennes Angers.", br(), br(), 
                    "Données :", a(href = "https://www.kaggle.com/datasets/thedevastator/airbnb-prices-in-european-cities", target = "_blank",
                                   "Airbnb Prices in European Cities, par The Devastator"
                    ), 
                    footer = "© 2023"
                )
              )
              
      ),
      # 2e onglet : "Base de données"
      tabItem(tabName = "BaseDonnees",
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
      # 3e onglet : "Prix moyen"
      tabItem(tabName = "PrixMoyVill",
              fluidRow(
                column(width = 8,
                       tabBox(title = span(icon("circle-plus"), "Prix moyen d'une nuit en fonction du critère selectionné"),
                              width = 12,
                              selected = "Semaine/Week-end",
                              tabPanel("Semaine/Week-end", 
                                       plotOutput("PrixMoyWDWE")
                              ),
                              tabPanel("Type logement", 
                                       plotOutput("PrixMoyTypLgmt")
                              ),
                              tabPanel("Nombre personnes", 
                                       plotOutput("PrixMoyPersCap")
                              ),
                              tabPanel("Propreté", 
                                       plotOutput("PrixMoyPropr")
                              ),
                              tabPanel("Satisfaction", 
                                       plotOutput("PrixSatisf")
                              )
                       )
                ),
                column(width = 4,
                       box(title = span(icon("database"), "Résumé des données"), 
                           status = "info", 
                           width = 12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           verbatimTextOutput("summary")
                       ),
                       box(title = span(icon("receipt"), "Prix moyen d'une nuit pour les différentes villes"), 
                           status = "warning", 
                           width = 12,
                           solidHeader = TRUE,
                           collapsible = TRUE,
                           plotOutput("PrixMoy")
                       )
                )
              )
              
      ),
      
      # 4e onglet : "Recherche"
      tabItem(tabName = "RechercherunAirBNB",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(title = span(icon("map"), "Carte"),
                    status = "success", 
                    width = 12, 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    #Crer un élément de l'interface qui indique qu'on doit adfficher une sortie carte leaflet ici
                    leafletOutput(outputId = "map", 
                                  height = "100%", 
                                  width = "100%"
                                  )
                )
              )
      ),
      # 5e onglet : "Dépôt annonce"
      tabItem(tabName = "Deposeruneannonce",
              fluidRow(
                box(title = span(icon("circle-info"), " Après avoir sélectionné les caractéristiques de votre bien à louer, veuillez cliquer sur sa localisation"),
                    status = "success", 
                    width = 12, 
                    solidHeader = TRUE,
                    collapsible = TRUE,
                    #Crer un élément de l'interface qui indique qu'on doit adfficher une sortie carte leaflet ici
                    leafletOutput(outputId = "map2", 
                                  height = "100%", 
                                  width = "100%"
                                  )
                ),
                box(title = span(icon("dollar-sign"), " Prix estimé"),
                    solidHeader = TRUE, 
                    collapsible = TRUE, 
                    status = "info",
                    textOutput("predicted_price")
                )
              )
      )
    )
  ) 
)



server <- function(input, output) {  
  #### Pour l'onglet "Base de données"
  # Afficher la base de données dans l'onglet "Base de Données"
  output$data_table <- DT::renderDataTable(
    {data_cities}, 
    options = list(scrollX = TRUE)
    )
  
  ### Pour l'onglet "Prix moyen"
  #Visualisation des données
  output$summary <- renderPrint({
    summary(data_cities[, c(1, 2, 3, 4, 7, 11, 12, 16, 17)]) #Sélectionne uniquement les variables sur lesquelles on filtre pour plus de claireté
  })
  
  #Associe des couleurs à chaque ville
  city_colors <-setNames(c("#00B0F6","#F8766D", "#F5C710","pink","#00BF7D"), levels(data_cities$City))
  
  #Graphe de prix moyen par ville
  output$PrixMoy <- renderPlot({
    data_cities %>% 
      group_by(City) %>% 
      summarise(Prix_Moy = mean(Price)) %>% 
      ggplot() +
      aes(x = City, y = Prix_Moy, fill = City) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = paste(round(Prix_Moy, 2), " €")), vjust = 1.5) + 
      scale_fill_manual(values = city_colors) +
      ylab('Prix moyen par nuit (en euros)') +
      xlab('Ville')
  })
  
  #Graphe de prix moyen par ville en fonction d'une caractéristique
  city_filtered3 <- reactive({
    data_cities %>% 
      filter(City %in% input$Ville3) 
  })
  
  #En fonction du jour
  output$PrixMoyWDWE <- renderPlot({
    data_cities3 <- city_filtered3()
    
    data_cities3 %>%
      group_by(City, Weekdays_WE) %>% 
      summarise(Prix_Moy = mean(Price)) %>% 
      ggplot() +
      aes(x = Weekdays_WE, y = Prix_Moy, fill = City) +
      geom_bar(position="dodge", stat = "identity") + 
      geom_text(position = position_dodge2(width = 0.9, preserve = "single"), aes(label = paste(round(Prix_Moy, 2), " €")), vjust = 1.5) + 
      scale_fill_manual(values = city_colors) +
      ylab('Prix moyen par nuit (en euros)') +
      xlab('En semaine ou en week-end')
  })
  
  #En fonction du type de logement
  output$PrixMoyTypLgmt <- renderPlot({
    data_cities3 <- city_filtered3()
    
    data_cities3 %>%
      group_by(City, room_type) %>% 
      summarise(Prix_Moy = mean(Price)) %>% 
      ggplot() +
      aes(x = room_type, y = Prix_Moy, fill = City) +
      geom_bar(position="dodge", stat = "identity") + 
      geom_text(position = position_dodge2(width = 0.9, preserve = "single"), aes(label = paste(round(Prix_Moy, 2), " €")), vjust = 1.5) + 
      scale_fill_manual(values = city_colors) +
      ylab('Prix moyen par nuit (en euros)') +
      xlab('Type de logement')
  })
  
  #En fonction du nombre de personne
  output$PrixMoyPersCap <- renderPlot({
    data_cities3 <- city_filtered3()
    
    data_cities3$person_capacity <- as.factor(data_cities3$person_capacity)
    
    data_cities3 %>%
      group_by(City, person_capacity) %>% 
      summarise(Prix_Moy = mean(Price)) %>% 
      ggplot() +
      aes(x = person_capacity, y = Prix_Moy, fill = City) +
      geom_bar(position="dodge", stat = "identity") + 
      geom_text(position = position_dodge2(width = 0.9, preserve = "single"), aes(label = paste(round(Prix_Moy, 2), " €")), vjust = 1.5) + 
      scale_fill_manual(values = city_colors) +
      ylab('Prix moyen par nuit (en euros)') +
      xlab('Nombre de personne')
  })
  
  #En fonction de la propreté
  output$PrixMoyPropr <- renderPlot({
    data_cities3 <- city_filtered3()
    
    data_cities3 %>%
      group_by(City, cleanliness_rating) %>% 
      summarise(Prix_Moy = mean(Price)) %>% 
      ggplot() +
      aes(x = cleanliness_rating, y = Prix_Moy, color = City)  +
      geom_line(aes(group = City)) +
      geom_point() +
      geom_text(position = position_dodge2(width = 0.9, preserve = "single"), color = "black", aes(label = paste(round(Prix_Moy, 2), " €")), vjust = 1.5) + 
      scale_color_manual(values = city_colors) +
      ylab('Prix moyen par nuit (en euros)') +
      xlab('Note de propreté')
  })
  
  #En fonction de la satisfation
  output$PrixSatisf <- renderPlot({
    data_cities3 <- city_filtered3()
    
    data_cities3$guest_satisfaction_overall <- as.factor(data_cities3$guest_satisfaction_overall)
    
    data_cities3 %>%
      group_by(City, guest_satisfaction_overall) %>% 
      summarise(Prix_Moy = mean(Price)) %>% 
      ggplot() +
      aes(x = guest_satisfaction_overall, y = Prix_Moy, color = City) +
      geom_line(aes(group = City)) +
      geom_point() +
      #geom_text(position = position_dodge2(width = 0.9, preserve = "single"), color = "black", aes(label = paste(round(Prix_Moy, 2), " €")), vjust = 1.5) + 
      scale_color_manual(values = city_colors) +
      ylab('Prix moyen par nuit (en euros)') +
      xlab('Note de satisfaction')
  })
  
  ### Pour l'onglet "Recherche"
  # Création d'un outil réactif: ensemble donnée géographique, chq fois user modifie variable l'application exécutera des actions
  # On crée une variable réactive qui change quand l'user change la valeur de la variable
  city_filtered <- reactive({
    data_cities %>% 
      filter(City == input$Ville & 
               Price >= input$Prix[1] & 
               Price <= input$Prix[2] & 
               Weekdays_WE==input$Day & 
               cleanliness_rating>=input$Clean & 
               room_type==input$Room & 
               person_capacity>=input$Capacity & 
               guest_satisfaction_overall >= input$Satisfaction
      )
  })
  
  #Création de la 1ere map (Recherche)
  output$map = renderLeaflet({
    data_cities <- city_filtered()
    
    #Définit les icones
    icons <- awesomeIcons(
      icon = 'house',
      iconColor = 'black',
      library = 'fa',
      markerColor = getColor(data_cities)
    )
    
    #Affichage de la 1ere map (Recherche)
    leaflet(data_cities) %>%
      addTiles() %>%
      addAwesomeMarkers(lng = ~lng,
                        lat = ~lat,
                        icon = icons,
                        popup = paste0(
                          strong("Prix : "), 
                          round(data_cities$Price,2), " €",
                          br(),
                          strong("Nombre de personnes max : "),
                          data_cities$person_capacity,
                          br(),
                          strong("Propreté : "),
                          data_cities$cleanliness_rating, " /10",
                          br(),
                          strong("Satisfaction client : "),
                          data_cities$guest_satisfaction_overall, " /100"
                        ),
                        label = paste0(round(data_cities$Price,2), " €"
                        ),
                        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend("bottomright", 
                colors = c("lightgreen", "green", "beige", "orange", "#FF8778", "red"), 
                labels = c("< 100 €", "101-200 €", "201-300 €", "301-400 €", "401-500 €", "> 500 €"),
                title = "Prix par nuit",
                opacity = 1
      )
  })
  
  #Pour l'onglet "Dépôt annonce"
  #Création d'un outil réactif: ensemble donnée géographique, chq fois user modifie variable l'application exécutera des actions
  #On crée une variable réactive qui change quand l'user change la valeur de la variable
  city_filtered2 <- reactive({
    data_cities %>% 
      filter(City == input$Ville2 & 
               Weekdays_WE == input$Day2 & 
               room_type == input$Room2 & 
               person_capacity >= input$Capacity2
      )
  })
  
  #Création de la 2eme map (Prédiction)
  output$map2 = renderLeaflet({
    data_cities2 <- city_filtered2()
    
    #Définit les icones
    icons <- awesomeIcons(
      icon = 'house',
      iconColor = 'black',
      library = 'fa',
      markerColor = getColor(data_cities2)
    )
    
    #Affichage de la 2eme map (Prédiction)
    leaflet(data_cities2) %>%
      addTiles() %>%
      addAwesomeMarkers(lng = ~lng,
                        lat = ~lat,
                        icon = icons,
                        popup = paste0(
                          strong("Prix : "), 
                          round(data_cities2$Price,2), " €",
                          br(),
                          strong("Nombre de personnes max : "),
                          data_cities2$person_capacity,
                          br(),
                          strong("Propreté : "),
                          data_cities2$cleanliness_rating, " /10",
                          br(),
                          strong("Satisfaction client : "),
                          data_cities2$guest_satisfaction_overall, " /100"
                        ),
                        label = paste0(round(data_cities2$Price,2), " €"
                        ),
                        clusterOptions = markerClusterOptions()
      ) %>%
      addLegend("bottomright", 
                colors = c("lightgreen", "green", "beige", "orange", "#FF8778", "red"), 
                labels = c("< 100 €", "101-200 €", "201-300 €", "301-400 €", "401-500 €", "> 500 €"),
                title = "Prix par nuit",
                opacity = 1
      )
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
      lng = input$map2_click$lng,
      lat = input$map2_click$lat
    )
    
    # Prédiction du prix avec le modèle
    predict(trained_model, new_data)
  })
  
  # Afficher la prédiction
  output$predicted_price <- renderText({
    paste("Prix estimé :", round(predict_price(), 2), "€")
  })
  
}


# Run the application 
shinyApp(ui = ui, server = server)