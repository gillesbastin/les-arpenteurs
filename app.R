# app.R Shiny code to produce Les Arpenteurs
# Based on a first try by Amine Gandriche et Hamza Moussafir
# Final code by Gilles Bastin

library(dplyr)
library(ggplot2)
library(leaflet)
library(htmlwidgets)
library(htmltools)
library(geojsonio)
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(RColorBrewer)
library(DT)
library(leaflet.extras)
library(shinyjs)
library(flexdashboard)
library(shinyBS)
library(shinyalert)
library(readr)
library(RColorBrewer)
library(pals)
library(lubridate)
library(scales)

setwd("~/zPublish/shiny/les-arpenteurs")

# READING DATA
reportages <- read.delim("data_reportages_final.tsv", header=TRUE, sep="\t", dec = ".")
reporters <- read.delim("data_reporters_final.tsv", header=TRUE, sep="\t", dec = ".")


# PROCESSING DATA
reportages <- reportages %>%
  filter(!is.na(reportages$LONGITUDE) | !is.na(reportages$LATITUDE)) %>%
  select(-c(A_LA_UNE,SUJET)) %>%
  mutate(REPORTER = as.character(reportages$REPORTER)) %>%
  mutate(JOURNAL = as.character(reportages$JOURNAL)) %>%
  mutate(DATE = as.Date(reportages$DATE, format ="%Y-%m-%d")) %>%
  mutate(DATE.Y = as.factor(substr(DATE, 0, 4))) %>%
  mutate(LATITUDE = as.numeric(as.character(reportages$LATITUDE))) %>%
  mutate(LONGITUDE = as.numeric(as.character(reportages$LONGITUDE))) %>%
  mutate(LIEU = as.character(reportages$LIEU)) %>%
  mutate(PAYS = as.character(reportages$PAYS)) %>%
  mutate(TITRE = as.character(reportages$TITRE)) %>%
  mutate(URL = as.character(reportages$URL))
attach(reportages)

reporters <- reporters %>%
  mutate(PHOTO = paste0("<img src='",PHOTO,"' alt='",REPORTER,"', width = '200'>"))

# BUILDING THE PAGE

ui <- dashboardPage(
  skin = "blue",
  header <- dashboardHeader(title = "Les Arpenteurs (bêta)", titleWidth = 300,
                            tags$li(class = "dropdown", tags$a(href="https://twitter.com/gillesbastin?lang=fr", tags$span(style="color:black", icon("twitter")), tags$span(style="color:black", "Fil d'actualité"), target = "_blank")),
                            tags$li(class = "dropdown", tags$a(href="https://github.com/gillesbastin/les-arpenteurs", tags$span(style="color:black", icon("github")), tags$span(style="color:black", "Code Source"), target = "_blank"))
                            ),
  
  
  # SIDEBAR
  
  sidebar <- dashboardSidebar(
    width = 300, sidebarMenu(
      menuItem("Carte", tabName = "myTabForMap", icon = icon("globe")), #tags$style(HTML('body {font-family:"Times New Roman",Georgia,Serif}')),
      menuItem("Statistiques", tabName = "myTabForStats", icon = icon("chart-bar")),
      menuItem("Données", tabName = "myTabForDataTable", icon = icon("th")),
      menuItem("Biographies", tabName = "myTabForBio", icon = icon("id-badge")),
      menuItem("À propos", tabName = "myTabForAbout", icon = icon("fa fa-compass")),
      br(),
      #div("Filtrer les données :", style = "margin-left:15px"),
      sliderInput("RangeInput", label = "Période", min = min(DATE), max = max(DATE), value = range(DATE), timeFormat = "%Y", step = 100),
      selectInput("ReporterInput", label = "Reporter", selected = "Tous", choices = c("Tous", unique(REPORTER)), multiple = F), # [Ce serait bien d'être en choix multiple mais si multiple = T il y a des erreurs]
      selectInput("JournalInput", label = "Journal", selected = "Tous", choices = c("Tous", unique(JOURNAL)), multiple = F), # [Ce serait bien d'être en choix multiple mais si multiple = T il y a des erreurs]
      selectInput("PaysInput", label = "Pays", selected = "Tous", choices = c("Tous", unique(PAYS)), multiple = F), # [Ce serait bien d'être en choix multiple mais si multiple = T il y a des erreurs]
      br(),
      actionButton("resetAll", "Réinitialiser")
    )
  ),
  
  # BODY
  
  body <- dashboardBody(
    theme_blue_gradient, tabItems(
      
      # ABOUT
      
      tabItem(
        "myTabForAbout",
        h4("Sur les traces des reporters de la presse française dans la première moitié du XXème siècle"),
        h5("Une cartographie collaborative du reportage dans la presse française au début du XXème siècle"),
        hr(),
        h4("Le projet"),
        "Le projet « Les arpenteurs » a pris naissance dans le cadre d'un cours d'Histoire des pratiques journalistiques donné à l'",
        tags$a(href="http://www.ejdg.fr/", "École de journalisme de Grenoble"),
        "(Université Grenoble Alpes / Sciences Po Grenoble) à partir de l'année 2017-2018.",
        "L'objectif de ce projet est de documenter et de cartographier de la manière la plus exhaustive possible 
        la façon dont les premières générations de reporters de la presse française ont parcouru le monde qui 
        s'ouvrait de plus en plus à eux, avec le perfectionnement des moyens de transport, l'intensification de la curiosité 
        du lectorat pour les récits de leurs voyages et l'augmentation des moyens des journaux qui les envoyaient en reportage.",
        "Les étudiant•e•s qui participent à ce cours ont contribué au projet en choisissant chacun un ou 
        une reporter dont ils ont recherché tous les reportages dans la presse française sur le portail",
        tags$a(href="https://www.retronews.fr/", "RetroNews."),
        "Ces données ont été nettoyées et complétées (notamment en recherchant dans le texte de l'article le lieu dans lequel 
        se trouve le reporter (en général une ville) puis en cherchant ses coordonnées géographiques).",
        "Elles ont été agrégées dans une base de données à laquelle ce site donne accès sous différentes formes.", 
        "Les étudiant•e•s de ce cours ont aussi écrit un portrait de leur reporter.",
        br(),
        h4("Mode d'emploi"),
        "Les reportages collectés dans le cadre de ce projet sont accessibles ici, via le menu de gauche, sous forme de carte, de graphiques ou de données brutes.
        Un onglet permet aussi d'accéder aux biographies des reporters.",
        "Plusieurs filtres permettent de restreindre les résultats affichés en fonction d'une période, d'un nom de reporter, d'un titre de journal ou d'un pays.",
        br(),
        "En choisissant un•e reporter il est donc possible d'observer l'ensemble des lieux qu'il ou elle a visités mais aussi 
        l'ensemble des journaux pour lesquels il ou elle a travaillé. La carte permet aussi de rendre visible l'évolution de l'attention 
        publique au monde à laquelle les journaux ont largement participé à cette période. L'évolution de la carte des reportages 
        dans les temps montre bien par exemple les pays, les régions ou les sujets qui ont tour à tour capté une part de cette attention.",
        br(),
        h4("Participer"),
        "Vous pouvez participer au projet « Les arpenteurs » de plusieurs manières :",
        br(),
        "- En suggérant le nom d'un•e reporter qui mériterait selon vous de figurer dans la base de données",
        br(),
        "- En faisant vous-même (avec un mode d'emploi) la recherche des informations concernant ce ou cette reporter",
        br(),
        "- En signalant les erreurs que vous constateriez sur ce site (mauvaises coordonnées géographiques d'une ville, 
        erreur d'attribution d'un reportage, lien hypertexte corrompu…)",
        br(),
        "- En contribuant à l'écriture du code qui permet d'afficher les données si vous avez des compétences en R et en Shiny",
        br(),
        "Dans tous les cas, prendre contact à l'adresse suivante :",
        tags$a(href = "mailto:gilles.bastin@iepg.fr", "gilles.bastin@iepg.fr"),
        h4("Crédits"),
        "Le projet « Les arpenteurs » est réalisé en partenariat avec le portail ",
        tags$a(href = "http://retronews.fr/", "RetroNews"),
        "de la BnF.",
        "Il a bénéficié du soutien du ",
        tags$a(href="https://data-institute.univ-grenoble-alpes.fr/", "Data Institute"),
        "de l'Université Grenoble Alpes.",
        br(),
        "La collecte des données a été assurée principalement par les étudiant•e•s de l'",
        tags$a(href="http://www.ejdg.fr/", "École de journalisme de Grenoble"),
        ": Clothilde Dumay (Alexandra David-Néel et Lucie Delarue-Mardrus), Céline Legay et Lysandra Chadefaux (Edouard Helsey),
        Pierre Mouny et Augustine Peny (Jean et Jérôme Tharaud), Alexandre Lepère et Leo Corcos (Gaston Leroux)…",
        br(),
        "La première version de la carte interactive a été réalisée en Shiny (R) par Amine Gandriche et Hamza Moussafir dans le cadre d'un projet tutoré du",
        tags$a(href="http://formations.univ-grenoble-alpes.fr/fr/catalogue/master-XB/sciences-humaines-et-sociales-SHS/master-mathematiques-et-informatique-appliquees-aux-sciences-humaines-et-sociales-miashs-program-master-mathematiques-informatique-appliquees-et-sciences-humaines-et-sociales/parcours-statistique-et-sciences-de-donnees-ssd-subprogram-parcours-statistiques-et-sciences-de-donnees-ssd.html", "Master MIASHS"),
        "de l'Université Grenoble Alpes)."
      ),
      
      # BIOGRAPHIES
      
      tabItem(
        "myTabForBio",
        h4("Sur les traces des reporters de la presse française dans la première moitié du XXème siècle"),
        h5("Utilisez le moteur de recherche pour filtrer les données."),
        br(),
        fluidPage(style = "font-size: 75%; width: 100%", dataTableOutput("myTabler"), autoWidth = TRUE)
      ),
      
      # DATA
      
      tabItem(
        "myTabForDataTable",
        h4("Sur les traces des reporters de la presse française dans la première moitié du XXème siècle"),
        h5("Utilisez le menu de gauche ou le moteur de recherche pour filtrer les données."),
        br(),
        fluidPage(style = "font-size: 75%; width: 100%", dataTableOutput("myTable"), autoWidth = TRUE)
      ),
      
      # STATISTIQUES
      
      tabItem(
        "myTabForStats",
        h4("Sur les traces des reporters de la presse française dans la première moitié du XXème siècle"),
        h5("Utilisez le menu de gauche pour filtrer les données."),
        br(),
        "La base de données Les arpenteurs compte aujourd'hui (",
        Sys.Date(),
        ") :",
        br(),br(),
        fluidRow(
          infoBox("Reportages", length(unique(reportages$URL)), icon = icon("map-marker"), width = 3),
          infoBox("Reporters", length(unique(reportages$REPORTER)), icon = icon("id-badge"), width = 3),
          infoBox("Journaux", length(unique(reportages$JOURNAL)), icon = icon("newspaper"), width = 3),
          infoBox("Pays", length(unique(reportages$PAYS)), icon = icon("globe"), width = 3)
        ),
        br(),
        tabsetPanel(type = "tabs",
                    tabPanel(title = "Timeline", plotOutput(outputId = "main_plot")),
                    tabPanel("Reporters", plotOutput(outputId = "rep_plot")),
                    tabPanel("Journaux", plotOutput(outputId = "jal_plot")),
                    tabPanel("Pays", plotOutput(outputId = "pays_plot", height = 900))
        )
      ),
      
      # CARTE
      
      tabItem(
        tabName = "myTabForMap",
        h4("Sur les traces des reporters de la presse française dans la première moitié du XXème siècle"),
        h5("Utilisez le menu de gauche pour filtrer les données. Les reportages sont signalés par des points de couleur regroupés 
           en grandes zones géographiques. Pour lire les informations concernant un reportage faites apparaître l'îcone grise 
           représentant un exemplaire de journal en zoomant sur la zone."),
        br(),
        #sliderInput("DateInput", width = "100%", label = "Choisir une date ou faire défiler", min(DATE), max(DATE), timeFormat = "%Y", value = min(DATE), step = 100, animate = animationOptions(interval = 200, loop = TRUE, playButton = NULL, pauseButton = NULL)),
        leafletOutput('map', width = "100%", height = 900),
        useShinyalert(),
        useShinyjs()
      )
    )
  )
)

# SERVER and INTERACTIVITY

server <- function(input, output,session) {
  colorpal <- reactive({
    colorNumeric(input$colors, DATE)
  })
  
  # REINITIALISATION
  
  observeEvent(input$resetAll, {
    shinyjs::reset("ReporterInput")
    shinyjs::reset("JournalInput")
    shinyjs::reset("PaysInput")
    #shinyjs::reset("RangeInput") [bug : provoque un message d'erreur et se réinitialise dans un format numérique au lieu de date]
  })
  
  # FILTERS
  
  filteredData <- reactive({
    req(input$ReporterInput)
    req(input$JournalInput)
    req(input$RangeInput)
    req(input$PaysInput)
    
    if (input$ReporterInput == "Tous") {filt1 <- quote(REPORTER != "@?><")}
    else {filt1 <- quote(REPORTER == input$ReporterInput)}
    if (input$JournalInput == "Tous") {filt2 <- quote(JOURNAL != "@?><")}
    else {filt2 <- quote(JOURNAL == input$JournalInput)}
    if (input$PaysInput == "Tous") {filt3 <- quote(JOURNAL != "@?><")}
    else {filt3 <- quote(PAYS == input$PaysInput)}
    filt4 <- quote(DATE >= input$RangeInput[1] & DATE <= input$RangeInput[2])
    reportages %>%
      filter_(filt1)%>%
      filter_(filt2)%>%
      filter_(filt3)%>%
      filter_(filt4)
  })
  
  # DATA DOWNLOAD BUTTON
  
  # output$downloadData <- downloadHandler(
  #   filename = function() {
  #     paste("reportages", ".csv", sep="")
  #   },
  #   content = function(file) {
  #     write.csv(filteredData(), file)
  #   }
  # )

  # REPORTAGES DATA TABLE
  
  output$myTable <- DT::renderDataTable({
    dat <- datatable(filteredData()[,c(1:9)], options = list(paging=T, pageLength = 25)) %>%
      formatStyle('REPORTER', color = 'black', backgroundColor = NULL, fontWeight = 'bold')
    return(dat)
  })
  
  # BIOS DATA TABLE
  
  output$myTabler <- DT::renderDataTable({
    dat <- datatable(reporters[,c(1,3,5:7)], options = list(paging = T, pageLength = 10), escape = FALSE) %>% # escape = FALSE permet d'interpréter le HTML dans le texte des bios
      formatStyle('REPORTER', color = 'black', backgroundColor = NULL, fontWeight = 'bold')
    return(dat)
  })

  # CHOIX DE LA PALETTE DE COULEURS
  
  factpal <- colorFactor("Paired", domain = NULL, JOURNAL)
  
  # CHOIX DE L'ICONE DE CHAQUE REPORTAGES
  # NB : SERAIT INTERESSANT D'AJUSTER LA COULEUR AU JOURNAL
  
  icons <- awesomeIcons(
     icon = 'ios-paper',
     markerColor = 'black',
     iconColor = 'white',
     library = 'ion'
   )
  
  # PRODUCTION DE LA CARTE
  
  output$map <- renderLeaflet({
    if(dim(filteredData())[1]==0){
      shinyalert("Oops!", "Il n'y a pas de reportages correspondant à vos choix", type = "warning")
      shinyjs::reset("ReporterInput")
      shinyjs::reset("JournalInput")
      shinyjs::reset("PaysInput")
      #shinyjs::reset("RangeInput") [même problème que plus haut]
    }
    
    leaflet(filteredData(),
            options = leafletOptions(zoomControl = F,
                                     minZoom = 2,
                                     maxZoom = 10,
                                     dragging = T)) %>%
      addProviderTiles(providers$CartoDB.Positron)%>%
      addResetMapButton() %>%
      #addSearchOSM() %>%
      addBootstrapDependency() %>%
      addCircleMarkers(
        data = filteredData(),
        lat = ~LATITUDE,
        lng = ~LONGITUDE,
        weight = 1,
        radius = 5,
        color = ~factpal(JOURNAL),
        fillOpacity = 0.5,
        label = NULL,
        popup = ~paste("Zoomez sur l'icône","<br>","du groupe de points","<br>","pour voir les articles")
        ) %>%
    addLegend("topright", pal = factpal, values = ~JOURNAL, 
              title = "Journal", opacity = .5) %>%
      addAwesomeMarkers(~LONGITUDE, ~LATITUDE, icon=icons, popup = ~paste("<strong>Reporter :</strong>",REPORTER,
                                                               "<br>",
                                                               "<strong>Date :</strong>",DATE,
                                                               "<br>",
                                                               "<strong>Lieu :</strong>",LIEU,
                                                               "<br>",
                                                               "<strong>Titre :</strong>",TITRE,
                                                               "<br>",
                                                               "<strong>Journal :</strong>",JOURNAL,
                                                               "<br>",
                                                               "<strong>Voir :</strong><a href=",URL,">",URL,"</a>",
                                                               "<br>"), label = NULL,
                 
                 clusterOptions = markerClusterOptions(),
                 options = popupOptions(closeButton = FALSE, color = ~factpal(JOURNAL),
                                        opacity = 0.6),
                 labelOptions = labelOptions(noHide = F,direction = "bottom",
                                             markerZoomAnimation = 2,
                                             style = list(
                                               "color" = ~factpal(JOURNAL),
                                               "font-family" = "serif",
                                               "font-style" = "normal",
                                               "box-shadow" = "3px 3px rgba(0,0,0,0.25)",
                                               "font-size" = " 1.2em",
                                               "border-style" = "inset",
                                               "border-color" = "rgba(0,0,0,0.5)"
                                             )))
  })
  
  # TIMELINE DE L'ONGLET STATS
  
  output$main_plot <- renderPlot({
    if(dim(filteredData())[1]==0){
      shinyalert("Oops!", "Il n'y a pas de reportages correspondant à vos choix", type = "warning")
      shinyjs::reset("ReporterInput")
      shinyjs::reset("JournalInput")
      #shinyjs::reset("RangeInput") [même problème que plus haut]
    }

    main <- ggplot(filteredData(), aes(DATE.Y, fill = JOURNAL))
    main +
      geom_histogram(stat = "count", binwidth = 1) +
      labs(x = "Années", y = "Nombre de reportages publiés") +
      theme(axis.text.x = element_text(angle=65, vjust=0.6))
  })
  
  # AUTRES GRAPHES DE CET ONGLET
  
  output$rep_plot <- renderPlot({
    if(dim(filteredData())[1]==0){
      shinyalert("Oops!", "Il n'y a pas de reportages correspondant à vos choix", type = "warning")
      shinyjs::reset("ReporterInput")
      shinyjs::reset("JournalInput")
      #shinyjs::reset("RangeInput") [même problème que plus haut]
    }
    reporter <- ggplot(filteredData(), aes(REPORTER))
    reporter +
      geom_bar(stat = "count") +
      labs(x = "Reporters", y = "Nombre de reportages publiés") +
      coord_flip()
  })
  
  output$jal_plot <- renderPlot({
    if(dim(filteredData())[1]==0){
      shinyalert("Oops!", "Il n'y a pas de reportages correspondant à vos choix", type = "warning")
      shinyjs::reset("ReporterInput")
      shinyjs::reset("JournalInput")
      #shinyjs::reset("RangeInput") [même problème que plus haut]
    }
    journal <- ggplot(filteredData(), aes(JOURNAL))
    journal +
      geom_bar(stat = "count") +
      labs(x = "Journaux", y = "Nombre de reportages publiés") +
      coord_flip()
  })
  
  output$pays_plot <- renderPlot({
    if(dim(filteredData())[1]==0){
      shinyalert("Oops!", "Il n'y a pas de reportages correspondant à vos choix", type = "warning")
      shinyjs::reset("ReporterInput")
      shinyjs::reset("JournalInput")
      #shinyjs::reset("RangeInput") [même problème que plus haut]
    }
    pays <- ggplot(filteredData(), aes(PAYS))
    pays +
      geom_bar(stat = "count") +
      labs(x = "Pays", y = "Nombre de reportages publiés") +
      coord_flip()
  })
  
}

# END

shinyApp(ui = ui, server = server)
