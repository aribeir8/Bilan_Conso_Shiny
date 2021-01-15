# librairies

library(shiny)
library(dplyr)
library(shinydashboard)
library(DT)
library(tidyverse)
library(plotly)
library(ggplot2)

# preparation des donnees

setwd("C:/Users/alexa/OneDrive/Documents/Cours/Ravanceu")

conso_reg <- read.csv("consommation-electrique-par-secteur-dactivite-region.csv", sep=";", fileEncoding = "UTF-8")
conso_dep <- read.csv("consommation-electrique-par-secteur-dactivite-departement.csv", sep=";", fileEncoding = "UTF-8")
conso_com <- read.csv("consommation-electrique-par-secteur-dactivite-commune.csv", sep=";", fileEncoding = "UTF-8")
prod_reg <- read.csv("production-electrique-par-filiere-a-la-maille-region.csv", sep=";", fileEncoding = "UTF-8")
prod_dep <- read.csv("production-electrique-par-filiere-a-la-maille-departement.csv", sep=";", fileEncoding = "UTF-8")
prod_com <- read.csv("production-electrique-par-filiere-a-la-maille-commune.csv", sep=";", fileEncoding = "UTF-8")

# Define UI for application that draws a histogram
ui <- dashboardPage(
    
    dashboardHeader(
        title="Bilan de consommation electrique",
        titleWidth=400
    ),
    
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("Resume", tabName = "Resume"),
            menuItem("Details", tabName = "Details")
        ),
        
        selectInput('Maille',
                    'Choisissez la collectivite',
                    choices = c('Moyenne france', 'Region', 'Departement', 'Commune')),
        
        ##ui conditionnelle
        uiOutput('ui_moyenne'),
        
        uiOutput('ui_region'),
        
        uiOutput('ui_departement'),
        
        uiOutput('ui_commune'),
        
        # Choix de l'annee 
        selectInput("annee",
                    "Choisissez votre annee:",
                    choices = sort(unique(conso_com$Année)),
                    multiple = TRUE),
        
        ##action button 
        actionButton(inputId = 'action','Lancer l analyse')
    ),
    
    dashboardBody(
        tabItems(
            # Premier onglet : Resume
            tabItem('Resume',
                    h5(textOutput('nom_commune'), 
                    dataTableOutput('ma_table',width = "40%"))
                    
                    # barplots conso moyenne et production
                    , plotOutput('consommation_production')
                    
                    , valueBox(value = textOutput('value_conso'),
                             subtitle = 'conso totale en TWh'
                             , icon = NULL, color = "aqua", width = 6,
                             href = NULL)
                    , valueBox(value = textOutput('value_prod'),
                             subtitle = 'prod totale en TWh'
                             , icon = NULL, color = "aqua", width = 6,
                             href = NULL)
                    , h5(plotOutput('graph_interactif'))
                    
            ),
            
            tabItem('Details',
                    h5(dataTableOutput('data_conso'))
                    
                    , downloadButton("downloadData", "Telecharger")
                    
                    , h5(dataTableOutput('data_prod'))
                    
                    , downloadButton("downloadData2", "Telecharger")
            )
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$ui_moyenne <-  renderUI({
        
        ##selectionner les bonnes communes
        if (input$Maille == "Departement"){
            selectInput("moyenne",
                        "Choisissez la ou les moyennes :",
                        choices = c('Moyenne Region'),
                        multiple = TRUE)
        }
        
        else if (input$Maille == "Commune"){
            selectInput("moyenne",
                        "Choisissez la ou les moyennes :",
                        choices = c('Moyenne Region', 'Moyenne Departement'),
                        multiple = TRUE)
        }
    })
    
    output$ui_region <-  renderUI({
        
        ##selectionner la bonne region
        if (input$Maille == "Region" | input$Maille == "Departement" | input$Maille == "Commune"){
        choix_possibles <- conso_reg %>%
            pull(Nom.Région) %>%
            unique()
        
        selectInput("region",
                    "Choisissez votre region:",
                    choices = sort(choix_possibles),
                    selected = choix_possibles[1])
        }
    })
    
    output$ui_departement <-  renderUI({
        
        ##selectionner les bons departements
        if (input$Maille == "Departement" | input$Maille == "Commune"){
            choix_possibles1 <- conso_dep %>% 
                filter(Nom.Région == input$region) %>%
                pull(Nom.Département) %>%
                unique()
            
            selectInput("dep",
                        "Choisissez votre departement:",
                        choices = sort(choix_possibles1),
                        selected = choix_possibles1[1])
        }
    })
    
    output$ui_commune <-  renderUI({
        
        ##selectionner les bonnes communes
        if (input$Maille == "Commune"){
            choix_possibles2 <- conso_com %>% 
                filter(Nom.Département == input$dep) %>%
                pull(Nom.Commune) %>%
                unique()
            
            selectInput("commune",
                        "Choisissez votre commune:",
                        choices = sort(choix_possibles2),
                        selected = choix_possibles2[1])
        }
    })
    
    filtre <- reactive({
        if (input$Maille == "Commune"){
            out <- conso_com %>%
                filter(Nom.Commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out <- conso_dep %>% 
                filter(Nom.Département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out <- conso_reg %>% 
                filter(Nom.Région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out <- conso_reg %>%
                filter(Année %in% input$annee)
        }
        out <- out %>%
            select(Année, CODE.GRAND.SECTEUR, Conso.totale..MWh.) %>%
            filter(Conso.totale..MWh. != "NA") %>%
            filter(Conso.totale..MWh. != 0) %>%
            group_by(Année, CODE.GRAND.SECTEUR) %>%
            summarise('Conso_totale' = sum(Conso.totale..MWh.)) %>%
            rename(Secteur = CODE.GRAND.SECTEUR)
            
        out
    })
    
    filtre_valuebox_tot <- reactive({
        if (input$Maille == "Commune"){
            out_valuebox_tot <- conso_com %>% 
                filter(Nom.Commune == input$commune) %>%
                filter(Nom.Département == input$dep) %>%
                filter(Nom.Région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_valuebox_tot <- conso_dep %>%
                filter(Nom.Département == input$dep) %>%
                filter(Nom.Région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_valuebox_tot <- conso_reg %>%
                filter(Nom.Région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_valuebox_tot <- conso_reg %>%
                filter(Année %in% input$annee)
        }
        out_valuebox_tot <- out_valuebox_tot %>%
            select(Année, Conso.totale..MWh.) %>%
            filter(Conso.totale..MWh. != "NA") %>%
            filter(Conso.totale..MWh. != 0) %>%
            summarise(sum(Conso.totale..MWh.)/1000000)
        
        out_valuebox_tot 
    })
    
    output$value_conso <- renderText({
        
        Act_valuebox_conso()
        
    })
    
    filtre_valuebox_tot2 <- reactive({
        if (input$Maille == "Commune"){
            out_valuebox_tot2 <- prod_com %>% 
                filter(Nom.commune == input$commune) %>%
                filter(Nom.département == input$dep) %>%
                filter(Nom.région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_valuebox_tot2 <- prod_dep %>% 
                filter(Nom.département == input$dep) %>%
                filter(Nom.région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_valuebox_tot2 <- prod_reg %>%
                filter(Nom.région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_valuebox_tot2 <- prod_reg %>%
                filter(Année %in% input$annee)
        }
        out_valuebox_tot2[is.na(out_valuebox_tot2)] <- 0
        out_valuebox_tot2 <- out_valuebox_tot2 %>%
            select(Année, Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                   Energie.produite.annuelle.Eolien.Enedis..MWh.,
                   Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                   Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                   Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                   Energie.produite.annuelle.Autres.filières.Enedis..MWh.)
        out_valuebox_tot2 <- mutate(out_valuebox_tot2, prod.totale = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.+
                                        Energie.produite.annuelle.Eolien.Enedis..MWh.+
                                        Energie.produite.annuelle.Hydraulique.Enedis..MWh.+
                                        Energie.produite.annuelle.Bio.Energie.Enedis..MWh.+
                                        Energie.produite.annuelle.Cogénération.Enedis..MWh.+
                                        Energie.produite.annuelle.Autres.filières.Enedis..MWh.)
        out_valuebox_tot2 <- out_valuebox_tot2 %>% summarise(sum(prod.totale)/1000000)
        
        out_valuebox_tot2 
    })
    
    output$value_prod <- renderText({
        
        Act_valuebox_prod()
        
    })
    
    filtrePlot <- reactive({
        outPlot <- filtre() %>%
            group_by(Secteur) %>%
            summarise('total_moyen_MWh' = round(mean(Conso_totale), 2))
            
        outPlot <- mutate(outPlot, Type = "Consommation")
        
        outPlot
    })
    
    filtre2 <- reactive({
        if (input$Maille == "Commune"){
            out2 <- prod_com %>% 
                filter(Nom.commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out2 <- prod_dep %>% 
                filter(Nom.département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out2 <- prod_reg %>% 
                filter(Nom.région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out2 <- prod_reg %>%
                filter(Année %in% input$annee)
        }
        
        out2 <- out2 %>%
            select(Année,
                   Energie.produite.annuelle.Autres.filières.Enedis..MWh.,
                   Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                   Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                   Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                   Energie.produite.annuelle.Eolien.Enedis..MWh.,
                   Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
            rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                   Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.,
                   Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                   Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                   Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                   Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.)
            out2[is.na(out2)] <- 0
        
        out2
    })
    
    filtrePlot2 <- reactive({
        outPlot2 <- filtre2() %>%
            group_by(Année) %>%
            summarise(Autres = sum(Autres),
                      Hydraulique = sum(Hydraulique),
                      Cogeneration = sum(Cogeneration),
                      Photovoltaique = sum(Photovoltaique),
                      Eolien = sum(Eolien),
                      Bio = sum(Bio)) %>%
            summarise(Autres = round(mean(Autres),2),
                      Hydraulique = round(mean(Hydraulique),2),
                      Cogeneration = round(mean(Cogeneration),2),
                      Photovoltaique = round(mean(Photovoltaique),2),
                      Eolien = round(mean(Eolien),2),
                      Bio = round(mean(Bio),2))
        
        outPlot2 <- t(outPlot2)
        names <- c("Autres","Hydraulique","Cogeneration","Photovoltaique","Eolien","Bio")
        produc <- outPlot2[,1]
        outPlot2 <- data.frame(Secteur = names, total_moyen_MWh = produc)
        outPlot2 <- mutate(outPlot2, Type = "Production")
        outPlot2
    })
    
    output$ma_table <- renderDataTable({
        Act_table()
    })
    
    filtreTable <- reactive({
        conso <- filtrePlot()
        production <- filtrePlot2()
        bar <- rbind(conso, production)
        
        bar <- bar %>% filter(total_moyen_MWh != 0)
        
        bar
    })
    
    output$consommation_production <- renderPlot({
        Action_barplot()
        
    })
    
    Action_barplot <- eventReactive(input$action,{
        
        bar <- filtreTable()
        
        ggplot(bar) + 
            geom_bar(stat="identity") +
            aes(x = Type ,y = total_moyen_MWh, fill = Secteur) +
            ggtitle("Consommation et production pour une annee moyenne de la collectivite")
        
    })
    
    Act_table <- eventReactive(input$action,{
        
        out <-  filtreTable()
        out
        
    })
    
    Act_valuebox_conso <- eventReactive(input$action,{
        
        out_valuebox_tot <-  round(filtre_valuebox_tot(),2)
        out_valuebox_tot <- as.character(out_valuebox_tot)
        out_valuebox_tot
        
    })
    
    Act_valuebox_prod <- eventReactive(input$action,{
        
        out_valuebox_tot2 <-  round(filtre_valuebox_tot2(),2)
        out_valuebox_tot2 <- as.character(out_valuebox_tot2)
        out_valuebox_tot2
        
    })
    
    filtre_graph_industrie <- reactive({
        
        if (input$Maille == "Commune"){
            out_int <- conso_com %>% 
                filter(Nom.Commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_int <- conso_dep %>% 
                filter(Nom.Département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_int <- conso_reg %>% 
                filter(Nom.Région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_int <- conso_reg %>%
                filter(Année %in% input$annee)
        }
        
        out_int[is.na(out_int)] <- 0
        out_int <- out_int %>%
            filter(CODE.GRAND.SECTEUR == "INDUSTRIE") %>%
            group_by(Année)%>%
            summarise(a = sum(Conso.totale..MWh.))
        out_int <- mutate(out_int, sect = 'industrie', type = 'consommation')
        out_int
    })
    
    filtre_graph_agriculture <- reactive({
        
        if (input$Maille == "Commune"){
            out_int <- conso_com %>% 
                filter(Nom.Commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_int <- conso_dep %>% 
                filter(Nom.Département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_int <- conso_reg %>% 
                filter(Nom.Région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_int <- conso_reg %>%
                filter(Année %in% input$annee)
        }
        
        out_int[is.na(out_int)] <- 0
        out_int <- out_int %>%
            filter(CODE.GRAND.SECTEUR == "AGRICULTURE") %>%
            group_by(Année)%>%
            summarise(a = sum(Conso.totale..MWh.))
        out_int <- mutate(out_int, sect = 'agriculture', type = 'consommation')
        out_int
    })
    
    filtre_graph_inconnu <- reactive({
        
        if (input$Maille == "Commune"){
            out_int <- conso_com %>% 
                filter(Nom.Commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_int <- conso_dep %>% 
                filter(Nom.Département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_int <- conso_reg %>% 
                filter(Nom.Région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_int <- conso_reg %>%
                filter(Année %in% input$annee)
        }
        
        out_int[is.na(out_int)] <- 0
        out_int <- out_int %>%
            filter(CODE.GRAND.SECTEUR == "INCONNU") %>%
            group_by(Année)%>%
            summarise(a = sum(Conso.totale..MWh.))
        out_int <- mutate(out_int, sect = 'inconnu', type = 'consommation')
        out_int
    })
    
    filtre_graph_petit <- reactive({
        
        if (input$Maille == "Commune"){
            out_int <- conso_com %>% 
                filter(Nom.Commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_int <- conso_dep %>% 
                filter(Nom.Département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_int <- conso_reg %>% 
                filter(Nom.Région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_int <- conso_reg %>%
                filter(Année %in% input$annee)
        }
        
        out_int[is.na(out_int)] <- 0
        out_int <- out_int %>%
            filter(CODE.GRAND.SECTEUR == "PETIT_PROFESSIONNEL") %>%
            group_by(Année)%>%
            summarise(a = sum(Conso.totale..MWh.))
        out_int <- mutate(out_int, sect = 'petit professionnel', type = 'consommation')
        out_int
    })
    
    filtre_graph_residentiel <- reactive({
        
        if (input$Maille == "Commune"){
            out_int <- conso_com %>% 
                filter(Nom.Commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_int <- conso_dep %>% 
                filter(Nom.Département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_int <- conso_reg %>% 
                filter(Nom.Région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_int <- conso_reg %>%
                filter(Année %in% input$annee)
        }
        
        out_int[is.na(out_int)] <- 0
        out_int <- out_int %>%
            filter(CODE.GRAND.SECTEUR == "RESIDENTIEL") %>%
            group_by(Année)%>%
            summarise(a = sum(Conso.totale..MWh.))
        out_int <- mutate(out_int, sect = 'residentiel', type = 'consommation')
        out_int
    })
    
    filtre_graph_tertiaire <- reactive({
        
        if (input$Maille == "Commune"){
            out_int <- conso_com %>% 
                filter(Nom.Commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_int <- conso_dep %>% 
                filter(Nom.Département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_int <- conso_reg %>% 
                filter(Nom.Région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_int <- conso_reg %>%
                filter(Année %in% input$annee)
        }
        
        out_int[is.na(out_int)] <- 0
        out_int <- out_int %>%
            filter(CODE.GRAND.SECTEUR == "TERTIAIRE") %>%
            group_by(Année)%>%
            summarise(a = sum(Conso.totale..MWh.))
        out_int <- mutate(out_int, sect = 'tertiaire', type = 'consommation')
        out_int
    })
    
    filtre_graph_interactif2_photo <- reactive({
        
        if (input$Maille == "Commune"){
            out_int2 <- prod_com %>% 
                filter(Nom.commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_int2 <- prod_dep %>% 
                filter(Nom.département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_int2 <- prod_reg %>% 
                filter(Nom.région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_int2 <- prod_reg %>%
                filter(Année %in% input$annee)
        }
        
        out_int2[is.na(out_int2)] <- 0
        out_int2 <- out_int2 %>%
            select(Année, Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.)
        out_int2 <- out_int2 %>%
            group_by(Année) %>%
            summarise(a = sum(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.))
        out_int2 <- mutate(out_int2, sect = 'photovoltaique', type = 'production')
    })
    
    filtre_graph_interactif2_eolien <- reactive({
        
        
        if (input$Maille == "Commune"){
            out_int2 <- prod_com %>% 
                filter(Nom.commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_int2 <- prod_dep %>% 
                filter(Nom.département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_int2 <- prod_reg %>% 
                filter(Nom.région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_int2 <- prod_reg %>%
                filter(Année %in% input$annee)
        }
        
        out_int2[is.na(out_int2)] <- 0
        out_int2 <- out_int2 %>%
            select(Année,Energie.produite.annuelle.Eolien.Enedis..MWh.)
        out_int2 <- out_int2 %>%
            group_by(Année) %>%
            summarise(a = sum(Energie.produite.annuelle.Eolien.Enedis..MWh.))
        out_int2 <- mutate(out_int2, sect = 'eolien', type = 'production')
    })
    
    filtre_graph_interactif2_hydro <- reactive({
        
        
        if (input$Maille == "Commune"){
            out_int2 <- prod_com %>% 
                filter(Nom.commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_int2 <- prod_dep %>% 
                filter(Nom.département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_int2 <- prod_reg %>% 
                filter(Nom.région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_int2 <- prod_reg %>%
                filter(Année %in% input$annee)
        }
        
        out_int2[is.na(out_int2)] <- 0
        out_int2 <- out_int2 %>%
            select(Année,Energie.produite.annuelle.Hydraulique.Enedis..MWh.)
        out_int2 <- out_int2 %>%
            group_by(Année) %>%
            summarise(a = sum(Energie.produite.annuelle.Hydraulique.Enedis..MWh.))
        out_int2 <- mutate(out_int2, sect = 'hydraulique', type = 'production')
    })
    
    filtre_graph_interactif2_bio <- reactive({
        
        
        if (input$Maille == "Commune"){
            out_int2 <- prod_com %>% 
                filter(Nom.commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_int2 <- prod_dep %>% 
                filter(Nom.département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_int2 <- prod_reg %>% 
                filter(Nom.région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_int2 <- prod_reg %>%
                filter(Année %in% input$annee)
        }
        
        out_int2[is.na(out_int2)] <- 0
        out_int2 <- out_int2 %>%
            select(Année,Energie.produite.annuelle.Bio.Energie.Enedis..MWh.)
        out_int2 <- out_int2 %>%
            group_by(Année) %>%
            summarise(a = sum(Energie.produite.annuelle.Bio.Energie.Enedis..MWh.))
        out_int2 <- mutate(out_int2, sect = 'bio', type = 'production')
    })
    
    filtre_graph_interactif2_co <- reactive({
        
        
        if (input$Maille == "Commune"){
            out_int2 <- prod_com %>% 
                filter(Nom.commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_int2 <- prod_dep %>% 
                filter(Nom.département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_int2 <- prod_reg %>% 
                filter(Nom.région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_int2 <- prod_reg %>%
                filter(Année %in% input$annee)
        }
        
        out_int2[is.na(out_int2)] <- 0
        out_int2 <- out_int2 %>%
            select(Année,Energie.produite.annuelle.Cogénération.Enedis..MWh.)
        out_int2 <- out_int2 %>%
            group_by(Année) %>%
            summarise(a = sum(Energie.produite.annuelle.Cogénération.Enedis..MWh.))
        out_int2 <- mutate(out_int2, sect = 'cogeneration', type = 'production')
    })
    
    filtre_graph_interactif2_autre <- reactive({
        
        
        if (input$Maille == "Commune"){
            out_int2 <- prod_com %>% 
                filter(Nom.commune == input$commune) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Departement"){
            out_int2 <- prod_dep %>% 
                filter(Nom.département == input$dep) %>%
                filter(Année %in% input$annee)
        }
        else if (input$Maille == "Region"){
            out_int2 <- prod_reg %>% 
                filter(Nom.région == input$region) %>%
                filter(Année %in% input$annee)
        }
        else{
            out_int2 <- prod_reg %>%
                filter(Année %in% input$annee)
        }
        
        out_int2[is.na(out_int2)] <- 0
        out_int2 <- out_int2 %>%
            select(Année,Energie.produite.annuelle.Autres.filières.Enedis..MWh.)
        out_int2 <- out_int2 %>%
            group_by(Année) %>%
            summarise(a = sum(Energie.produite.annuelle.Autres.filières.Enedis..MWh.))
        out_int2 <- mutate(out_int2, sect = 'autres', type = 'production')
    })
    
    output$graph_interactif <- renderPlot({
        Act_evolution()
    })
    
    Act_evolution <- eventReactive(input$action,{
        
        pl_industrie <- filtre_graph_industrie()
        pl_agriculture <- filtre_graph_agriculture()
        pl_inconnu <- filtre_graph_inconnu()
        pl_petit <- filtre_graph_petit()
        pl_residentiel <- filtre_graph_residentiel()
        pl_tertiaire <- filtre_graph_tertiaire()
        pl2_autre <- filtre_graph_interactif2_autre()
        pl2_co <- filtre_graph_interactif2_co()
        pl2_bio <- filtre_graph_interactif2_bio()
        pl2_hydro <- filtre_graph_interactif2_hydro()
        pl2_eolien <- filtre_graph_interactif2_eolien()
        pl2_photo <- filtre_graph_interactif2_photo()
        
        data <- rbind(pl_industrie,
                      pl_agriculture,
                      pl_inconnu,
                      pl_petit,
                      pl_residentiel,
                      pl_tertiaire,
                      pl2_autre,
                      pl2_co,
                      pl2_bio,
                      pl2_hydro,
                      pl2_eolien,
                      pl2_photo)
        
        print(data)
        
        ggplot(data=data, aes(x = Année, y = a)) +
            geom_line(aes(group = sect, colour = sect), size=1.2) +
            ggtitle("Evolution de la consommation et de la production au fil des annees") +
            ylab("MWh") + xlab("Annee") +
            facet_grid(. ~ type)
        
    })
    
    filtre_data_conso <- reactive({
        
        if(input$Maille == "Commune"){
            
            data_conso <- conso_com %>%
                filter(Nom.Région == input$region) %>%
                filter(Nom.Département == input$dep) %>%
                filter(Nom.Commune == input$commune) %>%
                filter(Année %in% input$annee) %>%
                filter(Conso.totale..MWh. != "NA") %>%
                group_by(CODE.GRAND.SECTEUR) %>%
                summarise(Conso_tot_moy_commune_MWh = round(mean(Conso.totale..MWh.), 2)) %>%
                rename(Secteur = CODE.GRAND.SECTEUR)
            
            if ("Moyenne Departement" %in% input$moyenne){
                
                moyenne_departement <- conso_com %>%
                    filter(Nom.Région == input$region) %>%
                    filter(Nom.Département == input$dep) %>%
                    filter(Année %in% input$annee) %>%
                    filter(Conso.totale..MWh. != "NA") %>%
                    group_by(CODE.GRAND.SECTEUR) %>%
                    summarise(Conso_tot_moy_communes_in_dep_MWh = round(mean(Conso.totale..MWh.), 2)) %>%
                    select(Conso_tot_moy_communes_in_dep_MWh)
                
                data_conso <- cbind(data_conso, moyenne_departement)
                
            }
            
            if ("Moyenne Region" %in% input$moyenne){
                
                moyenne_region <- conso_com %>%
                    filter(Nom.Région == input$region) %>%
                    filter(Année %in% input$annee) %>%
                    filter(Conso.totale..MWh. != "NA") %>%
                    group_by(CODE.GRAND.SECTEUR) %>%
                    summarise(Conso_tot_moy_communes_in_reg_MWh = round(mean(Conso.totale..MWh.), 2)) %>%
                    select(Conso_tot_moy_communes_in_reg_MWh)
                
                data_conso <- cbind(data_conso, moyenne_region)
                
            }
            
            moyenne_france <- conso_com %>%
                filter(Année %in% input$annee) %>% 
                filter(Conso.totale..MWh. != "NA") %>%
                group_by(CODE.GRAND.SECTEUR) %>%
                summarise(Moyenne_france_MWh = round(mean(Conso.totale..MWh.), 2)) %>%
                select(Moyenne_france_MWh)
            
            data_conso <- cbind(data_conso, moyenne_france)
            
            data_conso <- mutate(data_conso, ecart_moy_pourcent =  round(Conso_tot_moy_commune_MWh*100/Moyenne_france_MWh,2))
            
        }
        
        else if(input$Maille == "Departement"){
            
            data_conso <- conso_dep %>%
                filter(Nom.Région == input$region) %>%
                filter(Nom.Département == input$dep) %>%
                filter(Année %in% input$annee) %>%
                filter(Conso.totale..MWh. != "NA") %>%
                group_by(CODE.GRAND.SECTEUR) %>%
                summarise(Conso_tot_moy_dep_MWh = round(mean(Conso.totale..MWh.), 2)) %>%
                rename(Secteur = CODE.GRAND.SECTEUR)
            
            if ("Moyenne Region" %in% input$moyenne){
                
                moyenne_region <- conso_dep %>%
                    filter(Nom.Région == input$region) %>%
                    filter(Année %in% input$annee) %>%
                    filter(Conso.totale..MWh. != "NA") %>%
                    group_by(CODE.GRAND.SECTEUR) %>%
                    summarise(Conso_tot_moy_communes_in_reg_MWh = round(mean(Conso.totale..MWh.), 2)) %>%
                    select(Conso_tot_moy_communes_in_reg_MWh)
                
                data_conso <- cbind(data_conso, moyenne_region)
                
            }
            
            moyenne_france <- conso_dep %>%
                filter(Année %in% input$annee) %>% 
                filter(Conso.totale..MWh. != "NA") %>%
                group_by(CODE.GRAND.SECTEUR) %>%
                summarise(Moyenne_france_MWh = round(mean(Conso.totale..MWh.), 2)) %>%
                select(Moyenne_france_MWh)
            
            data_conso <- cbind(data_conso, moyenne_france)
            
            data_conso <- mutate(data_conso, ecart_moy_pourcent =  round(Conso_tot_moy_dep_MWh*100/Moyenne_france_MWh,2))
            
        }
        
        else if(input$Maille == "Region"){
            
            data_conso <- conso_reg %>%
                filter(Nom.Région == input$region) %>%
                filter(Année %in% input$annee) %>%
                filter(Conso.totale..MWh. != "NA") %>%
                group_by(CODE.GRAND.SECTEUR) %>%
                summarise(Conso_tot_moy_reg_MWh = round(mean(Conso.totale..MWh.), 2)) %>%
                rename(Secteur = CODE.GRAND.SECTEUR)
            
            moyenne_france <- conso_reg %>%
                filter(Année %in% input$annee) %>% 
                filter(Conso.totale..MWh. != "NA") %>%
                group_by(CODE.GRAND.SECTEUR) %>%
                summarise(Moyenne_france_MWh = round(mean(Conso.totale..MWh.), 2)) %>%
                select(Moyenne_france_MWh)
            
            data_conso <- cbind(data_conso, moyenne_france)
            
            data_conso <- mutate(data_conso, ecart_moy_pourcent = round(Conso_tot_moy_reg_MWh*100/Moyenne_france_MWh,2))
            
        }
        
        else{
            data_conso <- conso_reg %>%
                filter(Année %in% input$annee) %>%
                filter(Conso.totale..MWh. != "NA") %>%
                group_by(CODE.GRAND.SECTEUR) %>%
                summarise(Moyenne_france_MWh = round(mean(Conso.totale..MWh.), 2)) %>%
                rename(Secteur = CODE.GRAND.SECTEUR)
        }
        
    })
    
    Action_data_conso <- eventReactive(input$action,{
        
        out_data_conso <- filtre_data_conso()
        
    })
    
    output$data_conso <- renderDataTable({
        
        Action_data_conso()
        
    })
    
    filtre_data_prod <- reactive({
        
        if(input$Maille == "Commune"){
            
            data_prod <- prod_com %>%
                filter(Nom.région == input$region) %>%
                filter(Nom.département == input$dep) %>%
                filter(Nom.commune == input$commune) %>%
                filter(Année %in% input$annee) %>%
                select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                       Energie.produite.annuelle.Eolien.Enedis..MWh.,
                       Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                       Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                       Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                       Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
                rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
                rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
                rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
                rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
                rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
                rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                summarise(Photovoltaique = round(mean(Photovoltaique),2),
                          Eolien = round(mean(Eolien),2),
                          Hydraulique = round(mean(Hydraulique),2),
                          Bio = round(mean(Bio),2),
                          Cogeneration = round(mean(Cogeneration),2),
                          Autres = round(mean(Autres),2))
            
            data_prod <- t(data_prod)
            colnames(data_prod)[1] <- "prod_moy_com"
            
            
            if ("Moyenne Departement" %in% input$moyenne){
                
                moyenne_departement <- prod_com %>%
                    filter(Nom.région == input$region) %>%
                    filter(Nom.département == input$dep) %>%
                    filter(Année %in% input$annee) %>%
                    select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                           Energie.produite.annuelle.Eolien.Enedis..MWh.,
                           Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                           Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                           Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                           Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                    filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
                    rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
                    rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
                    rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
                    rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
                    rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
                    rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                    summarise(Photovoltaique = round(mean(Photovoltaique),2),
                              Eolien = round(mean(Eolien),2),
                              Hydraulique = round(mean(Hydraulique),2),
                              Bio = round(mean(Bio),2),
                              Cogeneration = round(mean(Cogeneration),2),
                              Autres = round(mean(Autres),2))
                
                moyenne_departement <- t(moyenne_departement)
                colnames(moyenne_departement)[1] <- "prod_moy_com_in_dep"
                
                data_prod <- cbind(data_prod, moyenne_departement)
                
            }
            
            if ("Moyenne Region" %in% input$moyenne){
                
                moyenne_region <- prod_com %>%
                    filter(Nom.région == input$region) %>%
                    filter(Année %in% input$annee) %>%
                    select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                           Energie.produite.annuelle.Eolien.Enedis..MWh.,
                           Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                           Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                           Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                           Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                    filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
                    rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
                    rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
                    rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
                    rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
                    rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
                    rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                    summarise(Photovoltaique = round(mean(Photovoltaique),2),
                              Eolien = round(mean(Eolien),2),
                              Hydraulique = round(mean(Hydraulique),2),
                              Bio = round(mean(Bio),2),
                              Cogeneration = round(mean(Cogeneration),2),
                              Autres = round(mean(Autres),2))
                
                moyenne_region <- t(moyenne_region)
                colnames(moyenne_region)[1] <- "prod_moy_com_in_reg"
                
                data_conso <- cbind(data_prod, moyenne_region)
                
            }
            
            moyenne_france <- prod_com %>%
                filter(Année %in% input$annee) %>%
                select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                       Energie.produite.annuelle.Eolien.Enedis..MWh.,
                       Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                       Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                       Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                       Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
                rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
                rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
                rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
                rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
                rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
                rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                summarise(Photovoltaique = round(mean(Photovoltaique),2),
                          Eolien = round(mean(Eolien),2),
                          Hydraulique = round(mean(Hydraulique),2),
                          Bio = round(mean(Bio),2),
                          Cogeneration = round(mean(Cogeneration),2),
                          Autres = round(mean(Autres),2))
            
            moyenne_france <- t(moyenne_france)
            colnames(moyenne_france)[1] <- "Moyenne_france"
            
            data_prod <- cbind(Secteur = rownames(data_prod), data_prod, moyenne_france)
            
            data_prod <- mutate(data.frame(data_prod),
                                ecart_moy_pourcent = round(as.numeric(prod_moy_com)*100/as.numeric(Moyenne_france),2))
            
        }
        
        else if(input$Maille == "Departement"){
            
            data_prod <- prod_dep %>%
                filter(Nom.région == input$region) %>%
                filter(Nom.département == input$dep) %>%
                filter(Année %in% input$annee) %>%
                select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                       Energie.produite.annuelle.Eolien.Enedis..MWh.,
                       Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                       Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                       Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                       Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
                rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
                rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
                rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
                rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
                rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
                rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                summarise(Photovoltaique = round(mean(Photovoltaique),2),
                          Eolien = round(mean(Eolien),2),
                          Hydraulique = round(mean(Hydraulique),2),
                          Bio = round(mean(Bio),2),
                          Cogeneration = round(mean(Cogeneration),2),
                          Autres = round(mean(Autres),2))
            
            data_prod <- t(data_prod)
            
            colnames(data_prod)[1] <- "prod_moy_dep"
            
            if ("Moyenne Region" %in% input$moyenne){
                
                moyenne_region <- prod_dep %>%
                    filter(Nom.région == input$region) %>%
                    filter(Année %in% input$annee) %>%
                    select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                           Energie.produite.annuelle.Eolien.Enedis..MWh.,
                           Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                           Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                           Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                           Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                    filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
                    filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
                    rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
                    rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
                    rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
                    rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
                    rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
                    rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                    summarise(Photovoltaique = round(mean(Photovoltaique),2),
                              Eolien = round(mean(Eolien),2),
                              Hydraulique = round(mean(Hydraulique),2),
                              Bio = round(mean(Bio),2),
                              Cogeneration = round(mean(Cogeneration),2),
                              Autres = round(mean(Autres),2))
                
                moyenne_region <- t(moyenne_region)
                
                colnames(moyenne_region)[1] <- "prod_moy_dep_in_reg"
                
                data_prod <- cbind(data_prod, moyenne_region)
                
            }
            
            moyenne_france <- prod_dep %>%
                filter(Année %in% input$annee) %>%
                select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                       Energie.produite.annuelle.Eolien.Enedis..MWh.,
                       Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                       Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                       Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                       Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
                rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
                rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
                rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
                rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
                rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
                rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                summarise(Photovoltaique = round(mean(Photovoltaique),2),
                          Eolien = round(mean(Eolien),2),
                          Hydraulique = round(mean(Hydraulique),2),
                          Bio = round(mean(Bio),2),
                          Cogeneration = round(mean(Cogeneration),2),
                          Autres = round(mean(Autres),2))
            
            moyenne_france <- t(moyenne_france)
            colnames(moyenne_france)[1] <- "Moyenne_france"
            
            data_prod <- cbind(Secteur = rownames(data_prod), data_prod, moyenne_france)
            
            data_prod <- mutate(data.frame(data_prod), 
                                ecart_moy_pourcent = round(as.numeric(prod_moy_dep)*100/as.numeric(Moyenne_france),2))
            
        }
        
        else if(input$Maille == "Region"){
            
            data_prod <- prod_reg %>%
                filter(Nom.région == input$region) %>%
                filter(Année %in% input$annee) %>%
                select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                       Energie.produite.annuelle.Eolien.Enedis..MWh.,
                       Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                       Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                       Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                       Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
                rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
                rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
                rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
                rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
                rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
                rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                summarise(Photovoltaique = round(mean(Photovoltaique),2),
                          Eolien = round(mean(Eolien),2),
                          Hydraulique = round(mean(Hydraulique),2),
                          Bio = round(mean(Bio),2),
                          Cogeneration = round(mean(Cogeneration),2),
                          Autres = round(mean(Autres),2))
            
            data_prod <- t(data_prod)
            
            colnames(data_prod)[1] <- "prod_moy_reg"
            
            moyenne_france <- prod_reg %>%
                filter(Année %in% input$annee) %>%
                select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                       Energie.produite.annuelle.Eolien.Enedis..MWh.,
                       Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                       Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                       Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                       Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
                rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
                rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
                rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
                rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
                rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
                rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                summarise(Photovoltaique = round(mean(Photovoltaique),2),
                          Eolien = round(mean(Eolien),2),
                          Hydraulique = round(mean(Hydraulique),2),
                          Bio = round(mean(Bio),2),
                          Cogeneration = round(mean(Cogeneration),2),
                          Autres = round(mean(Autres),2))
            
            moyenne_france <- t(moyenne_france)
            
            colnames(moyenne_france)[1] <- "Moyenne_france"
            
            data_prod <- cbind(Secteur = rownames(data_prod), data_prod, moyenne_france)
            
            data_prod <- mutate(data.frame(data_prod),
                                ecart_moy_pourcent = round(as.numeric(prod_moy_reg)*100/as.numeric(Moyenne_france),2))
            
        }
        
        else{
            data_prod <- prod_reg %>%
                filter(Année %in% input$annee) %>%
                select(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.,
                       Energie.produite.annuelle.Eolien.Enedis..MWh.,
                       Energie.produite.annuelle.Hydraulique.Enedis..MWh.,
                       Energie.produite.annuelle.Bio.Energie.Enedis..MWh.,
                       Energie.produite.annuelle.Cogénération.Enedis..MWh.,
                       Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                filter(Energie.produite.annuelle.Photovoltaïque.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Eolien.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Hydraulique.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Bio.Energie.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Cogénération.Enedis..MWh. != "NA") %>%
                filter(Energie.produite.annuelle.Autres.filières.Enedis..MWh. != "NA") %>%
                rename(Photovoltaique = Energie.produite.annuelle.Photovoltaïque.Enedis..MWh.) %>%
                rename(Eolien = Energie.produite.annuelle.Eolien.Enedis..MWh.) %>%
                rename(Hydraulique = Energie.produite.annuelle.Hydraulique.Enedis..MWh.) %>%
                rename(Bio = Energie.produite.annuelle.Bio.Energie.Enedis..MWh.) %>%
                rename(Cogeneration = Energie.produite.annuelle.Cogénération.Enedis..MWh.) %>%
                rename(Autres = Energie.produite.annuelle.Autres.filières.Enedis..MWh.) %>%
                summarise(Photovoltaique = round(mean(Photovoltaique),2),
                          Eolien = round(mean(Eolien),2),
                          Hydraulique = round(mean(Hydraulique),2),
                          Bio = round(mean(Bio),2),
                          Cogeneration = round(mean(Cogeneration),2),
                          Autres = round(mean(Autres),2))
            
            data_prod <- t(data_prod)
            
            data_prod <- cbind(Secteur = rownames(data_prod), data_prod)
            
            rownames(data_prod) <- c(1,2,3,4,5,6)
            
            colnames(data_prod)[2] <- "Moyenne_france"
        }
        
        data_prod
    })
    
    Action_data_prod <- eventReactive(input$action,{
        
        out_data_prod <- filtre_data_prod()
        
    })
    
    output$data_prod <- renderDataTable({
       
        Action_data_prod()
        
    })
    
    output$downloadData <- downloadHandler(
        
        filename = function() {
            paste("data_conso", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(filtre_data_conso(), file, row.names = FALSE)
        }
    )
    
    output$downloadData2 <- downloadHandler(
        
        filename = function() {
            paste("data_prod", ".csv", sep = "")
        },
        content = function(file) {
            write.csv(filtre_data_prod(), file, row.names = FALSE)
        }
    )
    
}

# Run the application 
shinyApp(ui = ui, server = server)