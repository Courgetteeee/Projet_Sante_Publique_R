#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Les packages
library(dplyr)
library(ggplot2)
library(purrr)
library(RColorBrewer)
library(readxl)
library(shiny)
library(stringr)
library(tidyr)
library(sf)


# Import tables Clara :
medecin_long <- readRDS("../donnees_traitees/medecin_long.rds")
inf_lib_long <- readRDS("../donnees_traitees/inf_lib_long.rds")
inf_sal_long <- readRDS("../donnees_traitees/inf_sal_long.rds")
inf_lib_sal <- bind_rows(inf_lib_long, inf_sal_long)

# Import tables Karla : 
morta_cs<-readRDS("../donnees_traitees/morta_cs.rds")
morta_dip<-readRDS("../donnees_traitees/morta_dip.rds")
mortalite_cause<-readRDS("../donnees_traitees/mortalite_cause.rds")
mortalite_combine<-readRDS("../donnees_traitees/mortalite_combine.rds")
mortalite_perinatale<-readRDS("../donnees_traitees/mortalite_perinatale.rds")

# Import tables Cindy :
donnees_apl_generalistes <- readRDS("../donnees_traitees/donnees_propres_apl_generalistes.rds")
communes_avec_geo_apl_gener <- donnees_apl_generalistes$communes
regions_apl_gener <- donnees_apl_generalistes$regions


# ---------------------------- UI -----------------------------------


# Define UI for application that draws a histogram
ui <- navbarPage(
  "Santé publique sur le territoire",

  #UI de Clara
  tabPanel("Offre de soin et de prévention d’un territoire",
    tabsetPanel(
      
      tabPanel("Evolution des effectifs chez les medecins",
        sidebarLayout(
            sidebarPanel(
              selectInput(inputId="departement", label="Choisir un département :", choices=sort(unique(medecin_long$departement)), 
                          selected=medecin_long$departement[1]),
              selectInput(inputId="specialites", label="Choisir une spécialités :", choices=sort(unique(medecin_long$specialites)), 
                          selected=medecin_long$specialites[1]),
              downloadLink('downloadData', 'Download')
            ),
    
            # Show a plot of the generated distribution
            mainPanel(
              plotOutput("effectifs_medecin")
            )
          )
        ),
      
      tabPanel("Evolution des effectifs chez les infirmiers",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId="departement2", label="Choisir un département :", 
                               choices=sort(unique(inf_lib_sal$departement)), 
                               selected=inf_lib_sal$departement[1]),
                   downloadLink('downloadData', 'Download')
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   plotOutput("effectifs_infirmiers")
                 )
               )
            )
      )
    ),
  
  #UI de Karla
  tabPanel("Études de la mortalité sur le territoire",
           
    tabsetPanel(
             
     tabPanel("Mortalité Périnatale",
        fluidPage(
          plotOutput("mortalite_peri_reg")
        )
     ),
             
     tabPanel("Mortalité et causes par région",
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId="Cause", label="Choisir la cause du décès :", 
                              choices=mortalite_cause$variable),
                ),
                mainPanel(
                  plotOutput("mortalite_cause_bar")
                )
              )
     ),
     tabPanel("Indicateur de mortalité selon le diplôme ",
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId="Indicateur", label="Choisir l'indicateur a représenter :", 
                              choices=morta_dip$indicateur),
                  selectInput(inputId="Annees", label="Années :", 
                              choices=morta_dip$int_annee),
                ),
                mainPanel(
                  plotOutput("mortalite_diplome")
                )
              )
     ),      
     tabPanel("Indicateur de mortalité selon la classe sociale ",
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId="Indicateur_cs", label="Choisir l'indicateur a représenter :", 
                              choices=morta_cs$indicateur),
                  selectInput(inputId="Annees_cs", label="Années :", 
                              choices=morta_cs$int_annee),
                ),
                mainPanel(
                  plotOutput("mortalite_classe")
                )
              )
     )
             
    )
  ),

  # IU de Cindy
  tabPanel("Accessibilité aux médecins généralistes (APL)",
           #Selection de l'année global en haut
           fluidRow(
             column(
               width = 3,
               selectInput(
                 inputId = "annee_choisie",
                 label = "Année",
                 choices = sort(unique(communes_avec_geo_apl_gener$annee),
                                decreasing = TRUE),
                 selected = max(communes_avec_geo_apl_gener$annee,
                                na.rm = TRUE)
               )
             )
           ),
           
           
           tabsetPanel(  
             #Onglet 1 : France entière
             tabPanel(
               #Titre de l'onglet
               "France entière",
               #break line : saut de ligne
               br(),
               #h4 : Titre niveau 4 (1 a 6)
               h4("Carte par région"),
               plotOutput("carte_france_gener", height = "600px")
             ),
             
             
             #Onglet 2 : Par région
             tabPanel(
               #Titre
               "Par région",
               #Saut de ligne
               br(),
               #Divise la page en deux colonnes
               sidebarLayout(
                 
                 #Zone side à gauche
                 sidebarPanel(
                   
                   #Menu déroulant
                   selectInput(
                     inputId = "region_choisie",
                     label = "Choisissez une région",
                     choices = { communes_avec_geo_apl_gener %>%
                         st_drop_geometry() %>%
                         pull(reg_name) %>% as.character() %>%
                         unique() %>% na.omit() %>% sort()
                     },
                     selected = "Bourgogne-Franche-Comté"),
                   
                   #Barre horizontale
                   hr(),
                   #titre niveau 5
                   h5("Informations"),
                   textOutput("info_region")
                 ),
                 
                 #Zone principale à droite
                 mainPanel( plotOutput("carte_region_gener", height = "600px")
                 )
               )
             ),
             
             #Onglet 3 : Evolution
             tabPanel(
               "Evolution 2022-2023",
               br(),
               h3("Evolution de l'APL moyen par région"),
               #plotOutput("graphique_evolution_apl_gener", height="580px")
             )
           )
    )
  )

# ---------------------------- SERVER -----------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  
  #Server de Clara
  
    # Affiche le bouton de téléchargement
    output$downloadData <- downloadHandler(filename = function(){
      paste('data-', Sys.Date(), '.csv', sep='')}, 
      content = function(con){
        write.csv(data, con)})
    
    # Graphe effectifs medecins
    output$effectifs_medecin <- renderPlot({
      medecin_long %>% filter(departement==input$departement, specialites==input$specialites) %>% 
      ggplot(aes(x=annee, y=effectif)) +
        geom_line(color="steelblue", linewidth=1.5) +
        geom_point(color="steelblue", size=3) +
        xlab("Année") +
        ylab("Effectif") +
        ggtitle("Évolution des effectifs") +
        theme_bw()
    })
    
    # Graphe effectifs infirmiers
    output$effectifs_infirmiers <- renderPlot({
      inf_lib_sal %>% filter(departement==input$departement2) %>%
      ggplot(aes(x = annee, y = effectif, col=data_type)) +
        geom_line() +
        facet_wrap(~data_type, scales = "free_y") +
        scale_x_continuous(breaks = unique(inf_lib_sal$annee)) +
        theme_minimal() +
        labs(title = "Effectifs par année")
    })
    
    #Server de Karla
    
    #Graphe mortalité périnatale
    output$mortalite_peri_reg<- renderPlot({
      mortalite_perinatale$Région <- factor(mortalite_perinatale$Région,levels = mortalite_perinatale$Région[order(mortalite_perinatale$valeur, decreasing = TRUE)])
      
      ggplot(mortalite_perinatale) +
        aes(x = Région, y = valeur) +
        geom_segment( aes(x=Région, xend=Région, y=0, yend=valeur), color="grey") +
        geom_point(color="darkorchid3", size = 2)+
        labs(y = "mortalité", 
             title = "Mortalité périnatale sur 1000 naissances") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45L, hjust=1))
    })
    
    #Graphe cause mortalité
    output$mortalite_cause_bar <- renderPlot({
      mortalite_cause%>% filter(variable==input$Cause) %>%
      ggplot(aes(x = Région, y = valeur)) +
        geom_col(fill = "#ACA3C8") +
        facet_wrap(~sexe)+
        labs(y = "taux de mortalité", 
             title = "Taux de mortalité standardisé pour 100 000 habitants selon la région") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45L, hjust = 1L))
    })
    
    #Graphe mortalité diplome
    output$mortalite_diplome <- renderPlot({
      morta_dip %>%
        filter(int_annee == input$Annees) %>%
        filter(indicateur == input$Indicateur) %>%
        ggplot() +
        aes(x = age, y = valeur_dip, colour = diplome) +
        geom_line() +
        scale_color_hue(direction = 1) +
        labs(x = "âge", y = "indic...", title = "Indicateur de mortalité en fonction de l'âge et du diplome", 
             color = "Diplôme") +
        facet_wrap(~sexe)+
        theme_minimal()

    })
    
    #Graphe mortalité classe sociale
    output$mortalite_classe <- renderPlot({
      
      morta_cs %>%
        filter(int_annee == input$Annees_cs) %>%
        filter(indicateur == input$Indicateur_cs) %>%
        ggplot() +
        aes(x = age, y = valeur_cs, colour = classe_sociale) +
        geom_line() +
        scale_color_brewer(palette = "Set2") +
        labs(x = "âge", y = "indic...", title = "Indicateur de mortalité en fonction de l'âge et de la classe sociale", 
             color = "Classe sociale") +
        facet_wrap(~sexe)+
        theme_minimal()
      
    })
    
    #Server de Cindy
    
    #### Données filtrer par année
    regions_filtre_apl_gener <- reactive({
      regions_apl_gener  %>% filter(annee==input$annee_choisie)
    })
    communes_filtre_apl_gener2 <- reactive({
      communes_avec_geo_apl_gener  %>% filter(annee==input$annee_choisie)
    })
    
    
    ###### Carte France entière
    output$carte_france_gener <- renderPlot ({
      ggplot(regions_filtre_apl_gener())+
        geom_sf(aes(fill=apl_moyen), color ="black", size=0.1)+
        scale_fill_viridis_c( option = "magma",
                              name = "APL moyen")+
        theme_minimal()+
        labs( title = "APL moyen par région",
              subtitle = "Médecins généralistes")
    })
    
    
    ####### Carte REGION séléctionné
    output$carte_region_gener <- renderPlot({
      
      ##Filter les communes de la région choisie
      region_filtree <- communes_filtre_apl_gener2() %>%
        filter( reg_name == input$region_choisie)
      
      ##Carte
      ggplot(region_filtree)+
        geom_sf( aes(fill = apl_generalistes), color=NA)+
        scale_fill_viridis_c(option = "magma", name = "APL")+
        theme_minimal()+
        labs( title = paste("APL par commune -", input$region_choisie),
              subtitle = paste0("Médecins généralistes ", input$annee_choisie))
    })
    
    
    #Information sur la région
    output$info_region <- renderText({
      
      region_info <- regions_filtre_apl_gener() %>%
        st_drop_geometry() %>%
        filter(reg_name == input$region_choisie)
      
      #Vérification des infos
      if (nrow(region_info)==0){
        return("Aucune données disponible pour cette région.")
      }
      
      apl <- as.numeric(region_info$apl_moyen)
      pop <- as.numeric(region_info$population_totale)
      
      paste0( "APL moyen : ", round(apl,2), "   ",
              "\nPopulation totale : ", format(pop, big.mark=" "))
    })
}

# ---------------------------- GLOBAL -----------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
