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
library(sf)
library(shiny)
library(shinycssloaders)
library(shinythemes)
library(stringr)
library(tidyr)

# Fonction de téléchargement des graphes
download_plot <- function(output, input=NULL, output_id, plot_name, name_pdf) {
  output[[output_id]] <- downloadHandler(
    filename = if (is.function(name_pdf)) {
      function() paste0(name_pdf(input), ".pdf")
    } else {
      function() paste0(name_pdf, ".pdf")
    },
    content = function(file) {
      ggsave(file, plot=plot_name(), device="pdf")

      # Notification si succès de téléchargement
      showNotification("Téléchargement réussi !", type="message", duration=5)
    }
  )
}

# Fonction de téléchargement des cartes
download_map <- function(output, input, output_id, plot_reactive, prefix) {
  output[[output_id]] <- downloadHandler(
    filename = function() {
      paste0(prefix, "_", input$annee_choisie, "_", Sys.Date(), ".png")
    },
    content = function(file) {
      ggsave(file, plot = plot_reactive())

      showNotification("Téléchargement réussi !", type="message", duration=5)
    }
  )
}

# Import tables Clara :
medecin_long <- readRDS("../donnees_traitees/medecin_long.rds")
inf_lib_long <- readRDS("../donnees_traitees/inf_lib_long.rds")
inf_sal_long <- readRDS("../donnees_traitees/inf_sal_long.rds")
inf_lib_sal <- bind_rows(inf_lib_long, inf_sal_long)
APL_med_long <- readRDS("../donnees_traitees/APL_med_long.rds")
APL_inf <- readRDS("../donnees_traitees/APL_inf.rds")

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
  theme = shinytheme("flatly"),
  
  #Titre onglet interractif
  tags$head(
    tags$style(HTML("
                    .navbar-default .navbar-nav > li > a:hover, .nav-tabs > li > a:hover{
                    background-color: #f0f7ff !important;
                    color : #667eea  !important;
                    transition: all 0.2s;")), 
  ),
                 

  #UI de Clara
  tabPanel(title = tagList(icon("user-md",
                                style = "font-size: 2em; margin-right:10px;"),
                           "Offre de soin et de prévention d’un territoire"
  ),
    tabsetPanel(
      
      tabPanel("Evolution des effectifs chez les medecins",
        sidebarLayout(
            sidebarPanel(
              selectInput(inputId="departement", label="Choisir un département :", choices=sort(unique(medecin_long$departement)), 
                          selected=medecin_long$departement[1]),
              selectInput(inputId="specialites", label="Choisir une spécialité :", choices=sort(unique(medecin_long$specialites)), 
                          selected=medecin_long$specialites[1]),
              downloadLink('downloadData_1', 'Télécharger')
            ),
    
            # Show a plot of the generated distribution
            mainPanel(
              plotOutput("effectifs_medecin")%>% withSpinner(color="#667eea")
            )
          )
        ),
      
      tabPanel("Evolution des effectifs chez les infirmiers",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId="departement2", label="Choisir un département :", 
                               choices=sort(unique(inf_lib_sal$departement)), 
                               selected=inf_lib_sal$departement[1]),
                   downloadLink('downloadData_2', 'Télécharger')
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   plotOutput("effectifs_infirmiers")%>% withSpinner(color="#667eea")
                 )
               )
            ),
      
      tabPanel("Décalage entre l’offre et le besoin de soins",
               sidebarLayout(
                 sidebarPanel(
                   selectInput(inputId="Commune", label="Choisir une Commune :", 
                               choices=sort(unique(APL_med_long$Commune)), 
                               selected=APL_med_long$Commune[1]),
                   downloadLink('downloadData_3', 'Télécharger graphe médecins'),
                   br(),
                   downloadLink('downloadData_4', 'Télécharger graphe infirmiers'),
                   div("Indicateurs d’accès aux soins (APL) :",
                     tags$ul(
                       tags$li("Médecins : nombre de consultations par habitant"),
                       tags$li("Infirmiers : équivalent temps plein pour 100 000 habitants")
                     )
                    )
                 ),
                 
                 # Show a plot of the generated distribution
                 mainPanel(
                   plotOutput("offre_besoin_med")%>% withSpinner(color="#667eea"),
                   plotOutput("offre_besoin_inf")%>% withSpinner(color="#667eea")
                 )
               )
            )
      
      )
    ),
  
  #UI de Karla
  tabPanel(title = tagList(icon("heartbeat",
                                style = "font-size: 2em; margin-right:10px;"),
                           "Études de la mortalité sur le territoire"
  ),
           
    tabsetPanel(
             
     tabPanel("Mortalité Périnatale",
        fluidPage(
          downloadLink('downloadData_5', 'Télécharger'),
          plotOutput("mortalite_peri_reg")%>% withSpinner(color="#667eea")
        )
     ),
             
     tabPanel("Mortalité et causes par région",
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId="Cause", label="Choisir la cause du décès :", 
                              choices=sort(unique(mortalite_cause$variable))),
                  downloadLink('downloadData_6', 'Télécharger')
                ),
                mainPanel(
                  plotOutput("mortalite_cause_bar")%>% withSpinner(color="#667eea")
                )
              )
     ),
     tabPanel("Indicateur de mortalité selon le diplôme ",
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId="Indicateur", label="Choisir l'indicateur a représenter :", 
                              choices=sort(unique(morta_dip$indicateur))),
                  selectInput(inputId="Annees", label="Années :", 
                              choices=sort(unique(morta_dip$int_annee))),
                  downloadLink('downloadData_7', 'Télécharger'),
                  div("Indicateurs mortalité :",
                      tags$ul(
                        tags$li("Quotient de mortalité : Mesure la probabilité à un certain âge, pour les personnes survivantes à cet âge, de décéder avant l'âge suivant."),
                        tags$li("Esprérance de vie : nombre moyen d'années restant à vivre au-delà de cet âge x"),
                        tags$li("Survie : Représente le nombre d’individus survivants à l’âge x dans une génération fictive de 100 000 naissances")
                        
                      )
                  )
                ),
                mainPanel(
                  plotOutput("mortalite_diplome")%>% withSpinner(color="#667eea")
                )
              )
     ),      
     tabPanel("Indicateur de mortalité selon la classe sociale ",
              sidebarLayout(
                sidebarPanel(
                  selectInput(inputId="Indicateur_cs", label="Choisir l'indicateur a représenter :", 
                              choices=sort(unique(morta_cs$indicateur))),
                  selectInput(inputId="Annees_cs", label="Années :", 
                              choices=sort(unique(morta_cs$int_annee))),
                  downloadLink('downloadData_8', 'Télécharger'),
                  div("Indicateurs mortalité :",
                      tags$ul(
                        tags$li("Quotient de mortalité : Mesure la probabilité à un certain âge, pour les personnes survivantes à cet âge, de décéder avant l'âge suivant."),
                        tags$li("Esprérance de vie : nombre moyen d'années restant à vivre au-delà de cet âge x"),
                        tags$li("Survie : Représente le nombre d’individus survivants à l’âge x dans une génération fictive de 100 000 naissances")
                        
                      )
                  )
                ),
                mainPanel(
                  plotOutput("mortalite_classe")%>% withSpinner(color="#667eea")
                )
              )
     )
             
    )
  ),

  # IU de Cindy
  tabPanel(
    title = tagList(icon("map-marked-alt", style = "font-size: 2em; margin-right:10px;"),
                    " Accessibilité aux médecins généralistes (APL)"
    ),
    
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
    
    #Info sur l'apl / Définition
    fluidRow(
      column(12,
             div(
               style="background: #e3f2fd; margin: 20px 0;border-radius: 5px;",
               icon("info-circle", style="color:#2196F3; margin-right: 10px;"),
               strong("Qu'est ce que l'APL ? "),
               "L'Accessibilité Potentielle Localisée mesure l'adéquation entre l'offre de médecins et
                      la demande de soins sur un territoire. Plus l'APL est elevé, meilleure est l'accessibilité."
             ))
    ),
    
    
    
    tabsetPanel(  
      #Onglet 1 : Par région
      tabPanel(
        #Titre
        "Carte régionale",
        #Saut de ligne
        br(),
        
        
        #Divise la page en deux colonnes
        fluidRow(
          
          #Colonne de gauche
          column(width=5,
                 #Menu déroulant
                 selectInput(
                   inputId = "region_choisie",
                   label = "Choisissez une région",
                   choices = { communes_avec_geo_apl_gener %>%
                       st_drop_geometry() %>%
                       pull(reg_name) %>% as.character() %>%
                       unique() %>% na.omit() %>% sort()
                   },
                   selected = "Bourgogne-Franche-Comté"
                 ),
                 
                 #Bouton dowload
                 downloadButton("download_carte_region", "Télécharger la carte",
                                style="background-color: #667eea; border:none;margin-bottom: 20px;"),
                 
                 br(),
                 
                 
                 #Indicateur 1 : Classement de la région
                 div(
                   style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                                  color: white; 
                                  padding: 30px 20px; 
                                  border-radius: 10px; 
                                  margin-bottom: 20px;
                                  box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                  min-height: 150px;
                                  text-align: center;",
                   h5("Classement national", style="margin-bottom: 0; font-size: 1.7em;"),
                   uiOutput("classement_region"),
                   p("Sur l'accessibilité aux médecins généralistes", style = "margin-bottom: 0;")
                 ),
                 
                 #Indicateur 2 : APL moyen
                 div(
                   style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                                  color: white; 
                                  padding: 30px 20px; 
                                  border-radius: 10px; 
                                  margin-bottom: 20px;
                                  box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                  min-height: 150px;
                                  text-align: center;",
                   h5("APL moyen de la région", style="margin-bottom: 0; font-size: 1.7em;"),
                   uiOutput("apl_region_selectionnee"),
                   p("Médecin généraliste", style = "margin-bottom: 0;")
                 ),
                 
                 #Indicateur 3 : Population de la région
                 div(
                   style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                                  color: white; 
                                  padding: 30px 20px; 
                                  border-radius: 10px; 
                                  margin-bottom: 20px;
                                  box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                  min-height: 150px;
                                  text-align: center;",
                   h5("Population totale de la région", style="margin-bottom: 0; font-size: 1.7em;"),
                   uiOutput("population_region"),
                   p("Habitants (recensement)", style = "margin-bottom: 0;")
                 )
                 
          ),
          
          #Colonne de droite : carte
          column(width=7,
                 h4("Carte par région", style="font-size: 2.2em;text-align: center;"),
                 plotOutput("carte_region_gener", height = "600px")%>% withSpinner(color="#667eea")
          )
        )
      ),
      
      
      #Onglet 2 : France entière
      tabPanel(
        #Titre de l'onglet
        "Carte de France",
        br(),
        
        fluidRow(
          #Carte à gauche
          column(width=8,
                 h4("Carte de la France", style= "font-size: 2.4em;text-align: center;"),
                 plotOutput("carte_france_gener", height = "600px") %>% withSpinner(color="#667eea")
          ),
          
          
          #Indicateurs à droite
          column(width=3,
                 h4("Indicateurs", style="font-size: 2.4em;text-align: center; margin-bottom: 20px;"),
                 #Bouton dowload
                 downloadButton("download_carte_france", "Télécharger la carte",
                                style="background-color: #667eea; border:none;margin-bottom: 20px;width: 100%;text-align: center;"),
                 
                 #Indicateur 1 : APL moyen france
                 div(
                   style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                                  color: white; 
                                  padding: 30px 20px; 
                                  border-radius: 10px; 
                                  margin-bottom: 20px;
                                  box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                  min-height: 150px;
                                  text-align: center;",
                   h5("APL moyen en France", style="margin-top: 0; font-size: 1.7em;"), 
                   uiOutput("apl_moyen_france"),
                   p("Médecins généralistes", style = "margin-bottom: 0;"),
                 ),
                 
                 #Indicateur 2 : Région avec APL max
                 div(
                   style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                                  color: white; 
                                  padding: 30px 20px; 
                                  border-radius: 10px; 
                                  margin-bottom: 20px;
                                  box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                  min-height: 150px;
                                  text-align: center;",
                   h5("Meilleure accessibilité", style="margin-bottom: 0; font-size: 1.7em;"),
                   uiOutput("region_max"),
                   uiOutput("apl_max_valeur"), 
                 ),
                 
                 #Indicateur 3 : Région avec APL min
                 div(
                   style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                                  color: white; 
                                  padding: 30px 20px; 
                                  border-radius: 10px; 
                                  margin-bottom: 20px;
                                  box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                                  min-height: 150px;
                                  text-align: center;",
                   h5("Pire accessibilité", style="margin-bottom: 0; font-size: 1.7em;"),
                   uiOutput("region_min"),
                   uiOutput("apl_min_valeur"), 
                   
                 ),
                 
                 #br(),
                 
          ),
          
          
          
        ),
      ),
    )
  ),
  
  #Pied de page
  tags$footer( style="background-color: #ebe8e8;text-align: center;",
               p("2025/2026 - Projet Santé Publique - Clara GAMBARDELLO, Karla PEM, Cindy LANCON"),
               p("Master Modélisation Statistique | R avancé")),
  
  )

# ---------------------------- SERVER -----------------------------------

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #MESSAGE de bienvenue
  showModal(modalDialog(
    title = div(icon("hand-paper"), "Bienvenue !"),
    "Bienvenue sur notre tableau de bord de santé publique. Explorez les données sur 
    l'offre de soins, la mortalité et l'accessiblité aux médecins en France.",
    footer=modalButton("Explorer !"),
    easyClose = TRUE
  ))  
  

  
    #Server de Clara
    output$effectifs_medecin <- renderPlot({
      
      p <- medecin_long %>% 
        filter(departement==input$departement, specialites==input$specialites) %>% 
        ggplot(aes(x=annee, y=effectif)) +
          geom_line(color="steelblue", linewidth=1.5) +
          geom_point(color="steelblue", size=3) +
          xlab("Année") +
          ylab("Effectif") +
          ggtitle("Évolution des effectifs des médecins") +
          theme_bw()
      
      plot_medecin(p)
      print(p)
    })
    
    
    # Graphe effectifs infirmiers
    output$effectifs_infirmiers <- renderPlot({
      p<-inf_lib_sal %>% filter(departement==input$departement2) %>%
      ggplot(aes(x=annee, y=effectif, col=data_type)) +
        geom_line() +
        facet_wrap(~data_type, scales="free_y") +
        scale_x_continuous(breaks=unique(inf_lib_sal$annee)) +
        theme_bw() +
        labs(title="Effectifs par année des infirmiers",
             x="Année", y="Effectif") +
        scale_color_discrete(name="Type d'infirmiers")
      
      plot_infirmiers(p)
      print(p)
    })
    
    # Pour APL med
    output$offre_besoin_med <- renderPlot({
      p<-APL_med_long %>% 
        filter(Commune==input$Commune) %>% 
        ggplot(aes(x=age_medecins, y=APL, fill=age_medecins)) +
          geom_col() +
          geom_text(aes(label=round(APL, 2)), vjust=-0.5, size=4) +
          facet_wrap(~annee) +
          labs(x="Tranche d'âge des médecins", y="Accessibilité potentielle localisée",
          title=paste("Nombre de consultations (médecins) par habitant : ", input$Commune)) +
          scale_fill_manual(name="Âge des médecins", 
                            values=c("APL_60"="#C7CEEA", 
                                     "APL_65"="#A2C8F2", 
                                     "APL_62"="#FFDAC1",
                                     "APL_tous"="#FFB7B2"),
                            labels=c("APL_60"="Médecins ≤ 60 ans",
                                     "APL_62"="Médecins ≤ 62 ans",
                                     "APL_65"="Médecins ≤ 65 ans", 
                                     "APL_tous"="Tous les médecins")) +
          theme_bw()
      plot_apl_med(p)
      print(p)
      
    })
    
    # Pour APL inf
    output$offre_besoin_inf <- renderPlot({
      p<-APL_inf %>% 
        filter(Commune==input$Commune) %>% 
        ggplot(aes(x=factor(annee), y=APL_infirmiere, fill=factor(annee))) +
        geom_bar(stat="identity", width=0.6) +
        geom_text(aes(label=round(APL_infirmiere, 2)), vjust=-0.8, size=4) +
        labs(y="Accessibilité potentielle localisée", x="Année",
          title=paste("APL infirmiers : ", input$Commune)) +
        scale_fill_manual(name="Année",
          values=c("2022"="#A2C8F2", 
                   "2023"="#F7C6A3")) +
        theme_bw()
      plot_apl_inf(p)
      print(p)
    })
    
    # --------------------------- Téléchargements -------------------------
    
    # Téléchargement Graphique effectif medecin
    plot_medecin <- reactiveVal(NULL)
    download_plot(output=output, output_id="downloadData_1",
                  plot_name=plot_medecin, name_pdf="Effectif_medecin")
    
    # Téléchargement Graphique effectif infirmier
    plot_infirmiers <- reactiveVal(NULL)
    download_plot(output=output, output_id="downloadData_2",
                  plot_name=plot_infirmiers, name_pdf="Effectif_infirmiers")
    
    # Téléchargement Graphique APL médecins
    plot_apl_med  <- reactiveVal(NULL)
    download_plot(output=output, input=input, output_id="downloadData_3",
                  plot_name=plot_apl_med, 
                  name_pdf=function(input){
                    paste0("APL_medecins_", input$Commune)
                    })
    
    # Téléchargement Graphique APL infirmiers
    plot_apl_inf  <- reactiveVal(NULL)
    download_plot(output=output, input=input, output_id="downloadData_4",
                  plot_name=plot_apl_inf, 
                  name_pdf=function(input){
                    paste0("APL_infirmiers_", input$Commune)
                  })
    
    
    #Server de Karla
    
    #Graphe mortalité périnatale
    output$mortalite_peri_reg<- renderPlot({
      mortalite_perinatale$Région <- factor(mortalite_perinatale$Région,levels = mortalite_perinatale$Région[order(mortalite_perinatale$valeur, decreasing = TRUE)])
      
      p<-ggplot(mortalite_perinatale) +
        aes(x = Région, y = valeur) +
        geom_segment( aes(x=Région, xend=Région, y=0, yend=valeur), color="grey") +
        geom_point(color="darkorchid3", size = 2)+
        labs(y = "mortalité", 
             title = "Mortalité périnatale sur 1000 naissances") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45L, hjust=1))
      plot_morta_peri(p)
      print(p)
    })
    
    #Graphe cause mortalité
    output$mortalite_cause_bar <- renderPlot({
      p<-mortalite_cause%>% filter(variable==input$Cause) %>%
      ggplot(aes(x = Région, y = valeur)) +
        geom_col(fill = "#ACA3C8") +
        facet_wrap(~sexe)+
        labs(y = "taux de mortalité", 
             title = "Taux de mortalité standardisé pour 100 000 habitants selon la région") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45L, hjust = 1L))
      
      plot_morta_cause(p)
      print(p)
    })
    
    #Graphe mortalité diplome
    output$mortalite_diplome <- renderPlot({
      p<-morta_dip %>%
        filter(int_annee == input$Annees) %>%
        filter(indicateur == input$Indicateur) %>%
        ggplot() +
        aes(x = age, y = valeur_dip, colour = diplome) +
        geom_line() +
        scale_color_manual(
          name = "Diplôme",
          labels = c(
            "Baccalauréat",
            "Brevet / CEP",
            "CAP / BEP",
            "Ensemble",
            "Sans diplôme",
            "Supérieur au baccalauréat"
          ),
          values = c(
            "Baccalaurat" = "#E41A1C",
            "BrevetCEP" = "#FFD92F",
            "CAPBEP" = "#4DAF4A",
            "Ensemble" = "#34AADD",
            "Sansdiplme" = "#377EB8",
            "Suprieuraubaccalaurat" = "#E78AC3"
          )
        )+
        labs(x = "âge", y = input$Indicateur, title = "Indicateur de mortalité en fonction de l'âge et du diplome", 
             color = "Diplôme") +
        facet_wrap(~sexe)+
        theme_minimal()
      
      plot_morta_dip(p)
      print(p)

    })
    
    #Graphe mortalité classe sociale
    output$mortalite_classe <- renderPlot({
      
      p<-morta_cs %>%
        filter(int_annee == input$Annees_cs) %>%
        filter(indicateur == input$Indicateur_cs) %>%
        ggplot() +
        aes(x = age, y = valeur_cs, colour = classe_sociale) +
        geom_line() +
        scale_color_manual(
          name = "Classe sociale",
          labels = c(
            "Agriculteurs",
            "Artisans, commerçants et chefs d’entreprise",
            "Cadres et professions intellectuelles supérieures",
            "Employés",
            "Ensemble",
            "Inactifs non retraités",
            "Ouvriers",
            "Professions intermédiaires"
          ),
          values = c(
            "Agriculteurs" = "#1B9E77",
            "Artisanscommerantschefsdentreprise" = "#D95F02",
            "Cadresetprofintellectuellessuperieures" = "#1F78B4",
            "Employs" = "#E7298A",
            "Ensemble" = "#66A61E",
            "Inactifsnonretraits" = "#E6AB02",
            "Ouvriers" = "#E31A1C",
            "Professionsintermdiaires" = "#7570B3"
          )
        )+
        labs(x = "âge", y = input$Indicateur_cs, title = "Indicateur de mortalité en fonction de l'âge et de la classe sociale", 
             color = "Classe sociale") +
        facet_wrap(~sexe)+
        theme_minimal()
      
      plot_morta_cs(p)
      print(p)
      
    })
    
    # --------------------------- Téléchargements -------------------------
    
    # Téléchargement mortalité périnatale
    plot_morta_peri <- reactiveVal(NULL)
    download_plot(output=output, output_id="downloadData_5",
                  plot_name=plot_morta_peri, name_pdf="Mortalite_perinatale")
    
    # Téléchargement mortalité cause
    plot_morta_cause <- reactiveVal(NULL)
    download_plot(output=output, input=input, output_id="downloadData_6",
                  plot_name=plot_morta_cause, 
                  name_pdf=function(input){
                    paste0("Mortalite_cause_", input$Cause)
                  })
    
    # Téléchargement mortalité diplome
    plot_morta_dip <- reactiveVal(NULL)
    download_plot(output=output, input=input, output_id="downloadData_7",
                  plot_name=plot_morta_dip, 
                  name_pdf=function(input){
                    paste0("Mortalite_diplome_", input$Annees)
                  })
    
    # Téléchargement mortalité classe sociale
    plot_morta_cs <- reactiveVal(NULL)
    download_plot(output=output, input=input, output_id="downloadData_8",
                  plot_name=plot_morta_cs, 
                  name_pdf=function(input){
                    paste0("Mortalite_classe_sociale_", input$Annees_cs)
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
    #Plot réactif (calculé une seule fois)
    plot_france <- reactive({
      ggplot(regions_filtre_apl_gener())+
        geom_sf(aes(fill=apl_moyen), color ="black", size=0.1)+
        scale_fill_viridis_c( option = "magma",
                              name = "APL moyen")+
        theme_minimal()+
        labs( title = "APL moyen par région",
              subtitle = "Médecins généralistes")
    })
    
    #output carte france
    output$carte_france_gener <- renderPlot ({
      plot_france()
    })
    
    #INDICATEUR 2.1 APL moyen France
    output$apl_moyen_france <- renderUI({
      apl_moyen <- regions_filtre_apl_gener() %>% st_drop_geometry() %>%
        summarise(apl = mean(apl_moyen, na.rm=TRUE)) %>% pull(apl)
      tags$div(round(apl_moyen, 1), style="font-size: 2.2em;")
    })
    
    #INDICATEUR 2.2 Région avec APL max
    ##la région
    output$region_max <- renderUI({
      region <- regions_filtre_apl_gener() %>% st_drop_geometry() %>% slice_max(apl_moyen, n=1) %>%
        pull(reg_name)
      tags$div(region, style="font-size: 1.8em;font-weight: bold;")
    })
    ##l'apl max
    output$apl_max_valeur <- renderUI({
      apl_max <- regions_filtre_apl_gener() %>% st_drop_geometry() %>% slice_max(apl_moyen, n=1) %>%
        pull(apl_moyen)
      tags$div("APL : ", round(apl_max,1), style="font-size: 2em;")
    })
    
    #INDICATEUR 2.3 Région avec APL min
    ##la région
    output$region_min <- renderUI({
      region <- regions_filtre_apl_gener() %>% st_drop_geometry() %>% slice_min(apl_moyen, n=1) %>%
        pull(reg_name)
      tags$div(region, style="font-size: 1.8em;font-weight: bold;")
    })
    ##l'apl min
    output$apl_min_valeur <- renderUI({
      apl_min <- regions_filtre_apl_gener() %>% st_drop_geometry() %>% slice_min(apl_moyen, n=1) %>%
        pull(apl_moyen)
      tags$div("APL : ", round(apl_min,1), style="font-size: 2em;")
    })
    
    
    ####### Carte REGION séléctionné
    #Graphique réactif (calculé que une fois)
    plot_region <- reactive({
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
    
    #Plot de la carte
    output$carte_region_gener <- renderPlot({
      plot_region()
    })
    
    #INDICATEUR 1.1 Classement de la région select
    output$classement_region <- renderUI({
      toutes_regions <- regions_filtre_apl_gener() %>% st_drop_geometry()%>%
        arrange(desc(apl_moyen)) %>% mutate(rang=row_number())
      
      #Trouver le rang
      region_info <- toutes_regions %>% filter(reg_name==input$region_choisie)
      #Si pb information
      if (nrow(region_info)==0){
        return("Aucune données disponible pour cette région.")
      }
      rang <- region_info$rang
      total <- nrow(toutes_regions)
      
      tags$div(
        tags$span(paste0(rang,"e "), style="font-size: 2em;"),
        tags$span(paste0("sur ",total," régions"), style="font-size: 2em;"))
    })
    
    #INDICATEUR 1.2 APL moyen de la région sélectionnée
    output$apl_region_selectionnee <- renderUI({
      region_info <- regions_filtre_apl_gener() %>%
        st_drop_geometry() %>%
        filter(reg_name == input$region_choisie)
      #Vérification des infos
      if (nrow(region_info)==0){
        return("Aucune données disponible pour cette région.")
      }
      apl <- as.numeric(region_info$apl_moyen)
      
      tags$div(round(apl,1), style="font-size: 2em;")
    })
    
    #INDICATEUR 1.3 Population de la region
    output$population_region <- renderUI({
      region_info <- regions_filtre_apl_gener() %>%
        st_drop_geometry() %>%
        filter(reg_name == input$region_choisie)
      #Vérification des infos
      if (nrow(region_info)==0){
        return("Aucune données disponible pour cette région.")
      }
      
      pop <- region_info$population_totale
      
      tags$div(format(pop, big.mark = " ", scientific = FALSE), style="font-size: 2em;")
    })
    
    # Téléchargement des cartes
    ##Carte france
    download_map(output=output, input=input, output_id="download_carte_france",
      plot_reactive=plot_france, prefix="carte_france_apl")
    
    ##Carte régions
    download_map(output=output, input=input, output_id="download_carte_region",
      plot_reactive=plot_region, prefix="carte_region_apl")
}

# ---------------------------- GLOBAL -----------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
