#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

# Les packages

library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(purrr)
library(stringr)
library(ggplot2)

# Importation des données

medecin<-read_excel("../data/Med_2012_2025.xlsx", sheet=2)

# Traitement des données de Clara

medecin_clean <- medecin %>% filter(!(substr(territoire, 1, 1) %in% c("0", "3")), region != "00-Ensemble", 
                                    sexe=="0-Ensemble", departement!="000-Ensemble", exercice == "0-Ensemble",
                                    tranche_age=="00-Ensemble", specialites != "00-Ensemble")

medecin_long <- medecin_clean %>% pivot_longer(cols=starts_with("effectif_"),
  names_to = "annee", values_to = "effectif") %>% 
  mutate(annee=as.integer(sub("effectif_", "", annee)))


# Import tables Karla : 
morta_cs<-readRDS("../donnees_traitees/morta_cs.rds")
morta_dip<-readRDS("../donnees_traitees/morta_dip.rds")
mortalite_cause<-readRDS("../donnees_traitees/mortalite_cause.rds")
mortalite_combine<-readRDS("../donnees_traitees/mortalite_combine.rds")
mortalite_perinatale<-readRDS("../donnees_traitees/mortalite_perinatale.rds")

mortalite_perinatale$Région <- factor(mortalite_perinatale$Région,levels = mortalite_perinatale$Région[order(mortalite_perinatale$valeur, decreasing = TRUE)])



# ---------------------------- UI -----------------------------------


# Define UI for application that draws a histogram
ui <- navbarPage(
  "Santé publique sur le territoire",

  #UI de Clara
  tabPanel("Page de Clara, modifie ton titre comme tu veux",
    sidebarLayout(
        sidebarPanel(
          selectInput(inputId="departement", label="Choisir un département :", choices=medecin_long$departement, 
                      selected=medecin_long$departement[1]),
          selectInput(inputId="specialites", label="Choisir une spécialités :", choices=medecin_long$specialites, 
                      selected=medecin_long$specialites[1]),
          downloadLink('downloadData', 'Download')
        ),

        # Show a plot of the generated distribution
        mainPanel(
          tabsetPanel(
            tabPanel("Evolution des effectifs",
                     plotOutput("effectifs_medecin")
            ),
          )
        )
      )
    ),
  tabPanel("Études autour de la mortalité",
           # Sidebar with a slider input for number of bins 
             mainPanel(
               tabsetPanel(
                 tabPanel("Mortalité Périnatale",
                          plotOutput("mortalite_peri_reg")
                 ),
               )
               
             ),
             
  ),


tabPanel("Page de Cindy Modifie ton titre comme tu veux",
         # Sidebar with a slider input for number of bins 
         sidebarLayout(
            sidebarPanel(
              
            ),
            mainPanel(
              
              
            ),

          ),
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

    output$effectifs_medecin <- renderPlot({
      medecin_long %>% filter(departement==input$departement, specialites==input$specialites) %>% 
      ggplot(aes(x=annee, y=effectif)) +
        geom_line(color="steelblue", size=1.5) +
        geom_point(color="steelblue", size=3) +
        xlab("Année") +
        ylab("Effectif") +
        ggtitle("Évolution des effectifs") +
        theme_bw()
    })
    
    #Server de Karla
    
    #Graphe mortalité périnatale
    output$mortalite_peri_reg<- renderPlot({
      ggplot(mortalite_perinatale) +
        aes(x = Région, y = valeur) +
        geom_col(fill = "#AF0D0D") +
        labs(y = "mortalité", 
             title = "Mortalité périnatale sur 1000 naissances") +
        theme_bw() +
        theme(axis.text.x = element_text(angle = 45L, hjust=1))
    })
    
    #Server de Cindy
}

# ---------------------------- GLOBAL -----------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
