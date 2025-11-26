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
library(readxl)
library(shiny)
library(stringr)
library(tidyr)


# Import tables Clara
medecin_long <- readRDS("../donnees_traitees/medecin_long.rds")


# Import tables Karla : 
morta_cs<-readRDS("../donnees_traitees/morta_cs.rds")
morta_dip<-readRDS("../donnees_traitees/morta_dip.rds")
mortalite_cause<-readRDS("../donnees_traitees/mortalite_cause.rds")
mortalite_combine<-readRDS("../donnees_traitees/mortalite_combine.rds")
mortalite_perinatale<-readRDS("../donnees_traitees/mortalite_perinatale.rds")


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
       )
             
    )
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
        geom_line(color="steelblue", linewidth=1.5) +
        geom_point(color="steelblue", size=3) +
        xlab("Année") +
        ylab("Effectif") +
        ggtitle("Évolution des effectifs") +
        theme_bw()
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
    
    #Server de Cindy
}

# ---------------------------- GLOBAL -----------------------------------

# Run the application 
shinyApp(ui = ui, server = server)
