
options(encoding = "UTF-8")
library(shiny)
library(ggplot2)
library(plotly)

#Chargement du jeu de donnees
World_Risk <- read.csv2("C:/Users/ameli/OneDrive/Documents/M2/Donnees_massives_sous_R/Projet/world_risk2.csv", header=TRUE, dec=".", stringsAsFactors = TRUE)
World_Risk$Year <- as.factor(World_Risk$Year)


shinyServer(function(input, output, session) {
  
  #mise a jour de la liste deroulante des regions dans l'ui
  updateSelectizeInput(session, 'RegionList', choices = World_Risk$Region, server = TRUE)
  
  #jeu de donnees contenant uniquement les donnees sur la region selectionnee par l'utilisateur
  DataWR <- reactive({
    subset(
      x = World_Risk,
      subset = Region == input$RegionList
      )
    })

  
  output$PlotWRI <- renderPlotly({
    p <- ggplot(DataWR(), aes(x=Year, y=WRI)) +
        geom_point(na.rm=TRUE, size=3) +
        ggtitle("Evolution de l'indicateur au cours du temps") +
        xlab("Années") +
        ylab("Indicateur")+
        theme(plot.title = element_text(face="bold", hjust=0.5, size=16), 
              axis.title.x = element_text(size=12, face="bold"),
              axis.title.y = element_text(size=12, face="bold"))
    
    ggplotly(p)
    
    })
  
})
