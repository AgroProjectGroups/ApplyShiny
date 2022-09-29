
options(encoding = "UTF-8")
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyverse)

#Chargement du jeu de donnees
World_Risk <- read.csv2("C:/Users/ameli/OneDrive/Documents/M2/Donnees_massives_sous_R/Projet/world_risk.csv", header=TRUE, na.strings=c("","NA"), dec=".", stringsAsFactors = TRUE)
World_Risk$Year <- as.factor(World_Risk$Year)


shinyServer(function(input, output, session) {
  
  #mise a jour de la liste deroulante des regions dans l'ui
  updateSelectizeInput(session, 'RegionList', choices = World_Risk$Region, server = TRUE)
  
  #jeu de donnes avec les moyennes par année pour chaque indicateur
  data_overall <- aggregate(x= World_Risk[,2:7],     
                            by = list(World_Risk$Year),      
                            FUN = mean, na.rm=TRUE)
  colnames(data_overall)[1]="Year"
  data_overall<-data_overall[,c(2,3,4,5,6,7,1)]
  
  data_group_all <- data.frame(Region=rep("Overall Average", times=11))
  
  Data_aggregate <- cbind(data_group_all, data_overall)
  
  Overall_average <- World_Risk %>%
    group_by(Year) %>%
    summarize_at(c("WRI","Exposure","Vulnerability","Susceptibility", "Lack.of.Coping.Capabilities", "Lack.of.Adaptive.Capacities"), 
                 ~ mean(.x, na.rm = TRUE))
  
  Reg_selected <- reactive({
    World_Risk %>%
    dplyr::filter(Region == input$RegionList)%>%
    select(WRI:Year)
  })
  #verifier la nature des variables
  Data_final_byRegion <- reactive({
    bind_rows("All" = Overall_average, "Region selected" = Reg_selected(), .id="Region")
  })
  
  output$PlotWRI <- renderPlotly({
    p <- ggplot(Data_final_byRegion(), aes(x=Year, y=WRI, color=Region)) +
        geom_point(na.rm=TRUE, size=input$sizepoint) +
        ggtitle("Evolution de l'indicateur WRI au cours du temps") +
        xlab("Années") +
        ylab("WRI")+
        scale_color_manual(values=c("#999999", "#E69F00"))+
        theme(plot.title = element_text(face="bold", hjust=0.5, size=input$sizetitle), 
              axis.title.x = element_text(size=input$sizetitleaxis, face="bold"),
              axis.title.y = element_text(size=input$sizetitleaxis, face="bold"))
    
    ggplotly(p)
    
    })
  
})
