#Chargement des packages
options(encoding = "UTF-8")
library(shiny)
library(ggplot2)
library(plotly)
library(colourpicker)
library(dplyr)
library(tidyverse)


shinyUI(fluidPage(

    # Titre Global
    titlePanel("Welcome"),
    
    #Choisir la region d'interet
    selectizeInput(
      inputId = "RegionList", 
      label = "Sélectionner une région", 
      choices = NULL, 
      options = list(maxItems = 1)
      ),
    
    #Choisir l'indicateur
    radioButtons(inputId = "yvar", 
                 label = "Sélectionner un indicateur", 
                 selected = "WRI",
                 choices = c("WRI" = "WRI", "Exposure" = "Exposure", "Vulnerability" = "Vulnerability", "Susceptibility" = "Susceptibility", "Lack.of.Coping.Capabilities" = "Lack.of.Coping.Capabilities", "Lack.of.Adaptive.Capacities" = "Lack.of.Adaptive.Capacities")),
    
    #Choisir la taille des points des graphiques
    sliderInput(inputId = "sizepoint", 
                label = "Sélectionnez la taille des points des graphiques", 
                min = 1, max = 8,
                value = 3, step = 1),
    
    #Choisir la taille des titres des graphiques
    sliderInput(inputId = "sizetitle", 
                label = "Sélectionnez la taille des titres des graphiques", 
                min = 1, max = 18,
                value = 12, step = 1),
    
    #Choisir la taille des titres des axes des graphs
    sliderInput(inputId = "sizetitleaxis", 
                label = "Sélectionnez la taille des titres des axes des graphiques", 
                min = 1, max = 18,
                value = 10, step = 1),
    
    #Choisir la couleur des points region
    colourInput(inputId = "color_region", 
                label = "Couleur pour la région sélectionnée :", 
                value = "orange"),
    
    #Choisir la couleur des points de la moyenne
    colourInput(inputId = "color_all", 
                label = "Couleur pour toutes les régions confondues :", 
                value = "grey"),
    
        # Affichage du graph
        mainPanel(
            plotlyOutput(outputId ="Plot_by_region")
        )
    )
)
