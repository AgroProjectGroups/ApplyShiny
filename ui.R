
options(encoding = "UTF-8")
library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Welcome"),
    
    #Choisir la region d'interet
    selectizeInput(
      inputId="RegionList", 
      label="Sélectionner une région", 
      choices=NULL, 
      options = list(maxItems = 1)
      ),
    
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
    
    
        # Affichage du graph
        mainPanel(
            plotlyOutput(outputId ="PlotWRI")
        )
    )
)
