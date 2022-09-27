
options(encoding = "UTF-8")
library(shiny)
library(ggplot2)
library(plotly)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Welcome"),
    
    selectizeInput(
      inputId="RegionList", 
      label="Sélectionner une région", 
      choices=NULL, 
      options = list(maxItems = 1)
      ),
    
        # Affichage du graph
        mainPanel(
            plotlyOutput(outputId ="PlotWRI")
        )
    )
)
