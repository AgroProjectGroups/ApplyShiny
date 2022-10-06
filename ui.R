#Chargement des packages
options(encoding = "UTF-8")
library(shiny)
library(ggplot2)
library(plotly)
library(colourpicker)
library(dplyr)
library(tidyverse)
library(highcharter)


shinyUI(

  navbarPage(
    title = "World Risk",
    
    navbarMenu("Visualization",
    
    tabPanel("Visualization of index by region",
             
             fluidRow(
               
               column(width = 2,
                
                        h4(textOutput(outputId = "Notforget")),
                      
                        br(),
                      
                      wellPanel(
                      
                        #Choisir la region d'interet
                        selectizeInput(
                          inputId = "RegionList", 
                          label = "Select one region :",
                          choices = NULL, 
                          options = list(maxItems = 1)),
    
                        #Choisir la taille des points des graphiques
                        sliderInput(
                          inputId = "sizepoint", 
                          label = "Select the size of points :", 
                          min = 1, max = 8,
                          value = 3, step = 1),
    
                        #Choisir la taille des titres des graphiques
                        sliderInput(
                          inputId = "sizetitle", 
                          label = "Select the size of titles :", 
                          min = 1, max = 18,
                          value = 12, step = 1),
    
                        #Choisir la taille des titres des axes des graphs
                        sliderInput(
                          inputId = "sizetitleaxis", 
                          label = "Select the size of titles axis :", 
                          min = 1, max = 18,
                          value = 10, step = 1),
    
                        #Choisir la couleur des points region
                        colourInput(
                          inputId = "color_region", 
                          label = "Select the color for the region selected :", 
                          value = "orange"),
    
                        #Choisir la couleur des points de la moyenne
                        colourInput(
                          inputId = "color_all", 
                          label = "Select the color for all region :", 
                          value = "grey"),
                        
                        actionButton("go", icon=icon(name="play", class="fa-solid fa-play"), label="Click me !")
                      )
                      ),
             
             column(width = 5,
            # Affichage du graph
            plotlyOutput(outputId ="Plot_by_region_WRI"),
            plotlyOutput(outputId ="Plot_by_region_Exposure"),
            plotlyOutput(outputId ="Plot_by_region_Vulnerability")
            ),
            
            column(width = 5,
            plotlyOutput(outputId ="Plot_by_region_Susceptibility"),
            plotlyOutput(outputId ="Plot_by_region_Lack.of.Coping.Capabilities"),
            plotlyOutput(outputId ="Plot_by_region_Lack.of.Adaptive.Capacities")
            )
            )
            ),
    
    tabPanel("Visualization of index by continent",
             
             fluidRow(
               
               column(width = 3,
                      
                      wellPanel(
                      
                      selectizeInput(
                        inputId = "IndexList", 
                        label = "Select among the list :",
                        choices = NULL, 
                        options = list(maxItems = 1)),
                      
                      colourInput(
                        inputId = "colorOceania", 
                        label = "Select the color for Oceania :", 
                        value = "#86EBCB"),
                      
                      colourInput(
                        inputId = "colorAmerica", 
                        label = "Select the color for America :", 
                        value = "#EBC138"),
                      
                      colourInput(
                        inputId = "colorEurope", 
                        label = "Select the color for Europe :", 
                        value = "#908BF0"),
                      
                      colourInput(
                        inputId = "colorAfrica", 
                        label = "Select the color for Africa :", 
                        value = "#73E68A"),
                      
                      colourInput(
                        inputId = "colorAsia", 
                        label = "Select the color for Asia :", 
                        value = "#FAA5D8")
                      )
               ),
               
               column(width = 9,
                      highchartOutput(outputId ="Plot_Index_Continent"),
                      br(),
                      plotlyOutput(outputId ="Boxplot_Continent")
                      )
             )
    ),
    
    tabPanel("Visualization for each index",
             
             tabsetPanel(
               tabPanel("Index WRI",
             
             column(width=6,
                    plotlyOutput(outputId ="plot_WRI_high"),
                    br(),
                    h4("Table of the 10 regions with the highest WRI index"),
                    br(),
                    wellPanel(
                    selectizeInput(
                      inputId = "YearListHigh_1", 
                      label = "Select among the list :",
                      choices = NULL, 
                      options = list(maxItems = 1))),
                    tableOutput("table_WRI_high")
                    ),
             
             column(width=6,
                    plotlyOutput(outputId ="plot_WRI_low") ,
                    br(),
                    h4("Table of the 10 regions with the lowest WRI index"),
                    br(),
                    wellPanel(
                      selectizeInput(
                        inputId = "YearListLow_1", 
                        label = "Select among the list :",
                        choices = NULL, 
                        options = list(maxItems = 1))),
                    tableOutput("table_WRI_low")
             )
             ),
             tabPanel("Index Exposure",
                      
                      column(width=6,
                             plotlyOutput(outputId ="plot_Exposure_high"),
                             br(),
                             h4("Table of the 10 regions with the highest Exposure index"),
                             br(),
                             wellPanel(
                               selectizeInput(
                                 inputId = "YearListHigh_2", 
                                 label = "Select among the list :",
                                 choices = NULL, 
                                 options = list(maxItems = 1))),
                             tableOutput("table_Exposure_high")
                      ),
                      
                      column(width=6,
                             plotlyOutput(outputId ="plot_Exposure_low") ,
                             br(),
                             h4("Table of the 10 regions with the lowest Exposure index"),
                             br(),
                             wellPanel(
                               selectizeInput(
                                 inputId = "YearListLow_2", 
                                 label = "Select among the list :",
                                 choices = NULL, 
                                 options = list(maxItems = 1))),
                             tableOutput("table_Exposure_low")
                      )
             ),
             
             tabPanel("Index Vulnerability",
                      
                      column(width=6,
                             plotlyOutput(outputId ="plot_Vulnerability_high"),
                             br(),
                             h4("Table of the 10 regions with the highest Vulnerability index"),
                             br(),
                             wellPanel(
                               selectizeInput(
                                 inputId = "YearListHigh_3", 
                                 label = "Select among the list :",
                                 choices = NULL, 
                                 options = list(maxItems = 1))),
                             tableOutput("table_Vulnerability_high")
                      ),
                      
                      column(width=6,
                             plotlyOutput(outputId ="plot_Vulnerability_low") ,
                             br(),
                             h4("Table of the 10 regions with the lowest Vulnerability index"),
                             br(),
                             wellPanel(
                               selectizeInput(
                                 inputId = "YearListLow_3", 
                                 label = "Select among the list :",
                                 choices = NULL, 
                                 options = list(maxItems = 1))),
                             tableOutput("table_Vulnerability_low")
                      )
             ),
             
             tabPanel("Index Susceptibility",
                      
                      column(width=6,
                             plotlyOutput(outputId ="plot_Susceptibility_high"),
                             br(),
                             h4("Table of the 10 regions with the highest Susceptibility index"),
                             br(),
                             wellPanel(
                               selectizeInput(
                                 inputId = "YearListHigh_4", 
                                 label = "Select among the list :",
                                 choices = NULL, 
                                 options = list(maxItems = 1))),
                             tableOutput("table_Susceptibility_high")
                      ),
                      column(width=6,
                             plotlyOutput(outputId ="plot_Susceptibility_low") ,
                             br(),
                             h4("Table of the 10 regions with the lowest Susceptibility index"),
                             br(),
                             wellPanel(
                               selectizeInput(
                                 inputId = "YearListLow_4", 
                                 label = "Select among the list :",
                                 choices = NULL, 
                                 options = list(maxItems = 1))),
                             tableOutput("table_Susceptibility_low")
                      )
             ),
             tabPanel("Index Lack of Coping Capabilities",
                      
                      column(width=6,
                             plotlyOutput(outputId ="plot_Lack.of.Coping.Capabilities_high"),
                             br(),
                             h4("Table of the 10 regions with the highest Lack of Coping Capabilities index"),
                             br(),
                             wellPanel(
                               selectizeInput(
                                 inputId = "YearListHigh_5", 
                                 label = "Select among the list :",
                                 choices = NULL, 
                                 options = list(maxItems = 1))),
                             tableOutput("table_Lack.of.Coping.Capabilities_high")
                      ),
                      column(width=6,
                             plotlyOutput(outputId ="plot_Lack.of.Coping.Capabilities_low") ,
                             br(),
                             h4("Table of the 10 regions with the lowest Lack of Coping Capabilities index"),
                             br(),
                             wellPanel(
                               selectizeInput(
                                 inputId = "YearListLow_5", 
                                 label = "Select among the list :",
                                 choices = NULL, 
                                 options = list(maxItems = 1))),
                             tableOutput("table_Lack.of.Coping.Capabilities_low")
                      )
             ),
             tabPanel("Index Lack.of.Adaptive.Capacities",
                      
                      column(width=6,
                             plotlyOutput(outputId ="plot_Lack.of.Adaptive.Capacities_high"),
                             br(),
                             h4("Table of the 10 regions with the highest Lack of Adaptive Capacities index"),
                             br(),
                             wellPanel(
                               selectizeInput(
                                 inputId = "YearListHigh_6", 
                                 label = "Select among the list :",
                                 choices = NULL, 
                                 options = list(maxItems = 1))),
                             tableOutput("table_Lack.of.Adaptive.Capacities_high")
                      ),
                      column(width=6,
                             plotlyOutput(outputId ="plot_Lack.of.Adaptive.Capacities_low") ,
                             br(),
                             h4("Table of the 10 regions with the lowest Lack of Adaptive Capacities index"),
                             br(),
                             wellPanel(
                               selectizeInput(
                                 inputId = "YearListLow_6", 
                                 label = "Select among the list :",
                                 choices = NULL, 
                                 options = list(maxItems = 1))),
                             tableOutput("table_Lack.of.Adaptive.Capacities_low")
                      )
             )
             )
    )
    )
  )
)