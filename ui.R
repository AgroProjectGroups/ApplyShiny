

library(shiny)
###
library(class)
library(gmodels)
library(nnet)
# Define UI for application
ui <- fluidPage(navbarPage(
  title = "Risque",
  navbarMenu(
    "Introduction",
    tabPanel(
      "Our Data",
      p("Here is a summary of the data"),
      tableOutput('summary'),
      tags$hr(),
      p("Here is the raw data from the CSV file"),
      DT::dataTableOutput('contents')
    ),
    # end  tab
#TAB PCA et HCPC 
  ),
  navbarMenu(
    "Analyses",
    tabPanel("PCA",
             sidebarLayout(
               sidebarPanel(
                 selectInput(
                   "year",
                   "Choose the year",
                   choices = c(2011:2021),
                   selected = " ",
                   multiple = FALSE
                 ),
               ),
               mainPanel(fluidRow(
                 column(width = 5,
                        div(
                          plotOutput("graph_pca_ind"),
                          height = 600,
                          width = 1000
                        )),
                 column(width = 5,
                        div(
                          plotOutput("graph_pca_var"),
                          height = 600,
                          width = 1000
                        )),
                 
                 column(width = 4,
                        div(
                          tableOutput("dimdesc_pca"),  #pas d'erreur pour les 3 dernieres annees ,# a ne pas ajouter?!
                          height = 600,
                          width = 1000
                        )),
                 column(width = 4,
                        div(
                          tableOutput("dimdesc_pca2"),  #pas d'erreur pour les 3 dernieres annees ,# a ne pas ajouter?!
                          height = 600,
                          width = 1000
                        )),
                 column(width = 4,
                        div(
                          tableOutput("dimdesc_pca3"),  #pas d'erreur pour les 3 dernieres annees ,# a ne pas ajouter?!
                          height = 600,
                          width = 1000
                        ))
               ))
             )),
#HCPCA    
    tabPanel("HCPCA",
             tabsetPanel(
               tabPanel("Graphes",
                        fluidRow(
                          column(
                            width = 5,
                            h3("Hierarchical Clustering:"),
                            div(plotOutput(
                              "graph_hcpca", height = 600 , width = 1000
                            )),
                            div(plotOutput(
                              "graph_hcpca2", height = 600 , width = 1000
                            ))
                          ),
                          column(width = 5,
                                 div(plotOutput(
                                   "graph_hcpca3", height = 600 , width = 1000
                                 )),
                                 div(plotOutput(
                                   "graph_hcpca4", height = 600 , width = 1000
                                 ))), 
                        )),
               tabPanel("Clusters",
                        fluidRow(
                          column(width = 5, h3("Cluster 1"),
                                 mainPanel(div(
                                   tableOutput("hcpcCbind"),
                                   h3("Cluster 2 "),
                                   tableOutput("hcpcCbind2")
                                 ),)),
                          
                          column(width = 5, h3("Cluster 3"),
                                 mainPanel(div(
                                   tableOutput("hcpcCbind3"),
                                   h3("Cluster 4 "),
                                   tableOutput("hcpcCbind4")
                                 ),))
                         
                        ))
             ))
  )
))
