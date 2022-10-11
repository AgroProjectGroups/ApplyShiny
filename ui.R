shinyUI(
  
  navbarPage(
    theme = shinytheme("united"),
    title = "World Risk Index",
    tabPanel(icon("home"),
             fluidRow(
               
               column(br(),
                      br(),
                      br(),
                      tags$img(src="Philippines.jpg",width="200px",height="250px"),width=2,
                      br(),
                      p(em('source : Agence française de développement'),style="text-align:center;color:grey")),
               
               column(
                 
                 
                 p("The World Risk Report is an annual technical report on global disaster risks. The yearly issues of the World Risk Report focus on varying critical topics related to disaster risk management. The report includes the World Risk Index, which identifies the risk of an extreme natural event becoming a disaster for numerous countries worldwide.",style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                 br(),
                 
                 p(em("The World Risk Index")," uses 27 aggregated, publicly available indicators to determine disaster risk for 181 countries worldwide. Conceptually, the index is composed of exposure to extreme natural hazards and the societal vulnerability of individual countries. Earthquakes, cyclones, floods, droughts, and climate-induced sea-level rise are considered in the exposure analysis. Societal vulnerability is divided into susceptibility to extreme natural events, lack of coping capacities, and lack of adaptive capacities. All index components are scaled to the value range from 0 to 100. The higher a country's index score on the WorldRiskIndex, the higher its national disaster risk.",style="text-align:justify;color:black;background-color:papayawhip;padding:15px;border-radius:10px"),
                 
                 br(),
                 
                 p('The dataset consists of 11 years data of multiple countries with features including: Region, WRI, Exposure, Vulnerability, Susceptibility, Lack of Coping Capabilities, Lack of Adaptive Capabilities, Year, WRI Category, Exposure Category, Vulnerability Category, Susceptibility Category.',br(),'We added the variables CODE and Continent in order to push the analysis further ',style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                 
                 width=8),
               column(br(),
                      br(),
                      br(),
                 
                 
                 tags$img(src="unnamed.jpg",width="200px",height="250px"),
                 br(),
                 p(em('source : Agence française de développement'),style="text-align:center;color:grey"),
                 
                 width=2)),
             hr(),
             tags$style(".fa-database {color:#E87722}"),
             h3(p(icon("database",lib = "font-awesome"),em("Dataset "),style="color:black;text-align:center")),
             fluidRow(column(DT::dataTableOutput("RawData"),
                             width = 12,
                             tableOutput('Summary')),),
             hr(),
             
             
    ),
    
    
    navbarMenu("Correlations", icon=icon('chart-line'),
               tabPanel('Correlation diagram',
                        fluidRow(
                          column(3,
                                 wellPanel(style = "background: papayawhip",
                                   selectInput('x',label='Select a x variable', choices=c("Exposure",'Vulnerability','Susceptibility','Lack.of.Adaptive.Capacities','Lack.of.Coping.Capabilities')))),
                          column(6,
                                 h2(paste('Correlation between WRI and selected variable')),
                                 plotlyOutput('graph',height = 500,width=1000)))
               ),
               tabPanel('Correlogram',
                        fluidRow(
                          column(3,
                                 wellPanel(style = "background: papayawhip",
                                   selectInput('yearcor',label='select a year', choices=c(2011:2021)))),
                          column(6,
                                 h2('Correlogram'),
                                 plotlyOutput('cor',height = 600,width=1000))
                        )),
               
               
    ),
    
    
    
    
    
    
    
    navbarMenu("Time visualization", icon=icon('hourglass-half'),
               
               tabPanel("Visualization of index by continent",
                        
                        fluidRow(
                          
                          column(width = 3,
                                 
                                 wellPanel(style = "background: papayawhip",
                                   
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
               
               tabPanel("Visualization of index by region",
                        
                        fluidRow(
                          
                          column(width = 2,
                                 
                                 h4(textOutput(outputId = "Notforget")),
                                 
                                 br(),
                                 
                                 wellPanel(style = "background: papayawhip",
                                   
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
                                   
                                   actionButton("go", icon=icon(name="play", class="fa-solid fa-play"), label="Update")
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
               
               tabPanel("Visualization for each index",
                        
                        tabsetPanel(
                          tabPanel("Index WRI",
                                   
                                   column(width=6,
                                          plotlyOutput(outputId ="plot_WRI_high"),
                                          br(),
                                          h4("Table of the 10 regions with the highest WRI index"),
                                          br(),
                                          wellPanel(style = "background: papayawhip",
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
                                          wellPanel(style = "background: papayawhip",
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
                                          wellPanel(style = "background: papayawhip",
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
                                          wellPanel(style = "background: papayawhip",
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
                                          wellPanel(style = "background: papayawhip",
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
                                          wellPanel(style = "background: papayawhip",
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
                                          wellPanel(style = "background: papayawhip",
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
                                          wellPanel(style = "background: papayawhip",
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
                                          wellPanel(style = "background: papayawhip",
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
                                          wellPanel(style = "background: papayawhip",
                                            selectizeInput(
                                              inputId = "YearListLow_5", 
                                              label = "Select among the list :",
                                              choices = NULL, 
                                              options = list(maxItems = 1))),
                                          tableOutput("table_Lack.of.Coping.Capabilities_low")
                                   )
                          ),
                          tabPanel("Index Lack of Adaptive Capacities",
                                   
                                   column(width=6,
                                          plotlyOutput(outputId ="plot_Lack.of.Adaptive.Capacities_high"),
                                          br(),
                                          h4("Table of the 10 regions with the highest Lack of Adaptive Capacities index"),
                                          br(),
                                          wellPanel(style = "background: papayawhip",
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
                                          wellPanel(style = "background: papayawhip",
                                            selectizeInput(
                                              inputId = "YearListLow_6", 
                                              label = "Select among the list :",
                                              choices = NULL, 
                                              options = list(maxItems = 1))),
                                          tableOutput("table_Lack.of.Adaptive.Capacities_low")
                                   )
                          )
                        )
               )),
    tabPanel('Map', icon=icon('earth-americas'),
             fluidRow(
               column(width = 3,
                      wellPanel(style = "background: papayawhip",
                        selectInput('indic',label='Select the variable to observe',choices=c('WRI',"Exposure",'Vulnerability','Susceptibility','Lack.of.Adaptive.Capacities','Lack.of.Coping.Capabilities')),
                        selectInput('couleur',label='Choose a colour palette', choices=c('Oranges','viridis','Purples','Greys','Greens')))
               ),
               column(width = 9, plotlyOutput('map',height = 600,width=1000))
               
             )),
    
    navbarMenu("Analysis", icon=icon('magnifying-glass-chart'),
      tabPanel("PCA",
               sidebarLayout(
                 sidebarPanel(width=3,style = "background: papayawhip",
                   selectInput(
                     "year",
                     "Choose the year",
                     choices = c(2011:2021),
                     selected = " ",
                     multiple = FALSE
                   ),
                   radioButtons('reponse',label='Visualize correlation and p-value',choices=c('yes','non'))
                 ),
                 mainPanel(fluidRow(
                   column(width = 6,
                          
                            plotOutput("graph_pca_ind",
                            height = 400,
                            
                            
                            dblclick = "plot1_dblclick",
                            brush = brushOpts(
                              id = "plot1_brush",
                              resetOnNew = TRUE
                            )
                          )),
                   column(width = 6,
                          div(
                            plotOutput("graph_pca_var"),
                            height = 700,
                            width = 1000
                          )),
                   
                   column(width = 6, 
                          h3(textOutput(outputId = "firstdim")),
                          div(
                            tableOutput("dimdesc_pca"),  #pas d'erreur pour les 3 dernieres annees ,# a ne pas ajouter?!
                            height = 600,
                            width = 500
                          )),
                   column(width = 6,
                          h3(textOutput(outputId = "seconddim")),
                          div(
                            tableOutput("dimdesc_pca2"),  #pas d'erreur pour les 3 dernieres annees ,# a ne pas ajouter?!
                            height = 600,
                            width = 500
                          )),
                   column(width = 6, 
                          h3(textOutput(outputId = "thirddim")),
                          div(
                            tableOutput("dimdesc_pca3"),  #pas d'erreur pour les 3 dernieres annees ,# a ne pas ajouter?!
                            height = 300,
                            width = 100
                          )),
                 ))
               )),
      #HCPCA    
      tabPanel("HCPC",
               tabsetPanel(
                 tabPanel("Graphs",
                          fluidRow(
                            h3("Hierarchical Clustering:"),
                            column(
                              width = 6,
                              
                              plotOutput(
                                "graph_hcpca", height = 600 , width = 800,
                                dblclick = "plot2_dblclick",
                                brush = brushOpts(
                                  id = "plot2_brush",
                                  resetOnNew = TRUE
                                )
                              ),
                              div(plotOutput(
                                "graph_hcpca2", height = 600 , width = 800
                              ))
                            ),
                            column(width = 6,
                                   div(plotOutput(
                                     "graph_hcpca3", height = 600 , width = 800
                                   )),
                                   div(plotOutput(
                                     "graph_hcpca4", height = 600 , width = 800
                                   ))), 
                          )),
                 tabPanel("Clusters",
                          fluidRow(
                            column(width = 6, h3("Cluster 1"),
                                   mainPanel(div(
                                     tableOutput("hcpcCbind"),
                                     h3("Cluster 2 "),
                                     tableOutput("hcpcCbind2")
                                   ),)),
                            
                            column(width = 6, h3("Cluster 3"),
                                   mainPanel(div(
                                     tableOutput("hcpcCbind3"),
                                     h3("Cluster 4 "),
                                     tableOutput("hcpcCbind4")
                                   ),))
                            
                          ))
               ))
    ),
    tabPanel(icon('link'),
      

      column(3,

             h2("Link to reports"),
             br(),
             uiOutput("tab1"),
             uiOutput("tab2"),
             uiOutput("tab3"),
             uiOutput("tab4")),
      column(9,
             tags$img(src="cata.jpg",width="1000px",height="600px"))
                    ),
    
            
             
    )
    
)
    
    
    
  
  

