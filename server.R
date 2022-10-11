#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
options(encoding = "UTF-8")

############
library(shiny)
library(shinydashboard)        

library(shinyLP)
library(FactoMineR)
library(gmodels)



risque <- read.csv("C:/Users/Shema/Desktop/shinyApp/risque.csv",sep = ",")
server <- function(input, output, session) {
  # read in the CSV
  the_data_fn <- reactive({
    
    the_data <-   read.csv("C:/Users/Shema/Desktop/shinyApp/risque.csv",sep = ",")
    return(the_data)
  })
  
  # display a table of the CSV contents
  output$contents <-  DT::renderDataTable({
    #
    the_data_fn()
  })
 
  #PCA
  output$graph_pca_ind <- renderPlot({
    a = subset(risque,Year==input$year)
    rownames(a)=a$Region
    res_pca <- PCA(a[,c("WRI","Exposure","Vulnerability","Susceptibility","Lack.of.Coping.Capabilities","Lack.of.Adaptive.Capacities")], graph = FALSE)
    plot.PCA(res_pca, choix = "ind", axes = c(1,2))
  })
  output$graph_pca_var <- renderPlot({
    res_pca <- PCA(risque[,c("WRI","Exposure","Vulnerability","Susceptibility","Lack.of.Coping.Capabilities","Lack.of.Adaptive.Capacities")], graph = FALSE)
    plot.PCA(res_pca, choix = "var", habillage = 'contrib' , axes = c(1,2))
  })
  
  output$dimdesc_pca <- renderTable({
    a = subset(risque,Year==input$year)
    res <- PCA(a[,c("WRI","Exposure","Vulnerability","Susceptibility","Lack.of.Coping.Capabilities","Lack.of.Adaptive.Capacities")], graph = FALSE)
    dimdesc(res)
  })
  #HCPCA
  output$graph_hcpca <- renderPlot({
    a = subset(risque,Year==input$year)
    rownames(a)=a$Region
    res_pcahc <- PCA(a[,c("WRI","Exposure","Vulnerability","Susceptibility","Lack.of.Coping.Capabilities","Lack.of.Adaptive.Capacities")], graph = FALSE)
    
    res_class <- HCPC(res_pcahc, nb.clust = -1 , min = 4, max = 4 , graph = TRUE)
    plot(res_class, choice = "map")
  })
 
 
  #HCPC graph 2
  output$graph_hcpca2 <- renderPlot({
    a = subset(risque,Year==input$year)
    res_pcahc <- PCA(a[,c("WRI","Exposure","Vulnerability","Susceptibility","Lack.of.Coping.Capabilities","Lack.of.Adaptive.Capacities")], graph = FALSE)
    
    res_class <- HCPC(res_pcahc, nb.clust = -1 , min = 4, max = 4 , graph = TRUE)
    plot(res_class, choix = "tree")
  })
  #HCPC graph 3
  output$graph_hcpca3 <- renderPlot({
    a = subset(risque,Year==input$year)
    res_pcahc <- PCA(a[,c("WRI","Exposure","Vulnerability","Susceptibility","Lack.of.Coping.Capabilities","Lack.of.Adaptive.Capacities")], graph = FALSE)
    
    res_class <- HCPC(res_pcahc, nb.clust = -1 , min = 4, max = 4 , graph = TRUE)
    plot(res_class, choice = "3D.map")
  })
  #HCPC graph 4
  output$graph_hcpca4 <- renderPlot({
    a = subset(risque,Year==input$year)
    res_pcahc <- PCA(a[,c("WRI","Exposure","Vulnerability","Susceptibility","Lack.of.Coping.Capabilities","Lack.of.Adaptive.Capacities")], graph = FALSE)
    
    res_class <- HCPC(res_pcahc,  nb.clust = -1 , min = 4, max = 4 , graph = TRUE)
    plot(res_class,choice = "bar" )
  })
  #cluster1
  output$hcpcCbind <- renderTable({
    a = subset(risque,Year==input$year)
    res_pcahc <- PCA(a[,c("WRI","Exposure","Vulnerability","Susceptibility","Lack.of.Coping.Capabilities","Lack.of.Adaptive.Capacities")], graph = FALSE)
    res_class <- HCPC(res_pcahc, nb.clust = -1 , min = 4, max = 4 , graph = TRUE)
    dta.final <- cbind.data.frame(a, class=as.factor(res_class$data.clust$clust))
    round(res_class$desc.var$quanti$'1',3)
  })
  #cluster2
  output$hcpcCbind2 <- renderTable({
    a = subset(risque,Year==input$year)
    res_pcahc <- PCA(a[,c("WRI","Exposure","Vulnerability","Susceptibility","Lack.of.Coping.Capabilities","Lack.of.Adaptive.Capacities")], graph = FALSE)
    res_class <- HCPC(res_pcahc, nb.clust = -1 , min = 4, max = 4 , graph = TRUE)
    dta.final <- cbind.data.frame(a, class=as.factor(res_class$data.clust$clust))
    round(res_class$desc.var$quanti$'2',3)
  })
  #cluster3
  output$hcpcCbind3 <- renderTable({
    a = subset(risque,Year==input$year)
    res_pcahc <- PCA(a[,c("WRI","Exposure","Vulnerability","Susceptibility","Lack.of.Coping.Capabilities","Lack.of.Adaptive.Capacities")], graph = FALSE)
    res_class <- HCPC(res_pcahc, nb.clust = -1 , min = 4, max = 4 , graph = TRUE)
    dta.final <- cbind.data.frame(a, class=as.factor(res_class$data.clust$clust))
    round(res_class$desc.var$quanti$'1',3)
  })
  #cluster4
  output$hcpcCbind4 <- renderTable({
    a = subset(risque,Year==input$year)
    res_pcahc <- PCA(a[,c("WRI","Exposure","Vulnerability","Susceptibility","Lack.of.Coping.Capabilities","Lack.of.Adaptive.Capacities")], graph = FALSE)
    res_class <- HCPC(res_pcahc, nb.clust = -1 , min = 4, max = 4 , graph = TRUE)
    dta.final <- cbind.data.frame(a, class=as.factor(res_class$data.clust$clust))
    round(res_class$desc.var$quanti$'1',3)
  })
  
  
  
  
  
  
}



