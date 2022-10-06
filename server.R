
#Chargement du jeu de donnees
World_Risk <- read.csv2("C:/Users/ameli/OneDrive/Documents/M2/Donnees_massives_sous_R/Projet/Dataset_Risk.csv", header=TRUE, na.strings=c("","NA"), dec=".", stringsAsFactors = TRUE)
World_Risk$Year <- as.factor(World_Risk$Year)


shinyServer(function(input, output, session) {
  
  # Onglet region
  
  # mise a jour de la liste deroulante des regions dans l'ui
  updateSelectizeInput(session, 'RegionList', choices = World_Risk$Region, server = TRUE)
  
  # Jeu de donnees avec les quantiles (Q1, median, Q3) par indicateur par annee
  Overall_quantile <- World_Risk %>%
    group_by(Year)%>%
    summarise(across(c("WRI","Exposure","Vulnerability","Susceptibility", "Lack.of.Coping.Capabilities", "Lack.of.Adaptive.Capacities"),
                     list("25" = ~quantile(.x,0.25), 
                          "50" = ~quantile(.x,0.5),
                          "75"= ~quantile(.x,0.75))))
  
  # jeu de donnees avec les lignes concernant la region selectionnee 
  Reg_selected <- reactive({
    World_Risk %>%
      dplyr::filter(Region == input$RegionList) %>%
      select(WRI:Year) %>%
      mutate("WRI_25" = WRI, "WRI_50" = WRI, "WRI_75" = WRI,
             "Exposure_25" = Exposure, "Exposure_50" = Exposure, "Exposure_75" = Exposure,
             "Vulnerability_25" = Vulnerability, "Vulnerability_50" = Vulnerability, "Vulnerability_75" = Vulnerability,
             "Susceptibility_25" = Susceptibility, "Susceptibility_50" = Susceptibility, "Susceptibility_75" = Susceptibility,
             "Lack.of.Coping.Capabilities_25" = Lack.of.Coping.Capabilities, "Lack.of.Coping.Capabilities_50" = Lack.of.Coping.Capabilities, "Lack.of.Coping.Capabilities_75" = Lack.of.Coping.Capabilities,
             "Lack.of.Adaptive.Capacities_25" = Lack.of.Adaptive.Capacities, "Lack.of.Adaptive.Capacities_50" = Lack.of.Adaptive.Capacities, "Lack.of.Adaptive.Capacities_75" = Lack.of.Adaptive.Capacities
      ) %>%
      select(-c(WRI:Lack.of.Adaptive.Capacities))
  })
  
  # Concatenation des 2 jeux de donnees
  Data_final_by_Region <- reactive({
    bind_rows("All Region" = Overall_quantile, "Region selected" = Reg_selected(), .id="Region") %>%
      rapply(f=round, classes ="numeric", how = "replace", digits=2) %>%
      mutate(Region = if_else(Region == 'Region selected', 
      paste("Value for", input$RegionList), 
      paste("Median for", Region, "\n The line represents \n the first and third quantiles")))
  })
  
  output$Notforget <- renderText({
    c("Warnings : Don't forget to click the play button at the bottom !!")
  })

  # Graphique WRI
  output$Plot_by_region_WRI <- renderPlotly({
    input$go 
    isolate({
    p <- ggplot(Data_final_by_Region(), aes(x=Year, y=WRI_50, ymax=WRI_75, ymin=WRI_25, group=Region, color=Region, text=paste("Value :", WRI_50))) +        
        geom_linerange() +
        geom_point(size=input$sizepoint) +
        geom_line(aes(y=WRI_50)) +
        ggtitle("Evolution of the index WRI over time") +
        xlab("Years") +
        ylab("WRI") +
        scale_color_manual(values=c(input$color_all,input$color_region)) +
        theme(legend.position = "bottom",
              plot.title = element_text(face="bold", hjust=0.5, size=input$sizetitle), 
              axis.title.x = element_text(size=input$sizetitleaxis, face="bold"),
              axis.title.y = element_text(size=input$sizetitleaxis, face="bold")) 
    
    p <- p %>%
      ggplotly(tooltip="text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
    })
  })

  output$Plot_by_region_Exposure <- renderPlotly({
    input$go 
    isolate({
    p <- ggplot(Data_final_by_Region(), aes(x=Year, y=Exposure_50, ymax=Exposure_75, ymin=Exposure_25, group=Region, color=Region, text=paste("Value :", Exposure_50))) +        
      geom_linerange() + 
      geom_point(size=input$sizepoint) +
      geom_line(aes(y=Exposure_50)) +
      ggtitle("Evolution of the index Exposure over time") +
      xlab("Years") +
      ylab("WRI") +
      scale_color_manual(values=c(input$color_all,input$color_region)) +
      theme(legend.position = "bottom",
            plot.title = element_text(face="bold", hjust=0.5, size=input$sizetitle), 
            axis.title.x = element_text(size=input$sizetitleaxis, face="bold"),
            axis.title.y = element_text(size=input$sizetitleaxis, face="bold")) 
    
    p <- p %>%
      ggplotly(tooltip="text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
    })
  })
  
  output$Plot_by_region_Vulnerability <- renderPlotly({
    input$go 
    isolate({
    p <- ggplot(Data_final_by_Region(), aes(x=Year, y=Vulnerability_50, ymax=Vulnerability_75, ymin=Vulnerability_25, group=Region, color=Region, text=paste("Value :", Vulnerability_50))) +        
      geom_linerange() + 
      geom_point(size=input$sizepoint) +
      geom_line(aes(y=Vulnerability_50)) +
      ggtitle("Evolution of the index Vulnerability over time") +
      xlab("Years") +
      ylab("WRI") +
      scale_color_manual(values=c(input$color_all,input$color_region)) +
      theme(legend.position = "bottom",
            plot.title = element_text(face="bold", hjust=0.5, size=input$sizetitle), 
            axis.title.x = element_text(size=input$sizetitleaxis, face="bold"),
            axis.title.y = element_text(size=input$sizetitleaxis, face="bold")) 
    
    p <- p %>%
      ggplotly(tooltip="text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
    })
  })
  
  output$Plot_by_region_Susceptibility <- renderPlotly({
    input$go 
    isolate({
    p <- ggplot(Data_final_by_Region(), aes(x=Year, y=Susceptibility_50, ymax=Susceptibility_75, ymin=Susceptibility_25, group=Region, color=Region, text=paste("Value :", Susceptibility_50))) +        
      geom_linerange() + 
      geom_point(size=input$sizepoint) +
      geom_line(aes(y=Susceptibility_50)) +
      ggtitle("Evolution of the index Susceptibility over time") +
      xlab("Years") +
      ylab("WRI") +
      scale_color_manual(values=c(input$color_all,input$color_region)) +
      theme(legend.position = "bottom",
            plot.title = element_text(face="bold", hjust=0.5, size=input$sizetitle), 
            axis.title.x = element_text(size=input$sizetitleaxis, face="bold"),
            axis.title.y = element_text(size=input$sizetitleaxis, face="bold")) 
    
    p <- p %>%
      ggplotly(tooltip="text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
    })
  })
  
  output$Plot_by_region_Lack.of.Coping.Capabilities <- renderPlotly({
    input$go 
    isolate({
    p <- ggplot(Data_final_by_Region(), aes(x=Year, y=Lack.of.Coping.Capabilities_50, ymax=Lack.of.Coping.Capabilities_75, ymin=Lack.of.Coping.Capabilities_25, group=Region, color=Region, text=paste("Value :", Lack.of.Coping.Capabilities_50))) +        
      geom_linerange() + 
      geom_point(size=input$sizepoint) +
      geom_line(aes(y=Lack.of.Coping.Capabilities_50)) +
      ggtitle("Evolution of the index Lack.of.Coping.Capabilities over time") +
      xlab("Years") +
      ylab("WRI") +
      scale_color_manual(values=c(input$color_all,input$color_region)) +
      theme(legend.position = "bottom",
            plot.title = element_text(face="bold", hjust=0.5, size=input$sizetitle), 
            axis.title.x = element_text(size=input$sizetitleaxis, face="bold"),
            axis.title.y = element_text(size=input$sizetitleaxis, face="bold")) 
    
    p <- p %>%
      ggplotly(tooltip="text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
    })
  })
    
  output$Plot_by_region_Lack.of.Adaptive.Capacities <- renderPlotly({
    input$go 
    isolate({
    p <- ggplot(Data_final_by_Region(), aes(x=Year, y=Lack.of.Adaptive.Capacities_50, ymax=Lack.of.Adaptive.Capacities_75, ymin=Lack.of.Adaptive.Capacities_25, group=Region, color=Region, text=paste("Value :", Lack.of.Adaptive.Capacities_50))) +        
      geom_linerange() + 
      geom_point(size=input$sizepoint) +
      geom_line(aes(y=Lack.of.Adaptive.Capacities_50)) +
      ggtitle("Evolution of the index Lack.of.Adaptive.Capacities over time") +
      xlab("Years") +
      ylab("WRI") +
      scale_color_manual(values=c(input$color_all,input$color_region)) +
      theme(legend.position = "bottom",
            plot.title = element_text(face="bold", hjust=0.5, size=input$sizetitle), 
            axis.title.x = element_text(size=input$sizetitleaxis, face="bold"),
            axis.title.y = element_text(size=input$sizetitleaxis, face="bold")) 
    
    p <- p %>%
      ggplotly(tooltip="text") %>%
      layout(legend = list(orientation = "h", y = -0.2))
    })
  })
  
  # Onglet continents
  
  #mise a jour de la liste deroulante des continents dans l'ui
  updateSelectizeInput(session, "IndexList", choices = colnames(World_Risk[,2:7]), server = TRUE)
 
  #Calculer la moyenne de l'indicateur en fonction de ce que l'utilisateur a selectione
  plot_data <- reactive({
    if (input$IndexList == "Exposure"){
      World_Risk %>%
        group_by(Year, Continent)  %>%
        summarise(mean = round(mean(Exposure)),2)
    }
    if (input$IndexList == "Vulnerability"){
      World_Risk %>%
        group_by(Year, Continent)  %>%
        summarise(mean = round(mean(Vulnerability)),2)
    }
    if (input$IndexList == "Susceptibility"){
      World_Risk %>%
        group_by(Year, Continent)  %>%
        summarise(mean = round(mean(Susceptibility)),2)
    }
    if (input$IndexList == "Lack.of.Coping.Capabilities"){
      World_Risk %>%
        group_by(Year, Continent)  %>%
        summarise(mean = round(mean(Lack.of.Coping.Capabilities)),2)
    }
    if (input$IndexList == "Lack.of.Adaptive.Capacities"){
      World_Risk %>%
        group_by(Year, Continent)  %>%
        summarise(mean = round(mean(Lack.of.Adaptive.Capacities)),2)
    }
    else {
      World_Risk %>%
        group_by(Year, Continent)  %>%
        summarise(mean = round(mean(WRI)),2)
    }
    
  })
  
  plot_index <- reactive({ 
    spread(plot_data(), Continent, mean)
  })
  
  # Graph interactif de l'evolution de l'indicateur au cours du temps pour les continents
  
  output$Plot_Index_Continent <- renderHighchart({
    highchart() %>% 
      hc_xAxis(categories = plot_index()$Year) %>% 
      hc_add_series(name = "Oceania", 
                    data = plot_index()$Oceania,
                    color= input$colorOceania) %>% 
      hc_add_series(name = "Asia", 
                    data = plot_index()$Asia,
                    color= input$colorAsia) %>%
      hc_add_series(name = "America", 
                    data = plot_index()$America,
                    color= input$colorAmerica) %>%
      hc_add_series(name = "Europe", 
                    data = plot_index()$Europe,
                    color= input$colorEurope) %>%
      hc_add_series(name = "Africa", 
                    data = plot_index()$Africa,
                    color= input$colorAfrica)%>%
      hc_title(text = paste("Evolution of", input$IndexList, "index over time"),
               margin = 20, 
               align = "left",
               style = list(color = "steelblue")) %>% 
      hc_subtitle(text = "2011 to 2021",
                  align = "left",
                  style = list(color = "#2b908f", 
                               fontWeight = "bold")) %>% 
      hc_legend(align = "left", 
                verticalAlign = "top",
                layout = "vertical", 
                x = 0, 
                y = 100) %>%
      hc_tooltip(crosshairs = TRUE, 
                 backgroundColor = "#FCFFC5",
                 shared = TRUE, 
                 borderWidth = 4) %>% 
      hc_exporting(enabled = TRUE)
    
  })
  
  data_boxplot <-reactive ({
    if (input$IndexList == "Exposure"){
      World_Risk %>%
        mutate("Index"=Exposure)
    }
    if (input$IndexList == "Vulnerability"){
      World_Risk %>%
        mutate("Index"=Vulnerability)
    }
    if (input$IndexList == "Susceptibility"){
      World_Risk %>%
        mutate("Index"=Susceptibility)
    }
    if (input$IndexList == "Lack.of.Coping.Capabilities"){
      World_Risk %>%
        mutate("Index"=Lack.of.Coping.Capabilities)
    }
    if (input$IndexList == "Lack.of.Adaptive.Capacities"){
      World_Risk %>%
        mutate("Index"=Lack.of.Adaptive.Capacities)
    }
    else{
      World_Risk %>%
        mutate("Index"=WRI)
    }
  })

  #Graph interactif boxplot par continent par annees
  output$Boxplot_Continent <- renderPlotly({
    plot_ly(data_boxplot(), x = ~Year, y = ~Index, color = ~Continent, 
            colors = c("Oceania" = input$colorOceania, "Asia"=input$colorAsia, "America"=input$colorAmerica, 
                       "Europe"=input$colorEurope, "Africa"=input$colorAfrica), type = "box") %>%
        layout(title =list(text= paste("Box Plot", input$IndexList, "Index for each continent and each year"), 
                           font=list(color = "steelblue", size=16, y=0.9, x=0.55),xanchor="right"),
               yaxis=list(title=input$IndexList),
               legend=list(xanchor="left", x=-0.35, y=0.8),
               plot_bgcolor = "#FFFFFF", boxmode = "group")
    })
  
  options(warn = -1)# boxmode est toujours dans la doc officielle
  
  #Onglet sur les indicateurs
  
  # WRI
  
  updateSelectizeInput(session, "YearListHigh_1", choices = World_Risk$Year, server = TRUE)

  updateSelectizeInput(session, "YearListLow_1", choices = World_Risk$Year, server = TRUE)
  
  # 10 regions par annee avec le WRI le plus eleve
  Region_WRI_high <- World_Risk %>%
    select(Year, WRI, Continent, Region) %>%
    group_by(Year) %>%
    arrange(desc(WRI)) %>%
    slice(1:10)
  
  output$plot_WRI_high <- renderPlotly ({
    ggplotly(ggplot(Region_WRI_high, aes(x=Year, y=WRI, color=WRI,
                                          text=paste("Region :", Region, "\n WRI :", WRI)))+
               geom_point() +
               ggtitle("The 10 regions with the highest WRI index") +
               xlab("Years") +
               ylab("WRI") +
               scale_color_gradient(low="#FFCC00", high="#FF0000") +
               theme(plot.title = element_text(face="bold", hjust=0.5, size=14), 
                     axis.title.x = element_text(size=12, face="bold"),
                     axis.title.y = element_text(size=12, face="bold")), tooltip="text")
  })
  
  output$table_WRI_high <- renderTable({
    Region_WRI_high %>%
      filter(Year==input$YearListHigh_1)
  })
  
  # 10 regions par annee avec le WRI le plus faible
  Region_WRI_low <- World_Risk %>%
    select(Year, WRI, Continent, Region) %>%
    group_by(Year) %>%
    arrange(WRI) %>%
    slice(1:10)
  
  output$plot_WRI_low <- renderPlotly ({
    ggplotly(ggplot(Region_WRI_low, aes(x=Year, y=WRI, color=WRI, 
                                           text=paste("Region :", Region, "\n WRI :", WRI)))+
               geom_point() +
               ggtitle("The 10 regions with the lowest WRI index") +
               xlab("Years") +
               ylab("WRI") +
               scale_color_gradient(low="#33FF33", high="#CCFF66") +
               theme(plot.title = element_text(face="bold", hjust=0.5, size=14), 
                     axis.title.x = element_text(size=12, face="bold"),
                     axis.title.y = element_text(size=12, face="bold")), tooltip="text")
  })
  
  output$table_WRI_low <- renderTable({
    Region_WRI_low %>%
      filter(Year==input$YearListLow_1)
  })
  
  # EXPOSURE
  
  updateSelectizeInput(session, "YearListHigh_2", choices = World_Risk$Year, server = TRUE)
  
  updateSelectizeInput(session, "YearListLow_2", choices = World_Risk$Year, server = TRUE)


# 10 regions par annee avec l'indicateur Exposure le plus eleve
  Region_Exposure_high <- World_Risk %>%
    select(Year, Exposure, Continent, Region) %>%
      group_by(Year)  %>%
        arrange(desc(Exposure)) %>%
          slice(1:10)

  output$plot_Exposure_high <- renderPlotly ({
    ggplotly(ggplot(Region_Exposure_high, aes(x=Year, y=Exposure, color=Exposure,
                                       text=paste("Region :", Region, "\n Exposure :", Exposure)))+
             geom_point() +
             ggtitle("The 10 regions with the highest Exposure index") +
             xlab("Years") +
             ylab("Exposure") +
             scale_color_gradient(low="#FFCC00", high="#FF0000") +
             theme(plot.title = element_text(face="bold", hjust=0.5, size=12), 
                   axis.title.x = element_text(size=12, face="bold"),
                   axis.title.y = element_text(size=12, face="bold")), tooltip="text")})

  output$table_Exposure_high <- renderTable({
    Region_Exposure_high %>%
      filter(Year==input$YearListHigh_2)})

# 10 regions par annee avec l'indicateur exposure le plus faible
  Region_Exposure_low <- World_Risk %>%
    select(Year, Exposure, Continent, Region) %>%
      group_by(Year) %>%
        arrange(Exposure) %>%
          slice(1:10)

  output$plot_Exposure_low <- renderPlotly ({
    ggplotly(ggplot(Region_Exposure_low, aes(x=Year, y=Exposure, color=Exposure, 
                                      text=paste("Region :", Region, "\n Exposure :", Exposure)))+
             geom_point() +
             ggtitle("The 10 regions with the lowest Exposure index") +
             xlab("Years") +
             ylab("Exposure") +
             scale_color_gradient(low="#33FF33", high="#CCFF66") +
             theme(plot.title = element_text(face="bold", hjust=0.5, size=12), 
                   axis.title.x = element_text(size=12, face="bold"),
                   axis.title.y = element_text(size=12, face="bold")), tooltip="text")})

  output$table_Exposure_low <- renderTable({
    Region_Exposure_low %>%
      filter(Year==input$YearListLow_2)})

# VULNERABILITY

updateSelectizeInput(session, "YearListHigh_3", choices = World_Risk$Year, server = TRUE)

updateSelectizeInput(session, "YearListLow_3", choices = World_Risk$Year, server = TRUE)


# 10 regions par annee avec l'indicateur Vulnerability le plus eleve
Region_Vulnerability_high <- World_Risk %>%
  select(Year, Vulnerability, Continent, Region) %>%
  group_by(Year)  %>%
  arrange(desc(Vulnerability)) %>%
  slice(1:10)

output$plot_Vulnerability_high <- renderPlotly ({
  ggplotly(ggplot(Region_Vulnerability_high, aes(x=Year, y=Vulnerability, color=Vulnerability,
                                            text=paste("Region :", Region, "\n Vulnerability :", Vulnerability)))+
             geom_point() +
             ggtitle("The 10 regions with the highest Vulnerability index") +
             xlab("Years") +
             ylab("Vulnerability") +
             scale_color_gradient(low="#FFCC00", high="#FF0000") +
             theme(plot.title = element_text(face="bold", hjust=0.5, size=12), 
                   axis.title.x = element_text(size=12, face="bold"),
                   axis.title.y = element_text(size=12, face="bold")), tooltip="text")
})

output$table_Vulnerability_high <- renderTable({
  Region_Vulnerability_high %>%
    filter(Year==input$YearListHigh_3)
})

# 10 regions par annee avec l'indicateur Vulnerability le plus faible
Region_Vulnerability_low <- World_Risk %>%
  select(Year, Vulnerability, Continent, Region) %>%
    group_by(Year) %>%
      arrange(Vulnerability) %>%
        slice(1:10)

output$plot_Vulnerability_low <- renderPlotly ({
  ggplotly(ggplot(Region_Vulnerability_low, aes(x=Year, y=Vulnerability, color=Vulnerability, 
                                           text=paste("Region :", Region, "\n Vulnerability :", Vulnerability)))+
             geom_point() +
             ggtitle("The 10 regions with the lowest Vulnerability index") +
             xlab("Years") +
             ylab("Vulnerability") +
             scale_color_gradient(low="#33FF33", high="#CCFF66") +
             theme(plot.title = element_text(face="bold", hjust=0.5, size=12), 
                   axis.title.x = element_text(size=12, face="bold"),
                   axis.title.y = element_text(size=12, face="bold")), tooltip="text")})

output$table_Vulnerability_low <- renderTable({
  Region_Vulnerability_low %>%
    filter(Year==input$YearListLow_3)})

# SUSCEPTIBILITY

updateSelectizeInput(session, "YearListHigh_4", choices = World_Risk$Year, server = TRUE)

updateSelectizeInput(session, "YearListLow_4", choices = World_Risk$Year, server = TRUE)


# 10 regions par annee avec l'indicateur Susceptibility le plus eleve
Region_Susceptibility_high <- World_Risk %>%
  select(Year, Susceptibility, Continent, Region) %>%
  group_by(Year)  %>%
  arrange(desc(Susceptibility)) %>%
  slice(1:10)

output$plot_Susceptibility_high <- renderPlotly ({
  ggplotly(ggplot(Region_Susceptibility_high, aes(x=Year, y=Susceptibility, color=Susceptibility,
                                                 text=paste("Region :", Region, "\n Susceptibility :", Susceptibility)))+
             geom_point() +
             ggtitle("The 10 regions with the highest Susceptibility index") +
             xlab("Years") +
             ylab("Susceptibility") +
             scale_color_gradient(low="#FFCC00", high="#FF0000") +
             theme(plot.title = element_text(face="bold", hjust=0.5, size=12), 
                   axis.title.x = element_text(size=12, face="bold"),
                   axis.title.y = element_text(size=12, face="bold")), tooltip="text")
})

output$table_Susceptibility_high <- renderTable({
  Region_Susceptibility_high %>%
    filter(Year==input$YearListHigh_4)
})

# 10 regions par annee avec l'indicateur Susceptibility le plus faible
Region_Susceptibility_low <- World_Risk %>%
  select(Year, Susceptibility, Continent, Region) %>%
  group_by(Year) %>%
  arrange(Susceptibility) %>%
  slice(1:10)

output$plot_Susceptibility_low <- renderPlotly ({
  ggplotly(ggplot(Region_Susceptibility_low, aes(x=Year, y=Susceptibility, color=Susceptibility, 
                                                text=paste("Region :", Region, "\n Susceptibility :", Susceptibility)))+
             geom_point() +
             ggtitle("The 10 regions with the lowest Susceptibility index") +
             xlab("Years") +
             ylab("Susceptibility") +
             scale_color_gradient(low="#33FF33", high="#CCFF66") +
             theme(plot.title = element_text(face="bold", hjust=0.5, size=12), 
                   axis.title.x = element_text(size=12, face="bold"),
                   axis.title.y = element_text(size=12, face="bold")), tooltip="text")
})

output$table_Susceptibility_low <- renderTable({
  Region_Susceptibility_low %>%
    filter(Year==input$YearListLow_4)})

# LACK.OF.COPING.CAPABILITIES

updateSelectizeInput(session, "YearListHigh_5", choices = World_Risk$Year, server = TRUE)

updateSelectizeInput(session, "YearListLow_5", choices = World_Risk$Year, server = TRUE)


# 10 regions par annee avec l'indicateur Lack.of.Coping.Capabilities le plus eleve
  Region_Lack.of.Coping.Capabilities_high <- World_Risk %>%
    select(Year, Lack.of.Coping.Capabilities, Continent, Region) %>%
      group_by(Year)  %>%
        arrange(desc(Lack.of.Coping.Capabilities)) %>%
          slice(1:10)

output$plot_Lack.of.Coping.Capabilities_high <- renderPlotly ({
  ggplotly(ggplot(Region_Lack.of.Coping.Capabilities_high, aes(x=Year, y=Lack.of.Coping.Capabilities, color=Lack.of.Coping.Capabilities,
                                                  text=paste("Region :", Region, "\n Lack of Coping Capabilities :", Lack.of.Coping.Capabilities)))+
             geom_point() +
             ggtitle("The 10 regions with the highest \n Lack of Coping Capabilities index") +
             xlab("Years") +
             ylab("Lack.of.Coping.Capabilities") +
             scale_color_gradient(low="#FFCC00", high="#FF0000") +
             theme(plot.title = element_text(face="bold", hjust=0.5, size=12), 
                   axis.title.x = element_text(size=10, face="bold"),
                   axis.title.y = element_text(size=10, face="bold")), tooltip="text")
})

output$table_Lack.of.Coping.Capabilities_high <- renderTable({
  Region_Lack.of.Coping.Capabilities_high %>%
    filter(Year==input$YearListHigh_5)
})

# 10 regions par annee avec l'indicateur Lack.of.Coping.Capabilities le plus faible
Region_Lack.of.Coping.Capabilities_low <- World_Risk %>%
  select(Year, Lack.of.Coping.Capabilities, Continent, Region) %>%
  group_by(Year) %>%
  arrange(Lack.of.Coping.Capabilities) %>%
  slice(1:10)

output$plot_Lack.of.Coping.Capabilities_low <- renderPlotly ({
  ggplotly(ggplot(Region_Lack.of.Coping.Capabilities_low, aes(x=Year, y=Lack.of.Coping.Capabilities, color=Lack.of.Coping.Capabilities, 
                                                 text=paste("Region :", Region, "\n Lack of Coping Capabilities :", Lack.of.Coping.Capabilities)))+
             geom_point() +
             ggtitle("The 10 regions with the lowest \n Lack of Coping Capabilities index") +
             xlab("Years") +
             ylab("Lack.of.Coping.Capabilities") +
             scale_color_gradient(low="#33FF33", high="#CCFF66") +
             theme(plot.title = element_text(face="bold", hjust=0.5, size=12), 
                   axis.title.x = element_text(size=10, face="bold"),
                   axis.title.y = element_text(size=10, face="bold")), tooltip="text")
})

output$table_Lack.of.Coping.Capabilities_low <- renderTable({
  Region_Lack.of.Coping.Capabilities_low %>%
    filter(Year==input$YearListLow_5)})

# Lack.of.Adaptive.Capacities

updateSelectizeInput(session, "YearListHigh_6", choices = World_Risk$Year, server = TRUE)

updateSelectizeInput(session, "YearListLow_6", choices = World_Risk$Year, server = TRUE)


# 10 regions par annee avec l'indicateur Lack.of.Adaptive.Capacities le plus eleve
Region_Lack.of.Adaptive.Capacities_high <- World_Risk %>%
  select(Year, Lack.of.Adaptive.Capacities, Continent, Region) %>%
  group_by(Year)  %>%
  arrange(desc(Lack.of.Adaptive.Capacities)) %>%
  slice(1:10)

output$plot_Lack.of.Adaptive.Capacities_high <- renderPlotly ({
  ggplotly(ggplot(Region_Lack.of.Adaptive.Capacities_high, aes(x=Year, y=Lack.of.Adaptive.Capacities, color=Lack.of.Adaptive.Capacities,
                                                               text=paste("Region :", Region, "\n Lack of Adaptive Capacities :", Lack.of.Adaptive.Capacities)))+
             geom_point() +
             ggtitle("The 10 regions with the highest \n Lack of Adaptive Capacities index") +
             xlab("Years") +
             ylab("Lack.of.Adaptive.Capacities") +
             scale_color_gradient(low="#FFCC00", high="#FF0000") +
             theme(plot.title = element_text(face="bold", hjust=0.5, size=12), 
                   axis.title.x = element_text(size=10, face="bold"),
                   axis.title.y = element_text(size=10, face="bold")), tooltip="text")
})

output$table_Lack.of.Adaptive.Capacities_high <- renderTable({
  Region_Lack.of.Adaptive.Capacities_high %>%
    filter(Year==input$YearListHigh_6)
})

# 10 regions par annee avec l'indicateur Lack.of.Adaptive.Capacities le plus faible
Region_Lack.of.Adaptive.Capacities_low <- World_Risk %>%
  select(Year, Lack.of.Adaptive.Capacities, Continent, Region) %>%
  group_by(Year) %>%
  arrange(Lack.of.Adaptive.Capacities) %>%
  slice(1:10)

output$plot_Lack.of.Adaptive.Capacities_low <- renderPlotly ({
  ggplotly(ggplot(Region_Lack.of.Adaptive.Capacities_low, aes(x=Year, y=Lack.of.Adaptive.Capacities, color=Lack.of.Adaptive.Capacities, 
                                                              text=paste("Region :", Region, "\n Lack of Adaptive Capacities :", Lack.of.Adaptive.Capacities)))+
             geom_point() +
             ggtitle("The 10 regions with the lowest \n Lack of Adaptive Capacities index") +
             xlab("Years") +
             ylab("Lack.of.Adaptive.Capacities") +
             scale_color_gradient(low="#33FF33", high="#CCFF66") +
             theme(plot.title = element_text(face="bold", hjust=0.5, size=12), 
                   axis.title.x = element_text(size=10, face="bold"),
                   axis.title.y = element_text(size=10, face="bold")), tooltip="text")
})

output$table_Lack.of.Adaptive.Capacities_low <- renderTable({
  Region_Lack.of.Adaptive.Capacities_low %>%
    filter(Year==input$YearListLow_6)})


})
