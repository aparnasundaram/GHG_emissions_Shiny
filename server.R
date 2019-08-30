shinyServer(function(input, output){
  
##graphs  
  
  df_GHGpc1 <- reactive({
    GHG %>%
      filter(Country.Name == input$Country1)
  })
  
  df_GHGpc2 <- reactive({
    GHG %>%
      filter(Country.Name == input$Country2)
  })

  output$graph1 <- renderPlotly({
    df_GHGpc1 = df_GHGpc1()
    df_GHGpc2 = df_GHGpc2()

   g= ggplot() +
      geom_line(data = df_GHGpc1, aes(x=year, y=emissionsPC, color=Country.Name), 
                size = 1) +
      geom_point(data=df_GHGpc1, aes(x=year, y=emissionsPC),size = 2) + 
      
      #specify a color scheme
      scale_colour_wsj("colors6", "")+ theme_wsj()+
      
      #coord_cartesian(ylim=c(0.0001005067, 0.1656618), xlim=c(1970, 2012)) +
      
      #format the legend
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank())+
      guides(colour = guide_legend(override.aes = list(size=4))) +
      
      
      geom_line(data = df_GHGpc2, aes(x=year, y=emissionsPC, color=Country.Name), 
                size = 1) +
      geom_point(data=df_GHGpc2, aes(x=year, y=emissionsPC),size = 2) + 
      
      #specify a color scheme
      scale_colour_wsj("colors6", "")+ theme_wsj()+
      
      #coord_cartesian(ylim=c(0.0001005067, 0.1656618), xlim=c(1970, 2012)) +
      
      #format the legend.
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank()) +
      guides(colour = guide_legend(override.aes = list(size=4)))+
      
      
      #include a title and provide x and y axis labels.\n adds space b/w title and axis
      labs(title="Greenhouse Gas Emissions Per Capita by Year",
           x='\nYears',
           y='GHG Emissions Per Capita (KT of CO2 equiv.)\n',
           caption="Source: World Bank, World Develoment Indicators") +
      ylim(0.0001005067, 0.1656618) +
      
      
     #format the tick marks, axis labels, plot title and their size
      theme(
        axis.ticks.length.y = unit(.25, "cm"),
        axis.ticks.length.x = unit(.25, "cm"),
        axis.text.x = element_text(margin = margin(t = .3, unit = "cm"), size=10),
        axis.text.y = element_text(margin = margin(t = .3, unit = "cm"), size=10),
        axis.title  = element_text(margin = margin(t = .3, unit = "cm"), size=15,
                                   face = "bold"),
        axis.title.y= element_text(margin = margin(t = .3, unit = "cm"), size = rel(0.8),
                                   angle = 90, face = "bold"),
        plot.title =  element_text(margin = margin(10, 0, 10, 0), size=20, 
                                   face="bold"),
        plot.caption = element_text(size = 10, hjust=0))
        
    
   g=ggplotly(g) 
   
   #%>% layout(height = input$plotHeight, autosize=TRUE)
    
  })

  df_GHG1 <- reactive({
    GHG %>%
      filter(Country.Name == input$Country1)
  })
  
  df_GHG2 <- reactive({
    GHG %>%
      filter(Country.Name == input$Country2)
  })
  
  output$graph2 <- renderPlotly({
    df_GHG1 = df_GHG1()
    df_GHG2 = df_GHG2()
    
    p = ggplot() +
      geom_line(data = df_GHG1, aes(x = year, y = emissionsTotL, color=Country.Name), 
                size = 1) +
      geom_point(data = df_GHG1, aes(x = year, y = emissionsTotL), size = 2) +
      
      #specify a color scheme
      scale_colour_wsj("colors6", "") + theme_wsj() +
      
      #coord_cartesian(ylim = c(1.39021, 53526303), xlim = c(1970,2012)) +
      
      #format the legend
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank())+
      guides(colour = guide_legend(override.aes = list(size=4))) +
      
      
      geom_line(data = df_GHG2, aes(x = year, y = emissionsTotL, color=Country.Name), 
                size = 1) +
      geom_point(data = df_GHG2, aes(x = year, y = emissionsTotL), size = 2) +
      
      #specify a color scheme
      scale_colour_wsj("colors6", "")+ theme_wsj() +
        
      #coord_cartesian(ylim = c(1.39021, 53526303), xlim = c(1970,2012)) +
      
      #format the legend
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank())+
      guides(colour = guide_legend(override.aes = list(size=4))) +
      
      #include a title and provide x and y axis labels.\n adds space b/w title and axis
      labs(title = "Total Greenhouse Gas Emissions by Year",
           x='\nYears',
           y='Total GHG Emissions (Ln of KT of CO2 equiv.)\n',
           caption="Source: World Bank, World Develoment Indicators") +
      
      scale_y_continuous(limits = c(0.3294546, 17.79568), 
                         breaks = seq(0.3294546, 17.79568, by = 1),
                         labels = scales::comma) +
      #ylim(1.39021, 53526303) +
    
    #format the tick marks, axis labels, plot title and their size
    theme(
      axis.ticks.length.y = unit(.25, "cm"),
      axis.ticks.length.x = unit(.25, "cm"),
      axis.text.x = element_text(margin = margin(t = .3, unit = "cm"), size=10),
      axis.text.y = element_text(margin = margin(t = .3, unit = "cm"), size=10),
      axis.title  = element_text(margin = margin(t = .3, unit = "cm"), size=15,
                                 face = "bold"),
      axis.title.y= element_text(margin = margin(t = .3, unit = "cm"), size = rel(0.8),
                                 angle = 90, face = "bold"),
      plot.title =  element_text(margin = margin(10, 0, 10, 0), size=20, 
                                 face="bold"),
      plot.caption = element_text(size = 10, hjust=0))
      
    
    
    p=ggplotly(p)
  
  })
  
  df_pop1 = reactive({
    GHG %>%
      filter(Country.Name == input$Country1) 
  }) 
  
  df_pop2 = reactive({
    GHG %>%
      filter(Country.Name == input$Country2)
    
  })
  
  output$graph3 <- renderPlotly({
    df_pop1 = df_pop1()
    df_pop2 = df_pop2()
    
  l= ggplot() +
    
      geom_line(data = df_pop1, aes(x = year, y = populationL, color = Country.Name), 
                size = 1) +
      geom_point(data= df_pop1, aes(x = year, y = populationL),size = 2) +
      
      #specify a color scheme
      scale_colour_wsj("colors6", "") + theme_wsj() +
      
      #coord_cartesian(ylim=c(8.291296, 22.75066), xlim=c(1970, 2012)) +
      
      #format the legend
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank())+
      guides(colour = guide_legend(override.aes = list(size=4))) +
      
      geom_line(data = df_pop2, aes(x = year, y = populationL, color = Country.Name), 
                size = 1) +
      geom_point(data = df_pop2, aes(x = year, y = populationL), size = 2) +
      
      #specify a color scheme
      scale_colour_wsj("colors6", "") + theme_wsj() +
      
      #coord_cartesian(ylim=c(8.291296, 22.75066), xlim=c(1970, 2012)) +
      
      #format the legend
      theme(legend.position = "bottom") +
      theme(legend.title=element_blank())+
      guides(colour = guide_legend(override.aes = list(size=4))) +
      
      labs(title = "Total Population by Year (ln)",
           x='\nYears',
           y='Ln Population\n', 
           caption="Source: World Bank, World Develoment Indicators") +
      
      scale_y_continuous(limits = c(8.291296, 22.75066), 
                         breaks = seq(8.291296, 22.75066, by = 1),
                         labels = scales::comma) 
    #ylim(3989, 7594270356) +
    
    
    ## format the tick marks, axis labels, plot title and their size
    theme(
      axis.ticks.length.y = unit(.25, "cm"),
      axis.ticks.length.x = unit(.25, "cm"),
      axis.text.x = element_text(margin = margin(t = .3, unit = "cm"), size=10),
      axis.text.y = element_text(margin = margin(t = .3, unit = "cm"), size=10),
      axis.title  = element_text(margin = margin(t = .3, unit = "cm"), size=15,
                                 face = "bold"),
      axis.title.y= element_text(margin = margin(t = .3, unit = "cm"), size = rel(0.8),
                                 angle = 90, face = "bold"),
      plot.title =  element_text(margin = margin(10, 0, 10, 0), size=20,
                                 face="bold"),
      plot.caption = element_text(size = 10, hjust=0))
    
    l = ggplotly(l) #, height=800)
    
})

  
  ##Maps
  
  df_map_glob1 <- reactive({
    GHG %>%
      
      filter(year == input$year1)
    
  })
  
  output$map_glob1 <- renderGvis({
    df_map_glob1 = df_map_glob1()
    Sys.sleep(0.3)
    map=gvisGeoChart(df_map_glob1,
                 "Country.Name", 
                 "emissionsPC",
                  options=list(
                    region = 'world', 
                    displayMode = 'regions',
                    
                    colorAxis = "{colors:['lightblue', 'purple', 'blue']}",
                    
                    backgroundColor = 'white', #dceef7',
                    datalessRegionColor = 'grey',
                    forceFrame = TRUE,
                    
                    width="auto", 
                    height = "auto", 
                    projection="kavrayskiy-vii")) 
    return(map)
    
  })
  
  
  df_map_glob2 <- reactive({
    GHG %>%
      
      filter(year == input$year2)
    
  })
  
  output$map_glob2 <- renderGvis({
    df_map_glob2 = df_map_glob2()
    Sys.sleep(0.3)
    gvisGeoChart(df_map_glob2,
                     "Country.Name", 
                     "emissionsTotL",
                      options=list(
                        region = 'world', 
                        displayMode = 'regions',
                        
                        colorAxis = "{colors:['pink','lightblue', 'darkblue']}",
                        
                        backgroundColor = 'white', #dceef7',
                        datalessRegionColor = 'grey',
                        forceFrame = TRUE,
                        
                        width="auto", 
                        height = "auto", 
                        projection="kavrayskiy-vii"))
  })

  df_map_glob3 <- reactive({
    GHG %>%
      filter(year == input$year3)
    
  })
  
  output$map_glob3 <- renderGvis({
    df_map_glob3 = df_map_glob3()
    Sys.sleep(0.3)
    gvisGeoChart(df_map_glob3,
                     "Country.Name", 
                     "populationL",
                      options=list(
                        region = 'world', 
                        displayMode = 'regions',
                        
                        colorAxis = "{colors:['grey', 'lightblue', 'darkblue']}",
                        
                        backgroundColor = 'white', #dceef7',
                        datalessRegionColor = 'grey',
                        forceFrame = TRUE,
                        width="auto", 
                        height = "auto", 
                        projection="kavrayskiy-vii"))
    
  })
  
})  
  

