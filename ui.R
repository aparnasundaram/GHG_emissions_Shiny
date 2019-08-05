library(shinydashboard)

shinyUI(dashboardPage(
  dashboardHeader(title = "Shiny Project"),
  
  dashboardSidebar(
    sidebarUserPanel("Aparna Sundaram"),
    
    sidebarMenu(menuItem("Graph Emissions Per Capita", 
                         tabName = "GHGPC", 
                         icon = icon("chart-line")),
                
                menuItem("Graph Emissions Total", 
                         tabName = "GHGT", 
                         icon = icon("chart-line")),
                
                menuItem("Graph Population", 
                         tabName = "Pop", 
                         icon = icon("chart-line")),
                
                
                menuItem("Global Map Emissions Per Capita", 
                         tabName = "map1", 
                         icon = icon("map")),
                
                menuItem("Global Map Emissions Total", 
                        tabName = "map2", 
                        icon = icon("map")),
  
                menuItem("Global Map Population", 
                        tabName = "map3", 
                        icon = icon("map"))),
  
    
    selectizeInput(inputId = "Country1",
                   label = "Choose a country",
                   choices = unique(GHG$Country.Name),
                   options = list(maxOptions = 265)),
    
    selectizeInput(inputId = "Country2",
                   label = "Choose a second country",
                   choices = unique(GHG$Country.Name),
                   options = list(maxOptions = 265))
    
  ),
  
 dashboardBody(
   
   tabItems(
     
     tabItem(tabName = "GHGPC",
             fluidRow(box(plotlyOutput("graph1"), height = 500, width = 10))),
     
     tabItem(tabName = "GHGT",
             fluidRow(box(plotlyOutput("graph2"), height = 500, width = 10))),
     
     tabItem(tabName = "Pop",
             fluidRow(box(plotlyOutput("graph3"), height = 500, width = 10))),
     
     tabItem(tabName = "map1",
             fluidRow(box(htmlOutput("map_glob1"), width = 10)),
             fluidRow(box(sliderInput(
               "year1",
               "Years:",
               min = min(GHG$year),
               max = max(GHG$year),
               value = 1970,animate=animationOptions(interval=1500),step=1), 
               height = 110, width = 10))),
     
     tabItem(tabName = "map2",
             fluidRow(box(htmlOutput("map_glob2"), width = 10)),
             fluidRow(box(sliderInput(
               "year2",
               "Years:",
               min = min(GHG$year),
               max = max(GHG$year),
               value = 1970,animate=animationOptions(interval=1500),step=1), 
               height = 110, width = 10))),
     
     
     tabItem(tabName = "map3",
             fluidRow(box(htmlOutput("map_glob3"), width = 10)),
             fluidRow(box(sliderInput(
               "year3",
               "Years:",
               min = min(GHG$year),
               max = max(GHG$year),
               value = 1970,animate=animationOptions(interval=1500),step=1), 
               height = 110, width = 10)))
     
     
     
     
     
   
   )
 )
))

# 
# fluidPage(
#   titlePanel("Greenhouse Gas Emissions Per Capita and Over Time"),
#   sidebarLayout(
#     sidebarPanel(
#       selectizeInput(inputId = "Country",
#                      label = "Choose a country",
#                      choices = unique(GHG$Country.Name)
#                      ),
#       selectizeInput(inputID = "Year",
#                      label = "Year",
#                      choices= unique(GHG$year))
# 
#     ),
#     mainPanel(plotOutput("line")
# 
#     )
#   )
#      )
