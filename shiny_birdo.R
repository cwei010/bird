#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(plotly)
library(tidyverse)
library(sf)
library(sfnetworks)
library(leaflet)
library(colorRamps)
library(viridis)

ignoreyear <- function(date){
  year(date) <- 2022
  return(date)
} #will return 2022-mm-dd for any year
hr2 <- read_csv("hr2.csv")


# Define UI for application 
ui <- fluidPage(
  
  sidebarLayout(
    sidebarPanel(
      h2("Hampton Roads Bird Hotspots"),
      h3("Where's Birdo?"),
      tags$style(HTML("
    .tabbable > .nav > li > a                  {background-color: black;  color:white}
    .tabbable > .nav > li > a[data-value='Map'] {background-color: black;   color:white}
    .tabbable > .nav > li > a[data-value='plot'] {background-color: black;  color:white}
     ")),
     
      # date 
      dateInput(inputId = "date", label = "Date:", 
                format = "yyyy-mm-dd",
                value = Sys.Date(),
                min = Sys.Date()), 
      
      actionButton("do", "Show"),
      
      p(" "),
      
      p(" "),
      
      p(" "),
      
      p("Proudly developed by Chi Wei, Mohammad Shiri, and Oleksii Dubovyk"),
      
      br(),
      
      p("Old Dominion University | VA, USA | 2022"),
      
      br(),
      
      p("Size of the bubbles corresponds to spatial cluster size. Areas with large clusters have small density of past observations."),
      
      br(),
      
      p("Based on the data from eBird: https://ebird.org/"),
      
      br(),
      
      p("Contact: oadubovyk@gmail.com")
      
    ),
    mainPanel(
      htmlOutput("text"),
      
      tabsetPanel(
        
        tabPanel("Map", leafletOutput("map")),
        
        tabPanel("Plot", plotlyOutput(outputId = "plot"))
        
      ) # tabsetPanel
    ) # mainPanel
  ) # sidebarLayout
) # fluidPage




############# server ############

server <- function(input, output){
  
  filter_data2 <- eventReactive(input$do, {
    
    hr2 %>%
      filter(date == input$date %>% ignoreyear())
    
  })
  
  output$map <- renderLeaflet({
    
    pal <- colorNumeric(
      palette = colorRamps::matlab.like(50),
      domain = filter_data2()$value,
      na.color = "#808080",
      alpha = FALSE,
      reverse = FALSE
    )
    
    leaflet(filter_data2()) %>% 
      addTiles() %>%
      fitBounds(~min(long), ~min(lat), ~max(long), ~max(lat)) %>%
      addCircles(radius = ~80000*mdist, weight = 1, color = ~pal(filter_data2()$value),
                 fillColor = ~pal(filter_data2()$value), fillOpacity = 0.7) %>%
      addLegend(pal = pal, values = c(filter_data2()$value), opacity = 0.7,
                title = 'Species Richness')
       }) 
  
  output$plot <- renderPlotly({
    ggplotly({
      p <- ggplot(hr2 %>%
                    select(day, value) %>%
                    group_by(day) %>%
                    summarise(value = mean(value)), 
                  aes(x = day, y = value)) +
        geom_line(size = 0.5, color = "red")+
        geom_point(alpha = 0.5, color = "red") +
        ylab("Mean Species Richness")+
        xlab("Day of a Year")+
        scale_x_continuous(limits = c(1, 366))
        theme(legend.position = "none")
      p
    })
  })
  
  
  output$text <- renderUI({
    str1 <- paste("You have selected the date: ", input$date)
    HTML(str1)
  })
  
}


shinyApp(ui = ui, server = server)