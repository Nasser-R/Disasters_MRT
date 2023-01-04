
# # Load data on disasters in Mauritania

library(shiny)
library(shinydashboard)
library(plotly)
library(scales)
library(leaflet)
library(sp)
library(ggplot2)
library(tidyverse)
library(rsconnect)

disasters <- read.csv("DisastersR.csv")
disasters[1, 15] <- -10.250000
Sys.setenv(TZ='GMT')

ui <- dashboardPage(
  dashboardHeader(title = "SUMMARY OF DISASTERS IN MAURITANIA", titleWidth = 500),
  dashboardSidebar(sidebarMenu(selectInput(inputId = "Disaster.Type",
                                  label ="Disaster type",
                                  choices = unique(disasters$Disaster.Type),
                                  selected = "Flood"
                                  )
                               )),
  dashboardBody(
    fluidRow(
      box(leafletOutput("map2"),
          title = "DISASTERS BY YEAR OF OCCURENCE",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE,
          sliderInput(inputId = "year",
                      label = "year",
                      min = 1965,
                      max = 2020,
                      value = 2020)
          ),
      box(plotOutput("plotmp"),
          title = "YEARLY OCCURED DISASTERS",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE)
    ),
    fluidRow(
      box(plotOutput(outputId = "plot1"),
          title = "COUNT OF DISASTER BY SUBTYPE",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE),
      box(plotOutput(outputId = "plot2"),
          title = "TOTAL AFFECTED, DEATHS & HOMELESS",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE)
    ),
    fluidRow(
      box(plotOutput(outputId = "plot3"),
          title = "TOTAL AFFECTED BY REGION",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE),
      box(leafletOutput("map"),
          title = "SPATIAL DISTRIBUTION OF DISASTERS BY REGION",
          status = "primary",
          solidHeader = TRUE,
          collapsible = TRUE)
    )
    )
  )


server <- function(input, output, session){
  
    disaster_data <- reactive({disasters[disasters$Disaster.Type == input$Disaster.Type, ]})
    year <- reactive({
      disasters[disasters$Year == input$year, ]
    })
    tabpr <- reactive({
      year() %>% group_by(Disaster.Type) %>% summarise(n = n())
    })
    Aff <- reactive({disaster_data() %>% group_by(Year) %>% summarise(v1 = sum(Total.Affected),
                                                                     Deaths = sum(Total.Deaths),
                                                                     Homeless = sum(No.Homeless))}) 
    total_affected <- reactive({sum(disaster_data()$`Total Affected`, na.rm = TRUE)}) 
    
    tab <- disasters %>% group_by(Disaster.Type) %>% summarise(n = n())
    
  
################## OUTPUT SECTION #########################
    #################################################
    
    output$map2 <- renderLeaflet({
      
      df1 <- data.frame(longitude = year()$Longitude,
                       latitude = year()$Latitude)
      coordinates(df1) <- ~longitude+latitude
      leaflet(df1) %>% addMarkers(popup = year()$Disaster.Type) %>% addTiles()
    })
    
    output$plotmp <- renderPlot({
      ggplot(tabpr())+
        geom_bar(aes(y = prop.table(tabpr()$n)*100, fill = Disaster.Type, stat = "count"))+
        coord_polar("y", start = 0)+
        ggtitle(paste("Disaster by type in", input$year))+
        theme(plot.title = element_text(size = 20, color = "red", face = "bold"),
              axis.title = element_blank(),
              axis.text = element_blank(),
              axis.ticks = element_blank()
              )+
        guides(guide_legend(title = paste("Disaster type in", input$year))
                            )
    })
      
  output$plot1 <- renderPlot({
    ggplot(disaster_data())+
      geom_bar(aes(x = input$Disaster.Type, fill = Disaster.Subtype))+
      ggtitle(paste("Disaster count ::", input$Disaster.Type))+
      theme(plot.title = element_text(size = 20, color = "red",
                                      face = "bold"),
            axis.title = element_blank(),
            axis.title.y = element_text("Count of disaster's occurence"))
      
    })

  output$plot2 <- renderPlot({
  ggplot(Aff())+
      geom_line(aes(x=Year, y = v1, linetype = 'Affected' ), colour = "green", size = 2) +
      scale_linetype_manual('Total Affected', values = 'solid')+
      geom_col(aes(x = Year, y = Deaths, fill = factor(Deaths)))+
      geom_point(aes(x= Year, y= Homeless, color = Homeless), size = 12, stroke = 5,
                 alpha = 0.5 )+
      ggtitle("Total affected, Deaths & Homeless over time")+
      theme(axis.title = element_blank(), plot.title = element_text(size = 20,
                                                                    color = "red",
                                                                    face = "bold"),
            legend.title = element_text("Homeless"))
  })
  
  yr <- reactive({disaster_data() %>% group_by(Location) %>%
      summarise(sum(Total.Affected),Disaster.Subtype)})
  
  output$plot3 <- renderPlot({
    ggplot(yr())+
      geom_col(aes(x=reorder(Location, `sum(Total.Affected)`), y = `sum(Total.Affected)`,
                   fill = Disaster.Subtype))+
      ggtitle("Total Affected by Region")+
      theme(axis.title = element_blank(), plot.title = element_text(size = 20,
                                                                    color = "red",
                                                                    face = "bold"))+
      coord_flip()
  })
  output$map <- renderLeaflet({

        df <- data.frame(longitude = disaster_data()$Longitude,
                         latitude = disaster_data()$Latitude)
        coordinates(df) <- ~longitude+latitude
        leaflet(df) %>% addMarkers(popup = input$Disaster.Type) %>% addTiles()
      })
}
shinyApp(ui, server)













































