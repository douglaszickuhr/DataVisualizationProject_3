# National College of Ireland
# Data Visualization Project
# Interactive Visualization 1
# Title: Overview of Flights in Brazil by date
# Author: Douglas Zikcuhr

# Loading libraries
library(lubridate)
library(tidyverse)
library(dplyr)
library(shiny)
library(DT)
library(data.table)
library(highcharter)

loadFile <- function(file = NULL,
                     colClasses = NULL){
  
  df <- fread(input = file,
              colClasses = colClasses)
  
  return(df)
  
}

totalByFlight <- loadFile("AverageDelayByFlight.csv",
                          colClasses = c("factor","factor","factor","numeric",
                                        "numeric","integer"))

arrivalDelay <- loadFile("PercDelay.csv",
                         colClasses = c("factor","factor","numeric","numeric",
                                        "numeric","factor"))

# Function to return the description plus image
returnDescription <- function(type){
  HTML(paste(img(src=paste0('http://www.tijoletarustica.com.br/img/',type,'.png'),
                 height = "15px"),type))
}

# Shiny User interface
ui <- fluidPage(
  
  # Title of panel
  titlePanel(paste("Brazilian Air-traffic Airlines Overview"), 
             windowTitle = "Data Visualization CA2 - Plot 1"),
  helpText("The purpose on this dashboard is to have an overview of Airlines tha operates in Brazil."),
  sidebarLayout(
    
    # The panel has the select input
    sidebarPanel(
      width = 3,
      
      # Well panel keep things tidy - Flight selection is the first
      wellPanel(# Select Flight Type input
        selectInput(inputId = "type",
                    label = "Flight Type",
                    choices = levels(totalByFlight$Flight.Type),
                    multiple = F,
                    selected = "International"
        )
      ),
      wellPanel(
        h5(tags$a(img(src = "https://www.ncirl.ie/Portals/0/nciLogo.png", 
                      height = "30px"),
                  href = "https://www.ncirl.ie"
        ),
        br(),
        tags$a("Student: Douglas Zickuhr",
               href="https://www.linkedin.com/in/douglas-zickuhr/"),
        br(),
        "Student Number: 17111781"),
        tags$a(h5("Data extracted from Kaggle"),
               href = "https://www.kaggle.com/ramirobentes/exploring-civil-aviation-in-brazil/data")
      ),
      
      wellPanel(h5("Built with",
                   tags$a(img(src = "https://www.rstudio.com/wp-content/uploads/2014/04/shiny.png",
                              height = "30px"),
                          href="https://shiny.rstudio.com/"),
                   "by",
                   tags$a(img(src = "https://www.rstudio.com/wp-content/uploads/2014/07/RStudio-Logo-Blue-Gray.png",
                              height = "30px"),
                          href="https://www.rstudio.com"),
                   "."))
    ),
    
    # Output panel:
    mainPanel(
      
      # Tabset to create different tabs      
      tabsetPanel(
        
        #First tab for Map
        tabPanel("Top Airlines",
                 numericInput(inputId = "numberOfTop",
                              label = "Number of Airlines",
                              value = 10,
                              min = 5,
                              max = 20,
                              step = 1),
                 # Well panel to organise the output
                 wellPanel(
                   tabsetPanel(
                     tabPanel("Delaying",
                              highchartOutput(outputId = "top_delaying")
                     ),
                     tabPanel("No of Flights",
                              highchartOutput(outputId = "top_flights")
                     ),
                     tabPanel("Percentage of Delay",
                              tabsetPanel(
                                tabPanel(returnDescription(type = "Departures"),
                                         highchartOutput(outputId = "perc_delay_departure")
                                ),
                                tabPanel(returnDescription(type = "Arrivals"),
                                         highchartOutput(outputId = "perc_delay_arrival")
                                )
                              )
                     )
                   )
                 )),
        # Third tab - Showing the data and allowing the user to download it
        tabPanel("Data",
                 
                 # Well panel for tidying the visualization
                 wellPanel(
                   
                   #Description of the tab
                   h3("Detailed data"),
                   hr(),
                   
                   # A hint about the Data Table
                   helpText("It's possible to change the data that is being listed using the filter options."),
                   
                   # Outputting the table
                   dataTableOutput(outputId = "datatable"),
                   
                   # A hint about the Data Table
                   helpText("Click on the button to download the data"),
                   
                   # Option to download the correspondent data
                   downloadButton("downloadData", "Download")
                 )
        )
      ),
      
      helpText("The raw dataset contains over 2M flight observations from 31/Dec/2014 to 31/Jul/2017")
    )
  )
)

# Server - Shinny
server <- function(input, output, session) {
  
  df <- eventReactive(input$type,{
    totalByFlight %>%
      filter(Flight.Type == input$type)
  })
  
  topDelaying <- eventReactive(c(input$type,input$numberOfTop),{
    req(input$type)
    req(input$numberOfTop)
    df() %>%
      group_by(Flight.Type,Airline) %>%
      summarise(AverageDelay = round(mean(AverageDepartureDelay + AverageArrivalDelay),2),
                TotalFlight = sum(TotalFlight)) %>%
      arrange(desc(AverageDelay)) %>%
      head(input$numberOfTop)
  })
  
  topFlight <- eventReactive(c(input$type,input$numberOfTop),{
    req(input$type)
    req(input$numberOfTop)
    df() %>%
      group_by(Flight.Type,Airline) %>%
      summarise(AverageDelay = round(mean(AverageDepartureDelay + AverageArrivalDelay),2),
                TotalFlight = sum(TotalFlight)) %>%
      arrange(desc(TotalFlight)) %>%
      head(input$numberOfTop)
  })
  
  
  topBarPlot <- function(df,
                         categories,
                         series,
                         title,
                         yAxis){
    
    chart <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(title = list(text = "Airlines"),
               categories = categories) %>%
      hc_add_series(series,
                    showInLegend = F) %>%
      hc_yAxis(title = list(text = yAxis)) %>%
      hc_title(text = title,
               align = "center") %>%
      hc_subtitle(text = "Click on the bar to more information",
                  align = "center") %>%
      hc_credits(enabled = TRUE,
                 text = "Source: Brazillian National Civil Aviation Agency",
                 style = list(fontSize = "10px")) %>%
      hc_add_theme(hc_theme_google())
    
    return(chart)
  }
  
  Plot <- function(df,
                         categories,
                         series,
                         title,
                         yAxis){
    
    chart <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_xAxis(title = list(text = "Airlines"),
               categories = categories) %>%
      hc_add_series(series,
                    showInLegend = F) %>%
      hc_yAxis(title = list(text = yAxis)) %>%
      hc_title(text = title,
               align = "center") %>%
      hc_subtitle(text = "Click on the bar to more information",
                  align = "center") %>%
      hc_credits(enabled = TRUE,
                 text = "Source: Brazillian National Civil Aviation Agency",
                 style = list(fontSize = "10px")) %>%
      hc_add_theme(hc_theme_google())
    
    return(chart)
  }
  
  output$top_flights <- renderHighchart({
    topBarPlot(df = topFlight(),
               categories = topFlight()$Airline,
               series = topFlight()$TotalFlight,
               title = "Top Airlines in Number of Flights",
               yAxis = "Number of Flights")
  })
  
  output$top_delaying <- renderHighchart({
    print(topDelaying())
    topBarPlot(df = topDelaying(),
               categories = topDelaying()$Airline,
               series = topDelaying()$AverageDelay,
               title = "Top Airlines in Delaying",
               yAxis = "Average Delay time in minutes")
    
  })
  
  
  # Rendering the datatable
  output$datatable <- renderDataTable({
    df()
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)