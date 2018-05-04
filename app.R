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
  
  df <- df %>%
    mutate(ArrivalPercDelay = ArrivalPercDelay*100,
           DeparturePercDelay = DeparturePercDelay*100)
  
  names(df) <- gsub("\\.","",names(df))
    
  
  return(df)
  
}

totalByFlight <- loadFile("AverageDelayByFlight.csv",
                          colClasses = c("factor","factor","factor","numeric",
                                        "numeric","integer","numeric","numeric",
                                        "numeric","numeric"))

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
  helpText("The purpose on this dashboard is to have an overview of Airlines that operates in Brazil."),
  sidebarLayout(
    
    # The panel has the select input
    sidebarPanel(
      width = 3,
      
      # Well panel keep things tidy - Flight selection is the first
      wellPanel(# Select Flight Type input
        
        # Select Flight Type input
        checkboxGroupInput(inputId = "type",
                           label = "Flight Type",
                           choices = levels(totalByFlight$FlightType),
                           selected = levels(totalByFlight$FlightType)
        ),
        
        hr(),
        
        # Help for the select input
        helpText("Select one or more Airlines below (using ctrl (command on Mac) or shift + click). Selecting too many airlines may affect the performance of the plot"),
        
        # Select Airline input
        selectInput(inputId = "airline",
                    label = "Airline",
                    choices = levels(totalByFlight$Airline),
                    selectize = F,
                    multiple = T,
                    size = 15
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
        tabPanel("Percentage of Delay",
          wellPanel(
            tabPanel("Percentage of Delay",
                     highchartOutput(outputId = "perc_delay_by_flight")
            )
          )
        ),
        tabPanel("Top Airlines",
                 
                 # Well panel keep things tidy - Flight selection is the first
                 wellPanel(
                   div(style="display: inline-block;vertical-align:top; width: 150px;",
                       selectInput(inputId = "top_type",
                                   label = "Flight Type",
                                   choices = levels(totalByFlight$FlightType),
                                   multiple = F,
                                   selected = "International"
                       )),
                   div(style="display: inline-block;vertical-align:top; width: 150px;",
                       numericInput(inputId = "numberOfTop",
                                    label = "Number of Airlines",
                                    value = 10,
                                    min = 5,
                                    max = 20,
                                    step = 1))),
                 # Well panel to organise the output
                 wellPanel(
                   tabsetPanel(
                     tabPanel("Delaying",
                              highchartOutput(outputId = "top_delaying")
                     ),
                     tabPanel("No of Flights",
                              highchartOutput(outputId = "top_flights")
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
  
  df <- eventReactive(input$top_type,{
    totalByFlight %>%
      filter(FlightType == input$top_type)
  })
  
  topDelaying <- eventReactive(c(input$top_type,input$numberOfTop),{
    req(input$type)
    req(input$numberOfTop)
    df() %>%
      group_by(FlightType,Airline) %>%
      summarise(AverageDelay = round(mean(AverageDepartureDelay + AverageArrivalDelay),2),
                TotalFlight = sum(TotalFlight)) %>%
      arrange(desc(AverageDelay)) %>%
      head(input$numberOfTop)
  })
  
  topFlight <- eventReactive(c(input$top_type,input$numberOfTop),{
    req(input$type)
    req(input$numberOfTop)
    df() %>%
      group_by(FlightType,Airline) %>%
      summarise(AverageDelay = round(mean(AverageDepartureDelay + AverageArrivalDelay),2),
                TotalFlight = sum(TotalFlight)) %>%
      arrange(desc(TotalFlight)) %>%
      head(input$numberOfTop)
  })
  
  perc <-eventReactive(c(input$type,input$airline),{
    req(input$type)
    req(input$airline)
    totalByFlight %>%
      filter(FlightType %in% input$type & Airline %in% input$airline)
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
  
  delayByCompanyPlot <- function(df,
                                 title){
    
    chart <- hchart(df,type="scatter",hcaes(x=DeparturePercDelay,
                                              y=ArrivalPercDelay,
                                              group=FlightType,
                                              size=TotalFlight)) %>%
      hc_xAxis(title = list(text = "Percentage of Delayed Arrival  (Over 15 minutes)"),
               max = 100,
               min = 0) %>%
      hc_yAxis(title = list(text = "Percentage of Delayed Departure (Over 15 minutes)"),
               max = 100,
               min = 0) %>%
      hc_tooltip(pointFormat = "<b>Airline:</b> {point.Airline} <br>
                                <b>Number of Flights:</b> {point.TotalFlight} <br>
                                <b>Flight No.:</b> {point.FlightNo} <br>
                                <b>Percentage of Delayed Departure:</b> {point.DeparturePercDelay}% <br>
                                <b>Percentage of Delayed Arrival:</b> {point.ArrivalPercDelay}%") %>%
      hc_title(text = title,
               align = "center") %>%
      hc_subtitle(text = "Percentage of Delaying Departure and Arrival (Over 15 minutes)",
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
    topBarPlot(df = topDelaying(),
               categories = topDelaying()$Airline,
               series = topDelaying()$AverageDelay,
               title = "Top Airlines in Delaying",
               yAxis = "Average Delay time in minutes")
    
  })
  
  output$perc_delay_by_flight <- renderHighchart({
    req(input$airline)
    delayByCompanyPlot(df = perc(),
                       title = "Overview of Flights Delay by Company and Flight")
  })
  
  
  # Rendering the datatable
  output$datatable <- renderDataTable({
    df()
  })
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)