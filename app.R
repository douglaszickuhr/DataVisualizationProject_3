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

# Shiny User interface
ui <- fluidPage(
  
  # Title of panel
  titlePanel(paste("Brazilian Flight Overview."), 
             windowTitle = "Data Visualization CA2 - Plot 1"),
  helpText("The purpose on this dashboard is to have an overview of flights in Brazil."),
  sidebarLayout(
    
    # The panel has the select input
    sidebarPanel(
      width = 3,
      
      # Well panel keep things tidy - Flight selection is the first
      # wellPanel(# Select Flight Type input
      #           checkboxGroupInput(inputId = "type",
      #                              label = "Flight Type",
      #                              choices = levels(totalByDay$Flight.Type),
      #                              selected = levels(totalByDay$Flight.Type)
      #           )
      # ),
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
        tabPanel("Map",
                 
                 # Well panel to organise the output
                 wellPanel(
                   
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
      
      # Listing the total of records found.
      uiOutput(outputId = "n"),
      
      helpText("The raw dataset contains over 2M flight observations from 31/Dec/2014 to 31/Jul/2017")
    )
  )
)

# Server - Shinny
server <- function(input, output, session) {
  
  
  
  # # Rendering the information about the filtered data
  # output$n <- renderUI({
  #   
  #   # Requesting the type
  #   req(input$type)
  #   
  #   # Creating a new dataframe to list the data
  #   types <- oneDay()
  #   
  #   # Aggregating the data to generate the total by type
  #   types <- types %>%
  #     filter(Flight.Type %in% input$type) %>%
  #     group_by(Flight.Type) %>%
  #     summarise(Total = sum(Total))
  #   
  #   # Generating a matrix with the same length of the dataframe
  #   n <- matrix("",length(types$Flight.Type))
  #   
  #   # Iterating the dataframe and populating the matrix to output
  #   for (i in 1:length(types$Flight.Type)){
  #     desc <- levels(types$Flight.Type)[as.numeric(types[i,1])]
  #     n[i] <- paste(types[i,2],desc, "flights were found at this date.<br>")
  #   }
  #   
  #   # Returning the matrix as HTML
  #   return(HTML(n))
  # })
  # 
  # # Rendering the option to download the file
  # output$downloadData <- downloadHandler(
  #   
  #   # Function to return the filename
  #   filename = function() {
  #     paste("flights",format.Date(input$date,"%Y-%m-%d") , ".csv", sep = "")
  #   },
  #   # Function to return the data
  #   content = function(file) {
  #     write.csv(oneDay()[,c("Route","Total")], file, row.names = FALSE)
  #   }
  # )
}

# Create a Shiny app object
shinyApp(ui = ui, server = server)