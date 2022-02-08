#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(bslib)

# Define UI for application that draws a histogram
ui <- fluidPage(
  #theme setup
  theme = bs_theme(bootswatch="sandstone"),
  #create 3 tabs
    navbarPage("Remote Sensing Data Analyzer",
               tabPanel("Instructions",
                        fluidPage(
                          titlePanel("Instructions for use"),
                          helpText("Step 1. Download your data and put it in a single folder"),
                          hr(),
                          helpText("Step 2. Enter the file path to the folder with your data in the field on the next page"),
                          hr(),
                          helpText("Step 3. Enter the desired date range in the field on the next page")
                        )
                        ),
               
               tabPanel("Inputs",
                        fluidPage(
                          textInput("filepath",label=h3("File Directory Path")),
                          ###testing purposes only
                          hr(),
                          fluidRow(column(2,verbatimTextOutput("filepath"))),
                          ###
                          hr(),
                          dateRangeInput("daterange",label=h3("Input desired date range:")),
                          ###testing purposes only
                          hr(),
                          fluidRow(column(4,verbatimTextOutput("daterange"))),
                          ###
                          selectInput("calcType", label = h3("Select box"), 
                                      choices = list("Annual Average NDVI" = 1, "NDVI Difference Summer-Winter" = 2, "NDVI Difference Present - 1 Year " = 3), 
                                      selected = 1),
                          
                        )
                        ),
               
               
               tabPanel("View Output",
                        fluidPage(
                          #display a list of summary statistics
                          
                          #display a graph of spatially averaged NDVI [or other calculated output] over the date range
                          
                          #display an image showing spatially explicit NDIV [or other calculated output] averaged over all years
                          
                          #display an image showing spatially explicit NDIV [or other calculated output] for only a single year
                          #display a widget allowing the user to select the year for the above image
                          
                        )
                        
                        )
               )
)
  

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #inputs from settings page
  output$filepath = renderPrint({input$filepath})
  output$daterange=renderPrint({input$daterange})
  output$calcType=renderPrint({input$calcType})
  
  #calculated stuff
  
  #perform the actual calculation based on calcType
  ###Complex, fill in later after we go over handling spatial data
  #output is treated as calculatedvalue for now
  
  #calculate summary statistics:
  #average value across entire dataset
  #max value across dataset
  #min value across dataset
  summar_table=reactive({
    mean(calculatedvalue)
  })
  
  
  #graph calculatedvalue across years in daterange
  
  #
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
