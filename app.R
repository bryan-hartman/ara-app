library(shiny)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(stringr)
library(huxtable)
library(dtplyr)
library(shinycssloaders)

source('functions.R')

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

ui <- fluidPage(
  
  # App title 
  titlePanel("Automated Realization Analysis"),
  
  sidebarLayout(
    
    # Sidebar for csv upload and realization analysis level
    sidebarPanel(paste("Upload your csv file here.   
                 File should contain three fields: 
                 Alternative, Value Scores, Costs", sep ="<br/>"), 
                   fileInput(inputId = "filedata",
                             label = "Upload data. Choose csv file",
                             accept = c(".csv")),
                 radioButtons("radio", label = h4("Select Realization Analysis Level"),
                              choices = list("Level 0" = 0, "Level 1" = 1, 
                                             "Level 2" = 2), 
                              selected = 0),),
    
    # Main panel for Selected Level Analysis
    mainPanel(
      
      conditionalPanel(condition = "output.status && input.radio == 0",
      
                       # Level 0 Analysis Tabs
                       tabsetPanel(type = "tabs",
                                  tabPanel("Value Histogram", plotOutput("Valhist"),
                                           downloadButton("valuehistpng", "Download")),
                                  tabPanel("Value and Cost CDFs", plotOutput("Valcdf"),
                                           downloadButton("valuecdfpng", "Download"),
                                           plotOutput("Costcdf"),
                                           downloadButton("costcdfpng", "Download")),
                                  tabPanel("Cloud Plots", plotOutput("cloud"),
                                           downloadButton("cloudpng", "Download")))
        ),
      conditionalPanel(condition = "output.status && input.radio == 1",
                       
                       # Level 1 Analysis Tabs
                       tabsetPanel(type = "tabs",
                                   tabPanel("Pairwise Comparison", 
                                                            selectInput('alt1', 'Alternative 1', ""),
                                                            selectInput('alt2', 'Alternative 2', ""),
                                                            numericInput("samplesize", label = "Set Sample Size",
                                                                         1000, min = 1),
                                                            numericInput("setseed", label = "Set Seed Value",
                                                                         123, min=1),
                                   tableOutput("pareto_table")),
                                            #downloadButton("valuehistpng", "Download"))
                                   tabPanel("Average Dominance Score", 
                                            withSpinner(tableOutput("ads_table"),type=5))
                                
        )
      ),
      conditionalPanel(condition = "output.status && input.radio == 2",
                       
                       # Level 2 Analysis Tabs
                       tabsetPanel(type = "tabs",
                                   tabPanel("Trade zones",
                                            selectInput('alt1_2', 'Specify Trade Zone Alternative', ""),
                                            selectInput('alt2_2', 'In Relation to the Expected Value of which Alternative', ""),
                                            plotOutput("level2_trade"),
                                            downloadButton("level2png", "Download"))
                       )
      )
      )
    )
  )



server <- function(input, output, session) {

  # Increase file upload size to 30MB
  options(shiny.maxRequestSize=30*1024^2)
  data <- reactive({
    req(input$filedata)
    read.csv(input$filedata$datapath)
    })
  
  # Select Level of Analysis (0, 1, or 2)
  output$level <- renderPrint({ input$radio })
  
  # Level 0 Tab Plots
  output$Valhist <- renderPlot({valhist(data())})
  output$Valcdf <- renderPlot({valcdf(data())})
  output$Costcdf <- renderPlot({costcdf(data())})
  output$cloud <- renderPlot({cloudplot(data())})
  
  # Level 1 Tab Plots
  observe({
    updateSelectInput(session, "alt1",choices = unique(data()$Alternative)
    )})
  observe({
  updateSelectInput(session, "alt2",choices = unique(data()$Alternative),
                    selected = unique(data()$Alternative)[[2]])
  })
  output$pareto_table <- renderTable({gen_pareto_table(data(), input$alt1, input$alt2, input$samplesize, input$setseed)},
      colnames = FALSE)
  output$ads_table <- renderTable({ads_table(data(), input$samplesize, input$setseed)})
  
  # Level 2 Tab Plots
  observe({
    updateSelectInput(session, "alt1_2",choices = unique(data()$Alternative)
    )})
  observe({
    updateSelectInput(session, "alt2_2",choices = unique(data()$Alternative),
                      selected = unique(data()$Alternative)[[2]])
  })
  output$level2_trade <- renderPlot({gen_level2_plot(data(), input$alt1_2, input$alt2_2)})

  output$image <- renderImage({
    filename <- normalizePath(file.path(paste0('www/level_2.png')))
    list(
      src = filename, 
      height = 400
    )
  }, deleteFile = FALSE)
  
  # Download PNG Buttons
  output$valuehistpng <- downloadHandler(
    filename = "valuehist.png", 
    content = function(file) {
      valplot <- valhist(data())
      ggsave(file, valplot, width = 10, height = 8)
    }
  )
  
  output$valuecdfpng <- downloadHandler(
    filename = "valuecdf.png", 
    content = function(file) {
      valcdfplot <- valcdf(data())
      ggsave(file, valcdfplot, width = 10, height = 8)
    }
  )
  
  output$costcdfpng <- downloadHandler(
    filename = "costcdf.png", 
    content = function(file) {
      costcdfplot <- costcdf(data())
      ggsave(file, costcdfplot, width = 10, height = 8)
    }
  )
  
  output$cloudpng <- downloadHandler(
    filename = "cloud.png", 
    content = function(file) {
      cloud <- cloudplot(data())
      ggsave(file, cloud, width = 10, height = 8)
    }
  )
  
  output$level2png <- downloadHandler(
    filename = "level2.png", 
    content = function(file) {
      lvl2 <- gen_level2_plot(data(), input$alt1_2, input$alt2_2)
      ggsave(file, lvl2, width = 10, height = 8)
    }
  )
  
  # Condition that hides panels until data is loaded
  output$status <- reactive(if (is.null(data())) {
    return(FALSE)
  } else {return(TRUE)})
  outputOptions(output, "status", suspendWhenHidden = FALSE)
  
  
}

# shinyApp()
shinyApp(ui = ui, server = server)