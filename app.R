library(shiny)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(stringr)
library(huxtable)
library(dtplyr)
library(shinycssloaders)

source('R/functions.R')

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

ui <- fluidPage(
  
  # App title 
  titlePanel("Automated Realization Analysis"),
  
  sidebarLayout(
    
    # Sidebar for csv upload and realization analysis level
    sidebarPanel(fileInput(inputId = "valuedata",
                             label = "Upload value data here. Choose csv file:",
                             accept = c(".csv")),
                 fileInput(inputId = "costdata",
                           label = "Upload cost data here. Choose csv file:",
                           accept = c(".csv")),
                 numericInput("samplesize1", label = "Set Sample Size",
                              1000, min = 1),
                 numericInput("setseed1", label = "Set Seed Value",
                              123, min=1),
                 radioButtons("radio", label = h4("Select Realization Analysis Level"),
                              choices = list("Level 0" = 0, "Level 1" = 1, 
                                             "Level 2" = 2), 
                              selected = 0),
                 selectizeInput('alternative_select', label = "Select Alternatives for Evaluation", 
                                choices = "",
                                options = list('plugins' = list('remove_button')), 
                                multiple = TRUE)),
    
    # Main panel for Selected Level Analysis
    mainPanel(
      absolutePanel(h3("Copyright 2021 DSE"), bottom = 10, right = 10, fixed = TRUE),
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
                                            numericInput("delta_parameter", label = "Delta Parameter (enter value greater than or equal to 0)",
                                                         .05, min = 0),
                                            fluidRow( 
                                                     column(6,plotOutput("level2_trade")),
                                                     column(6,plotOutput("image"))
                                                     ),
                                            downloadButton("level2png", "Download"))
                       )
      )
      )
    )
  )



server <- function(input, output, session) {

  # Increase file upload size to 30MB
  options(shiny.maxRequestSize=30*1024^2)
  valuedata <- reactive({
    req(input$valuedata)
    read.csv(input$valuedata$datapath)
    })
  
  costdata <- reactive({
    req(input$costdata)
    read.csv(input$costdata$datapath)
  })
  
  # Merge value and Cost Data through resampling
  data <- reactive(if (is.null(valuedata()) == T || is.null(costdata()) ==T){
    return (FALSE)} else {
      
      # Call create_resamples function to create n data samples
      sample_alternatives <- create_resamples(valuedata(), costdata(), 
                                              input$samplesize1, input$setseed1)
      return(sample_alternatives)
    })
  

  # Select alternatives of interest
  observe({
    updateSelectizeInput(session, "alternative_select",
                         choices = unique(data()$Alternative),
                         selected = unique(data()$Alternative)
    )})
  
  
  
  # Select Level of Analysis (0, 1, or 2)
  output$level <- renderPrint({ input$radio })
  
  # Level 0 Tab Plots
  output$Valhist <- renderPlot({valhist(subset(data(), Alternative %in% input$alternative_select))})
  output$Valcdf <- renderPlot({valcdf(subset(data(), Alternative %in% input$alternative_select))})
  output$Costcdf <- renderPlot({costcdf(subset(data(), Alternative %in% input$alternative_select))})
  output$cloud <- renderPlot({cloudplot(subset(data(), Alternative %in% input$alternative_select))})
  
  # Level 1 Tab Plots
  observe({
    choices <- subset(data(), Alternative %in% input$alternative_select)
    updateSelectInput(session, "alt1",choices = unique(choices$Alternative)
    )})
  observe({
    choices <- subset(data(), Alternative %in% input$alternative_select)
    updateSelectInput(session, "alt2",choices = unique(choices$Alternative),
                    selected = unique(data()$Alternative)[[2]])
  })
  output$pareto_table <- renderTable({gen_pareto_table(subset(data(), Alternative %in% input$alternative_select), input$alt1, input$alt2)},
      colnames = FALSE)
  output$ads_table <- renderTable({ads_table(subset(data(), Alternative %in% input$alternative_select))})
  
  # Level 2 Tab Plots
  observe({
    pareto <- pareto_front(data())
    updateSelectInput(session, "alt1_2",choices = pareto[1:(length(pareto)-1)]
    )})
  observe({
    pareto <- pareto_front(data())
    pareto <- pareto[which(pareto==input$alt1_2)+1]
    updateSelectInput(session, "alt2_2",choices = pareto)
  })
  
  output$level2_trade <- renderPlot({gen_level2_plot(data(), input$alt1_2, 
                                                     input$alt2_2, tolerance = input$delta_parameter)})

  output$image <- renderImage({
    filename <- normalizePath(file.path(paste0('www/level_2.png')))
    list(
      src = filename, 
      height = 600,
      resolution = 72*12
    )
  }, deleteFile = FALSE)
  
  # Download PNG Buttons
  output$valuehistpng <- downloadHandler(
    filename = "valuehist.png", 
    content = function(file) {
      valplot <- valhist(subset(data(), Alternative %in% input$alternative_select))
      ggsave(file, valplot, width = 10, height = 8)
    }
  )
  
  output$valuecdfpng <- downloadHandler(
    filename = "valuecdf.png", 
    content = function(file) {
      valcdfplot <- valcdf(subset(data(), Alternative %in% input$alternative_select))
      ggsave(file, valcdfplot, width = 10, height = 8)
    }
  )
  
  output$costcdfpng <- downloadHandler(
    filename = "costcdf.png", 
    content = function(file) {
      costcdfplot <- costcdf(subset(data(), Alternative %in% input$alternative_select))
      ggsave(file, costcdfplot, width = 10, height = 8)
    }
  )
  
  output$cloudpng <- downloadHandler(
    filename = "cloud.png", 
    content = function(file) {
      cloud <- cloudplot(subset(data(), Alternative %in% input$alternative_select))
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
  output$status <- reactive(if (is.null(valuedata()) || is.null(costdata())) {
    return(FALSE)
  } else {return(TRUE)})
  outputOptions(output, "status", suspendWhenHidden = FALSE)
  
  
}

# shinyApp()
shinyApp(ui = ui, server = server)