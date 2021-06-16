library(shiny)
library(tidyverse)
library(ggthemes)
library(gridExtra)
library(stringr)
library(huxtable)
library(dtplyr)
library(shinycssloaders)
library(directlabels)
library(sparkline)
library(DT)
library(plotly)


source('R/functions.R')

# Options for Spinner
options(spinner.color="#0275D8", spinner.color.background="#ffffff", spinner.size=2)

ui <- fluidPage(
  
  # App title 
  titlePanel("Automated Realization Analysis"),
  
  sidebarLayout(
    
    # Sidebar for csv upload and realization analysis level
    sidebarPanel(fileInput(inputId = "valuedata",
                             label = "Upload value data here (csv file):",
                             accept = c(".csv")),
                 
                 fileInput(inputId = "costdata",
                           label = "Upload cost data here (csv file):",
                           accept = c(".csv")),
                 htmlOutput("download_option"),
                 downloadButton("download_val_sample", "Sample Value Data"),
                 downloadButton("download_cost_sample", "Sample Cost Data"),
                 numericInput("samplesize1", label = "Set Sample Size for Number of Alternative (Cost, Value) Pairs",
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
                                           # Input: Slider opacity level ----
                                           sliderInput("opacity", "Set the opacity level:",
                                                       min = 0, max = 1,
                                                       value = .7),
                                           downloadButton("valuehistpng", "Download")),
                                  tabPanel("Value and Cost CDFs", plotOutput("Valcdf"),
                                           downloadButton("valuecdfpng", "Download"),
                                           plotOutput("Costcdf"),
                                           downloadButton("costcdfpng", "Download")),
                                  tabPanel("Cloud Plots", plotOutput("cloud"),
                                           radioButtons("cloud_choice", label = h4("Choose Cloud Plot Type"),
                                                        choices = list("Default" = 0, "Embedded Legend" = 1), 
                                                        selected = 0),
                                           downloadButton("cloudpng", "Download")),
                                  tabPanel("Deterministic Dominance", 
                                           selectInput('dom_pair', 'Dominance Pairs', ""),
                                           plotOutput("domplot"),
                                           htmlOutput('dom_value')))
                                           #downloadButton("cloudpng", "Download")))
        ),
      conditionalPanel(condition = "output.status && input.radio == 1",
                       
                       # Level 1 Analysis Tabs
                       tabsetPanel(type = "tabs",
                                   tabPanel("Set Thompson's Method Parameters", 
                                            selectInput('sig_level', 
                                                        'Select the significance level (alpha) parameter:', 
                                                        choices = c(.5, .4, .3, .2, .1, .05, .025, .02, .01, .005, .001,.0005, .0001), 
                                                        selected = .05),
                                            numericInput('half_width', 
                                                         'Set the half-width interval (delta) parameter:', 
                                                         .005, min = .0001, max = 1),
                                            htmlOutput('thompson_value')),
                                   tabPanel("Pairwise Comparison", 
                                            selectInput('alt1', 'Alternative 1', ""),
                                            selectInput('alt2', 'Alternative 2', ""),
                                            dataTableOutput("pareto_table"),
                                            fluidRow(column(9,plotOutput("cloud_2")))),
                                   tabPanel("Average Dominance Score", 
                                            withSpinner(tableOutput("ads_table"),type=5),
                                            fluidRow(column(9,plotOutput("cloud_3"))))
                                   
                                
        )
      ),
      conditionalPanel(condition = "output.status && input.radio == 2",
                       
                       # Level 2 Analysis Tabs
                       tabsetPanel(type = "tabs",
                                   tabPanel("Trade zones",
                                            fluidRow(column(6,selectInput('alt1_2', 'Specify Trade Zone Alternative', ""),
                                            selectInput('alt2_2', 'In Relation to the Expected Value of which Alternative', ""),
                                            radioButtons("delta", label = h4("Set Delta:"),
                                                         choices = list("Set Delta by Percentage:" = 0, "Set Delta by Budget Limit:" = 1), 
                                                         selected = 0),
                                            conditionalPanel(condition="input.delta == 0",numericInput("delta_parameter", label = "Delta Parameter Percentage",
                                                         .05, min = 0, step=.01)),
                                            conditionalPanel(condition="input.delta == 1",numericInput("delta_value", label = "Budget Limit",
                                                         "", min = 0, step=1))),
                                            column(6,plotOutput("cloud_4"))),

                                            fluidRow( 
                                                     column(6,plotOutput("level2_trade")),
                                                     column(6,plotlyOutput("level2_dynamic"))
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
  
  # Below asks for importing value and cost data.
  # Additionally, sample data files from 'www/Data' folder
  # allow the user to download files to play with tool
  valuedata <- reactive({
    req(input$valuedata)
    read.csv(input$valuedata$datapath)
    })
  output$download_option <- renderText({"<b>Sample Data:</b>"})
  output$download_val_sample <- downloadHandler(
    filename = function() {
      "value.csv"
    },
    content = function(file) {
      data_needed <- read.csv("www/Data/value.csv")
      write.csv(data_needed,file,row.names = FALSE)
    }
  )
  
  costdata <- reactive({
    req(input$costdata)
    read.csv(input$costdata$datapath)
  })
  output$download_cost_sample <- downloadHandler(
    filename = function() {
      "cost.csv"
    },
    content = function(file) {
      data_needed <- read.csv("www/Data/cost.csv")
      write.csv(data_needed,file,row.names = FALSE)
    }
  )
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
  output$Valhist <- renderPlot({
    valhist(subset(data(), Alternative %in% input$alternative_select), input$opacity)})
  output$Valcdf <- renderPlot({
    valcdf(subset(data(), Alternative %in% input$alternative_select))})
  output$Costcdf <- renderPlot({
    costcdf(subset(data(), Alternative %in% input$alternative_select))})
  output$cloud <- renderPlot({
    cloudplot(subset(data(), Alternative %in% input$alternative_select),input$cloud_choice)})
  
  # The following three cloud plots are used in the level 1 and level 2 tabs
  output$cloud_2 <- renderPlot({
    cloudplot(subset(data(), Alternative %in% input$alternative_select),input$cloud_choice)})
  output$cloud_3 <- renderPlot({
    cloudplot(subset(data(), Alternative %in% input$alternative_select),input$cloud_choice)})
  output$cloud_4 <- renderPlot({
    cloudplot(subset(data(), Alternative %in% input$alternative_select),input$cloud_choice)})
  
  # Level 0 Dominance Tab
  # This section of code populates the dominance tab options based on 
  # whether any alternative completely dominates any other 
  observe({
    choices <- dominance_check(data())
    if (length(choices[,1]) > 0){
       choices <- paste0(choices[,1], " dominates ", choices[,2])
    } else {
      choices <- "There are no dominant solutions."
    }
    updateSelectInput(session, "dom_pair",choices = choices
    )})
  
  # The following generates the plot on the dominance tab, based off the choice
  # from the drop down menu above (input$dom_pair alternatives are extracted from
  # the text, based on the user's selection)
  output$domplot <- renderPlot({
    dominating_alt <- sub("\\ dominates .*", "", input$dom_pair)
    dominated_alt <- sub(".* dominates ", "",  input$dom_pair)
    dominance_plot(data(), dominating_alt, dominated_alt)
  })
  
  # Output to display Deterministic Domination Likelihood
  # Based on the user's selection, the two alternatives are passed into the
  # distance calculation using the function cheb_calc (see functions.R file and
  # paper for detailed methodology)
  output$dom_value <- renderText({
    if(input$dom_pair == "There are no dominant solutions."){
      return("")
      } else {
        dominating_alt <- sub("\\ dominates .*", "", input$dom_pair)
        dominated_alt <-  sub(".* dominates ", "",  input$dom_pair)
        paste("<font size=\"6 px\"><b>",
              paste0("Probability that ", dominating_alt," will continue to deterministically dominate ", 
                     dominated_alt, " is ", round(100*cheb_calc(data(),
                                                              dominating_alt,
                                                              dominated_alt,
                                                              input$samplesize1),2), " percent."),
                                        "</b></font>")}})
  
  # Level 1 Tab Plots
  # The following output provides the Sample Size used for level 1 analysis
  # based on Thompson's Method, described in the paper.
  output$thompson_value <- renderText({paste(
    "<font size=\"6 px\"><b>",
    "Sample Size Needed to Ensure Pairwise Comparison %s are Within Delta of Their Actual Values: ",
    thompson_method(input$sig_level, input$half_width),
    "</b></font>")})
  
  
  observe({
    choices <- subset(data(), Alternative %in% input$alternative_select)
    updateSelectInput(session, "alt1",choices = unique(choices$Alternative)
    )})
  observe({
    choices <- subset(data(), Alternative %in% input$alternative_select)
    updateSelectInput(session, "alt2",choices = unique(choices$Alternative),
                    selected = unique(data()$Alternative)[[2]])
  })
  output$pareto_table <- renderDataTable({gen_pareto_table(subset(data(), Alternative %in% input$alternative_select), input$alt1, input$alt2, 
                                                       input$sig_level, input$half_width)},
      colnames = FALSE)
  output$ads_table <- renderTable({ads_table(subset(data(), Alternative %in% input$alternative_select),
                                             input$sig_level, input$half_width)})
  
  # Level 2 Tab Plots
  # Allow the user to input the delta value used level2 analysis
  observe({
    updateNumericInput(session, "delta_value",
                         value = delta_value(data(), input$alt1_2, 
                                             input$alt2_2,.05),
                       min = mean(subset(data(), Alternative==input$alt1_2)$Cost))
    })
  
  # Specify the chosen alternative for level two analysis
  observe({
    updateSelectInput(session, "alt1_2",choices = data()$Alternative,
                      selected = unique(data()$Alternative)[1])
    updateSelectInput(session, "alt2_2",choices = data()$Alternative, 
                      selected = unique(data()$Alternative)[2])              
    })

  output$level2_trade <- renderPlot({
    if(input$delta == 0){
      gen_level2_plot(data(), input$alt1_2,
                                         input$alt2_2, 
                                         tolerance = input$delta_parameter)
    } else {
      gen_level2_plot(data(), input$alt1_2,
                      input$alt2_2, 
                      tolerance = delta_param(data(),input$alt1_2,input$delta_value))
      }
    })
  
  output$level2_dynamic <- renderPlotly({
    # delta == 0 means we set the delta value by entering percentage
    if(input$delta == 0){
    dynamic_level2(data(), input$alt1_2, 
                   input$alt2_2, 
                   delta_value(data(), input$alt1_2, input$alt2_2,
                               input$delta_parameter))
    } else {
    # else,
      dynamic_level2(data(), input$alt1_2, 
                     input$alt2_2, 
                     input$delta_value)
    }
    })
  output$image <- renderImage({
    filename <- normalizePath(file.path(paste0('www/level_2.png')))
    list(
      src = filename, 
      height = 600,
      resolution = 72*12
    )
  }, deleteFile = FALSE)
  
  # Download PNG Buttons for downloading charts.
  # The image_save function is located in the 'functions.R' file
  output$valuehistpng <- image_save("valuehist.png", valhist(subset(data(), 
                                                      Alternative %in% input$alternative_select), 
                                                      input$opacity))
  output$valuecdfpng <- image_save("valuecdf.png", valcdf(subset(data(), 
                                                    Alternative %in% input$alternative_select)))
  output$costcdfpng <- image_save("costcdf.png", costcdf(subset(data(), 
                                                 Alternative %in% input$alternative_select)))
  output$cloudpng <- image_save("cloud.png", cloudplot(subset(data(), 
                                                 Alternative %in% input$alternative_select),
                                                 input$cloud_choice))
  output$level2png <- image_save("level2.png", gen_level2_plot(data(), input$alt1_2, input$alt2_2))

  
  # Condition that hides panels until data is loaded
  output$status <- reactive(if (is.null(valuedata()) || is.null(costdata())) {
    return(FALSE)
  } else {return(TRUE)})
  outputOptions(output, "status", suspendWhenHidden = FALSE)
  
  
}

# shinyApp()
shinyApp(ui = ui, server = server)