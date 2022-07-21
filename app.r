library(shiny)
library(shinythemes)
if (!require("DT")) install.packages('DT')

get_mode = function(col){
  return(names(sort(table(col), decreasing = T, na.last = T)[1]))
}

fill_missing_vals = function(data, stat_measure){
  
  for (i in 1:ncol(data)){
    if(is.numeric(data[,i])){
      if (stat_measure == 'mean'){
        data[,i][is.na(data[,i])] = mean(data[,i], na.rm = TRUE)
      }
      if (stat_measure == 'std'){
        data[,i][is.na(data[,i])] = sd(data[,i],  na.rm = TRUE)
      }
      if (stat_measure == 'median'){
        data[,i][is.na(data[,i])] = median(data[,i],  na.rm = TRUE)
      }
    }
    else{
      data[,i][is.na(data[,i])] = get_mode(data[,i])
    }
  }
  
  return(data)
}


outlier_detection = function(data){
  for (i in 1:ncol(data)){
    if(is.numeric(data[,i])){
      
      quartiles <- quantile(data[,i], probs=c(.25, .75), na.rm = FALSE)
      IQR <- IQR(data[,i])
      
      Lower <- quartiles[1] - 1.5*IQR
      Upper <- quartiles[2] + 1.5*IQR 
      
      data <- subset(data, data[,i] > Lower & data[,i] < Upper)
      
    }
  }
  return (data)
}


min_max_norm = function(x) {
  (x - min(x)) / (max(x) - min(x))
}
transform_data = function(data, func){
  if (func == 'sqr'){
    for (i in 1:ncol(data)){
      if(is.numeric(data[,i])){
        data[,i] = data[,i]^2
        
      }
    }
  }
  if (func == 'log'){
    for (i in 1:ncol(data)){
      if(is.numeric(data[,i])){
        data[,i] = log(data[,i])
        
      }
    }
  }
  if (func == 'norm'){
    for (i in 1:ncol(data)){
      if(is.numeric(data[,i])){
        
        data[,i] = min_max_norm(data[,i])
        
      }
    }
  }
  return (data)
}
###################################################################


  ui <- fluidPage(theme = shinytheme("sandstone"),
      navbarPage(
      "",
      tabPanel("Data",
               
               sidebarPanel(      width = 3,
                                  fileInput(
                                    'file1',
                                    'Choose CSV File',
                                    accept = c('text/csv',
                                               'text/comma-separated-values,text/plain',
                                               '.csv')
                                  ),
                                  
                                  # # Input: Select separator ----
                                  #   radioButtons("sep", "Separator",inline=T,
                                  #                choices = c(Comma = ",",
                                  #                            Semicolon = ";",
                                  #                            Tab = "\t"),
                                  #                selected = ","),
                                  radioButtons(
                                    "display",
                                    "Display",
                                    inline = T,
                                    choices = c(Str = 'str',
                                                Summary = 'summary'),
                                    selected = "str"
                                  ),
                                  
                                  # Input: Select number of rows to display ----
                                  radioButtons(
                                    "prev",
                                    "Preview",
                                    inline = T,
                                    choices = c(Head = "head",
                                                All = "all"),
                                    selected = "all"
                                  ),
                                  
                                  uiOutput('select')
                                  ), # sidebarPanel
               mainPanel(
                 # tags$div(class="header", checked=NA,
                 #          tags$p("Ready to take the Shiny tutorial? If so"),
                 #          tags$a(href="shiny.rstudio.com/tutorial", "Click Here!")
                 # ),
                 
                 htmlOutput("DataPreview"),
                 DT::dataTableOutput('contents'),
                 htmlOutput("SummaryOfData"),
                 verbatimTextOutput('summary'),
                #,
                 # textOutput('text'),
                 # plotOutput("plot")
               )# mainPanel,
               
              
      ),#, # Navbar 1, tabPanel,
      tabPanel("Preprocessing", 
               sidebarPanel(
                 width = 4,
                 
                 selectInput("MissingVals", "Replace Missing Values:",
                             c("Mean" = "Mean",
                               "Std" = "Std",
                               "Median" = "Median"), selected = "Mean"),
                 radioButtons(
                   "transform",
                   "Transformation",
                   inline = T,
                   choices = c(
                               Normalize = 'norm',
                               Logarithmic = "log",
                               Square = "sqr"),
                   selected = "norm"
                 ),
                 # checkboxInput("rm_outl", "Remove Outliers",TRUE),
                 conditionalPanel(
                   condition = "rm_outl",
                   checkboxGroupInput("outliers", "Outliers",
                                      choices = c("Remove Outliers"), selected = "Remove Outliers"
                                      ),
               )
                 
                 
               
                
                 ),
               mainPanel(
                 
                 htmlOutput("header_preprocess"),
                 DT::dataTableOutput('data_transformed'),
                 DT::dataTableOutput('outlier_removed_table'),
                 htmlOutput('test'),
                 #,
                 # textOutput('text'),
                 # plotOutput("plot")
               )# mainPanel,
               ),
      tabPanel("Data Transformation"),
      tabPanel("Plots")
      
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output, session) {
    myData <- reactive({
      inFile <- input$file1
      if (is.null(inFile)) {
        return(mpg)
      }
      data <- read.csv(inFile$datapath, header = TRUE)
      data
    })
    
    output$contents <- DT::renderDataTable({
      if (input$prev == "head") {
        return(head(myData()))
      }
      DT::datatable(myData())
    }, options = list(scrollX = T))
    
    output$summary <- renderPrint({
      if (input$display == 'str') {
        str(myData())
        
      }
      else{
        summary(myData())
      }
      
    })
    
    output$select <- renderUI({
      df <- myData()
      selectInput("variable", "Variable:", names(df))
      
      
    })
    output$DataPreview <- renderUI({
      HTML("<h2>Data Preview</h2>")
    })
    output$SummaryOfData <- renderUI({
      if (input$display == 'summary') {
        HTML("<h2>Summary</h2>")
      }
      else{
        HTML("<h2>STR</h2>")
      }
    })
    
    
    
    
    # output$plot <- renderPlot({
    #   df <- myData()
    #   df <- df[, input$variable]
    #   hist(df)
    # })
    
    output$header_preprocess <- renderUI({

      HTML("<h2>Data Preporcessing</h2>")

    })
    
    output$data_transformed <- DT::renderDataTable({
      data = myData()
      data = fill_missing_vals(data, "mean")
      data_old = data.frame()
      if(input$MissingVals == 'Mean'){
        
        data = myData()
        data = fill_missing_vals(data, "mean")
        
      }
      else if(input$MissingVals == 'Std'){
        data = myData()
        data = fill_missing_vals(data, "std")
        
      }
      else if (input$MissingVals == 'Median'){
        data = myData()
        data = fill_missing_vals(data, "median")
        
      }
      if(is.null(input$outliers) == FALSE){
        data = outlier_detection(data)
      }

       if (input$transform == 'norm'){

        data = transform_data(data, 'norm')
        
       }
      if (input$transform == 'log'){
        
        data = transform_data(data, 'log')
        
      }
      if (input$transform == 'sqr'){
        
        data = transform_data(data, 'sqr')
        
      }


      
        

        
      # if (is.null(input$outliers)){
      #   data = data_old
      # }

      return(data)
      
    }, options = list(scrollX = T))

    # output$test <- renderUI({
    #    
    # })
    
    
  }

  # Create Shiny object
  shinyApp(ui = ui, server = server)
