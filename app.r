library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggthemes)
if (!require("DT")) install.packages('DT')
factorize = function(data){
  data_cat = select(data, is.character)
  col_names = names(data_cat)
  data[col_names] = lapply(data[col_names] , as.factor)
  return (data)
}
to_date = function(colname, data){
  data[,colname]<-as.Date(data[,colname], format="%d-%b-%y") 
  return (data)
  
}
get_mode = function(col){
  return(names(sort(table(col), decreasing = T, na.last = T)[1]))
}

fill_missing_vals = function(data, stat_measure){
  
  for (i in 1:ncol(data)){
    if(is.numeric(data[,i])){
      if (stat_measure == 'mean'){
        data[,i][is.na(data[,i])] = round(mean(data[,i], na.rm = TRUE),digits=2)
      }
      if (stat_measure == 'std'){
        data[,i][is.na(data[,i])] = round(sd(data[,i],  na.rm = TRUE),digits=2)
      }
      if (stat_measure == 'median'){
        data[,i][is.na(data[,i])] =round( median(data[,i],  na.rm = TRUE),digits=2)
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
        data[,i] = round(data[,i]^2, digits=2)
        
      }
    }
  }
  if (func == 'log'){
    for (i in 1:ncol(data)){
      if(is.numeric(data[,i])){
        data[,i] = round(log(data[,i]), digits=2)
        
      }
    }
  }
  if (func == 'norm'){
    for (i in 1:ncol(data)){
      if(is.numeric(data[,i])){
        
        data[,i] = round(min_max_norm(data[,i]), digits=2)
        
      }
    }
  }
  return (data)
}
###################################################################


  ui <- fluidPage(theme = shinytheme("flatly"),
                  tags$head(tags$style(HTML('.navbar-static-top {background-color: #ADD8E6;}',
                                            ".navbar-default .navbar-nav>li>a:hover, .navbar-default .navbar-nav>li>a:focus {color: #6495ED;}",
                                            '.navbar-default .navbar-nav>.active>a,.navbar-default .navbar-nav>.active>a:hover {color: #6495ED;}',
                                            '.navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover, .navbar-default .navbar-nav>.active>a:focus {background-color: #ADD8E6;}'))),
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
                                    choices = c(Structure = 'str',
                                                Summary = 'summary'),
                                    selected = "str"
                                  ),
                                  # selectInput('dateCol','Select Date column: ', ""),
                                  selectInput("date", "Select Date column:", ""),

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
      tabPanel("Transform", 
               sidebarPanel(
                 width = 4,
                 
                 selectInput("MissingVals", "Replace Missing Values:",
                             c("Mean" = "Mean",
                               "Std" = "Std",
                               "Median" = "Median"), selected = "Mean"),
                 radioButtons(
                   "transform",
                   "Transformation",
                   
                   choices = c(
                     "No Transformations" = "nothing",
                     Normalize = 'norm',
                     Logarithm = "log",
                     Square = "sqr"),
                   selected = "nothing"
                 ),                 # conditionalPanel(
                 #   condition = "transform",
                 #   checkboxGroupInput("transform", "Transformation",
                 #                      choices = c("norm", "log", "sqr"), selected = FALSE
                 #   ),
                 # ),
                 
                 # checkboxInput("rm_outl", "Remove Outliers",TRUE),
                 conditionalPanel(
                   condition = "rm_outl",
                   checkboxGroupInput("outliers", "Outliers",
                                      choices = c("Remove Outliers"), selected = FALSE
                                      ),
               ),
               selectInput("OutlierCol", "Choose column:", ""),
               selectInput("Col", "Choose cokumn:", choices  = c("OPEN", "CLOSE")),
                 
                 
               
                
                 ),
               mainPanel(
  
                 htmlOutput("header_preprocess"),
                 DT::dataTableOutput('data_transformed'),
                 # htmlOutput('test'),
                 
                 #,
                 # textOutput('text'),
                 # plotOutput("plot")
               )# mainPanel,
               ),
      
      tabPanel("Explore",
               sidebarPanel(
                 selectInput("groupby", "Column to group by:", "", multiple = TRUE),
                 selectInput("aggregate", "Column to aggregate:", "",multiple = TRUE),
                 selectInput("func", "Aggregate by:", choices = c(mean = "mean",
                                                                  std = "sd",
                                                                  median = "median",
                                                                  max = "max",
                                                                  min = "min"),multiple = TRUE),
                 selectInput("filter", "Select column to filter:", "", multiple = TRUE),
               ),
               
               mainPanel(
                 htmlOutput("header_agg"),
                 DT::dataTableOutput('agg_table'),
                 htmlOutput("header_filter"),
                 DT::dataTableOutput('filtered_table'),
               )
               ),
      tabPanel("Outliers",
               sidebarPanel(
                 selectInput("outl_col1", "Select X axis:", "", multiple = FALSE),
                 selectInput("outl_col2", "Select Y axis:", "", multiple = FALSE),
                 selectInput("col_fill", "Select fill column:", "", multiple = FALSE)

               ),
               
               mainPanel(
                 htmlOutput('test'),
                 htmlOutput("header_outliers"),
                 plotOutput("boxplotOutliers")

               )
               ),
      tabPanel("Histogram"),
      tabPanel("Scatter Plot"),
      tabPanel("Line Plot"),
      tabPanel("Bar Plot"),
      tabPanel("Heatmap"),
      
      
    ) # navbarPage
  ) # fluidPage

  
  # Define server function  
  server <- function(input, output, session) {


    inFile <- reactive({
      if (is.null(input$file1)) {
        return(NULL)
      } else {
        input$file1
      }
    })
    
    myData <- reactive({
      
      if (is.null(inFile())) {
        return(NULL)
      } else {
        data = read.csv(inFile()$datapath)
        data = factorize(data)
      }
      if (input$date != ""){
        data = fill_missing_vals(data, "median")
        col = input$date
        data = to_date(col, data)
      }
      # if(input$factorizeCol == "Yes"){
      # 
      #   data = factorize(data)
      # }
      # if(is.null(input$factorizeCol)){
      #   
      #   data  = read.csv(inFile()$datapath)
      # }
      # 

      return (data)
    })
    
    
    take_data <- reactive({
      
      if (is.null(inFile())) {
        return(NULL)
      } else {
        read.csv(inFile()$datapath)
        
      }

    })
    
    myData_cat <- reactive({
      
      if (is.null(inFile())) {
        return(NULL)
      } else {
        df = read.csv(inFile()$datapath)
        data_cat = select(df, is.character)
        return(data_cat)
      }
      
    })
    
    myData_numerics <- reactive({
      if (is.null(inFile())) {
        return(NULL)
      } else {
        df = read.csv(inFile()$datapath)
        data_num = select_if(df, is.numeric)
        return(data_num)
      }
    })
    
    observe({
      updateSelectInput(
        session,
        "date",
        choices=c("",names(take_data())))
      
      updateSelectInput(
        session,
        "groupby",
        choices=names(myData_cat()))
      updateSelectInput(
        session,
        "aggregate",
        choices=names(myData_numerics()))
      updateSelectInput(
        session,
        "OutlierCol",
        choices=names(take_data()))
      updateSelectInput(
        session,
        "filter",
        choices=names(take_data()))
      updateSelectInput(
        session,
        "outl_col1",
        choices=names(take_data()))
      updateSelectInput(
        session,
        "outl_col2",
        choices=names(take_data()))
      updateSelectInput(
        session,
        "col_fill",
        choices=names(myData_cat()))
      
      
      
      
    })
    

    output$agg_table <- DT::renderDataTable({
      grouped_df = myData() %>%
        group_by(!!!rlang::syms(input$groupby)) %>%
        summarise_at(input$aggregate, (input$func), na.rm=TRUE)
      return(grouped_df)
      
      
    }, options = list(scrollX = T))
    
    
    output$filtered_table <- DT::renderDataTable({
      filtered_df = myData() %>%
        select(!!!rlang::syms(input$filter))
      return(filtered_df)
      
      
    }, options = list(scrollX = T))
    
    
    output$contents <- DT::renderDataTable({

      if (input$prev == "head") {
        return(head(myData()))
      }
     return(myData())
    }, options = list(scrollX = T))
    
    output$summary <- renderPrint({
      if (input$display == 'str') {
        glimpse(myData())
        
      }
      else{
        summary(myData())
      }
      
      
    })
    
    # output$select <- renderUI({
    #   df <- myData()
    #   selectInput("variable", "Variable:", names(df))
    #   
    #   
    # })
    output$DataPreview <- renderUI({
      
      HTML("<h2>Data Preview</h2>")
    })
    output$SummaryOfData <- renderUI({

        HTML("<h2>Summary</h2>")

    })
    # output$test <- renderUI({
    # 
    #     HTML(paste(input$DateCol),"aa")
    # 
    #   
    # })
    
    
    
    # output$plot <- renderPlot({
    #   df <- myData()
    #   df <- df[, input$variable]
    #   hist(df)
    # })
    output$header_agg <- renderUI({
      
      HTML("<h2>Data aggregation</h2>")
      
    })
    
    output$header_filter <- renderUI({
      
      HTML("<h2>Data Filter</h2>")
      
    })
    
    output$header_outliers <- renderUI({
      
      HTML("<h2>Outlier detection </h2>")
      
    })
    
    
    output$boxplotOutliers <- renderPlot({ 

      data=myData()
      data = fill_missing_vals(data, "median")
      col_x = input$outl_col1
      col_y = input$outl_col2
      col_fill = input$col_fill
      ggplot(data=data, aes_string(y=col_y, x = col_x, fill=col_fill))+ geom_boxplot()+
      theme_bw()+
      labs(title=paste(col_x, " VS ", col_y))+
      scale_fill_brewer(palette = "Pastel2")+
      theme(axis.title.y = element_text(size=20),
            axis.title.x = element_text(size=20),
            title = element_text(size=20))
    })
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
      if (input$transform == 'nothing'){
        
        data = fill_missing_vals(data, "mean")
        
      }
      
      
      
      
      
      # if (is.null(input$outliers)){
      #   data = data_old
      # }
      
      return(data)
      
    }, options = list(scrollX = T))

  #   output$test <- renderUI({
  #     
  # 
  # # HTML())
  # HTML(paste(input$outl_col1, "bb"))
  #   })
    
    
  }

  # Create Shiny object
  shinyApp(ui = ui, server = server)
