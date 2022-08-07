library(shiny)
library(shinythemes)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(reshape2)
library(tidyr)
if (!require("DT"))
  install.packages('DT')

read_saved_data = function(path) {
  if (file.exists(path)) {
    data = read.csv("saved.csv", stringsAsFactors = F)
    data = factorize(data)
    return(data)
  }
  else{
    return (NULL)
  }
  
}

factorize = function(data) {
  for (i in 1:length(data)) {
    num_uniques = 0
    num_uniques = length(unique(data[[i]]))
    if (num_uniques <= 70) {
      if (is.numeric(data[, i])) {
        data[, i] = as.character(data[, i])
        
        
      }
      data[, i] = as.factor(data[, i])
      
    }
  }
  
  return (data)
}


to_date = function(colname, data) {
  possible_formats = c("%Y-%d-%m", "%d-%b-%y", "%m-%d-%Y", "%b-%d-%Y", "%d-%m-%Y")
  for (i in 1:length(possible_formats)) {
    old_col = data[, colname]
    data[, colname] = as.Date(data[, colname], format = possible_formats[i])
    
    if (sum(is.na(data[, colname])) == nrow(data)) {
      data[, colname] = old_col
    }
  }
  data[, colname] <- as.Date(data[, colname], format = format_given)
  return (data)
  
}


get_mode = function(col) {
  return(names(sort(
    table(col), decreasing = T, na.last = T
  )[1]))
}


fill_missing_vals = function(data, stat_measure) {
  for (i in 1:ncol(data)) {
    if (is.numeric(data[, i])) {
      if (stat_measure == 'dropna') {
        data = data %>% drop_na()
      }
      if (stat_measure == 'mean') {
        data[, i][is.na(data[, i])] = round(mean(data[, i], na.rm = TRUE),
                                            digits = 2)
      }
      if (stat_measure == 'std') {
        data[, i][is.na(data[, i])] = round(sd(data[, i],  na.rm = TRUE),
                                            digits = 2)
      }
      if (stat_measure == 'median') {
        data[, i][is.na(data[, i])] = round(median(data[, i],  na.rm = TRUE),
                                            digits = 2)
      }
    }
    else{
      data[, i][is.na(data[, i])] = get_mode(data[, i])
    }
  }
  
  return(data)
}


outlier_detection = function(data) {
  for (i in 1:ncol(data)) {
    if (is.numeric(data[, i])) {
      quartiles <- quantile(data[, i], probs = c(.25, .75), na.rm = TRUE)
      IQR <- IQR(data[, i])
      Lower <- quartiles[1] - 1.5 * IQR
      Upper <- quartiles[2] + 1.5 * IQR
      data <- subset(data, data[, i] >= Lower & data[, i] <= Upper)
      
    }
  }
  return (data)
}


min_max_norm = function(x) {
  (x - min(x)) / (max(x) - min(x))
}
transform_data = function(data, func) {
  if (func == 'sqr') {
    for (i in 1:ncol(data)) {
      if (is.numeric(data[, i])) {
        data[, i] = round(data[, i] ^ 2, digits = 2)
        
      }
    }
  }
  if (func == 'log') {
    for (i in 1:ncol(data)) {
      if (is.numeric(data[, i])) {
        data[, i] = round(log(data[, i]), digits = 2)
        
      }
    }
  }
  if (func == 'norm') {
    for (i in 1:ncol(data)) {
      if (is.numeric(data[, i])) {
        data[, i] = round(min_max_norm(data[, i]), digits = 2)
        
      }
    }
  }
  return (data)
}



###################################################################


ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(tags$style(
    HTML(
      '.navbar-static-top {background-color: #ADD8E6;}',
      ".navbar-default .navbar-nav>li>a:hover, .navbar-default .navbar-nav>li>a:focus {color: #6495ED;}",
      '.navbar-default .navbar-nav>.active>a,.navbar-default .navbar-nav>.active>a:hover {color: #6495ED;}',
      '.navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:hover, .navbar-default .navbar-nav>.active>a:focus {background-color: #ADD8E6;}'
    )
  )),
  navbarPage(
    "",
    tabPanel(
      "Data",
      
      sidebarPanel(
        width = 3,
        fileInput(
          'file1',
          'Choose CSV File',
          accept = c('text/csv',
                     'text/comma-separated-values,text/plain',
                     '.csv')
        ),
        
        radioButtons(
          "display",
          "Display",
          inline = T,
          choices = c(Structure = 'str',
                      Summary = 'summary'),
          selected = "str"
        ),
        selectInput("date", "Select date column:", ""),
        
        radioButtons(
          "prev",
          "Preview",
          inline = T,
          choices = c(Head = "head",
                      All = "all"),
          selected = "all"
        ),
        
        uiOutput('select')
      ),
      # sidebarPanel
      mainPanel(
        htmlOutput("DataPreview"),
        DT::dataTableOutput('contents'),
        htmlOutput("SummaryOfData"),
        verbatimTextOutput('summary'),
      )# mainPanel,
      
      
    ),
    #, # Navbar 1, tabPanel,
    tabPanel(
      "Transform",
      sidebarPanel(
        width = 4,
        
        selectInput(
          "MissingVals",
          "Replace Missing Values:",
          c(
            "Drop NaN values" = "dropna",
            "Mean" = "Mean",
            "Std" = "Std",
            "Median" = "Median"
          ),
          selected = "Mean"
        ),
        radioButtons(
          "transform",
          "Transformation",
          
          choices = c(
            "No Transformations" = "nothing",
            Normalize = 'norm',
            Logarithm = "log",
            Square = "sqr"
          ),
          selected = "nothing"
        ),
        
        conditionalPanel(
          condition = "rm_outl",
          checkboxGroupInput(
            "outliers",
            "Outliers",
            choices = c("Remove Outliers"),
            selected = FALSE
          ),
        ),
        
      ),
      mainPanel(
        htmlOutput("header_preprocess"),
        DT::dataTableOutput('data_transformed'),
      )# mainPanel,
    ),
    
    tabPanel(
      "Explore",
      sidebarPanel(
        selectInput("groupby", "Column to group by:", "", multiple = TRUE),
        selectInput("aggregate", "Column to aggregate:", "", multiple = TRUE),
        selectInput(
          "func",
          "Aggregate by:",
          choices = c(
            mean = "mean",
            std = "sd",
            median = "median",
            max = "max",
            min = "min"
          ),
          multiple = TRUE,
          selected = "mean"
        ),
        selectInput("filter", "Select column to filter:", "", multiple = TRUE),
      ),
      
      mainPanel(
        verbatimTextOutput('test'),
        htmlOutput("header_agg"),
        DT::dataTableOutput('agg_table'),
        htmlOutput("header_filter"),
        DT::dataTableOutput('filtered_table'),
      )
    ),
    tabPanel(
      "Box Plot",
      sidebarPanel(
        selectInput("outl_col1", "Select X axis:", "", multiple = FALSE),
        selectInput("outl_col2", "Select Y axis:", "", multiple = FALSE),
        selectInput("col_fill", "Select fill column:", "", multiple = FALSE),
        actionButton("boxPlt_button", label = "Update", width = 200),
        hr(),
        checkboxInput("flip_coords", "Flip axis", FALSE),
        
      ),
      
      mainPanel(
        htmlOutput("header_outliers"),
        plotOutput("boxplotOutliers")
        
      )
    ),
    tabPanel(
      "Histogram",
      sidebarPanel(
        selectInput("hist_x", "Select X axis:", "", multiple = FALSE),
        sliderInput(
          "binsize",
          label = h3("Bin Size"),
          min = 10,
          max = 100,
          value = 30,
          step = 5
        ),
        selectInput("fill_hist", "Select fill column:", "", multiple = FALSE),
        actionButton("hist_button", label = "Update", width = 200),
        hr(),
      ),
      
      mainPanel(htmlOutput("header_histogram"),
                plotOutput("pltHist"))
    ),
    tabPanel(
      "ScatterPlot",
      sidebarPanel(
        selectInput("first_variable", "Select X axis:", "", multiple = FALSE),
        selectInput("second_variable", "Select Y axis:", "", multiple = FALSE),
        selectInput("third_variable", "Select fill column:", "", multiple = FALSE),
        actionButton("scatterPlt_button", label = "Update", width = 200),
        hr()
        
      ),
      mainPanel(htmlOutput("scatter_plot"),
                plotOutput("pltScatterPlot")),
    ),
    tabPanel(
      "Line Plot",
      sidebarPanel(
        selectInput("first_var", "Select X axis:", "", multiple = FALSE),
        selectInput("second_var", "Select Y axis:", "", multiple = FALSE),
        selectInput("third_var", "Select fill column:", "", multiple = FALSE),
        actionButton("linePlt_button", label = "Update", width = 200),
        hr()
      ),
      mainPanel(htmlOutput("line_plot"),
                plotOutput("pltLinePlot")),
    ),
    tabPanel(
      "Bar Plot",
      sidebarPanel(
        selectInput("var_1", "Select X axis:", "", multiple = FALSE),
        selectInput("var_2", "Select fill column:", "", multiple = FALSE),
        actionButton("barPlt_button", label = "Update", width = 200),
        hr()
      ),
      mainPanel(htmlOutput("bar_plot"),
                plotOutput("pltBarPlot")),
    ),
    tabPanel(
      "Heatmap" ,
      sidebarPanel(
        selectInput("heat_x" , "Select columns", "", multiple = TRUE),
        actionButton("heatmap_button", label = "Update", width = 200),
        hr()
      ),
      mainPanel(htmlOutput("heat_map"),
                plotOutput("pltheat")),
    ),
    tabPanel(
      "Pie Chart" ,
      sidebarPanel(
        selectInput("pieChart" , "Select columns", "", multiple = F),
        actionButton("pieChart_button", label = "Update", width = 200),
        hr()
      ),
      mainPanel(htmlOutput("pie_chart"),
                plotOutput("pltPieChart")),
    ),
    
    
  ) # navbarPage
) # fluidPage
options(shiny.maxRequestSize = 30 * 1024 ^ 2)

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
      data = read.csv(inFile()$datapath, stringsAsFactors = F)
      
      if (input$date != "") {
        col = input$date
        data = to_date(col, data)
        
      }
      data = factorize(data)
      write.csv(data, "saved.csv", row.names = FALSE)
    }
    
    return (data)
  })
  
  
  take_data <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    } else {
      data = read.csv(inFile()$datapath, stringsAsFactors = F)
      
      
      data = factorize(data)
      return(data)
    }
    
  })
  
  myData_cat <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    }
    else {
      data = read.csv(inFile()$datapath, stringsAsFactors = F)
      
      data = factorize(data)
      data_cat = select_if(data, is.factor)
      return(data_cat)
    }
    
  })
  
  myData_numerics <- reactive({
    if (is.null(inFile())) {
      return(NULL)
    }
    else {
      data = read.csv(inFile()$datapath, stringsAsFactors = F)
      
      data = factorize(data)
      data_num = select_if(data, is.numeric)
      return(data_num)
    }
  })
  observeEvent(input$tabsetPanelID, {
    if (input$tabsetPanelID == "explore") {
      data = read.csv("saved.csv", stringsAsFactors = F)
    }
  })
  observe({
    updateSelectInput(session,
                      "date",
                      choices = c("", names(take_data())))
    
    updateSelectInput(session,
                      "groupby",
                      choices = names(myData_cat()))
    updateSelectInput(session,
                      "aggregate",
                      choices = names(myData_numerics()))
    updateSelectInput(session,
                      "filter",
                      choices = names(take_data()))
    updateSelectInput(session,
                      "outl_col1",
                      choices = names(myData_cat()))
    updateSelectInput(session,
                      "outl_col2",
                      choices = names(myData_numerics()))
    updateSelectInput(session,
                      "col_fill",
                      choices = c("None", names(myData_cat())))
    updateSelectInput(session,
                      "hist_x",
                      choices = names(myData_numerics()))
    
    updateSelectInput(session,
                      "fill_hist",
                      choices = c("None", names(myData_cat())))
    updateSelectInput(session,
                      "first_variable",
                      choices = names(myData_numerics()))
    
    updateSelectInput(session,
                      "second_variable",
                      choices = names(myData_numerics()))
    updateSelectInput(session,
                      "third_variable",
                      choices = c("None", names(myData_cat())))
    updateSelectInput(session,
                      "first_var",
                      choices = names(myData_numerics()))
    
    updateSelectInput(session,
                      "second_var",
                      choices = names(myData_numerics()))
    updateSelectInput(session,
                      "third_var",
                      choices = c("None", names(myData_cat())))
    updateSelectInput(session,
                      "var_1",
                      choices = c(names(myData_cat())))
    updateSelectInput(session,
                      "var_2",
                      choices = c("None", names(myData_cat())))
    updateSelectInput(session,
                      "fill_heat",
                      choices = c("None", names(myData_cat())))
    updateSelectInput(session,
                      "pieChart",
                      choices = c(names(myData_cat())))
    updateSelectInput(
      session,
      "heat_x",
      choices = names(myData_numerics()),
      selected = names(myData_numerics()[1:2])
    )
    
    
    
    
  })
  
  
  output$agg_table <- DT::renderDataTable({
    data = take_data()
    
    grouped_df = data %>%
      group_by(!!!rlang::syms(input$groupby)) %>%
      # summarise(aa = mean(amount_request))
      summarise_at(input$aggregate, (input$func), na.rm = TRUE)
    return(grouped_df)
    
    
  }, options = list(scrollX = T))
  
  
  output$filtered_table <- DT::renderDataTable({
    data = read_saved_data("saved.csv")
    filtered_df = data %>%
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
  output$DataPreview <- renderUI({
    HTML("<h2>Data Preview</h2>")
  })
  output$SummaryOfData <- renderUI({
    HTML("<h2>Summary</h2>")
    
  })
  output$pie_chart <- renderUI({
    HTML("<h2>Pie Chart</h2>")
  })
  output$header_agg <- renderUI({
    HTML("<h2>Data aggregation</h2>")
    
  })
  
  output$header_filter <- renderUI({
    HTML("<h2>Data Filter</h2>")
    
  })
  
  output$header_outliers <- renderUI({
    HTML("<h2>Box Plot </h2>")
    
  })
  
  output$scatter_plot <- renderUI({
    HTML("<h2>Scatter Plot</h2>")
    
  })
  
  output$line_plot <- renderUI({
    HTML("<h2>Line Plot</h2>")
    
  })
  output$bar_plot <- renderUI({
    HTML("<h2>Bar Plot</h2>")
    
  })
  output$heat_map <- renderUI({
    HTML("<h2>Heat map</h2>")
  })
  
  
  
  output$boxplotOutliers <- renderPlot({
    data = read_saved_data("saved.csv")
    if (input$boxPlt_button) {
      data = read_saved_data("saved.csv")
    }
    
    data = fill_missing_vals(data, "median")
    col_x = input$outl_col1
    col_y = input$outl_col2
    col_fill = input$col_fill
    
    
    
    if (col_fill == "None") {
      col_fill = NULL
      plt = ggplot(data = data, aes_string(y = col_y, x = col_x, fill =
                                             col_fill)) + geom_boxplot(fill = "#ADD8E6")
    } else{
      col_fill = input$col_fill
      plt = ggplot(data = data, aes_string(y = col_y, x = col_x, fill =
                                             col_fill)) + geom_boxplot()
    }
    if (input$flip_coords == TRUE) {
      plt = plt + coord_flip()
    }
    
    
    plt +
      theme_bw() +
      labs(title = paste(col_x, " VS ", col_y)) +
      scale_fill_brewer(palette = "Pastel2") +
      theme(
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title = element_text(size = 15)
      )
  })
  output$pltScatterPlot <- renderPlot({
    data = read_saved_data("saved.csv")
    if (input$scatterPlt_button) {
      data = read_saved_data("saved.csv")
    }
    first = input$first_variable
    second = input$second_variable
    third = input$third_variable
    
    if (third == "None") {
      third = NULL
      plt = ggplot(data = data, aes_string(x = first, y = second, color =
                                             third)) + geom_point(color = "#ADD8E6")
    } else{
      third = input$third_variable
      plt = ggplot(data = data, aes_string(x = first, y = second, color =
                                             third)) + geom_point()
    }
    
    
    plt +
      theme_bw() +
      labs(title = paste(toupper(first), " vs ", toupper(second))) +
      scale_fill_brewer(palette = "Pastel2") +
      theme(
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title = element_text(size = 15)
      )
  })
  
  output$pltLinePlot <- renderPlot({
    data = read_saved_data("saved.csv")
    if (input$linePlt_button) {
      data = read_saved_data("saved.csv")
    }
    first = input$first_var
    second = input$second_var
    third = input$third_var
    
    if (third == "None") {
      third = NULL
      plt = ggplot(data = data, aes_string(x = first, y = second, color =
                                             third)) + geom_line()
    } else{
      third = input$third_var
      plt = ggplot(data = data, aes_string(x = first, y = second, color =
                                             third)) + geom_line()
    }
    
    
    plt +
      theme_bw() +
      labs(title = paste(toupper(first), " vs ", toupper(second))) +
      scale_fill_brewer(palette = "Pastel2") +
      theme(
        axis.title.y = element_text(size = 20),
        axis.title.x = element_text(size = 20),
        title = element_text(size = 15)
      )
  })
  output$pltHist <- renderPlot({
    data = read_saved_data("saved.csv")
    if (input$hist_button) {
      data = read_saved_data("saved.csv")
    }
    
    col_x = input$hist_x
    binsize = input$binsize
    col_fill = input$fill_hist
    
    
    
    if (col_fill == "None") {
      col_fill = NULL
      plt = ggplot(data = data, aes_string(x = col_x, fill = col_fill)) +
        geom_histogram(fill =  "#ADD8E6",
                       bins = binsize,
                       stat = "count")
    } else{
      col_fill = input$fill_hist
      plt = ggplot(data = data, aes_string(x = col_x, fill = col_fill)) +
        geom_histogram(bins = binsize, stat = "count")
    }
    
    
    plt +
      theme_bw() +
      labs(title = paste(col_x, " histogram plot")) +
      scale_fill_brewer(palette = "Pastel2") +
      theme(axis.title.x = element_text(size = 20),
            title = element_text(size = 15))
  })
  
  
  output$header_preprocess <- renderUI({
    HTML("<h2>Data Preporcessing</h2>")
    
  })
  output$pltBarPlot <- renderPlot({
    data = read_saved_data("saved.csv")
    if (input$barPlt_button) {
      data = read_saved_data("saved.csv")
    }
    first = input$var_1
    second = input$var_2
    
    if (second == "None") {
      plt = ggplot(data = data, aes_string(x = first)) +
        geom_bar(fill =  "#ADD8E6")
    } else{
      second =  input$var_2
      plt = ggplot(data = data, aes_string(x = first, fill = second)) +
        geom_bar(position = 'dodge')
    }
    plt +
      theme_bw() +
      labs(title = paste(first, " Bar plot")) +
      scale_fill_brewer(palette = "Pastel2") +
      theme(axis.title.x = element_text(size = 20),
            title = element_text(size = 15))
    
  })
  
  
  output$pltheat <- renderPlot({
    data = read_saved_data("saved.csv")
    if (input$heatmap_button) {
      data = read_saved_data("saved.csv")
    }
    first_var = input$heat_x
    second_var = input$heat_y
    data = myData() %>%
      select(!!!rlang::syms(first_var))
    corr_mat <-
      round(cor(data[, unlist(lapply(data, is.numeric))], use = "complete.obs"), 2)
    melted_corr_mat <- melt(corr_mat)
    
    ggplot(data = melted_corr_mat, aes(x = Var1, y = Var2, fill = value)) +
      geom_tile() +
      geom_text(aes(Var2, Var1, label = value),
                color = "white",
                size = 10)
    
  })
  output$pltPieChart <- renderPlot({
    data = read_saved_data("saved.csv")
    if (input$pieChart_button) {
      data = read_saved_data("saved.csv")
    }
    first_var = input$pieChart
    
    ggplot(data, aes_string(x = factor(1), fill = first_var)) + geom_bar() +
      coord_polar(theta = "y", start = 0) +
      theme_minimal()
  })
  
  
  output$data_transformed <- DT::renderDataTable({
    data = myData()
    data = fill_missing_vals(data, "mean")
    data_old = data.frame()
    
    if (input$MissingVals == 'dropna') {
      data = myData()
      data = fill_missing_vals(data, "dropna")
      write.csv(data, "saved.csv", row.names = FALSE)
      
    }
    
    if (input$MissingVals == 'Mean') {
      data = myData()
      data = fill_missing_vals(data, "mean")
      write.csv(data, "saved.csv", row.names = FALSE)
      
    }
    else if (input$MissingVals == 'Std') {
      data = myData()
      data = fill_missing_vals(data, "std")
      write.csv(data, "saved.csv", row.names = FALSE)
      
    }
    else if (input$MissingVals == 'Median') {
      data = myData()
      data = fill_missing_vals(data, "median")
      # write.csv(data, "saved.csv", row.names = FALSE)
      
    }
    if (is.null(input$outliers) == FALSE) {
      data = read_saved_data("saved.csv")
      # data = fill_missing_vals(data, "median")
      data = outlier_detection(data)
      write.csv(data, "saved.csv", row.names = FALSE)
    }
    
    if (input$transform == 'norm') {
      data = transform_data(data, 'norm')
      write.csv(data, "saved.csv", row.names = FALSE)
      
    }
    if (input$transform == 'log') {
      data = transform_data(data, 'log')
      write.csv(data, "saved.csv", row.names = FALSE)
      
    }
    if (input$transform == 'sqr') {
      data = transform_data(data, 'sqr')
      write.csv(data, "saved.csv", row.names = FALSE)
      
    }
    if (input$transform == 'nothing') {
      data = fill_missing_vals(data, "mean")
      write.csv(data, "saved.csv", row.names = FALSE)
      
    }
    
    # write.csv(data, "saved.csv", row.names = FALSE)
    
    return(data)
    
  }, options = list(scrollX = T))
  
  
}


shinyApp(ui = ui, server = server)
