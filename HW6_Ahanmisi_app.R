library(shiny)
library(shinythemes)
library(data.table)
library(ggplot2)
library(car)
require(shinydashboard)
require(ggplot2)
require(dplyr)
require(tidyr)
require(magrittr)

# playdapp
# read and prepare the dataset
winedat <- read.table('https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data', sep=',')
winedat$V1 <- factor(winedat$V1, levels = c(1,2,3))
variables <- names(winedat) <- c('Class Identifier = V1', 'Alcohol = V2', 'Malic Acid = V3', 'Ash = V4', 'Alcalinity of Ash = V5',
                              'Magnesium = V6', 'Total Phenols = V7', 'Flavanoids = V8', 'Nonflavanoid Phenols = V9',
                              'Proanthocyanins = V10', 'Color Intensity = V11', 'Hue = V12', 
                              'OD280/OD315 Of Diluted Wines = V13', 'Proline = V14')





#Table tab; displays wine data set
table_page <- tabPanel(
    title = "Data Table",
    titlePanel("Wine Data"),
    sidebarLayout(
             sidebarPanel(
                 checkboxGroupInput('variablesList', 'Choose varibales to include',
                                    choices = variables, selected=variables)
             ),
             mainPanel(dataTableOutput('dataTable'))
         )
         
)

not_sel <- "Not Selected"

#main page for analysis
main_page <- tabPanel(
    title = "Analysis",
    titlePanel("Wine Analysis"),
    sidebarLayout(
        sidebarPanel(
            title = "Inputs",
            fileInput("csv_input", "Select CSV File to Import, 
                      Wine Data set can be found at https://archive.ics.uci.edu/ml/machine-learning-databases/wine/wine.data", accept = ".csv"),
            selectInput("num_var_1", "Numerical Variable 1",
                        choices = variables[2:14]),
            selectInput("num_var_2", "Numerical Variable 2",
                        choices = variables[2:14]),
            selectInput("fact_var", "Factor Variable", choices = variables[1]),
            br(),
            actionButton("run_button", "Run Analysis", icon = icon("play"))
        ),
        mainPanel(
            tabsetPanel(
                tabPanel(
                    title = "Plot",
                    plotOutput("plot_1")
                ),
                tabPanel(
                    title = "Statistics",
                    fluidRow(
                        column(width = 4, strong(textOutput("num_var_1_title"))),
                        column(width = 4, strong(textOutput("num_var_2_title"))),
                        column(width = 4, strong(textOutput("fact_var_title")))
                    ),
                    fluidRow(
                        column(width = 4, tableOutput("num_var_1_summary_table")),
                        column(width = 4, tableOutput("num_var_2_summary_table")),
                        column(width = 4, tableOutput("fact_var_summary_table"))
                    ),
                    fluidRow(
                        column(width = 12, strong("Combined Statistics"))
                    ),
                    fluidRow(
                        column(width = 12, tableOutput("combined_summary_table"))
                    )
                    
                )
            )
        )
    )
)

draw_plot_1 <- function(data_input, num_var_1, num_var_2, fact_var){
    if(fact_var!=not_sel){
        data_input[,(fact_var):= as.factor(data_input[,get(fact_var)])]
    }
    if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var != not_sel){
        ggplot(data = data_input,
               aes_string(x = num_var_1, y = num_var_2, color = fact_var)) +
            geom_point()
    }
    else if(num_var_1 != not_sel & num_var_2 != not_sel & fact_var == not_sel){
        ggplot(data = data_input,
               aes_string(x = num_var_1, y = num_var_2)) +
            geom_point()
    }
    else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var != not_sel){
        ggplot(data = data_input,
               aes_string(x = fact_var, y = num_var_1)) +
            geom_violin()
    }
    else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var != not_sel){
        ggplot(data = data_input,
               aes_string(x = fact_var, y = num_var_2)) +
            geom_violin()
    }
    else if(num_var_1 != not_sel & num_var_2 == not_sel & fact_var == not_sel){
        ggplot(data = data_input,
               aes_string(x = num_var_1)) +
            geom_histogram()
    }
    else if(num_var_1 == not_sel & num_var_2 != not_sel & fact_var == not_sel){
        ggplot(data = data_input,
               aes_string(x = num_var_2)) +
            geom_histogram()
    }
    else if(num_var_1 == not_sel & num_var_2 == not_sel & fact_var != not_sel){
        ggplot(data = data_input,
               aes_string(x = fact_var)) +
            geom_bar()
    }
}

create_num_var_table <- function(data_input, num_var){
    if(num_var != not_sel){
        col <- data_input[,get(num_var)]
        if (length(col)>5000) col_norm <- sample(col,5000) else col_norm <- col
        norm_test <- shapiro.test(col_norm)
        statistic <- c("mean", "median", "5th percentile", "95th percentile",
                       "Shapiro statistic", "Shapiro p-value")
        value <- c(round(mean(col),2), round(median(col),2),
                   round(quantile(col, 0.05),2), round(quantile(col, 0.95),2),
                   norm_test$statistic, norm_test$p.value)
        data.table(statistic, value)
    }
}

create_fact_var_table <- function(data_input, fact_var){
    if(fact_var != not_sel){
        freq_tbl <- data_input[,.N, by = get(fact_var)]
        freq_tbl <- setnames(freq_tbl,c("factor_value", "count"))
        freq_tbl
    }
}

create_combined_table <- function(data_input, num_var_1, num_var_2, fact_var){
    if(fact_var != not_sel){
        if(num_var_1 != not_sel & num_var_2 != not_sel){
            res_tbl <- data_input[,.(correlation = cor(get(num_var_1), get(num_var_2))), by = fact_var]
        }
        else if(num_var_1 != not_sel & num_var_2 == not_sel){
            res_tbl <- data_input[,.(mean = mean(get(num_var_1))), by = fact_var]
        }
        else if(num_var_1 == not_sel & num_var_2 != not_sel){
            res_tbl <- data_input[,.(mean = mean(get(num_var_2))), by = fact_var]
        }
    }
    else if(num_var_1 != not_sel & num_var_2 != not_sel){
        res_tbl <- data.table(
            statistic = c("correlation"),
            value = c(cor(
                data_input[,get(num_var_1)],
                data_input[,get(num_var_2)])))
    }
    return(res_tbl)
}


ui <- navbarPage(
    title = "Wine Data Analysis",
    theme = shinytheme('slate'),
    main_page,
    table_page
)


server <- function(input, output, session) {
    dataset <- reactive(winedat)
    
    # overview panel
    # none 
    
    # view data panel 
    output$dataTable <- renderDataTable(dataset()[,input$variablesList])

    
    options(shiny.maxRequestSize=10*1024^2) 
    
    data_input <- reactive({
        req(input$csv_input)
        fread(input$csv_input$datapath)
    })
    
    observeEvent(data_input(),{
        choices <- c(not_sel,names(data_input()))
        updateSelectInput(inputId = "num_var_1", choices = choices)
        updateSelectInput(inputId = "num_var_2", choices = choices)
        updateSelectInput(inputId = "fact_var", choices = choices)
    })
    
    num_var_1 <- eventReactive(input$run_button,input$num_var_1)
    num_var_2 <- eventReactive(input$run_button,input$num_var_2)
    fact_var <- eventReactive(input$run_button,input$fact_var)
    
    # plot
    
    plot_1 <- eventReactive(input$run_button,{
        draw_plot_1(data_input(), num_var_1(), num_var_2(), fact_var())
    })
    
    output$plot_1 <- renderPlot(plot_1())
    
    # 1-d summary tables
    
    output$num_var_1_title <- renderText(paste("Num Var 1:",num_var_1()))
    
    num_var_1_summary_table <- eventReactive(input$run_button,{
        create_num_var_table(data_input(), num_var_1())
    })
    
    output$num_var_1_summary_table <- renderTable(num_var_1_summary_table(),colnames = FALSE)
    
    output$num_var_2_title <- renderText(paste("Num Var 2:",num_var_2()))
    
    num_var_2_summary_table <- eventReactive(input$run_button,{
        create_num_var_table(data_input(), num_var_2())
    })
    
    output$num_var_2_summary_table <- renderTable(num_var_2_summary_table(),colnames = FALSE)
    
    output$fact_var_title <- renderText(paste("Factor Var:",fact_var()))
    
    fact_var_summary_table <- eventReactive(input$run_button,{
        create_fact_var_table(data_input(), fact_var())
    })
    
    output$fact_var_summary_table <- renderTable(fact_var_summary_table(),colnames = FALSE)
    
    # multi-d summary table
    
    combined_summary_table <- eventReactive(input$run_button,{
        create_combined_table(data_input(), num_var_1(), num_var_2(), fact_var())
    })
    
    output$combined_summary_table <- renderTable(combined_summary_table())
    
}

shinyApp(ui, server)
