#setwd("C:\\Users\\icecr\\OneDrive\\R\\CSVreader")
getwd()

#install packages
packages <- c('shinydashboard','shiny','DT','data.table','grid','ggplot2','C50','gmodels','e1071','neuralnet','dplyr','plotly','lattice','GGally', 'reticulate')

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages  ())))  
}

if (length(setdiff("keras", rownames(installed.packages()))) > 0) {
  devtools::install_github("rstudio/keras") 
}


library(keras)
library(reticulate)

require(shinydashboard)
require(shiny)
require(DT)
require(data.table)
require(grid)
require(ggplot2)
require(C50)
require(gmodels)
require(class)
require(gmodels)
require(e1071)
require(data.table)
require(neuralnet)
require(ggplot2)
require(dplyr)
require(plotly)
require(lattice)
require(GGally)


header <- dashboardHeader(
  title='Paka 994'
)

sidebar <- dashboardSidebar(
  fileInput("file1", "Choose CSV File",
            multiple = FALSE,
            accept = c("text/csv",".xlsx",".txt",
                       "text/comma-separated-values,text/plain",
                       ".csv") ),
  sidebarMenu(
    menuItem("Table",icon=icon('table'),
             menuSubItem('Tableformat',tabName='tableformat') ),
    
    menuItem("Statistics",icon=icon('file-contract'),
             menuSubItem('Data Summary',tabName='datasummary'),
             menuSubItem('Regression',tabName = 'regression'),
             menuSubItem('Decision Tree',tabName = 'decision')),
    menuItem("Machine Learning",icon=icon('laptop'),
             menuSubItem('JRip',tabName = 'jrip'),
             menuSubItem('KMeans',tabName='kmeans'),
             menuSubItem('KNN',tabName='knn'),
             menuSubItem('Naive bayes',tabName = 'naive'),
             menuSubItem('Neuralnet',tabName = 'neuralnet')),
    menuItem("Graph",icon=icon('chart-area'),
             menuSubItem('Barplot',tabName='barplot'),
             menuSubItem('Piechart',tabName='piechart'),
             menuSubItem('Lineplot',tabName='lineplot'),
             menuSubItem('Scatterplot',tabName='scatterplot'),
             menuSubItem('Boxplot',tabName='boxplot'),
             menuSubItem('Pairs',tabName='pairs'))
    
    
  )
  
  
)


body <- dashboardBody(
  
  
  tabItems(
    
    tabItem(tabName = "tableformat",
            mainPanel(
              DT::dataTableOutput("table")
            )
    ),

    tabItem(tabName = "datasummary",
            fluidRow(
              box(
                title = "Summary",   solidHeader = TRUE,
                collapsible = TRUE, width = 6,
                verbatimTextOutput("stat_summary")
              ),
              box(
                title = "Quantile",   solidHeader = TRUE,
                collapsible = TRUE, width = 6,
                verbatimTextOutput("quan")
              ),
              box(
                title = "Quantile Graph",   solidHeader = TRUE,
                collapsible = TRUE,width = 12,
                plotOutput("plot_quan",height='auto')
              ),
              box(
                title = "Distribution Graph",   solidHeader = TRUE,
                collapsible = TRUE,width = 12,
                plotOutput("plot_dist",height='auto')
              )
              
              
            )
    ),
  
    tabItem(tabName = "regression",
            
            fluidRow(
              box(
                title = "Data",   solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                verbatimTextOutput("submit_input_sample_regression")
              ),
              box(
                width=2, 
                uiOutput("dependents_delcol_regression",
                         style = "overflow-y:scroll;  
                         max-height: 100px;  background: 
                         ghostwhite;"),
                uiOutput("dependents_selcol_regression"), 
                
                uiOutput("dependents_button_regression") 
              ),
              box(
                title = "ANOVA Table",   solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                verbatimTextOutput("anova_Render_regression")
              ),
              box(
                title = "Regression",   solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                verbatimTextOutput("coefficient_Render_regression")
              ),
              box(
                title = "Parameter Confidence Interval",   solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                verbatimTextOutput("interval_Render_regression")
              ),
              box(
                title = "Model Plot",  solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                plotOutput("plot_reg",height='auto')
              )
            )
    ),
  
    tabItem(tabName = "decision",
            fluidRow(
              box(title = "Data",   solidHeader = TRUE,
                  collapsible = TRUE,width = 8,
                  verbatimTextOutput("submit_input_sample_decision")
              ),
              box(
                width=2, 
                uiOutput("dependents_delcol_decision",
                         style = "overflow-y:scroll;  max-height: 100px;  
                         background: ghostwhite;")
              ),
              box(
                width=2, 
                uiOutput("dependents_selcol_decision"),
                uiOutput("dependents_selmodel_decision"),
                uiOutput("dependents_selwinnow_decision"),
                numericInput("trials", "Trials:", 1, min = 1),
                numericInput("seed", "Seed:", 1),
                uiOutput("dependents_button_decision") 
              ),         
              box(title = "Cross Table",   solidHeader = TRUE,
                  collapsible = TRUE,width = 9,
                  verbatimTextOutput("TestTableRender_decision")
              ),
              box(title = "Model Summary",   solidHeader = TRUE,
                  collapsible = TRUE,width = 9,
                  verbatimTextOutput("TestSummaryRender_decision")
              )
            )
    ),
  
    tabItem(tabName = "jrip",
            fluidRow(
              box(title = "Data",   solidHeader = TRUE,
                  collapsible = TRUE,width = 9,
                  verbatimTextOutput("submit_input_sample_jrip")
              ),
              box(width=2,   
                  uiOutput("dependents_delcol_jrip",
                           style = "overflow-y:scroll;  max-height: 100px;  
                         background: ghostwhite;"), 
                  uiOutput("dependents_selcol_jrip"),
                  numericInput("numopt", "NumOpt", 1, min = 1),
                  numericInput("seed", "Seed:", 1),
                  uiOutput("dependents_button_jrip")
              ),   
              box(title = "Cross Table",   solidHeader = TRUE,
                  collapsible = TRUE,width = 9,
                  verbatimTextOutput("TestTableRender_jrip")
              ),
              box(title = "Model Summary",   solidHeader = TRUE,
                  collapsible = TRUE,width = 9,
                  verbatimTextOutput("TestSummaryRender_jrip")
              )
            )
            
    ),

    tabItem(tabName = "kmeans",
            fluidRow(
              box(title = "Data",   solidHeader = TRUE,
                  collapsible = TRUE,width = 9,
                  verbatimTextOutput("submit_input_sample_kmeans")
              ),
              box(title = "Data Summary",   solidHeader = TRUE,
                  collapsible = TRUE,width = 9,
                  verbatimTextOutput("TestSummaryRender_kmeans")
              ),
              box(width=2,   
                  uiOutput("dependents_delcol_kmeans",
                           style = "overflow-y:scroll;  max-height: 100px;  background: ghostwhite;"),
                  uiOutput("dependents_selcol_kmeans"), 
                  numericInput("seed", "Seed", 1),
                  numericInput("k_mv", "Centers", 2,min=2),
                  numericInput("k_nstart", "nstart", 1),
                  uiOutput("dependents_button_kmeans")
              ),
              box(title = "Cross Table",   solidHeader = TRUE,
                  collapsible = TRUE,width = 9,
                  verbatimTextOutput("TestTableRender_kmeans")
              ),
              box(title = "Model Centers",   solidHeader = TRUE,
                  collapsible = TRUE,width = 9,
                  verbatimTextOutput("TestCenterRender_kmeans")
              ),
              box(title = "Model Plot",   solidHeader = TRUE,
                  collapsible = TRUE,collapsed=TRUE,width = 9,
                  plotOutput("plot_kmeans",height='auto')
              )
            )
    ),
  
    tabItem(tabName = "knn",
            fluidRow(
              box(
                title = "Data",   solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                verbatimTextOutput("submit_input_sample_knn")
              ),
              box(
                title = "Data Summary",   solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                verbatimTextOutput("TestSummaryRender_knn")
              ),
              box(
                width=2,   
                uiOutput("dependents_delcol_knn", 
                         style = "overflow-y:scroll;  max-height: 100px;  background: ghostwhite;"),
                uiOutput("dependents_selcol_knn"),
                numericInput("knn_k", "k", 'Default',min=2),
                uiOutput("dependents_button_knn") 
                
                
              ),
              box(
                title = "Cross Table",   solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                verbatimTextOutput("TestTableRender_knn")
              )
            )
            
    ),
    
    tabItem(tabName = "naive",
            fluidRow(
              box(
                title = "Data",   solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                verbatimTextOutput("submit_input_sample_naive")
              ),
              box(
                width=2,   
                uiOutput("dependents_delcol_naive",
                         style = "overflow-y:scroll;  max-height: 100px;  background: ghostwhite;"),
                uiOutput("dependents_selcol_naive"),
                numericInput("laplace", "Laplace", 0, min = 0, max = 1),
                numericInput("seed", "Seed", 1),
                uiOutput("dependents_button_naive")
              ),
              box(
                title = "Cross Table",   solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                verbatimTextOutput("TestTableRender_naive")
              ),
              box(
                title = "Model",   solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                verbatimTextOutput("TestModelRender_naive")
              )
            )
            
    ),
    
    
    tabItem(tabName = "neuralnet",
            fluidRow(
              box(
                title = "Data",   solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                verbatimTextOutput("submit_input_sample_neuralnet")
              ),
              box(
                width=2, 
                uiOutput("dependents_delcol_neuralnet",
                         style = "overflow-y:scroll;  max-height: 100px;  background: ghostwhite;"),
                uiOutput("dependents_selcol_neuralnet"), 
                uiOutput("dependents_type_neuralnet"), 
                numericInput("seed", "Seed:", 1),
                numericInput("hidden_1", "Hidden Layer 1:", 3,min=1),
                numericInput("hidden_2", "Hidden Layer 2:", 2,min=1),
                uiOutput("dependents_button_neuralnet")  
              ),                              
              
              box(
                title = "Accuracy",   solidHeader = TRUE,
                collapsible = TRUE, width = 2,
                verbatimTextOutput("TestAccuracyRender_neuralnet")
              ),
              box(
                title = "Predict",   solidHeader = TRUE,
                collapsible = TRUE, width = 7,
                verbatimTextOutput("TestTableRender_neuralnet")
              ),
              box(
                title = "Model Plot",   solidHeader = TRUE,
                collapsible = TRUE,width = 9,
                plotOutput("plot_neural",height='auto')
              )
            )
            
            
    ),
 
    tabItem(tabName = "barplot",
            sidebarPanel(
              selectInput("in_sel_bar_yVar","y Variable:", choices = NULL),
              selectInput("in_sel_bar_xVar","x Variable:", choices = NULL)
            ),
            mainPanel(
              plotOutput('plot_bar')
            )
    ),

    tabItem(tabName = "piechart",
            sidebarPanel(
              selectInput("in_sel_pie_xVar","x Variable:", choices = NULL),
              selectInput("in_sel_pie_yVar","y Variable:", choices = NULL)
              
              
            ),
            mainPanel(
              plotlyOutput('plot_pie')
            )
    ),
  
    tabItem(tabName = "lineplot",
            sidebarPanel(
              selectInput("in_sel_line_yVar","y Variable:", choices = NULL),
              selectInput("in_sel_line_xVar","x Variable:", choices = NULL)
              
            ),
            mainPanel(
              plotlyOutput('plot_line')
            )
    ),
   
    tabItem(tabName = "scatterplot",
            sidebarPanel(
              selectInput("in_sel_scatter_yVar","y Variable:", choices = NULL),
              selectInput("in_sel_scatter_xVar","x Variable:", choices = NULL)
              
            ),
            mainPanel(
              plotOutput('plot_scatter'),
              textOutput('text_scatter')
            )
    ),
  
    tabItem(tabName = "boxplot",
            sidebarPanel(
              selectInput("in_sel_box_xVar","x Variable:", choices = NULL)
              
            ),
            mainPanel(
              plotOutput('plot_box'),
              verbatimTextOutput('text_box')
            )
    ),
  
    tabItem(tabName = "pairs",
            sidebarPanel(
              uiOutput("dependents_delcol_pairs", 
                       style = "overflow-y:scroll;  max-height: 100px;  background: ghostwhite;"), 
              uiOutput("dependents_selcol_pairs",title='Select Colour'), 
              numericInput("alpha", "Alpha", 0.5),
              uiOutput("dependents_button_pairs"),
              width=2
            ),
            mainPanel(
              plotOutput('plot_pairs')
            )
    )
    
  )
)

ui<-dashboardPage(
  skin='black',
  header,
  sidebar,
  body
)