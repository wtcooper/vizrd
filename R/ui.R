library(shiny)


shinyUI(		
    fluidPage(

      # Sidebar with controls to select a dataset and specify the
        sidebarLayout(
            sidebarPanel(
                
                
                ###########################
                ## Sidebar for data chooser 
                ###########################
                
                    uiOutput("choose_dataset"),
                
                
                ###############################
                ## Sidebar for data table 
                ###############################
                conditionalPanel(
                    condition = "(input.tabcond == 2 || input.tabcond == 3)",
                    strong("Choose the columns to include in the table."),
                    tags$hr(),
                    uiOutput("choose_columns")
                ) ,
                
                
                
                ###############################
                ## Sidebar for heatmap 
                ###############################
                conditionalPanel(
                    condition = "input.tabcond == 3",
                    #        htmlOutput("choose_columns")
                    tags$hr(),
                    strong("Do you want to dummy code factors (one-hot encode)?"),
                    checkboxInput("dummybox", label = "Yes", value = FALSE),
                    tags$hr(),
                    strong("Do you want to randomize a sample?"),
                    checkboxInput("randbox", label = "Yes", value = FALSE),
                    numericInput('headobs', 'Sample size:', 50)
                ), 
                
                
                
                ###############################
                ## Sidebar for distribution plots 
                ###############################
                conditionalPanel(
                    condition = "input.tabcond == 4",
                    strong("Choose the column to plot."),
                    tags$hr(),
                    htmlOutput("pick_a_column"),
                    tags$hr(),
                    numericInput('distobs', 'Sample size (randomized):', 1000),
                    tags$hr(),
                    sliderInput("binsize", "Histogram bin size", .1, 20, value=1)
                )
            ),
            
            mainPanel(
                
                tabsetPanel(
                    id = "tabcond",

                    
                    ###############################
                    ## File chooser
                    ###############################
                    
                    tabPanel("Choose File",
                        
                        br(),
                        h2("Summary of Dataset:"),
                        br(),
                        verbatimTextOutput("summary"),
                        
                        value=1),
                    
                    
                    
                    ###############################
                    ## Table
                    ###############################
                    
                    tabPanel("Data Table", 
                        
                        br(),
                        dataTableOutput(outputId="table"),
                        
                        value=2) ,
                    
                    
                    
                    ###############################
                    ## Heatmap
                    ###############################
                    
                    tabPanel("Heatmap", 
                        strong("Heatmap representation of the data (range scaled from 0-1)."),
                        br(),
                        downloadButton('heatsave', 'Save Heatmap Plot'),
                        br(),
                        plotOutput("heatplot"), 
                        
                        value=3) ,
                    
                    
                    
                    ###############################
                    ## Data distribution plots
                    ###############################
                    
                    tabPanel("Distribution", 
                        br(),
                        strong("Raw data plots. Note: color represents standard deviations from mean."),
                        br(),
                        downloadButton('rawsave', 'Save Raw Plot'),
                        br(),
                        plotOutput("rawplot"),
                        br(),
                        tags$hr(),
                        br(),
                        strong("Distribution of the data points."),
                        br(),
                        downloadButton('histsave', 'Save Density Plot'),
                        plotOutput("histplot"), 
                        
                        value=4)
                )
            
            )
        )
    )
)