## Network App ##

library(shiny)
library(shinyapps)
library(qgraph)

shinyUI(pageWithSidebar(
  titlePanel("Network App"),
  sidebarPanel(position = "right",
    # Upload file
    fileInput('input', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    
    tags$hr(),
    # Contains the file a header? 
    checkboxInput('header', 'Header', TRUE),
    
    tags$hr(),
    
    # Select separator
    radioButtons('sep', '',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 '\t'),
    
    tags$hr(),
    
    # Select appropriate quotes
    radioButtons('quote', '',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 '"'),
    
    tags$hr(),
    
    
    tags$hr(),
    
    # Download network as pdf
    downloadButton('downloadnetwork', 'Download PDF')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Network Visualization", 
               br(),
               br(),
               plotOutput("network"),
               
               br(),
               br(),
               br(),
               br(),
               br(),
               br(),
               fluidRow(
                 column(4,
                        # Insert title plot
                        textInput("title",
                                  label = "Title:")),
                
                 column(3,
                        # Use node labels TRUE/FALSE
                        checkboxInput("node_labels",
                                      label = "Node labels", 
                                      value = TRUE)),
                     
                 column(3,
                        # Select minimum value edge weights
                        sliderInput("minimum",
                                    label = "Minimum edge weight:",
                                    min = 0,
                                    max = 1,
                                    value = 0))),
               
               fluidRow(
                 column(4,
                        # Choose network estimation method
                        selectInput("method",
                                    label = "Network estimation method:",
                                    choices = c("Pearson Correlation"),
                                    selected = "Pearson Correlation")),
                       
                 column(3,
                        # Weighted graph TRUE/FALSE
                        checkboxInput("weighted",
                                      label = "Edge weights",
                                      value = TRUE)),
                        
                 column(3,
                        # Select maximum value edge weights
                        sliderInput("maximum",
                                    label = "Maximum edge weight:",
                                    min = 0,
                                    max = 1,
                                    value = 1))),
               
               fluidRow(
                 column(4,
                        # Select network layout
                        selectInput("layout",
                                    label = "Network layout:",
                                    choices = c("Circle", "Spring", "Grouped Circle"),
                                    selected = "Spring")),
                        
                column(3,        
                        # Directed edges TRUE/FALSE
                        checkboxInput("direction",
                                      label = "Directed edges",
                                      value = FALSE)),
                        
                column(3,
                        # Select cut-off value edge weights
                        sliderInput("cut",
                                    label = "Edge cut-off value",
                                    min = 0,
                                    max = 1,
                                    value = 0.6))),

               fluidRow(
                 column(4,             
                    # Plot graph details TRUE/FALSE
                     checkboxInput("details",
                              label = "Graph details",
                              value = TRUE)),        

               column(3,
               # Select width edge
               sliderInput("edgesize",
                           label = "Edge size:",
                           min = 0,
                           max = 25,
                           value = 1)),
               column(3,
               # Select size of nodes
               sliderInput("nodesize",
                           label = "Node size:",
                           min = 0,
                           max = 25,
                           value = 6.1))),
               br(),
               br()),

      
      
  
      tabPanel("Centrality Plot", 
               br(),
               br(),
               plotOutput("centplot"),
               
               checkboxInput("horizontal",
                             label = "Flip plot", 
                             value = FALSE),
               tags$hr(),         
               downloadButton('downloadcentralityplot', 'Download Centrality Plot'),
               br(),
               br()),
      
      
      tabPanel("Centrality Table",
               br(),
               br(),
               tableOutput("centtable"),
               
               tags$hr(),
               
               downloadButton('downloadcentralitytable', 'Download Centrality Table'),
               br(),
               br()),
      
      tabPanel("Clustering Plot", 
               br(),
               br(),
               plotOutput("clustplot"),
               
               tags$hr(),
               
               checkboxInput("horizontal",
                             label = "Flip plot", 
                             value = FALSE),
               tags$hr(),         
               downloadButton('downloadclusteringplot', 'Download Clustering Plot')),
      
      tabPanel("Clustering Table",
               br(),
               br(),
               tableOutput("clusttable"),
               
               tags$hr(),
               
               downloadButton('downloadclusteringtable', 'Download Clustering Table'),
               br(),
               br())
      )
    )  
  )
)
