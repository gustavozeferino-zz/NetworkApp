## Network App ##

library("shiny")
library("devtools")
library("ggplot2")
library("huge")
library("qgraph")
library("psych")

shinyUI(pageWithSidebar(
  titlePanel("Network App"),
  sidebarPanel(position = "right",
               # Upload file
               fileInput('input', 'Choose CSV or TXT File',
                         accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv')),
               
               # Specify kind of data
               selectInput('sortdata', 
                           label = "Specify the kind of data that is uploaded:",
                           choices = list("Raw Data",
                                          "Adjacency Matrix",
                                          "Edgelist"), selected = "Adjacency Matrix"),   
               tags$hr(),
               # Contains the file a header? 
               checkboxInput('header', 'Header', TRUE),
               
               tags$hr(),
               
               # Select separator
               radioButtons('sep', '',
                            c(Comma=',',
                              Semicolon=';',
                              Tab='\t',
                              Whitespace = ''),
                            '\t'),
               
               tags$hr(),
               
               # Select appropriate quotes
               radioButtons('quote', '',
                            c(None='',
                              'Double Quote'='"',
                              'Single Quote'="'"),
                            '"'),
               
               # Specify coding for NAs
               textInput("missing",
                         label = "Missing value code:"),
               
               tags$hr(),
               
               # Download network as pdf
               downloadButton('downloadnetwork', 'Download PDF'),
               
               # Download example data
               downloadButton('downloadexample', 'Download Example Data')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Network", 
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
                                      label = "Node Labels", 
                                      value = TRUE)),
                 
                 column(3,
                        # Select minimum value edge weights
                        sliderInput("minimum",
                                    label = "Minimum Edge Weight:",
                                    min = 0,
                                    max = 1,
                                    value = 0))),
               
               fluidRow(
                 column(4,
                        # Choose network estimation method
                        selectInput("method",
                                    label = "Network Estimation Method",
                                    choices = c("Pearson Correlation", 
                                                "Partial Correlation", 
                                                "GLASSO"),
                                    selected = "Partial Correlation")),
                 
                 column(3,
                        # Weighted graph TRUE/FALSE
                        checkboxInput("weighted",
                                      label = "Edge Weights",
                                      value = FALSE)),
                 
                 column(3,
                        # Select maximum value edge weights
                        sliderInput("maximum",
                                    label = "Maximum Edge Weight:",
                                    min = 0,
                                    max = 1,
                                    value = 1))),
               
               fluidRow(
                 column(4,
                        # Select network layout
                        selectInput("layout",
                                    label = "Network Layout",
                                    choices = c("Circle", "Spring", "Grouped Circle"),
                                    selected = "Spring")),
                 
                 column(3,        
                        # Directed edges TRUE/FALSE
                        checkboxInput("direction",
                                      label = "Directed Edges",
                                      value = FALSE)),
                 
                 column(3,
                        # Select cut-off value edge weights
                        sliderInput("cut",
                                    label = "Edge Cut-Off Value",
                                    min = 0,
                                    max = 1,
                                    value = 0.6))),
               
               fluidRow(
                 column(4),             
                 
                 
                 column(3,               
                        # Plot graph details TRUE/FALSE
                        checkboxInput("details",
                                      label = "Graph Details",
                                      value = FALSE)),
                 column(3,
                        # Select size of nodes
                        sliderInput("nodesize",
                                    label = "Node Size:",
                                    min = 0,
                                    max = 25,
                                    value = 6.1)),
                 
                 fluidRow(
                   column(4),
                   
                   column(3,
                          checkboxInput("normal",
                                        label = "Non-Paranormal Transformation",
                                        value = FALSE)),
                   
                   column(3,
                          # Select width edge
                          sliderInput("edgesize",
                                      label = "Edge Size:",
                                      min = 0,
                                      max = 25,
                                      value = 1)))),
               
               br(),
               br()),
      
      
      
      
      tabPanel("Centrality Plot", 
               br(),
               br(),
               plotOutput("centplot"),
               
               checkboxInput("horizontal",
                             label = "Flip plot", 
                             value = FALSE),
               
               checkboxInput("strength",
                             label = "Strength",
                             value = FALSE),
               
               checkboxInput("betweenness",
                             label = "Betweenness",
                             value = TRUE),
               
               checkboxInput("closeness",
                             label = "Closeness",
                             value = TRUE),
               
               checkboxInput("indegree",
                             label = "In Degree",
                             value = FALSE),
               
               checkboxInput("outdegree",
                             label = "Out Degree",
                             value = FALSE),
               
               tags$hr(),         
               downloadButton('downloadcentralityplot', 'Download Centrality Plot'),
               br(),
               br()),
      
      
      tabPanel("Centrality Table",
               br(),
               br(),
               tableOutput("centtable"),
               
               checkboxInput("strengthtab",
                             label = "Strength",
                             value = FALSE),
               
               checkboxInput("betweennesstab",
                             label = "Betweenness",
                             value = TRUE),
               
               checkboxInput("closenesstab",
                             label = "Closeness",
                             value = TRUE),
               
               checkboxInput("indegreetab",
                             label = "In Degree",
                             value = FALSE),
               
               checkboxInput("outdegreetab",
                             label = "Out Degree",
                             value = FALSE),
               
               
               
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
               
               checkboxInput("ws",
                             label = "WS", 
                             value = TRUE),
               
               checkboxInput("zhang",
                             label = "Zhang",
                             value = TRUE),
               
               checkboxInput("onnela",
                             label = "Onnela",
                             value = TRUE),
               
               checkboxInput("barrat",
                             label = "Barrat",
                             value = TRUE),               
               
               tags$hr(), 
               
               downloadButton('downloadclusteringplot', 'Download Clustering Plot')),
      
      tabPanel("Clustering Table",
               br(),
               br(),
               tableOutput("clusttable"),
               
               checkboxInput("wstab",
                             label = "WS", 
                             value = TRUE),
               
               checkboxInput("zhangtab",
                             label = "Zhang",
                             value = TRUE),
               
               checkboxInput("onnelatab",
                             label = "Onnela",
                             value = TRUE),
               
               checkboxInput("barrattab",
                             label = "Barrat",
                             value = TRUE),
               
               tags$hr(),
               
               downloadButton('downloadclusteringtable', 'Download Clustering Table'),
               br(),
               br())
    )
  )  
)
)


