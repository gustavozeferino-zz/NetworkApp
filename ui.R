## Network App ##

library("shiny")
library("devtools")
library("ggplot2")
library("huge")
library("qgraph")
library("psych")
library("pcalg")

shinyUI(pageWithSidebar(
  titlePanel("Network App"),
  sidebarPanel(position = "right",
               
               p("The options below are needed to specify how your file looks like. If you do not have any data but you want to see how the application works, click “Demo version”  and a dataset is automatically updated that is available via the psych package. The example dataset comprises of 25 NEO-PI-R items: 5 items per trait."),
               
               br(),
               
               # Upload file
               fileInput('input', 'Choose CSV or TXT File',
                         accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv')),
               
               # specify if demo data is to be used
               checkboxInput("demo", 
                             label = "Demo Version", 
                             value = FALSE),
               
               # Specify kind of data
               selectInput('sortdata', 
                           label = "Specify the kind of data that is uploaded:",
                           choices = list("Raw Data",
                                          "Adjacency Matrix",
                                          "Edgelist"), selected = "Raw Data"),   
               tags$hr(),
               # Contains the file a header? 
               checkboxInput('header', 'Header', TRUE),
               checkboxInput("stringfactors", "Strings as factors", FALSE),
               
               tags$hr(),
               
               # Select separator
               radioButtons('sep', 'Separator',
                            c(Comma=',',
                              Semicolon=';',
                              Tab='\t',
                              Whitespace = ''),
                            '\t'),
               
               tags$hr(),
               
               radioButtons('decimal', "Decimal",
                            c(Period = ".",
                              Comma = ","),
                            ","),
               
               tags$hr(),
               
               # Select appropriate quotes
               radioButtons('quote', 'Quote',
                            c(None='',
                              'Double Quote'='"',
                              'Single Quote'="'"),
                            '"'),
               
               # Specify coding for NAs
               textInput("missing",
                         label = "Missing value coding:"),
               
               tags$hr(),
               
               # Download example data
               downloadButton('downloadexample', 'Download Example Data'),
  
               h5("Authors"),
               p("Jolanda Kossakowski <mail@jolandakossakowski.eu> & Sacha Epskamp")),
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
                        
                        # Download network as pdf
                        downloadButton('downloadnetwork', 'Download PDF')),
                 
                 column(3,
                        # Use node labels TRUE/FALSE
                        checkboxInput("node_labels",
                                      label = "Node Labels", 
                                      value = TRUE)),
                 
                 column(3,
                        # Set threshold for edge weight
                        sliderInput("threshold",
                                    label = "Edge Threshold:",
                                    min = 0,
                                    max = 1,
                                    value = 0))),
               
               fluidRow(
                 column(4,
                        
                        # Insert title plot
                        textInput("title",
                                  label = "Title:")),
                 
                 column(3,
                        # Weighted graph TRUE/FALSE
                        checkboxInput("weighted",
                                      label = "Edge Weights",
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
                                    label = "Network Estimation Method:",
                                    choices = c("FDRnetwork",
                                                "GLASSO",
                                                "IC-Algorithm: DAG",
                                                "IC-Algorithm: Skeleton",
                                                "Partial Correlation",
                                                "Pearson Correlation",
                                                "VAR-model"),
                                    selected = "Partial Correlation")),
                 column(3,        
                        # Directed edges TRUE/FALSE
                        checkboxInput("direction",
                                      label = "Directed Edges",
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
                                    label = "Network Layout:",
                                    choices = c("Circle", 
                                                "Spring", 
                                                "Grouped Circle"),
                                    selected = "Spring")),
                 
                                  
                 column(3,               
                        # Plot graph details TRUE/FALSE
                        checkboxInput("details",
                                      label = "Graph Details",
                                      value = FALSE)),
                 column(3,
                        # Select cut-off value edge weights
                        sliderInput("cut",
                                    label = "Edge Cut-Off Value:",
                                    min = 0,
                                    max = 1,
                                    value = 0.1))),
                 
                 fluidRow(
                   column(4,
                          selectInput("FDRmethod",
                                      label = "Method for FDR Network:",
                                      choices = c("Local FDR",
                                                  "None",
                                                  "p-value",
                                                  "q-value"),
                                      selected = "None")),
                   
                   
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
                                      value = 5))),
               
               fluidRow(
                 column(4,
                        selectInput("VARmethod",
                                    label = "Distribution family for VAR-model:",
                                    choices = c("Binary",
                                                "Gaussian",
                                                "None"),
                                    selected = "None")),
                        
                 column(3,
                        checkboxInput("pastelcol",
                                      label = "Pastel colours",
                                      value = FALSE)),
                 column(3,
                        # Select size of nodes
                        sliderInput("nodesize",
                                    label = "Node Size:",
                                    min = 0,
                                    max = 25,
                                    value = 6.1))),
               
               fluidRow(
                 column(4,
                        selectInput("pcindep",
                                    label = "Method for Conditional Independence IC-Algorithm:",
                                    choices = c("Binary",
                                                "Discrete",
                                                "D-separation",
                                                "Gaussian",
                                                "None"),
                                    selected = "None")),
                 column(3,
                        checkboxInput("diagonal",
                                      label = "Plot Self-Loops",
                                      value = FALSE)),
                 column(3,
                        # Set cut-off value for FDR network
                        sliderInput("cutoffFDR",
                                    label = "Cut-off value FDR network:",
                                    min = 0.0001,
                                    max = 1,
                                    value = 0.05))),

               fluidRow(
                 column(4,
                        selectInput("nodeshape",
                                    label = "Node shape:",
                                    choices = c("Circle",
                                                "Diamond",
                                                "Heart",
                                                "Square",
                                                "Triangle"),
                                    selected = "Circle"))),
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
