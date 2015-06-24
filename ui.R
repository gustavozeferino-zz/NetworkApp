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
               
               # Specify if demo data is to be used
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
               
               # Are string to be coded as factor objects?
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
               
               # Select decimal symbol
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
                          
                          # Select what value to use for FDR-network
                          selectInput("FDRmethod",
                                      label = "Method for FDR Network:",
                                      choices = c("Local FDR",
                                                  "None",
                                                  "p-value",
                                                  "q-value"),
                                      selected = "None")),
                   
                   
                   column(3,
                          
                          # apply non-paranormal transformation TRUE/FALSE
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
                        
                        # Select distribution family VAR-network
                        selectInput("VARmethod",
                                    label = "Distribution family for VAR-model:",
                                    choices = c("Binary",
                                                "Gaussian",
                                                "None"),
                                    selected = "None")),
                        
                 column(3,
                        # Node colours pastel TRUE/FALSE
                        # NOTE: CURRENTLY NOT WORKING
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
                        
                        # Method for IC-algorithm
                        selectInput("pcindep",
                                    label = "Method for Conditional Independence IC-Algorithm:",
                                    choices = c("Binary",
                                                "Discrete",
                                                "D-separation",
                                                "Gaussian",
                                                "None"),
                                    selected = "None")),
                 column(3,
                        
                        # Plot self-loops TRUE/FALSE
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
                        
                        # Select node shape
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
               
               # Flip centrality plots 90 degrees
               checkboxInput("horizontal",
                             label = "Flip plot", 
                             value = FALSE),
               
               # Visualize strength measure
               checkboxInput("strength",
                             label = "Strength",
                             value = FALSE),
               
               # Visualize betweenness measure
               checkboxInput("betweenness",
                             label = "Betweenness",
                             value = TRUE),
               
               # Visualize closeness measure
               checkboxInput("closeness",
                             label = "Closeness",
                             value = TRUE),
               
               # Visualize indegree measure
               # ONLY WITH ADJACENCY MATRIX OR EDGELIST
               checkboxInput("indegree",
                             label = "In Degree",
                             value = FALSE),
               
               # Visualize outdegree measure
               # ONLY WITH ADJACENCY MATRIX OR EDGELIST
               checkboxInput("outdegree",
                             label = "Out Degree",
                             value = FALSE),
               
               tags$hr(),   
               
               # Download centrality plot
               downloadButton('downloadcentralityplot', 'Download Centrality Plot'),
               br(),
               br()),
      
      
      tabPanel("Centrality Table",
               br(),
               br(),
               tableOutput("centtable"),
               
               # Print strength measure
               checkboxInput("strengthtab",
                             label = "Strength",
                             value = FALSE),
               
               # Print betweenness measure
               checkboxInput("betweennesstab",
                             label = "Betweenness",
                             value = TRUE),
               
               # Print closeness measure
               checkboxInput("closenesstab",
                             label = "Closeness",
                             value = TRUE),
               
               # Print indegree measure
               # ONLY WITH ADJACENCY MATRIX OR EDGELIST
               checkboxInput("indegreetab",
                             label = "In Degree",
                             value = FALSE),
               
               # Print outdegree measure
               # ONLY WITH ADJACENCY MATRIX OR EDGELIST
               checkboxInput("outdegreetab",
                             label = "Out Degree",
                             value = FALSE),
               
               
               
               tags$hr(),
               
               # Download centrality table
               downloadButton('downloadcentralitytable', 'Download Centrality Table'),
               br(),
               br()),
      
      tabPanel("Clustering Plot", 
               br(),
               br(),
               plotOutput("clustplot"),
               
               tags$hr(),
               
               # Turn clustering plot 90 degrees
               checkboxInput("horizontal",
                             label = "Flip plot", 
                             value = FALSE),
               
               # Visualize WS measure
               checkboxInput("ws",
                             label = "WS", 
                             value = TRUE),
               
               # Visualize zhang measure
               checkboxInput("zhang",
                             label = "Zhang",
                             value = TRUE),
               
               # Visualize Onnela measure
               checkboxInput("onnela",
                             label = "Onnela",
                             value = TRUE),
               
               # Visualize Barrat measure
               checkboxInput("barrat",
                             label = "Barrat",
                             value = TRUE),               
               
               tags$hr(), 
               
               # Download clustering plot
               downloadButton('downloadclusteringplot', 'Download Clustering Plot')),
      
      tabPanel("Clustering Table",
               br(),
               br(),
               tableOutput("clusttable"),
               
               # Print WS measure
               checkboxInput("wstab",
                             label = "WS", 
                             value = TRUE),
               
               # Print Zhang measure
               checkboxInput("zhangtab",
                             label = "Zhang",
                             value = TRUE),
               
               # Print Onnela measure
               checkboxInput("onnelatab",
                             label = "Onnela",
                             value = TRUE),
               
               # Print Barrat measure
               checkboxInput("barrattab",
                             label = "Barrat",
                             value = TRUE),
               
               tags$hr(),
               
               # Download clustering table
               downloadButton('downloadclusteringtable', 'Download Clustering Table'),
               br(),
               br()),
      
      tabPanel("Network Comparison", 
               br(),
               br(),
               
                fluidRow(
                 column(6,     
                        
                        h2("Dataset 1"),
               
                      
               
                # Upload dataset 1
               fileInput('input1', 'Choose CSV or TXT File',
                         accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv'))),
              
               column(6,
               
               h2("Dataset 2"),
               
               
               
               fileInput('input2', 'Choose CSV or TXT File',
                         accept=c('text/csv', 
                                  'text/comma-separated-values,text/plain', 
                                  '.csv'))),
               
               column(6, 
              
               # Contains the file a header? 
               checkboxInput('header1', 'Header', TRUE)),
               
               
               column(6, 
               checkboxInput('header2', 'Header', TRUE)),
               
               column(6,
                      # Are string to be coded as factor objects?
                      checkboxInput("stringfactors1", "Strings as factors", FALSE)),
               column(6, 
                      # Are string to be coded as factor objects?
                      checkboxInput("stringfactors2", "Strings as factors", FALSE)),
               column(6,
                      # Select separator
                      radioButtons('sep1', 'Separator',
                                   c(Comma=',',
                                     Semicolon=';',
                                     Tab='\t',
                                     Whitespace = ''),
                                   '\t')),
                      
               column(6, 
                      # Select separator
                      radioButtons('sep2', 'Separator',
                                   c(Comma=',',
                                     Semicolon=';',
                                     Tab='\t',
                                     Whitespace = ''),
                                   '\t')),
               column(6,
                      radioButtons('decimal1', "Decimal",
                                   c(Period = ".",
                                     Comma = ","),
                                   ".")),
               column(6,
                      
                      radioButtons('decimal2', "Decimal",
                                   c(Period = ".",
                                     Comma = ","),
                                   ".")),
               
               column(6,
                      # Select appropriate quotes
                      radioButtons('quote1', 'Quote',
                                   c(None='',
                                     'Double Quote'='"',
                                     'Single Quote'="'"),
                                   '"')),
                      
               
               column(6,
                      # Select appropriate quotes
                      radioButtons('quote2', 'Quote',
                                   c(None='',
                                     'Double Quote'='"',
                                     'Single Quote'="'"),
                                   '"')),
               
               column(6,
                      # Specify coding for NAs
                      textInput("missing1",
                                label = "Missing value coding:")),
                 
               column(6,
                      # Specify coding for NAs
                      textInput("missing2",
                                label = "Missing value coding:")),
               
               h2("Network Comparison Test Specifications"),
               
               br(),
               br(),

               
               column(6,
                      
                      # specify amount of iterations
                      numericInput("it",
                                   label = "Amount of iterations",
                                   value = 100)),
               
               column(6,
                      
                      # specify gamma
                      numericInput("gamma",
                                   label = "Gamma",
                                   value = 0)),
               
               # weighted network TRUE/FALSE
               column(6, 
                      checkboxInput('weightedNCT', 'Weighted', FALSE)),
               
               # binary data TRUE/FALSE
               
               column(6, 
                      checkboxInput('binary', 'Binary data', FALSE)),
               
               br(),
               br(),
               verbatimTextOutput("compnetwork")
               
        )
      )
    )
  )
)
)

