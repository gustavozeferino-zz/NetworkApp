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

               # Are string to be coded as factor objects?
               checkboxInput("stringfactors", "Strings as factors", FALSE),
               
               tags$hr(),
               
               
               # Specify coding for NAs
               textInput("missing",
                         label = "Missing value coding:"),
               
               tags$hr(),
               
               # Download example data
               downloadButton('downloadexample', 'Download Example Data'),
  
               h5("Authors"),
               p("Jolanda Kossakowski <mail@jolandakossakowski.eu> & Sacha Epskamp & Claudia van Borkulo"),
               br(),
               p("If you want to run the application in R, please run the following R-code:"),
               tags$a(href = "https://raw.githubusercontent.com/JolandaKossakowski/NetworkApp/master/runapp.R", "R-code")),
  
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
               
               p("The Small-World Index of this network is"),
               verbatimTextOutput("swi"),
               br(),
               
               fluidRow(
                 column(4,
                        
                        # Download network as pdf
                        downloadButton('downloadnetwork', 'Download PDF')),
    
                 column(3,
                        
                        # Set threshold for edge weight
                        sliderInput("threshold",
                                    label = "Edge Threshold:",
                                    min = 0,
                                    max = 1,
                                    value = 0)),
               
                 column(3,
                        
                        # Use node labels TRUE/FALSE
                        checkboxInput("node_labels",
                                      label = "Node Labels", 
                                      value = TRUE))),
                 
               
               fluidRow(
                 column(4,
                        
                        # Insert title plot
                        textInput("title",
                                  label = "Title:")),
                 
                 column(3,
                        
                        # Select minimum value edge weights
                        sliderInput("minimum",
                                    label = "Minimum Edge Weight:",
                                    min = 0,
                                    max = 1,
                                    value = 0)),
                 
                 column(3,
                        
                        # Weighted graph TRUE/FALSE
                        checkboxInput("weighted",
                                      label = "Edge Weights",
                                      value = TRUE))),
               
               fluidRow(
                 column(4,
                        
                        # Choose network estimation method
                        selectInput("method",
                                    label = "Network Estimation Method:",
                                    choices = c("FDRnetwork",
                                                "GLASSO",
                                                "Graphical VAR: PCC",
                                                "Graphical VAR: PDC",
                                                "IC-Algorithm: DAG",
                                                "IC-Algorithm: Skeleton",
                                                "Partial Correlation",
                                                "Pearson Correlation",
                                                "VAR-model"),
                                    selected = "Partial Correlation")),
                 
                 column(3,
                        
                        # Select maximum value edge weights
                        sliderInput("maximum",
                                    label = "Maximum Edge Weight:",
                                    min = 0,
                                    max = 1,
                                    value = 1)),
                 
                 column(3,    
                        
                        # Directed edges TRUE/FALSE
                        checkboxInput("direction",
                                      label = "Directed Edges",
                                      value = FALSE))),
               
               fluidRow(
                 column(4,
                        
                        # Select network layout
                        selectInput("layout",
                                    label = "Network Layout:",
                                    choices = c("Circle", 
                                                "Spring"),
                                    selected = "Spring")),
                 
                 column(3,
                        
                        # Select cut-off value edge weights
                        sliderInput("cut",
                                    label = "Edge Cut-Off Value:",
                                    min = 0,
                                    max = 1,
                                    value = 0.1)),
                 
                 column(3,
                        
                        # Plot graph details TRUE/FALSE
                        checkboxInput("details",
                                      label = "Graph Details",
                                      value = FALSE))),
                 
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
                          # Select width edge
                          sliderInput("edgesize",
                                      label = "Edge Size:",
                                      min = 0,
                                      max = 25,
                                      value = 5)),
                   
                   column(3,
                          
                          # apply non-paranormal transformation TRUE/FALSE
                          checkboxInput("normal",
                                        label = "Non-Paranormal Transformation",
                                        value = FALSE))),
               
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
                        
                        # Select size of nodes
                        sliderInput("nodesize",
                                    label = "Node Size:",
                                    min = 0,
                                    max = 25,
                                    value = 6.1)),
                 
                 column(3,
                        
                        # Plot self-loops TRUE/FALSE
                        checkboxInput("diagonal",
                                      label = "Plot Self-Loops",
                                      value = FALSE))),
               
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
              
                
               fluidRow(
                        column(4,
                        # Visualize strength measure
                        checkboxInput("strength",
                                      label = "Strength",
                                      value = FALSE)),
                        
                        column(3,
                               # Flip centrality plots 90 degrees
                               checkboxInput("horizontal",
                                             label = "Flip plot", 
                                             value = FALSE))),
               
               fluidRow(
                 column(4,
                        # Visualize betweenness measure
                        checkboxInput("betweenness",
                                      label = "Betweenness",
                                      value = TRUE)),
                 
                 column(3,
                        # return standardized values
                        checkboxInput("standardizedcentplot",
                                      label = "Standardized",
                                      value = TRUE))),
               
               fluidRow(
                 column(4,
                        # Visualize closeness measure
                        checkboxInput("closeness",
                                      label = "Closeness",
                                      value = TRUE))),
               
               fluidRow(
                 column(4,
                        # Visualize indegree measure
                        # ONLY WITH ADJACENCY MATRIX OR EDGELIST
                        checkboxInput("indegree",
                                      label = "In Strength",
                                      value = FALSE))),
               
               fluidRow(
                 column(4,
                        # Visualize outdegree measure
                        # ONLY WITH ADJACENCY MATRIX OR EDGELIST
                        checkboxInput("outdegree",
                                      label = "Out Strength",
                                      value = FALSE))),
        
               tags$hr(),   
               
               # Download centrality plot
               downloadButton('downloadcentralityplot', 'Download Centrality Plot'),
               br(),
               br()),
      
      
      tabPanel("Centrality Table",
               br(),
               br(),
               tableOutput("centtable"),
               
               
               fluidRow(
                 column(4,
                        # Print strength measure
                        checkboxInput("strengthtab",
                                      label = "Strength",
                                      value = FALSE)),
                 column(3,
                        # return standardized values
                        checkboxInput("standardizedcenttab",
                                      label = "Standardized",
                                      value = TRUE))),
               
               fluidRow(
                 column(4,
                        # Print betweenness measure
                        checkboxInput("betweennesstab",
                                      label = "Betweenness",
                                      value = TRUE))),
               
               fluidRow(
                 column(4,
                        # Print closeness measure
                        checkboxInput("closenesstab",
                                      label = "Closeness",
                                      value = TRUE))),
               
               fluidRow(
                 column(4,
                        # Print indegree measure
                        # ONLY WITH ADJACENCY MATRIX OR EDGELIST
                        checkboxInput("indegreetab",
                                      label = "In Strength",
                                      value = FALSE))),
               
               fluidRow(
                 column(4,
                        # Print outdegree measure
                        # ONLY WITH ADJACENCY MATRIX OR EDGELIST
                        checkboxInput("outdegreetab",
                                      label = "Out Strength",
                                      value = FALSE))),
               
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
               
               fluidRow(
                 column(4,
                        # Visualize WS measure
                        checkboxInput("ws",
                                      label = "WS", 
                                      value = TRUE)),
                 
                 # Turn clustering plot 90 degrees
                 column(4,
                        checkboxInput("horizontal",
                               label = "Flip plot", 
                               value = FALSE))),
               
               fluidRow(
                 column(4,
                        # Visualize zhang measure
                        checkboxInput("zhang",
                                      label = "Zhang",
                                      value = TRUE)),
                 column(3,
                        # return standardized values
                        checkboxInput("standardizedclustplot",
                                      label = "Standardized",
                                      value = TRUE))),

               fluidRow(
                 column(4,
                        # Visualize Onnela measure
                        checkboxInput("onnela",
                                      label = "Onnela",
                                      value = TRUE))),
               
               fluidRow(
                 column(4,
                        # Visualize Barrat measure
                        checkboxInput("barrat",
                                      label = "Barrat",
                                      value = TRUE))),
               
               tags$hr(), 
               
               # Download clustering plot
               downloadButton('downloadclusteringplot', 'Download Clustering Plot')),
      
      tabPanel("Clustering Table",
               br(),
               br(),
               tableOutput("clusttable"),
               
               fluidRow(
                 column(4,
                        # Print WS measure
                        checkboxInput("wstab",
                                      label = "WS", 
                                      value = TRUE)),
                 
                 column(3,
                        # return standardized values
                        checkboxInput("standardizedclusttab",
                                      label = "Standardized",
                                      value = TRUE))),
               
               fluidRow(
                 column(4,
                        # Print Zhang measure
                        checkboxInput("zhangtab",
                                      label = "Zhang",
                                      value = TRUE))),
               
               fluidRow(
                 column(4,
                        # Print Onnela measure
                        checkboxInput("onnelatab",
                                      label = "Onnela",
                                      value = TRUE))),
               fluidRow(
                 column(4,
                        # Print Barrat measure
                        checkboxInput("barrattab",
                                      label = "Barrat",
                                      value = TRUE))),
               
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

