## Network App ##

library(shiny)
library(shinyapps)
library(qgraph)

shinyUI(pageWithSidebar(
  titlePanel("Network App"),
  sidebarPanel(
    # Upload file
    fileInput('file1', 'Choose CSV File',
              accept=c('text/csv', 
                       'text/comma-separated-values,text/plain', 
                       '.csv')),
    
    tags$hr(),
    # Contains the file a header? 
    checkboxInput('header', 'Header', TRUE),
    
    # Select separator
    radioButtons('sep', 'Separator',
                 c(Comma=',',
                   Semicolon=';',
                   Tab='\t'),
                 '\t'),
    
    # Select appropriate quotes
    radioButtons('quote', 'Quote',
                 c(None='',
                   'Double Quote'='"',
                   'Single Quote'="'"),
                 '"'),
    
    tags$hr(),
    
    # Insert title plot
    textInput("title",
              label = "Title:"),
    
    # Choose network estimation method
    selectInput("method",
                label = "Network estimation method:",
                choices = c("Pearson Correlation"),
                selected = "Pearson Correlation"),
    
    # Select network layout
    selectInput("layout",
                label = "Network layout:",
                choices = c("Circle", "Spring", "Grouped Circle"),
                selected = "Spring"),
    
    # Use node labels TRUE/FALSE
    checkboxInput("node_labels",
                  label = "Node labels", 
                  value = TRUE),
    
    # Weighted graph TRUE/FALSE
    checkboxInput("weighted",
                  label = "Edge weights",
                  value = TRUE),
    
    # Directed edges TRUE/FALSE
    checkboxInput("direction",
                  label = "Directed edges",
                  value = FALSE),
    
    # Plot graph details TRUE/FALSE
    checkboxInput("details",
                  label = "Graph details",
                  value = TRUE),
    
    # Select minimum value edge weights
    sliderInput("minimum",
                label = "Minimum edge weight:",
                min = 0,
                max = 1,
                value = 0),
    
    # Select maximum value edge weights
    sliderInput("maximum",
                label = "Maximum edge weight:",
                min = 0,
                max = 1,
                value = 1),
    
    # Select cut-off value edge weights
    sliderInput("cut",
                label = "Edge cut-off value",
                min = 0,
                max = 1,
                value = 0.6),
    
    # Select width edge
    sliderInput("edgesize",
                label = "Edge size:",
                min = 0,
                max = 25,
                value = 1),
    
    # Select size of nodes
    sliderInput("nodesize",
                label = "Node size:",
                min = 0,
                max = 25,
                value = 6.1),
    tags$hr(),
    
    # Download network as pdf
    downloadButton('downloadnetwork', 'Download PDF')
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Network Visualization", 
               plotOutput("network")),
      
      tabPanel("Centrality", 
               plotOutput("centplot"),
               tableOutput("centtable"),
               
              tags$hr(),
                 
              checkboxInput("horizontal",
                             label = "Flip plot", 
                            value = FALSE),
              tags$hr(),         
              downloadButton('downloadcentralityplot', 'Download Centrality Plot'),
              tags$hr(),
              downloadButton('downloadcentralitytable', 'Download Centrality Table')
        )  
      )
    )  
  )
)


