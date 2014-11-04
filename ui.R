
library(shiny)
library(shinyapps)
library(qgraph)

data(big5)
data <- big5[,1:25]

q <- qgraph(cor(data), DoNotPlot = TRUE)

shinyUI(fluidPage(
  titlePanel("Network App"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Visualizing networks (now with only source data)."),
    
    textInput("title",
              label = "Title:"),
    
    selectInput("method",
                label = "Network estimation method:",
                choices = c("Pearson Correlation"),
                selected = "Pearson Correlation"),
    
    selectInput("layout",
                label = "Network layout:",
                choices = c("Circle", "Spring", "Grouped Circle"),
                selected = "Spring"),
    
    checkboxInput("node_labels",
                 label = "Node labels", 
                 value = TRUE),
    
    checkboxInput("weighted",
                 label = "Edge weights",
                 value = TRUE),
    
    checkboxInput("direction",
                 label = "Directed edges",
                 value = FALSE),
    
    checkboxInput("details",
                  label = "Graph details",
                  value = TRUE),
    
    sliderInput("minimum",
                label = "Minimum edge weight:",
                min = 0,
                max = 1,
                value = q$graphAttributes$Graph$minimum),
    
    sliderInput("maximum",
                label = "Maximum edge weight:",
                min = 0,
                max = 1,
                value = q$graphAttributes$Graph$maximum),
    
    sliderInput("cut",
                label = "Edge cut-off value",
                min = 0,
                max = 1,
                value = q$graphAttributes$Graph$cut),
    
    sliderInput("edgesize",
                label = "Edge size:",
                min = 0,
                max = 25,
                value = 1),
    
    sliderInput("nodesize",
                label = "Node size:",
                min = 0,
                max = 25,
                value = 6.1),
    
    downloadButton('downloadnetwork', 'Download PDF')),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Network Visualization", 
                 plotOutput("network"))
        
#         tabPanel("Centrality"),
#                 tableOutput("centtable"),
#                 plotOutput("centplot"),
#                 downloadButton('downloadcentrality', 'Download Centrality Image'))
#         tabPanel("Clustering"), 
#                 plotOutput("clus"),
#                 downloadButton("downloadclustering", "Download Clustering Image")
      #)  
    )  
  )
)))
