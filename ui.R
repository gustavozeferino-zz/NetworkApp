setwd("~/Documents/UvA/Jaar 5 2014-2015/Semester 2/Programming The Next Step/Network App")

library(shiny)
library(shinyapps)
library(qgraph)

qol <- read.delim("SF_36_NKI_HEALTHY.txt", 
                  na.strings = 0, 
                  header = TRUE)
q <- qgraph(cor(qol), DoNotPlot = TRUE)

shinyUI(fluidPage(
  titlePanel("Network App"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Visualizing networks (now with only source data)."),
    
    textInput("title",
              label = "Insert your title here:"),
    
    selectInput("method",
                label = "Choose a correlation method to estimate the network",
                choices = c("Pearson Correlation"),
                selected = "Pearson Correlation"),
    
    selectInput("layout",
                label = "What layout do you want?",
                choices = c("Circle", "Spring", "Grouped Circle"),
                selected = "Spring"),
    
    radioButtons("node_labels",
                 label = "Do you want node labels?",
                 choices = c("Yes", "No")),
    
    radioButtons("weighted",
                 label = "Do you want weighted edges?",
                 choices = c("Yes", "No")),
    
    radioButtons("direction",
                 label = "Do you want directed edges?",
                 choices = c("Yes", "No")),
    
    sliderInput("minimum",
                label = "Insert the minimum edge weight here:",
                min = 0,
                max = 1,
                value = q$graphAttributes$Graph$minimum),
    
    sliderInput("maximum",
                label = "Insert the maximum edge weight here:",
                min = 0,
                max = 1,
                value = q$graphAttributes$Graph$maximum),
    
    sliderInput("cut",
                label = "Insert a cut value for edge scaling here:",
                min = 0,
                max = 1,
                value = q$graphAttributes$Graph$cut),
    
    radioButtons("details",
                 label = "Do you want to display the graph details?",
                 choices = c("Yes", "No")),
    
    sliderInput("edgesize",
                label = "How large do you want the edges to be?",
                min = 0,
                max = 25,
                value = 1),
    
    sliderInput("nodesize",
                label = "How large do you want the edges to be?",
                min = 0,
                max = 25,
                value = 6.1),
    
    downloadButton('downloadnetwork', 'Download Network Image')),
    
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
