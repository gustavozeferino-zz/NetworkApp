setwd("~/Documents/UvA/Jaar 5 2014-2015/Semester 2/Programming The Next Step/Network App")

library(shiny)
library(qgraph)

qol <- read.delim("SF_36_NKI_HEALTHY.txt", 
                   na.strings = 0, 
                   header = TRUE)

shinyServer(
  function(input, output) {
    
    #visualize network
    output$network <- renderPlot({  
      data <- switch(input$method,
                     "Pearson Correlation" = cor(qol, method = "pearson"))
      
      lay <- switch(input$layout,
                       "Circle" = "circle",
                       "Spring" = "spring",
                       "Grouped Circle" = "groups")
      
      lab <- switch(input$node_labels,
                    "Yes" = names(data),
                    "No" = NULL)
      
      det <- switch(input$details,
                    "Yes" = TRUE,
                    "No" = FALSE)
      tit <- input$title      
      min <- input$minimum        
      max <- input$maximum        
      ct <- input$cut      
      es <- input$edgesize      
      ns <- input$nodesize
      weight <- switch(input$weighted,
                       "Yes" = TRUE,
                       "No" = FALSE)
      direct <- switch(input$direction,
                       "Yes" = TRUE,
                       "No" = FALSE)
      #visualize network
      q1 <- qgraph(data,
            layout = lay, 
            labels = lab,
            title = tit,
            minimum = min,
            maximum = max,
            cut = ct,
            details = det,
            esize = es,
            vsize = ns,
            weighted = weight,
            directed = direct,
            sampleSize = nrow(data))
   
      #download network image
      output$downloadnetwork <- downloadHandler(
        filename = function()
          {
         paste("Download", label = "network_image", class = ".pdf", sep = "") 
        },
        content = function(file) 
          {
          pdf(file)
          qgraph(data,
                 layout = lay, 
                 labels = lab,
                 title = tit,
                 minimum = min,
                 maximum = max,
                 cut = ct,
                 details = det,
                 esize = es,
                 vsize = ns,
                 weighted = weight,
                 directed = direct,
                 mar = c(2,2,2,2),
                 sampleSize = nrow(data))
          dev.off()
        })
    })
    
    #print centrality table
    output$centtable <- renderTable({
      data <- switch(input$method,
                     "Pearson Correlation" = cor(qol, method = "pearson"))
      q2 <- qgraph(data, DoNotPlot = TRUE)
      
      ct <- centrality_auto(q2)$node.centrality
      print(ct)
    })
    
    #visualizing centrality plot
    output$cent <- renderPlot({
      
      data <- switch(input$method,
                     "Pearson Correlation" = cor(qol, method = "pearson"))
      q2 <- qgraph(data, DoNotPlot = TRUE)
      
      c <- centralityPlot(q2)
      print(c)
      
      #download centrality plot
#       output$downloadcentrality <- downloadHandler(
#         filename = function()
#         {
#           paste("Download", label = "centrality_image", class = ".pdf", sep = "") 
#         },
#         content = function(file) 
#         {
#           pdf(file)
#           centralityPlot(q2)
#           dev.off()
#         })
    })
  }
)