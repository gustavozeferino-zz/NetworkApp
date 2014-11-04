library(shiny)
library(qgraph)

qol <- read.delim("http://www.jolandakossakowski.eu/wp-content/uploads/2014/11/SF_36_NKI_HEALTHY.txt", 
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
    
      lab = NULL
     if(input$node_labels == TRUE)
      {
        lab <- names(data)
      } else
      {
        lab = FALSE
      }
      
    det <- NULL
     if(input$details == TRUE)
     {
       det = TRUE
     } else
     {
       det = FALSE
     }
     
      weight <- NULL
     if(input$weighted == TRUE)
     {
       weight = TRUE
     } else
     {
       weight = FALSE
     }
     
      direct <- NULL
     if(input$direction == TRUE)
     {
       direct = TRUE
     } else
     {
       direct = FALSE
     }
      tit <- input$title      
      min <- input$minimum        
      max <- input$maximum        
      ct <- input$cut      
      es <- input$edgesize      
      ns <- input$nodesize

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