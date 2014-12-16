## Network App ##

library(shiny)
library(shinyapps)
library(qgraph)
library(ggplot2)

shinyServer(
  function(input, output) {
    
    
    # Visualize network
    output$network <- renderPlot({
      
      # Read input file
      inFile <- input$file1
      
      if (is.null(inFile))
        return(NULL)
      
      data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      
      # Apply chosen estimation method
      data <- switch(input$method,
                     "Pearson Correlation" = cor(data, method = "pearson"))
      
      # Use chosen layout
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

#     # Print centrality plot
    output$centplot <- renderPlot({
      
      # Read input file
      inFile <- input$file1
      
      if (is.null(inFile))
      {
        return(NULL)
      }         
      
      data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
      
      # Apply chosen estimation method
      data <- switch(input$method,
                     "Pearson Correlation" = cor(data, method = "pearson"))  
      
      q2 <- qgraph(data, DoNotPlot = TRUE)
      
      # Plot centrality measures
      c <- centralityPlot(q2)
      
      # Flip plot if chosen
      if(input$horizontal == TRUE)
      {
        print(c + coord_flip())
      } else
      {
        print(c)
      }
    
      # Download centrality plot
      output$downloadcentralityplot <- downloadHandler(
                
        filename = function()
        {
          paste("centrality_plot", class = ".pdf", sep = "") 
        },
        content = function(file) 
        {
          pdf(file)
          centralityPlot(q2)
          dev.off()
        })    
   })  

        # Print centrality table
        output$centtable <- renderTable({
          
          # Read input file
          inFile <- input$file1
          
          if (is.null(inFile))
          {
            return(NULL)
          }         
          
          data <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
          
          # Apply chosen estimation method
          data <- switch(input$method,
                         "Pearson Correlation" = cor(data, method = "pearson"))       
          q2 <- qgraph(data, DoNotPlot = TRUE)
          
            # Compute centrality table
            t <- centralityTable(q2)
            t <- reshape(t, timevar = "measure",
                        idvar = c("graph", "node"),
                        direction = "wide")
            t <- t[, c(2, 4, 6, 8)]
            colnames(t) <- c("Node", "Betweenness", "Closeness", "Strength")
            return(t)
        })

          # Download centrality table
          output$downloadcentralitytable <- downloadHandler(            
            filename = function()
            {
              paste("centrality_table", class = ".csv", sep = "") 
            },            
            content = function(file) 
            {
              write.csv(t, file, row.names = FALSE)
            })       
       
   })