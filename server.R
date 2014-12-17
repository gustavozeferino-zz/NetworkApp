## Network App ##

library(shiny)
library(shinyapps)
library(qgraph)
library(ggplot2)

shinyServer(
  function(input, output) {
    ##############################
    # Reactive variable defining #
    ##############################
    
    # Define global data with estimation 
    data <- reactive({
      
      inFile <- input$input
      
      if (is.null(inFile))
      {
        return(NULL)
      }
      
      file <- read.csv(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote) 
      switch(input$method,
             "Pearson Correlation" = cor(file, method = "pearson")) 
    }) #exit data defining
    
    # Use chosen layout
    lay <- reactive({
      switch(input$layout,
             "Circle" = "circle",
             "Spring" = "spring",
             "Grouped Circle" = "groups")
    })
    
    lab <- reactive({
      if(input$node_labels == TRUE)
      {
        names(data())
      } else
      {
        FALSE
      }     
    })
    
    det <- reactive({
      if(input$details == TRUE)
      {
        TRUE
      } else
      {
        FALSE
      }
    })
    
    weight <- reactive ({
      if(input$weighted == TRUE)
      {
        TRUE
      } else
      {
        FALSE
      }
    })
    
    direct <- reactive({
      if(input$direction == TRUE)
      {
        TRUE
      } else
      {
        FALSE
      }
    })    
    
    tit <- reactive({
      input$title     
    })
    
    min <- reactive({
      input$minimum 
    })
    
    max <- reactive({
      input$maximum 
    })
    
    ct <- reactive({
      input$cut})
    
    es <- reactive({
      input$edgesize  
    })
    
    ns <- reactive({
      input$nodesize
    })   
    
    graph <- reactive({
      qgraph(data(),
             layout = lay(), 
             labels = lab(),
             title = tit(),
             minimum = min(),
             maximum = max(),
             cut = ct(),
             details = det(),
             esize = es(),
             vsize = ns(),
             weighted = weight(),
             directed = direct(),
             sampleSize = nrow(data()))
    })
    
    # Visualize network
    output$network <- renderPlot({       
      
      #visualize network
      if(is.null(data()))
      {
        return(NULL)
      } else
      {
        graph()
      }
    }) #exit visualizing network 
    
    #download network image
    output$downloadnetwork <- downloadHandler(
      filename = function()
      {
        paste("network", class = ".pdf", sep = "") 
      },
      content = function(file) 
      {
        pdf(file)
        qgraph(data(),
               layout = lay(), 
               labels = lab(),
               title = tit(),
               minimum = min(),
               maximum = max(),
               cut = ct(),
               details = det(),
               esize = es(),
               vsize = ns(),
               weighted = weight(),
               directed = direct(),
               sampleSize = nrow(data()))
        dev.off()
      }) #exit download network plot
    
    # Print centrality plot
    output$centplot <- renderPlot({
      
      # Plot centrality measures
      c <- centralityPlot(graph())
      
      # Flip plot if chosen
      if(input$horizontal == TRUE)
      {
        print(c + coord_flip() + theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 10), legend.text = element_text(size = 15), legend.title = element_text(size = 15), text = element_text(size = 20)))
      } else
      {
        print(c)
      }
    }) # exit centrality plot  
    
    # Download centrality plot
    output$downloadcentralityplot <- downloadHandler(
      
      filename = function()
      {
        paste("centrality_plot", class = ".pdf", sep = "") 
      },
      content = function(file) 
      {
        pdf(file)
        centralityPlot(graph())
        dev.off()
      })     #exit download centrality plot  
    
    centtable <- reactive({      
      reshape(centralityTable(graph()), timevar = "measure",
              idvar = c("graph", "node"),
              direction = "wide")[, c(2, 4, 6, 8)]
    }) #exit centrality table (global variable)
    
    # Print centrality table
    output$centtable <- renderTable({
      print(centtable())
    }) #exit centrality table 
    
    output$downloadcentralitytable <- downloadHandler(            
      filename = function()
      {
        paste("centrality_table", class = ".csv", sep = "") 
      },            
      content = function(file) 
      {
        write.csv(centtable(), file, row.names = FALSE)
      }) #exit download centrality table
    
    # Print clustering plot
    output$clustplot <- renderPlot({
      
      # Plot centrality measures
      c <- clusteringPlot(graph())
      
      # Flip plot if chosen
      if(input$horizontal == TRUE)
      {
        print(c + coord_flip() + theme(axis.text.x = element_text(size = 15, angle = 45, hjust = 1, vjust = 1), axis.text.y = element_text(size = 10), legend.text = element_text(size = 15), legend.title = element_text(size = 15), text = element_text(size = 20)))
      } else
      {
        print(c)
      }
    }) # exit centrality plot  
    
    # Download clustering plot
    output$downloadclusteringplot <- downloadHandler(
      
      filename = function()
      {
        paste("clustering_plot", class = ".pdf", sep = "") 
      },
      content = function(file) 
      {
        pdf(file)
        clusteringPlot(graph())
        dev.off()
      })     #exit download clustering plot  
    
    clusttable <- reactive({      
      reshape(clusteringTable(graph()), timevar = "measure",
              idvar = c("graph", "node"),
              direction = "wide")[, c(2, 4, 6, 8, 10)]
    }) #exit clustering table (global variable)
    
    # Print clustering table
    output$clusttable <- renderTable({
      print(clusttable())
    }) #exit clustering table 
    
    output$downloadcentralitytable <- downloadHandler(            
      filename = function()
      {
        paste("clustering_table", class = ".csv", sep = "") 
      },            
      content = function(file) 
      {
        write.csv(clusttable(), file, row.names = FALSE)
      }) #exit download clustering table
  }) #exit shinyservey