## Network App ##

library("shiny")
library("devtools")
library("ggplot2")
library("huge")
library("qgraph")


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
      
      file <- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote)
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

    est <- reactive({
      switch(input$method,
             "Pearson Correlation" = "cor",
             "Partial Correlation" = "pcor",
             "GLASSO" = "glasso")
    })

    norm <- reactive({
      if(input$normal == TRUE)
      {
        cor(huge.npn(data()), use = "pairwise.complete.obs")
      } else
      {
        cor(data(), use = "pairwise.complete.obs")
      }
    })
    graph <- reactive({
      qgraph(norm(),
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
             sampleSize = nrow(data()),
             graph = est())
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
    }, width = "auto", height = 500) #exit visualizing network 
    
    #download network image
    output$downloadnetwork <- downloadHandler(
      filename = function()
      {
        paste("network", class = ".pdf", sep = "") 
      },
      content = function(file) 
      {
        pdf(file)
        qgraph(norm(),
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
               sampleSize = nrow(data()),
               graph = est())
        dev.off()
      }) #exit download network plot
    
    # Set centrality measures 
    stren <- reactive({
      if(input$strength == TRUE)
      {
        "Strength"
      }
    })
    
    between <- reactive({
      if(input$betweenness == TRUE)
      {
        "Betweenness"
      }
    })
    
    close <- reactive({
      if(input$closeness == TRUE)
      {
        "Closeness"
      }
    })
    # Print centrality plot
    output$centplot <- renderPlot({
      
      # Plot centrality results
      cent <- centralityPlot(graph(), include = c(stren(), between(), close()))
      
      # Flip plot if chosen
      if(input$horizontal == TRUE)
      {
        print(cent + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1)) + coord_flip())
      } else
      {
        print(cent)
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
        if(input$horizontal == TRUE)
        {
          pdf(file)
          g <- centralityPlot(graph(), print = FALSE) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1)) + coord_flip()
          print(g)
          dev.off()
        } else
        {
          pdf(file)
          g <- centralityPlot(graph())
          print(g)
          dev.off()
        }

      }) #exit download centrality plot  
    
    centtable <- reactive({  
      ncol <- rep(FALSE, times = 8)
      
      ncol[2] = TRUE
      if(input$strength == TRUE)
      {
        ncol[8] = TRUE
      }
      if(input$betweenness == TRUE)
      {
        ncol[4] = TRUE
      }
      if(input$closeness == TRUE)
      {
        ncol[6] = TRUE
      }
     
      reshape(centralityTable(graph()), timevar = "measure",
              idvar = c("graph", "node"),
              direction = "wide")[, ncol]
      

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
    
    wes <- reactive({
      if(input$ws == TRUE)
      {
        "WS"
      }
    })
    
    zh <- reactive({
      if(input$zhang == TRUE)
      {
        "Zhang"
      }
    })
    
    onn <- reactive({
      if(input$onnela == TRUE)
      {
        "Onnela"
      }
    })
    
    bar <- reactive({
      if(input$barrat == TRUE)
      {
        "Barrat"
      }
      
    })
    # Print clustering plot
    output$clustplot <- renderPlot({
      
      # Plot centrality measures
      c <- clusteringPlot(graph(), include = c(wes(), zh(), onn(), bar()))
      
      # Flip plot if chosen
      if(input$horizontal == TRUE)
      {
        print(c + coord_flip() + theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)))
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
      ncol <- rep(FALSE, times = 10)
      
      ncol[2] = TRUE
      if(input$ws == TRUE)
      {
        ncol[4] = TRUE
      }
      if(input$zhang == TRUE)
      {
        ncol[6] = TRUE
      }
      if(input$onnela == TRUE)
      {
        ncol[8] = TRUE
      }
      if(input$barrat == TRUE)
      {
        ncol[10] = TRUE
      }
      reshape(clusteringTable(graph()), timevar = "measure",
              idvar = c("graph", "node"),
              direction = "wide")[, ncol]
    }) #exit clustering table (global variable)
    
    # Print clustering table
    output$clusttable <- renderTable({
      print(clusttable())
    }) #exit clustering table 
    
    output$downloadclusteringtable <- downloadHandler(            
      filename = function()
      {
        paste("clustering_table", class = ".csv", sep = "") 
      },            
      content = function(file) 
      {
        write.csv(clusttable(), file, row.names = FALSE)
      }) #exit download clustering table
  }) #exit shinyservey