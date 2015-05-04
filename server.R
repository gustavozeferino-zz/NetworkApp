## Network App ##

library("shiny")
library("devtools")
library("ggplot2")
library("huge")
library("qgraph")
library("psych")
library("pcalg")

data(bfi)
big5 <- bfi[,1:25]

shinyServer(
  function(input, output) {
    ##############################
    # Reactive variable defining #
    ##############################
    
    # Define global data with estimation 
    data <- reactive({
      
      inFile <- input$input
            
      if(input$demo == TRUE)
      {
        file <- big5
      } else 
      {
        if (is.null(inFile))
        {
          return(NULL)
        }
        
        # Code missing values
        na <- NULL
        if (input$missing == "NA")
        {
          na <- "NA"
        }
        else if (input$missing == FALSE)
        {
          na <- FALSE
        }
        else 
        {
          na <- as.numeric(input$missing)
        }
        
        file <- read.table(inFile$datapath, header=input$header, sep=input$sep, quote=input$quote, na.strings = na, stringsAsFactors = input$stringfactors, dec = input$decimal)
      }
      
      
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
    
    thres <- reactive({
      input$threshold
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
    
    past <- reactive({
      input$pastelcol
    })
    
    plotdiag <- reactive({
      input$diagonal
    })

    FDRmeth <- reactive({
      switch(input$FDRmethod,
             "local FDR" = "lfdr", 
             "p-value" = "pval",
             "q-value" = "qval")
    })
    
    VARfam <- reactive({
      switch(input$VARmethod,
             "Gaussian" = "gaussian",
             "Binary" = "binomial")
    })
  
    
    indeptest <- reactive({
      switch(input$pcindep,
             "Binary" = binCItest,
             "Discrete" = disCItest,
             "D-separation" = dsepTest,
             "Gaussian" = gaussCItest)
    })
    
    
    norm <- reactive({
      if(input$method == "FDRnetwork")
      {       
        if(input$normal == TRUE)
        {
          FDRnetwork(cor(huge.npn(data()), use = "pairwise.complete.obs"), cutoff = input$cutoffFDR, method = FDRmeth())
        } else  
        {
          FDRnetwork(cor(data(), use = "pairwise.complete.obs"), cutoff = input$cutoffFDR, method = FDRmeth())
        }  
      } else if(input$method == "VAR-model")
      {
        
        if(input$normal == TRUE)
        {
          VARglm(cor(huge.npn(data()), use = "pairwise.complete.obs"), family = VARfam())$graph 
        } else  
        {
          VARglm(cor(data(), use = "pairwise.complete.obs"), family = VARfam())$graph 
        }
      } else if(input$method == "IC-Algorithm: Skeleton")
      {
        if(input$normal == TRUE)
        {
          skeleton(suffStat = list(C = cor(huge.npn(data()), use = "pairwise.complete.obs"), n = nrow(data())), indepTest = indeptest(), alpha = 0.05, labels = colnames(data()))
        } else  
        {
          skeleton(suffStat = list(C = cor(data(), use = "pairwise.complete.obs"), n = nrow(data())), indepTest = indeptest(), alpha = 0.05, labels = colnames(data()))
        }  
      } else if(input$method == "IC-Algorithm: DAG")
      {
        if(input$normal == TRUE)
        {
          pc(suffStat = list(C = cor(huge.npn(data()), use = "pairwise.complete.obs"), n = nrow(data())), indepTest = indeptest(), alpha = 0.05, labels = colnames(data()))
        } else  
        {
          pc(suffStat = list(C = cor(data(), use = "pairwise.complete.obs"), n = nrow(data())), indepTest = indeptest(), alpha = 0.05, labels = colnames(data()))
        } 
      } else  
        {
          if(input$normal == TRUE)
          {
            cor(huge.npn(data()), use = "pairwise.complete.obs")
          } else  
          {
            cor(data(), use = "pairwise.complete.obs")
          }
        }
    })
    
    est <- reactive({
      if(!input$method == "FDRnetwork")
      {
        switch(input$method,
               "GLASSO" = "glasso",
               "Pearson Correlation" = "cor",
               "Partial Correlation" = "pcor") 
      }         
    })

    shapenode <- reactive({
      switch(input$nodeshape,
             "Circle" = "circle",
             "Diamond" = "diamond",
             "Heart" = "heart",
             "Square" = "square",
             "Triangle" = "triangle")
    })
      
    # Visualize network
    output$network <- renderPlot({       
      
      #visualize network
      if(is.null(data()))
      {
        return(NULL)
      } else if(input$sortdata == "Raw Data")
      {
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
                 graph = est(),
                 threshold = thres(),
                 shape = shapenode(),
                 diag = plotdiag())
      } else if(input$sortdata == "Adjacency Matrix")
      {
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
               threshold = thres(),
               shape = shapenode(),
               pastel = past(),
               diag = plotdiag())
      } else if(input$sortdata == "Edgelist")
      {
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
               threshold = thres(),
               shape = shapenode(),
               pastel = past(),
               diag = plotdiag())
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
       if(input$sortdata == "Raw Data")
      {
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
               graph = est(),
               threshold = thres(),
               shape = shapenode(),
               pastel = past(),
               diag = plotdiag())
      } else if(input$sortdata == "Adjacency Matrix")
      {
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
               threshold = thres(),
               shape = shapenode(),
               pastel = past(),
               diag = plotdiag())
      } else if(input$sortdata == "Edgelist")
      {
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
               threshold = thres(),
               shape = shapenode(),
               pastel = past(),
               diag = plotdiag())
      }
        dev.off()
      }) #exit download network plot
    
    # Download example dataset
    exampledata <- reactive({
      bfi[,1:25]
    })
    
    output$downloadexample <- downloadHandler(
      filename = function()
      {
        paste("bfi", class = ".csv", sep = "") 
      },
      content = function(file) 
      {
        write.csv(exampledata(), file)
      }) #exit download example dataset
    
    # Set centrality measures for plot
    stren <- reactive({
      if(input$sortdata == "Raw Data")
      {
        if(input$strength == TRUE)
        {
          "Strength"
        }
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
    
    indeg <- reactive({
      if(input$sortdata == "Adjacency Matrix" | input$sortdata == "Edgelist")
      {
        if(input$indegree == TRUE)
        {
          "InDegree"
        }
      }
    })
    
    outdeg <- reactive({
      if(input$sortdata == "Adjacency Matrix" | input$sortdata == "Edgelist")
      {
        if(input$outdegree == TRUE)
        {
          "OutDegree"
        }
      }
    })
    
    # Print centrality plot
    output$centplot <- renderPlot({
      
      # Plot centrality results
      if(input$sortdata == "Raw Data")
      {
        cent <- centralityPlot(qgraph(norm(),
               sampleSize = nrow(data()),
               graph = est(),
               weighted = weight(),
               directed = direct(), 
               DoNotPlot = TRUE), include = c(stren(), between(), close()))
      } else if(input$sortdata == "Adjacency Matrix")
      {
        cent <- centralityPlot(qgraph(data(),
                                      weighted = weight(),
                                      directed = direct(),
                                      DoNotPlot = TRUE), include = c(between(), close(), indeg(), outdeg()))
      } else if(input$sortdata == "Edgelist")
      {
        cent <- centralityPlot(qgraph(data(),
                                      weighted = weight(),
                                      directed = direct(),
                                      DoNotPlot = TRUE), include = c(between(), close(), indeg(), outdeg()))
      }
      
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
          if(input$sortdata == "Raw Data")
          {
            g <- centralityPlot(qgraph(norm(),
                                      sampleSize = nrow(data()),
                                       graph = est(),
                                       weighted = weight(),
                                       directed = direct(),
                                      DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg())) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1)) + coord_flip()
          } else if(input$sortdata == "Adjacency Matrix")
          {
            g <- centralityPlot(qgraph(data(), 
                                       weighted = weight(),
                                       directed = direct(),
                                       DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg())) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1)) + coord_flip()
          } else if(input$sortdata == "Edgelist")
          {
            g <- centralityPlot(qgraph(data(), 
                                       weighted = weight(),
                                       directed = direct(),
                                       DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg())) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1)) + coord_flip()
          }
          print(g)
          dev.off()
        } else
        {
          pdf(file)
          if(input$sortdata == "Raw Data")
          {
            g <- centralityPlot(qgraph(norm(),
                                       sampleSize = nrow(data()),
                                       graph = est(),
                                       weighted = weight(),
                                       directed = direct(),
                                       DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg())) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1))
          } else if(input$sortdata == "Adjacency Matrix")
          {
            g <- centralityPlot(qgraph(data(),
                                       weighted = weight(),
                                       directed = direct(),
                                       DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg())) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1))
          } else if(input$sortdata == "Edgelist")
          {
            g <- centralityPlot(qgraph(data(),
                                       weighted = weight(),
                                       directed = direct(),
                                       DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg())) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1))
          }
          print(g)
          dev.off()
        }

      }) #exit download centrality plot  
    
    # Set centrality measures for table
    strentab <- reactive({
      if(input$strengthtab == TRUE)
      {
        "Strength"
      }
    })
    
    betweentab <- reactive({
      if(input$betweennesstab == TRUE)
      {
        "Betweenness"
      }
    })
    
    closetab <- reactive({
      if(input$closenesstab == TRUE)
      {
        "Closeness"
      }
    })

    
    centtable <- reactive({  
      ncol <- rep(FALSE, times = 10)
      
      ncol[2] = TRUE
      if(input$sortdata == "Raw Data")
      {
        if(input$strengthtab == TRUE)
        {
          ncol[8] = TRUE
        }
      }
      if(input$betweennesstab == TRUE)
      {
        ncol[4] = TRUE
      }
      if(input$closenesstab == TRUE)
      {
        ncol[6] = TRUE
      }
      if(input$sortdata == "Adjacency Matrix" | input$sortdata == "Edgelist")
      {
        if(input$indegreetab == TRUE)
        {
          ncol[8] = TRUE
        }      
      }
      if(input$sortdata == "Adjacency Matrix" | input$sortdata == "Edgelist")
      {
        if(input$outdegreetab == TRUE)
        {
          ncol[10] = TRUE
        }
      }
      
      
      centtab <- reactive({
        if(input$sortdata == "Raw Data")
        {
          centralityTable(qgraph(norm(),
                                     sampleSize = nrow(data()),
                                     graph = est(),
                                     weighted = weight(),
                                     directed = direct(),
                                     DoNotPlot = TRUE))
        } else if(input$sortdata == "Adjacency Matrix")
        {
          centralityTable(qgraph(data(),
                                     weighted = weight(),
                                     directed = direct(),
                                     DoNotPlot = TRUE))
        } else if(input$sortdata == "Edgelist")
        {
          centralityTable(qgraph(data(),
                                     weighted = weight(),
                                     directed = direct(),
                                     DoNotPlot = TRUE))
        }
      })
     
      reshape(centtab(), timevar = "measure",
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
    
    # Set clustering measures for plot
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
      
      # Plot clustering measures    
      if(input$sortdata == "Raw Data")
      {
        c <- clusteringPlot(qgraph(norm(),
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
                                   graph = est(),
                                   DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()))
      } else if(input$sortdata == "Adjacency Matrix")
      {
        c <- clusteringPlot(qgraph(data(),
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
                                   DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()))
      } else if(input$sortdata == "Edgelist")
      {
        c <- clusteringPlot(qgraph(data(),
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
                                   DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()))
      }
      
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
        if(input$sortdata == "Raw Data")
        {
          clusteringPlot(qgraph(norm(),
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
                                     graph = est(),
                                     DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()))
        } else if(input$sortdata == "Adjacency Matrix")
        {
          clusteringPlot(qgraph(data(),
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
                                     DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()))
        } else if(input$sortdata == "Edgelist")
        {
          clusteringPlot(qgraph(data(),
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
                                     DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()))
        }
        dev.off()
      })     #exit download clustering plot  
    
    clusttable <- reactive({  
      ncol <- rep(FALSE, times = 10)
      
      ncol[2] = TRUE
      if(input$wstab == TRUE)
      {
        ncol[4] = TRUE
      }
      if(input$zhangtab == TRUE)
      {
        ncol[6] = TRUE
      }
      if(input$onnelatab == TRUE)
      {
        ncol[8] = TRUE
      }
      if(input$barrattab == TRUE)
      {
        ncol[10] = TRUE
      }
      
      clusttab <- reactive({
        if(input$sortdata == "Raw Data")
        {
          clusteringTable(qgraph(norm(),
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
                                 graph = est(),
                                 DoNotPlot = TRUE))
        } else if(input$sortdata == "Adjacency Matrix")
        {
          clusteringTable(qgraph(data(),
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
                                 DoNotPlot = TRUE))
        } else if(input$sortdata == "Edgelist")
        {
          clusteringTable(qgraph(data(),
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
                                 DoNotPlot = TRUE))
        }
      })
      
      
      reshape(clusttab(), timevar = "measure",
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
  }) #exit shinyserver

