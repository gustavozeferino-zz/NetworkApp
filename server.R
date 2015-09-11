## Network App ##

library(shiny)
library(devtools)
library(ggplot2)
library(huge)
library(qgraph)
library(psych)
library(pcalg)
library(igraph)
library(graphicalVAR)

# example dataset for demo versions
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
        
        if(input$sortdata == "Adjacency Matrix")
        {
          file <- read.delim2(inFile$datapath, na.strings = na, stringsAsFactors = input$stringfactors)
        } else
        {
          file <- read.delim(inFile$datapath, na.strings = na, stringsAsFactors = input$stringfactors)
        }
      }
    }) #exit data defining
    
    # Use chosen layout
    lay <- reactive({
      switch(input$layout,
             "Circle" = "circle",
             "Spring" = "spring",
             "Grouped Circle" = "groups")
    })
    
    # Print node labels
    lab <- reactive({
      if(input$node_labels == TRUE)
      {
        names(data())
      } else
      {
        FALSE
      }     
    })
    
    # Print graph details
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
    
    # Use directed edges TRUE/FALSE
    direct <- reactive({
      if(input$direction == TRUE)
      {
        TRUE
      } else
      {
        FALSE
      }
    })    
    
    # Add a title
    tit <- reactive({
      input$title     
    })
    
    # Specify threshold value
    thres <- reactive({
      input$threshold
    })
    
    # Specify minimum edge weight
    min <- reactive({
      input$minimum 
    })
    
    # Specify maximum edge weight
    max <- reactive({
      input$maximum 
    })
    
    # Specify cut-off value
    ct <- reactive({
      input$cut})
    
    # Specify edge size
    es <- reactive({
      input$edgesize  
    })
    
    # Specify node size
    ns <- reactive({
      input$nodesize
    })  
    
    plotdiag <- reactive({
      input$diagonal
    })
    
    # Indicate method for FDR-network
    FDRmeth <- reactive({
      switch(input$FDRmethod,
             "local FDR" = "lfdr", 
             "p-value" = "pval",
             "q-value" = "qval")
    })
    
    # Indicate method for VAR-network
    VARfam <- reactive({
      switch(input$VARmethod,
             "Gaussian" = "gaussian",
             "Binary" = "binomial")
    })
  
    # indicate method for IC-algorithm
    indeptest <- reactive({
      switch(input$pcindep,
             "Binary" = binCItest,
             "Discrete" = disCItest,
             "D-separation" = dsepTest,
             "Gaussian" = gaussCItest)
    })
    
    # Apply non-paranormal transformation TRUE/FALSE in different networks
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
      } else if(input$method == "Graphical VAR: PCC" | input$method == "Graphical VAR: PDC")
      {
        if(input$normal == TRUE)
        {
          graphicalVAR(huge.npn(data()))
        } else
        {
          graphicalVAR(data())
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
    
    #  Estimation method network
    est <- reactive({
      if(!input$method == "FDRnetwork")
      {
        switch(input$method,
               "GLASSO" = "glasso",
               "Pearson Correlation" = "cor",
               "Partial Correlation" = "pcor") 
      }         
    })
    # Indicate nodeshape
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
      
      # Visualize raw data
      if(is.null(data()))
      {
        return(NULL)
      } else if(input$sortdata == "Raw Data" & input$method != "Graphical VAR: PDC" & input$method != "Graphical VAR: PCC")
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
        
        # Visualize adjacency matrix
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
               diag = plotdiag())
      } else if(input$sortdata == "Edgelist")
      {
        # Visualize edgelist
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
               diag = plotdiag())
      } else if(input$method == "Graphical VAR: PCC")
      {
        qgraph(norm()$PCC,
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
               diag = plotdiag())
      } else if(input$method == "Graphical VAR: PDC")
      {
        qgraph(norm()$PDC,
               layout = lay(), 
               labels = lab(),
               title = tit(),
               minimum = min(),
               maximum = max(),
               cut = ct(),
               details = det(),
               esize = es(),
               vsize = ns(),
               asize = es(),
               weighted = weight(),
               directed = direct(),
               threshold = thres(),
               shape = shapenode(),
               diag = plotdiag())
      }
    }, width = "auto", height = 500) #exit visualizing network 
    
    # Calculate small-world index
    
   SWI <- reactive({
     if(input$sortdata == "Raw Data" & input$method != "Graphical VAR: PDC" & input$method != "Graphical VAR: PCC")
     {
       as.numeric(smallworldness(qgraph(norm(),
                             graph = est(),
                             sampleSize = nrow(data()),
                             weighted = weight(),
                             directed = direct(),
                             threshold = thres(),
                             minimum = min(),
                             maximum = max(),
                             cut = ct(),
                             diag = plotdiag()))[1])
     } else if(input$sortdata == "Adjacency Matrix")
     {
       as.numeric(smallworldness(qgraph(data(),
                             weighted = weight(),
                             directed = direct(),
                             threshold = thres(),
                             minimum = min(),
                             maximum = max(),
                             cut = ct(),
                             diag = plotdiag()))[1])
     } else if(input$sortdata == "Edgelist")
     {
       as.numeric(smallworldness(qgraph(data(),
                             weighted = weight(),
                             directed = direct(),
                             threshold = thres(),
                             minimum = min(),
                             maximum = max(),
                             cut = ct(),
                             diag = plotdiag()))[1])
     } else if(input$method == "Graphical VAR: PCC" | input$method == "Graphical VAR: PDC")
     {
      "not applicable"
     } 
   })
   
   # display SWI
   
   output$swi <- renderPrint({
     SWI()
   })
    
    # Download network image
    output$downloadnetwork <- downloadHandler(
      filename = function()
      {
        paste("network", class = ".pdf", sep = "") 
      },
      content = function(file) 
      {
        pdf(file)
       if(input$sortdata == "Raw Data" & input$method != "Graphical VAR: PDC" & input$method != "Graphical VAR: PCC")
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
               diag = plotdiag())
      } else if(input$method == "Graphical VAR: PCC")
      {
        qgraph(norm()$PCC,
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
               diag = plotdiag())
      } else if(input$method == "Graphical VAR: PDC")
      {
        qgraph(norm()$PDC,
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
          "InStrength"
        }
      }
    })
    
    outdeg <- reactive({
      if(input$sortdata == "Adjacency Matrix" | input$sortdata == "Edgelist")
      {
        if(input$outdegree == TRUE)
        {
          "OutStrength"
        }
      }
    })
    
    #######################
    ### CENTRALITY PLOT ###
    #######################
    
    output$centplot <- renderPlot({
      
      # Plot centrality results
      if(input$sortdata == "Raw Data" & input$method != "Graphical VAR: PDC" & input$method != "Graphical VAR: PCC")
      {
        cent <- centralityPlot(qgraph(norm(),
               sampleSize = nrow(data()),
               graph = est(),
               weighted = weight(),
               directed = direct(),
               labels = lab(),
               DoNotPlot = TRUE), include = c(stren(), between(), close()), standardized = input$standardizedcentplot)
      } else if(input$sortdata == "Adjacency Matrix")
      {
        cent <- centralityPlot(qgraph(data(),
                                      weighted = weight(),
                                      directed = direct(),
                                      labels = lab(),
                                      DoNotPlot = TRUE), include = c(between(), close(), indeg(), outdeg()), standardized = input$standardizedcentplot)
      } else if(input$sortdata == "Edgelist")
      {
        cent <- centralityPlot(qgraph(data(),
                                      weighted = weight(),
                                      directed = direct(),
                                      DoNotPlot = TRUE), include = c(between(), close(), indeg(), outdeg()), standardized = input$standardizedcentplot)
      } else if(input$method == "Graphical VAR: PCC")
      {
        cent <- centralityPlot(qgraph(norm()$PCC,
                                      weighted = weight(),
                                      directed = direct(),
                                      labels = lab(),
                                      DoNotPlot = TRUE), include = c(between(), close(), indeg(), outdeg()), standardized = input$standardizedcentplot)
      } else if(input$method == "Graphical VAR: PDC")
      {
        cent <- centralityPlot(qgraph(norm()$PDC,
                                      weighted = weight(),
                                      directed = direct(),
                                      labels = lab(),
                                      DoNotPlot = TRUE), include = c(between(), close(), indeg(), outdeg()), standardized = input$standardizedcentplot)
      }
      
      # Flip plot if chosen
      if(input$horizontal == TRUE)
      {
        print(cent + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1)) + coord_flip())
      } else
      {
        print(cent + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1)))
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
          if(input$sortdata == "Raw Data" & input$method != "Graphical VAR: PDC" & input$method != "Graphical VAR: PCC")
          {
            g <- centralityPlot(qgraph(norm(),
                                      sampleSize = nrow(data()),
                                       graph = est(),
                                       weighted = weight(),
                                       directed = direct(),
                                      labels = lab(),
                                      DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg()), standardized = input$standardizedcentplot) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1)) + coord_flip()
          } else if(input$sortdata == "Adjacency Matrix")
          {
            g <- centralityPlot(qgraph(data(), 
                                       weighted = weight(),
                                       directed = direct(),
                                       labels = lab(),
                                       DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg()), standardized = input$standardizedcentplot) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1)) + coord_flip()
          } else if(input$sortdata == "Edgelist")
          {
            g <- centralityPlot(qgraph(data(), 
                                       weighted = weight(),
                                       directed = direct(),
                                       labels = lab(),
                                       DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg()), standardized = input$standardizedcentplot) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1)) + coord_flip()
          } else if(input$method == "Graphical VAR: PCC")
          {
            g <- centralityPlot(qgraph(norm()$PCC,
                                          weighted = weight(),
                                          directed = direct(),
                                       labels = lab(),
                                          DoNotPlot = TRUE), print = FALSE, include = c(between(), close(), indeg(), outdeg()), standardized = input$standardizedcentplot) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1)) + coord_flip()
          } else if(input$method == "Graphical VAR: PDC")
          {
            g <- centralityPlot(qgraph(norm()$PDC,
                                          weighted = weight(),
                                          directed = direct(),
                                       labels = lab(),
                                          DoNotPlot = TRUE), print = FALSE, include = c(between(), close(), indeg(), outdeg()), standardized = input$standardizedcentplot) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1)) + coord_flip()
          }
          print(g)
          dev.off()
        } else
        {
          pdf(file)
          if(input$sortdata == "Raw Data" & input$method != "Graphical VAR: PDC" & input$method != "Graphical VAR: PCC")
          {
            g <- centralityPlot(qgraph(norm(),
                                       sampleSize = nrow(data()),
                                       graph = est(),
                                       weighted = weight(),
                                       directed = direct(),
                                       labels = lab(),
                                       DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg()), standardized = input$standardizedcentplot) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1))
          } else if(input$sortdata == "Adjacency Matrix")
          {
            g <- centralityPlot(qgraph(data(),
                                       weighted = weight(),
                                       directed = direct(),
                                       labels = lab(),
                                       DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg()), standardized = input$standardizedcentplot) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1))
          } else if(input$sortdata == "Edgelist")
          {
            g <- centralityPlot(qgraph(data(),
                                       weighted = weight(),
                                       directed = direct(),
                                       labels = lab(),
                                       DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg()), standardized = input$standardizedcentplot) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1))
          } else if(input$method == "Graphical VAR: PCC")
          {
            g <- centralityPlot(qgraph(norm()$PCC,
                                       sampleSize = nrow(data()),
                                       graph = est(),
                                       weighted = weight(),
                                       directed = direct(),
                                       labels = lab(),
                                       DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg()), standardized = input$standardizedcentplot) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1))
          } else if(input$method == "Graphical VAR: PDC")
          {
            g <- centralityPlot(qgraph(norm()$PDC,
                                       sampleSize = nrow(data()),
                                       graph = est(),
                                       weighted = weight(),
                                       directed = direct(),
                                       labels = lab(),
                                       DoNotPlot = TRUE), print = FALSE, include = c(stren(), between(), close(), indeg()), standardized = input$standardizedcentplot) + theme(axis.text.x = element_text(size = 5, angle = 45, hjust = 1, vjust = 1))
          }
          print(g)
          dev.off()
        }
      }) #exit download centrality plot  
    
    ########################
    ### CENTRALITY TABLE ###
    ########################
    
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

    # Create centrality table
    centtable <- reactive({  
      ncol <- rep(FALSE, times = 10)
      
      ncol[2] = TRUE
      if(input$direction == FALSE)
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
      if(input$direction == TRUE)
      {
        if(input$indegreetab == TRUE)
        {
          ncol[8] = TRUE
        }      
      }
      if(input$direction == TRUE)
      {
        if(input$outdegreetab == TRUE)
        {
          ncol[10] = TRUE
        }
      }
      
      # Print centrality table
      centtab <- reactive({
        if(input$sortdata == "Raw Data" & input$method != "Graphical VAR: PDC" & input$method != "Graphical VAR: PCC")
        {
          centralityTable(qgraph(norm(),
                                     sampleSize = nrow(data()),
                                     graph = est(),
                                     weighted = weight(),
                                     directed = direct(),
                                 labels = lab(),
                                     DoNotPlot = TRUE), standardized = input$standardizedcenttab)
        } else if(input$sortdata == "Adjacency Matrix")
        {
          centralityTable(qgraph(data(),
                                     weighted = weight(),
                                     directed = direct(),
                                 labels = lab(),
                                     DoNotPlot = TRUE), standardized = input$standardizedcenttab)
        } else if(input$sortdata == "Edgelist")
        {
          centralityTable(qgraph(data(),
                                     weighted = weight(),
                                     directed = direct(),
                                 labels = lab(),
                                     DoNotPlot = TRUE), standardized = input$standardizedcenttab)
          } else if(input$method == "Graphical VAR: PCC")
          {
            centralityTable(qgraph(norm()$PCC,
                                   sampleSize = nrow(data()),
                                   weighted = weight(),
                                   directed = direct(),
                                   labels = lab(),
                                   DoNotPlot = TRUE), standardized = input$standardizedcenttab)
          } else if(input$method == "Graphical VAR: PDC")
          {
            centralityTable(qgraph(norm()$PDC,
                                   sampleSize = nrow(data()),
                                   weighted = weight(),
                                   directed = direct(),
                                   labels = lab(),
                                   DoNotPlot = TRUE), standardized = input$standardizedcenttab)
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
    
    # Download centrality table
    output$downloadcentralitytable <- downloadHandler(            
      filename = function()
      {
        paste("centrality_table", class = ".csv", sep = "") 
      },            
      content = function(file) 
      {
        write.csv(centtable(), file, row.names = FALSE)
      }) #exit download centrality table
    
    # Print Max Table
    
    output$maxcenttable <- renderTable({
      print()
    })
    
    #######################
    ### CLUSTERING PLOT ###
    #######################
    
    
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
      if(input$sortdata == "Raw Data" & input$method != "Graphical VAR: PDC" & input$method != "Graphical VAR: PCC")
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
                                   directed = FALSE,
                                   sampleSize = nrow(data()),
                                   graph = est(),
                                   DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()), standardized = input$standardizedclustplot)
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
                                   directed = FALSE,
                                   DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()), standardized = input$standardizedclustplot)
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
                                   directed = FALSE,
                                   DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()), standardized = input$standardizedclustplot)
      } else if(input$method == "Graphical VAR: PCC")
      {
        c <- clusteringPlot(qgraph(norm()$PCC,
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
                                   directed = FALSE,
                                   sampleSize = nrow(data()),
                                   graph = est(),
                                   DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()), standardized = input$standardizedclustplot)
      } else if(input$method == "Graphical VAR: PDC")
      {
        c <- clusteringPlot(qgraph(norm()$PDC,
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
                                   directed = FALSE,
                                   sampleSize = nrow(data()),
                                   graph = est(),
                                   DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()), standardized = input$standardizedclustplot)
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
        if(input$sortdata == "Raw Data" & input$method != "Graphical VAR: PDC" & input$method != "Graphical VAR: PCC")
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
                                     directed = FALSE,
                                     sampleSize = nrow(data()),
                                     graph = est(),
                                     DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()), standardized = input$standardizedclustplot)
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
                                     directed = FALSE,
                                     DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()), standardized = input$standardizedclustplot)
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
                                     directed = FALSE,
                                     DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()), standardized = input$standardizedclustplot)
        } else if(input$method == "Graphical VAR: PCC")
        {
          clusteringPlot(qgraph(norm()$PCC,
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
                                directed = FALSE,
                                sampleSize = nrow(data()),
                                graph = est(),
                                DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()), standardized = input$standardizedclustplot)
        } else if(input$method == "Graphical VAR: PCC")
        {
          clusteringPlot(qgraph(norm()$PDC,
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
                                directed = FALSE,
                                sampleSize = nrow(data()),
                                graph = est(),
                                DoNotPlot = TRUE), include = c(wes(), zh(), onn(), bar()), standardized = input$standardizedclustplot)
        }
        dev.off()
      })     #exit download clustering plot  
    
    ########################
    ### CLUSTERING TABLE ###
    ########################
    
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
      
      # Print clustering table
      clusttab <- reactive({
        if(input$sortdata == "Raw Data" & input$method != "Graphical VAR: PDC" & input$method != "Graphical VAR: PCC")
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
                                 directed = FALSE,
                                 sampleSize = nrow(data()),
                                 graph = est(),
                                 DoNotPlot = TRUE), standardized = input$standardizedclusttab)
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
                                 directed = FALSE,
                                 DoNotPlot = TRUE), standardized = input$standardizedclusttab)
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
                                 directed = FALSE,
                                 DoNotPlot = TRUE), standardized = input$standardizedclusttab)
        } else if(input$method == "Graphical VAR: PCC")
        {
          clusteringTable(qgraph(norm()$PCC,
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
                                 directed = FALSE,
                                 sampleSize = nrow(data()),
                                 graph = est(),
                                 DoNotPlot = TRUE), standardized = input$standardizedclusttab)
        } else if(input$method == "Graphical VAR: PDC")
        {
          clusteringTable(qgraph(norm()$PDC,
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
                                 directed = FALSE,
                                 sampleSize = nrow(data()),
                                 graph = est(),
                                 DoNotPlot = TRUE), standardized = input$standardizedclusttab)
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
    
    # Download clustering table
    output$downloadclusteringtable <- downloadHandler(            
      filename = function()
      {
        paste("clustering_table", class = ".csv", sep = "") 
      },            
      content = function(file) 
      {
        write.csv(clusttable(), file, row.names = FALSE)
      }) #exit download clustering table
    
    ##########################
    ### Network Comparison ###
    ##########################
    
    # Dataset 1
    data1 <- reactive({
      
      inFile <- input$input1
      
      if (is.null(inFile))
      {
        return(NULL)
      }
      
      # Code missing values
      na <- NULL
      if (input$missing1 == "NA")
      {
        na <- "NA"
      }
      else if (input$missing1 == FALSE)
      {
        na <- FALSE
      }
      else 
      {
        na <- as.numeric(input$missing1)
      }
      
      file <- read.table(inFile$datapath, header=input$header1, sep=input$sep1, quote=input$quote1, na.strings = na, stringsAsFactors = input$stringfactors1, dec = input$decimal1)
    })
    
    
    # Dataset 2
    data2 <- reactive({
      
      inFile <- input$input2
      
  if (is.null(inFile))
  {
    return(NULL)
  }
  
  # Code missing values
  na <- NULL
  if (input$missing2 == "NA")
  {
    na <- "NA"
  }
  else if (input$missing2 == FALSE)
  {
    na <- FALSE
  }
  else 
  {
    na <- as.numeric(input$missing2)
  }
  
  file <- read.table(inFile$datapath, header=input$header2, sep=input$sep2, quote=input$quote2, na.strings = na, stringsAsFactors = input$stringfactors2, dec = input$decimal2)
  })

itt <- reactive({
  input$it
})

gamm <- reactive({
  input$gamma
})

weightNCT <- reactive({
  input$weightedNCT
})

binarydata <- reactive({
  input$binary
})

# NCT function #

NCT <- function(data1, data2, gamma, it, binary.data, weighted=TRUE, AND=TRUE, 
                progressbar=TRUE, ...){ 
  
  if (progressbar==TRUE) pb <- txtProgressBar(max=it, style = 3)
  x1 <- data1
  x2 <- data2
  nobs1 <- nrow(x1)
  nobs2 <- nrow(x2)
  dataall <- rbind(x1,x2)
  b <- 1:(nobs1+nobs2)  
  
  ### procedure for non-binary data
  if(binary.data==FALSE) {
    ## test at single gamma
    if(is.numeric(gamma)){
      diffperm <- diffreal <- c()
      # real data
      res1 <- EBICglasso(cor(x1, use = "complete.obs"),nrow(x1),gamma=gamma)
      res2 <- EBICglasso(cor(x2, use = "complete.obs"),nrow(x2),gamma=gamma)
      if(weighted==FALSE){
        res1=(res1!=0)*1
        res2=(res2!=0)*1
      }
      diffreal <- abs(sum(abs(res1[upper.tri(res1)]))-sum(abs(res2[upper.tri(res2)])))
      # permuted data
      for (i in 1:it){
        s <- sample(1:(nobs1+nobs2),nobs1,replace=FALSE)
        x1perm <- dataall[s,]
        x2perm <- dataall[b[-s],]
        r1perm <- EBICglasso(cor(x1perm, use = "complete.obs"),nrow(x1perm),gamma=gamma)
        r2perm <- EBICglasso(cor(x2perm, use = "complete.obs"),nrow(x2perm),gamma=gamma)
        if(weighted==FALSE){
          r1perm=(r1perm!=0)*1
          r2perm=(r2perm!=0)*1
        }
        diffperm[i] <- abs(sum(abs(r1perm[upper.tri(r1perm)]))-sum(abs(r2perm[upper.tri(r2perm)])))
        if (progressbar==TRUE) setTxtProgressBar(pb, i)
      }
    }
    ## test across whole range of gamma's (0-1)
    if(is.character(gamma)){
      gseq <- seq(0,1,by=.1)
      diffreal <- diffperm <- c()
      diffrealtemp <- matrix(NA,length(gseq),2)
      diffpermtemp <- matrix(NA,it,length(gseq))
      # real data
      for(k in 1:length(gseq)){
        gamma=gseq[k]
        res1 <- EBICglasso(cor(x1, use = "complete.obs"),nrow(x1),gamma=gamma)
        res2 <- EBICglasso(cor(x2, use = "complete.obs"),nrow(x2),gamma=gamma)
        if(weighted==FALSE){
          res1=(res1!=0)*1
          res2=(res2!=0)*1
        }
        diffrealtemp[k,] <- c(sum(abs(res1[upper.tri(res1)])),sum(abs(res2[upper.tri(res2)])))
      }
      diffreal <- sum(abs(diffrealtemp[,1]-diffrealtemp[,2]))
      # permuted data
      for (i in 1:it){
        s <- sample(1:(nobs1+nobs2),nobs1,replace=FALSE)
        x1perm <- dataall[s,]
        x2perm <- dataall[b[-s],]
        for(j in 1:length(gseq)){
          gamma=gseq[j]
          r1perm <- EBICglasso(cor(x1perm, use = "complete.obs"),nrow(x1perm),gamma=gamma)
          r2perm <- EBICglasso(cor(x2perm, use = "complete.obs"),nrow(x2perm),gamma=gamma)
          if(weighted==FALSE){
            r1perm=(r1perm!=0)*1
            r2perm=(r2perm!=0)*1
          }
          # make this suited for plotting area under curve
          temp1 <- sum(abs(r1perm[upper.tri(r1perm)]))
          temp2 <- sum(abs(r2perm[upper.tri(r2perm)]))
          diffpermtemp[i,j] <- abs(temp1-temp2)
        }
        if (progressbar==TRUE) setTxtProgressBar(pb, i)
      }
      diffperm <- abs(rowSums(diffpermtemp))
    }
    if (progressbar==TRUE) close(pb)
    res <- list(diffreal = diffreal, 
                diffperm = diffperm,
                pval = sum(diffperm >= diffreal)/it)
  }
  
  ### procedure for binary data
  if(binary.data==TRUE) {
    ## test at single gamma
    if(is.numeric(gamma)){
      diffperm <- diffreal <- c()
      # real data
      IF1 <- IsingFit(x1,AND = AND, gamma=gamma,plot=FALSE,progressbar=FALSE)
      IF2 <- IsingFit(x2,AND = AND, gamma=gamma,plot=FALSE,progressbar=FALSE)
      res1 <- IF1$weiadj
      res2 <- IF2$weiadj
      if(weighted==FALSE){
        res1=(res1!=0)*1
        res2=(res2!=0)*1
      }
      diffreal <- abs(sum(abs(res1[upper.tri(res1)]))-sum(abs(res2[upper.tri(res2)])))
      # permuted data
      for (i in 1:it){
        s <- sample(1:(nobs1+nobs2),nobs1,replace=FALSE)
        x1perm <- dataall[s,]
        x2perm <- dataall[b[-s],]
        IF1perm <- IsingFit(x1perm,AND = AND, gamma=gamma,plot=FALSE,progressbar=FALSE)
        IF2perm <- IsingFit(x2perm,AND = AND, gamma=gamma,plot=FALSE,progressbar=FALSE)
        r1perm <- IF1perm$weiadj
        r2perm <- IF2perm$weiadj
        if(weighted==FALSE){
          r1perm=(r1perm!=0)*1
          r2perm=(r2perm!=0)*1
        }
        diffperm[i] <- abs(sum(abs(r1perm[upper.tri(r1perm)]))-sum(abs(r2perm[upper.tri(r2perm)])))
        if (progressbar==TRUE) setTxtProgressBar(pb, i)
      }
    }
    ## test across whole range of gamma's (0-1)
    if(is.character(gamma)){
      gseq <- seq(0,1,by=.1)
      diffreal <- diffperm <- c()
      diffrealtemp <- matrix(NA,length(gseq),2)
      diffpermtemp <- matrix(NA,it,length(gseq))
      # real data
      for(k in 1:length(gseq)){
        gamma=gseq[k]
        IF1 <- IsingFit(x1,AND = AND, gamma=gamma,plot=FALSE,progressbar=FALSE)
        IF2 <- IsingFit(x2,AND = AND, gamma=gamma,plot=FALSE,progressbar=FALSE)
        res1 <- IF1$weiadj
        res2 <- IF2$weiadj
        if(weighted==FALSE){
          res1=(res1!=0)*1
          res2=(res2!=0)*1
        }
        diffrealtemp[k,] <- c(sum(abs(res1[upper.tri(res1)])),sum(abs(res2[upper.tri(res2)])))
      }
      diffreal <- sum(abs(diffrealtemp[,1]-diffrealtemp[,2]))
      # permuted data
      for (i in 1:it){
        s <- sample(1:(nobs1+nobs2),nobs1,replace=FALSE)
        x1perm <- dataall[s,]
        x2perm <- dataall[b[-s],]
        for(j in 1:length(gseq)){
          gamma=gseq[j]
          IF1perm <- IsingFit(x1perm,AND = AND, gamma=gamma,plot=FALSE,progressbar=FALSE)
          IF2perm <- IsingFit(x2perm,AND = AND, gamma=gamma,plot=FALSE,progressbar=FALSE)
          r1perm <- IF1perm$weiadj
          r2perm <- IF2perm$weiadj
          if(weighted==FALSE){
            r1perm=(r1perm!=0)*1
            r2perm=(r2perm!=0)*1
          }
          # make this suited for plotting area under curve
          temp1 <- sum(abs(r1perm[upper.tri(r1perm)]))
          temp2 <- sum(abs(r2perm[upper.tri(r2perm)]))
          diffpermtemp[i,j] <- abs(temp1-temp2)
        }
        if (progressbar==TRUE) setTxtProgressBar(pb, i)
      }
      diffperm <- abs(rowSums(diffpermtemp))
    }
    if (progressbar==TRUE) close(pb)
    res <- list(diffreal = diffreal, 
                diffperm = diffperm,
                pval = sum(diffperm >= diffreal)/it)
  }
  class(res) <- "NCT"
  return(res)
}

output$compnetwork <- renderPrint({
print("p-value")
NCT(data1(), data2(), gamma = gamm(), it = itt(), weighted = weightNCT(), binary = binarydata(), progressbar=FALSE)$pval
})


}) #exit shinyserver

