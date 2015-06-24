## READ ME for Shiny Network Application

This Shiny application visualizes a graph structure for a dataset. The user can adjust all features of the visualized graph and download a pdf version of it. Furthermore, centrality analyses will automatically be performed; a centrality table and plot will be visualized in the second and third tab and both can be downloaded. The fourth and fifth tab visualizes the Clustering table and plot; these can also be downloaded. Code for running the application can be found in the runapp.R file.

This application is a project that is under construction, features will be added along the way. If you have any suggestions as to what features are useful to be added, please feel free to contact me at mail[at]jolandakossakowski[dot]eu.

Version 0.2 of this application has now been released. Visit https://jolandakos.shinyapps.io/NetworkApp/ to try it out!

## Current available features:

* General
  * Demo version option that will automatically upload predefined data if chosen
  
* Network Estimation
  * Add a title
  * Change the graph layout
  * Add node labels (columnames)
  * Use weighted edges
  * Use directed edges
  * Display graph details
  * Change minimum edge weight
  * Change maximum edge weight
  * Change cut-off value
  * Change edge size
  * Change node size
  * Download pdf of graph
  * Upload your own dataset
  * Various network estimation options
  * Option for performing non-paranormal transformation
  * Option for coding missing values
  
* Centrality tab
  * Add second tab with centrality table and plot
  * Download centrality table and plot
  * Change asthetics of centrality plot
  * Choose centrality measures 
  
* Clustering tab
  * Add third tab with clustering table and plot
  * Download clustering table and plot
  * Change asthetics of clustering plot
  * Choose clustering measures 

## Features that will be implemented:

* Network visualization
  * Enter group specification that will be visualized
  * Add a legend
  * Change node colours
  * Extend functions for more than 1 dataset
  * Implement network comparison method
  * Small World Index
  * Implement Graphical VAR function
  * Implement option for uploading Time Series Data
    * Implement option for lag-0 network
    * Implement option for lag-1 network

* Centrality tab
  * Add asthetics of centrality plot
  * Add option to sort centrality measures based on value
  * Add centrality measures:
    * Eigenvector centrality
    * Bonacich power
    * Alpha centrality
  
* Clustering tab
  * Add asthetics of clustering plot
  * Add option to sort clustering measures based on value
  
* Other features
  * Plot that compares centrality measure against clustering measure with median values of both
  * Add possibility to mention which column had participant numbers in them
  * create "busy" notion 
 
All features are implemented using the R-package qgraph version 1.3.1.


## Latest Updates

UPDATE (06-02-2015):
* Added the options to estimate an FDR (false discovery rate) network and a VAR (vector autoregressive) network.
* Added the option to estimate the skeleton and DAG with the IC-algorithm

## References:

Epskamp, S., Cramer, A. O. J., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012) qgraph: Network Visualizations of Relationships in Psychometric Data. *Journal of Statistical Software, 48*, 1 - 18.


Costantini, G., Epskamp, S., Borsboom, D., Perugini, M., Mõttus, R., Waldorp, L. J., & Cramer, A. O. (2014). State of the aRt personality research: A tutorial on network analysis of personality data in R. *Journal of Research in Personality*.
