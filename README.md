## READ ME for Shiny Network Application

This Shiny application visualizes a graph structure for a dataset. The user can adjust all features of the visualized graph and download a pdf version of it. Furthermore, centrality analyses will automatically be performed; a centrality table and plot will be visualized in the second tab and both can be downloaded. The third tab visualizes the Clustering table and plot; these can also be downloaded. 

This application is a project that is under construction, features will be added along the way. If you have any suggestions as to what features are useful to be added, please feel free to contact me at mail[at]jolandakossakowski[dot]eu.

Current available features:

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

Features that will be implemented:
* Network visualization

  * Enter group specification that will be visualized
  * Add a legend
  * Change node labels
  * Change node colours

* Centrality tab
  * Add second tab with centrality table and plot
  * Download centrality table and plot
  * Choose centrality measures 
  * Option to merge centrality measures into one plot
  * Change asthetics of centrality plot
  
* Clustering tab
  * Add third tab with clustering table and plot
  * Download clustering table and plot
  * Option to merge clustering into one plot
  * Change asthetics of clustering plot
  
Code for running the application:

```
if(!"shiny" %in% installed.packages()) 
{ 
  install.packages("shiny") 
}

if(!"qgraph" %in% installed.packages()) 
{ 
  install.packages("qgraph") 
}

if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/shinyapps")

library("shiny")
library("qgraph")
library("shinyapps")

runGitHub( "NetworkApp", "JolandaKossakowski") 

```

All features are implemented using the R-package qgraph version 1.2.5

Citations:

Epskamp, S., Cramer, A. O. J., Waldorp, L. J., Schmittmann, V. D., & Borsboom, D. (2012) qgraph: Network Visualizations of Relationships in Psychometric Data. *Journal of Statistical Software, 48*, 1 - 18.


Costantini, G., Epskamp, S., Borsboom, D., Perugini, M., Mõttus, R., Waldorp, L. J., & Cramer, A. O. (2014). State of the aRt personality research: A tutorial on network analysis of personality data in R. *Journal of Research in Personality*.