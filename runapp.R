if(!"shiny" %in% installed.packages()) 
{ 
  install.packages("shiny") 
}

if(!"qgraph" %in% installed.packages()) 
{ 
  install.packages("qgraph") 
}

if(!"ggplot2" %in% installed.packages()) 
{ 
  install.packages("ggplot2") 
}

if(!"huge" %in% installed.packages()) 
{ 
  install.packages("huge") 
}

if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/shinyapps")

library("shiny")
library("qgraph")
library("shinyapps")
library("ggplot2")

runGitHub("NetworkApp", "JolandaKossakowski") 

