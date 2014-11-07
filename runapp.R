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
