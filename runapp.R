if(!"shiny" %in% installed.packages()) 
{ 
  install.packages("shiny") 
}

if (!require("devtools"))
  install.packages("devtools")
devtools::install_github("rstudio/shinyapps")

library("shiny")
library("shinyapps")

runGitHub( "NetworkApp", "JolandaKossakowski") 
