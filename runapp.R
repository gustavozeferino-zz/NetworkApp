if(!"shiny" %in% installed.packages()) 
{ 
  install.packages("shiny") 
}
library(shiny)

if(!"devtools" %in% installed.packages())
{
  install.packages("devtools")
}
library(devtools)

runGitHub("NetworkApp", "JolandaKossakowski") 
