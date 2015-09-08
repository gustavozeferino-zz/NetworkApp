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

if(!"huge" %in% installed.packages())
{
  install.packages("huge")
}
library(huge)

runGitHub("NetworkApp", "JolandaKossakowski") 
