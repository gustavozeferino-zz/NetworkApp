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

if(!"qgraph" %in% installed.packages())
{
  install.packages("qgraph")
}
library(qgraph)

if(!"igraph" %in% installed.packages())
{
  install.packages("igraph")
}
library(igraph)

if(!"pcalg" %in% installed.packages())
{
  install.packages("pcalg")
}
library(pcalg)

if(!"psych" %in% installed.packages())
{
  install.packages("psych")
}
library(psych)

if(!"ggplot2" %in% installed.packages())
{
  install.packages("ggplot2")
}
library(ggplot2)

runGitHub("NetworkApp", "JolandaKossakowski") 
