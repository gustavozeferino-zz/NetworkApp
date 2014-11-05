if(!"shiny" %in% installed.packages()) 
{ 
  install.packages("shiny") 
}
library("shiny")
runGitHub( "NetworkApp", "JolandaKossakowski") 