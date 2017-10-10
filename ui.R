library(LDAvis) 
library(shiny)
library(rCharts)
data("TwentyNewsgroups")
shinyUI( 
fluidPage( 
sliderInput("nTerms", "Number of terms to display", min = 20, max = 40, value = 30), 
visOutput('myChart') 
)
) 
