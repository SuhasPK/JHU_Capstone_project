#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("global.R")

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  output$ngram_output <- renderText({
    
    suppressWarnings(predict_next_word(input$user_input, ngram_list))
    
    
    
  })
  
})