# Data Science Capstone Project

library(shiny)

shinyUI(fluidPage(
    titlePanel("Next word prediction - Coursera Data Science Capstone Project"),
    fluidRow(HTML("<strong>Author: Suhas. P. K</strong>") ),

    fluidRow(
        br(),
        p("The purpose of this Shiny app is that takes as input a phrase
        (multiple words) in a text box input and outputs a prediction
          of the next word")),
    br(),
    br(),
    
    sidebarLayout(
        sidebarPanel(
            textInput("InputString", "Enter a sentence without the last word", value = ""),
            actionButton("do", "Predict")
        ),
        
        mainPanel(
            h4("The predicted next word is"), 
            fluidRow(column(5, verbatimTextOutput("PredictedWord", placeholder = TRUE)))
        )
    )
))
