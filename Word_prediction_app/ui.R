# Data Science Capstone Project


library(shiny)
library(shinythemes)

shinyUI(fluidPage(
  theme = shinytheme("cyborg"),  # Set the theme to "cyborg"
  
  tags$head(
    # Add custom CSS to change the menu bar color, style the text, and set the selected page text to black
    tags$style(HTML(".navbar-default { background-color: #57F287; border-color: #57F287; } .navbar-default .navbar-nav>li>a { color: #000000; font-weight: bold; } .navbar-default .navbar-nav>.active>a, .navbar-default .navbar-nav>.active>a:focus, .navbar-default .navbar-nav>.active>a:hover { color: #000000; background-color: #F257C2; } .navbar-default .navbar-brand { color: #000000; }"))
  ),
  
  titlePanel("Word Prediction Application"), br(),
  
  fluidRow(
    column(12, align = "left",
           HTML("<strong style='font-size: 22px;'>JHU Data Science Course: Capstone Project</strong>")
    ),
    br(),
    column(12, align = "left",
           HTML("<strong style='font-size: 20px;'>Suhas. P. K</strong>")
    )
  ),
  
  fluidRow(
    column(6, align = "left",
           p("This application takes input from the user and predicts the next word based on statistical language modeling. 
             This project is a brief introduction to Natural Language Processing. This app has taken only a small part of the 
             given dataset. The accuracy of the prediction depends on the sample dataset.")
    )
  ),
  
  navbarPage(
    # Change the text color of "Menu" to black
    tags$style(HTML(".navbar-default .navbar-brand { color: #000000; }")),
    
    tabPanel("Home",
             sidebarLayout(
               sidebarPanel(
                 textInput("InputString", HTML("Enter a sentence without the last word"), value = ""),
                 actionButton("do", "Predict")
               ),
               mainPanel(
                 h4("The predicted next word is"), 
                 fluidRow(column(5, verbatimTextOutput("PredictedWord", placeholder = TRUE)))
               )
             )
    ),
    tabPanel("About Project",
             p("As a beginner, this project takes whole lot of my time, almost a month. Mostly because the videos and
               resource material given during the was very breif and I had to explore more all by myself. This included 
               going through many articles, videos and github pages. This really made me experience how a data scientist
               would go through in a real case project."),
             br(),
             p("One important tool which really helped in this project was ChatGPT. Even though I feel like I have not
               used its resources to maximum, it did play a very crucial role in project's web app part of the code, because
               eve though I am familiar with HTML, CSS and JavaScript very briefly but this helped me in customising the app."),
             br(),
             p("The data sets given but Coursera-SwiftKey was cleaned by all the numbers, urls, punctuations, stopwords and profane word. The report on 
               the data preprocessing will be linked in this page. As the model is based on Ngram tokens, the code for generating 
               n-grams will in that report. Do check it out."),
             br(),
             tags$a(href = "https://rpubs.com/suhasPK/NLP-Milestone-project", "NLP Milestone Project Report"),
             br(),
             tags$a(href = "https://github.com/SuhasPK/JHU_Capstone_project", "My Github Project Repository")
             
    ),
    tabPanel("Presentation",
             tags$a(href = "https://www.link2.com", "Link 2"),
             br(),
             p("This is the second link page. You can provide a brief description of the content available at this link. Describe what users can expect when they click on this link.")
    )
  )
))
