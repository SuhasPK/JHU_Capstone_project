#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

# Load required libraries
library(tokenizers)
library(tm)
library(stringi)
library(stringr)
library(ngramr)
library(NLP)
library(RWeka)
library(SnowballC)

# Load required libraries
library(shiny)

# Define a function to generate ngrams
generate_ngrams <- function(text, n) {
  ngram_list <- list()
  for (i in 1:(length(text) - n + 1)) {
    ngram <- paste(text[i:(i + n - 1)], collapse = " ")
    ngram_list <- append(ngram_list, ngram)
  }
  return(ngram_list)
}

# Define UI
ui <- fluidPage(
  titlePanel("Word Prediction App"),
  sidebarLayout(
    sidebarPanel(
      textInput("input_word", "Enter a word for prediction:"),
      sliderInput("ngram_slider", "Select Ngram Type:",
                  min = 1, max = 3, step = 1, value = 1),
      actionButton("predict_button", "Predict")
    ),
    mainPanel(
      verbatimTextOutput("prediction_output")
    )
  )
)

# Define server
server <- function(input, output, session) {
  
  # Load ngrams (unigram, bigram, trigram)
  corpus_path <- "E:\\my data science journey\\john_hopkins_data_science\\JHU_Capstone_project\\word_prediction_project\\en_US.corpus.txt"
  corpus_text <- tolower(readLines(corpus_path, warn = FALSE))
  corpus_words <- unlist(tokenize_words(corpus_text))
  unigram_list <- generate_ngrams(corpus_words, 1)
  bigram_list <- generate_ngrams(corpus_words, 2)
  trigram_list <- generate_ngrams(corpus_words, 3)
  
  # Function to predict the next word
  predict_next_word <- function(input_word, ngram_list) {
    input_word <- tolower(input_word)
    matching_ngrams <- grep(paste("^", input_word, sep = ""), ngram_list, value = TRUE)
    if (length(matching_ngrams) > 0) {
      next_ngram <- matching_ngrams[1]
      next_word <- strsplit(next_ngram, " ")[[1]][length(input_word) + 1]
      return(next_word)
    } else {
      return(NULL)
    }
  }
  
  # Update slider labels
  observe({
    updateSliderInput(session, "ngram_slider",
                      labels = c("Unigram", "Bigram", "Trigram"))
  })
  
  observeEvent(input$predict_button, {
    input_word <- tolower(input$input_word)
    selected_ngram <- switch(input$ngram_slider,
                             "1" = unigram_list,
                             "2" = bigram_list,
                             "3" = trigram_list
    )
    predicted_next_word <- predict_next_word(input_word, selected_ngram)
    
    if (!is.null(predicted_next_word)) {
      output$prediction_output <- renderText({
        paste("Predicted next word:", predicted_next_word)
      })
    } else {
      output$prediction_output <- renderText({
        "No prediction available"
      })
    }
  })
}

# Create Shiny app
shinyApp(ui, server)
