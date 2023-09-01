#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  
  corpus_path <- "E:\my data science journey\john_hopkins_data_science\JHU_Capstone_project\NLP_word_prediction_app\en_US.corpus.txt"
  
  corpus_text <- tolower(
    readLines(corpus_path, warn = FALSE)
  )
  
  corpus_words <- unlist(tokenize_words(
    corpus_text
  ))
  
  generate_ngrams <- function(text, n) {
    ngram_list <- list()
    for (i in 1:(length(text) - n + 1)) {
      ngram <- paste(text[i:(i + n - 1)], collapse = " ")
      ngram_list <- append(ngram_list, ngram)
    }
    return(ngram_list)
  }
  
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
  
  n <-2 # Choose the n-gram order (e.g., 2 for bigrams)
  ngram_list <- generate_ngrams(corpus_words, n)
  
  
  
  output$ngram_output <- renderText({
    #suppressWarnings(predict(input$user_input)) 
    suppressWarnings(predict_next_word(input$user_input, ngram_list))
    
    
    
  })
  
})