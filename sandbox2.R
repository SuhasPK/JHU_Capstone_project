library(stringi)
library(stringr)
library(tm)
library(ngramr)
library(NLP)
library(RWeka)
library(SnowballC)

# 1. Importing the sample data set.
word_sample <- readLines(
  paste(getwd(),
        "Coursera-SwiftKey/final/en_US/en_US.sample.txt",
        sep = "/")
)


# 2. Building corpus using custom function
build_corpus <- function(dataset){
  docs <- VCorpus(VectorSource(dataset))
  to_space <- content_transformer(
    function(x,pattern)
      gsub(pattern, " ",x)
  )
  
  # Removing URL, Twitter handles, email pattern.
  docs <- tm_map(docs, to_space,"(f|ht)tp(s?)://(.*)[.][a-z]+")
  docs <- tm_map(docs, to_space,"@[^\\s]+")
  docs <- tm_map(docs, to_space, "\\b[A-Z a-z 0-9._ - ]*[@](.*?)[.]{1,3} \\b")
  
  # Removing Profane words from the sample data.
  
  con <- file(
    paste(getwd(),'Coursera-SwiftKey/final/en_US/en_US.profanity.txt',sep = "/"),
    "r"
  )
  profanity <-readLines(
    con, warn = FALSE, encoding = "UTF-8", skipNul = TRUE
  )
  close(con)
  
  profanity <- iconv(profanity, "latin1", "ASCII", sub = "")
  docs <- tm_map(docs, removeWords,profanity)
  
  # Converting the words to lower case.
  docs <- tm_map(docs, tolower)
  # Removing stopwords.
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Removing punctuation marks.
  docs <- tm_map(docs, removePunctuation)
  # Removing white spaces.
  docs <- tm_map(docs, stripWhitespace)
  # Removing Plain text document
  docs <- tm_map(docs, PlainTextDocument)
  # Removing numbers.
  docs <- tm_map(docs, removeNumbers)
  
  return(docs)
}

# 3. Building a corpus based on the word sample.

word_sample_corpus <- build_corpus(word_sample)

# 4. N-gram tokenization function.

unigram_tokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min=1, max = 1))
}

bigram_tokenizer <- function(x){
  NGramTokenizer(x, Weka_control(min=2,max=2))
}

trigram_tokenizer <- function(x){
  NGramTokenizer(x,
                 Weka_control(min=3, max=3))
}

# 5. Creating words & frequency data frame
# Uni gram tokens 
# creating term document matrix for the corpus
unigram_matrix <- TermDocumentMatrix(word_sample_corpus, control = list(tokenize = unigram_tokenizer))

# Eliminating sparse terms for each N-gram and get frequencies of most common n-grams

unigram_matrix_freq <- sort(rowSums(as.matrix(
  removeSparseTerms(
    unigram_matrix, 0.99
  )
)), decreasing = TRUE)


unigram_df <- data.frame(
  word = names(unigram_matrix_freq),
  freq = unigram_matrix_freq,
  row.names = NULL
)

# bi gram tokens
# creating term document matrix for the corpus
bigram_matrix <- TermDocumentMatrix(word_sample_corpus, control = list(tokenize = bigram_tokenizer))

# Eliminating sparse terms for each N-gram and get frequencies of most common n-grams

bigram_matrix_freq <- sort(rowSums(as.matrix(
  removeSparseTerms(
    bigram_matrix, 0.999
  )
)), decreasing = TRUE)


bigram_df <- data.frame(
  word = names(bigram_matrix_freq),
  freq = bigram_matrix_freq,
  row.names = NULL
)

# tri gram tokens
# creating term document matrix for the corpus
trigram_matrix <- TermDocumentMatrix(word_sample_corpus, control = list(tokenize = trigram_tokenizer))

# Eliminating sparse terms for each N-gram and get frequencies of most common n-grams

trigram_matrix_freq <- sort(rowSums(as.matrix(
  removeSparseTerms(
    trigram_matrix, 0.9999
  )
)), decreasing = TRUE)


trigram_df <- data.frame(
  word = names(trigram_matrix_freq),
  freq = trigram_matrix_freq,
  row.names = NULL
)


# 6. Prediction model



# A. Combining the data frames into a list.
word_df_list <- list(unigram_df,
                     bigram_df,
                     trigram_df)

combined_df <- rbind(
  unigram_df,
  bigram_df,
  trigram_df
)


extract_features <- function(input_data, data_frame){
  input_phrase <- paste(input_data$word, collapse = " ")
  matching_rows <- data_frame[
    grep(input_data,
         data_frame$word),
  ]
  return(matching_rows$freq)
}


library(randomForest)
# 
# model <- randomForest(
#   freq~., data = combined_df
# )
# 
# get_user_input <- function(){
#   cat("Enter a phrase (e.g., 'this is') for next word prediction: ")
#   input_phrase <- tolower(readline())
#   input_data <- data.frame(word = input_phrase)
#   return(input_phrase)
# }

# input_text <- get_user_input()
# 
# predict_next_word <- function(input_text, data_frame) {
#   input_words <- strsplit(input_text, " ")[[1]]
#   input_phrase <- paste(input_words, collapse = " ")
#   
#   matching_rows <- data_frame[grep(input_phrase, data_frame$word), ]
#   
#   if (nrow(matching_rows) > 0) {
#     matching_rows$probability <- matching_rows$freq / sum(matching_rows$freq)
#     next_word <- sample(matching_rows$word, size = 1, prob = matching_rows$probability)
#     return(next_word)
#   } else {
#     return(NULL)
#   }
# }
# 
# predicted_next_word <- predict_next_word(input_text, combined_df)
# 
# if (!is.null(predicted_next_word)) {
#   cat("Predicted next word:", predicted_next_word, "\n")
# } else {
#   cat("No prediction available\n")
# }
# 
# 
# 
get_user_input <- function() {
  cat("Enter a phrase (e.g., 'this is') for next word prediction: ")
  input_phrase <- tolower(readline())
  return(input_phrase)
}



input_data <- get_user_input()

library(stringdist)

predict_next_word_rf <- function(input_text, data_frame) {
  input_words <- strsplit(input_text, " ")[[1]]
  input_phrase <- paste(input_words, collapse = " ")
  
  matching_rows <- data_frame[grep(input_phrase, data_frame$word), ]
  
  if (nrow(matching_rows) > 0) {
    distances <- stringdistmatrix(input_phrase,matching_rows$word)
    min_distance_index <- which.min(distances)
    
    rf_model <- randomForest(freq ~ ., data = matching_rows)
    next_word_row <- matching_rows[which.max(predict(rf_model, matching_rows)), ]
    return(next_word_row$word)
  } else {
    return(NULL)
  }
}

fold_accuracy <- c()
for (fold in 1:5) {
  # Train and test data splitting
  set.seed(fold)
  sample_rows <- sample(1:nrow(combined_df), size = 0.8 * nrow(combined_df))
  train_data <- combined_df[sample_rows, ]
  test_data <- combined_df[-sample_rows, ]
  
  # Train the model
  rf_model <- randomForest(freq ~ ., data = train_data)
  
  # Evaluate on test data
  predicted_frequencies <- predict(rf_model, newdata = test_data)
  accuracy <- sum(test_data$word == predicted_frequencies) / nrow(test_data)
  fold_accuracy <- c(fold_accuracy, accuracy)
}

average_accuracy <- mean(fold_accuracy)
cat("Average accuracy:", average_accuracy, "\n")


predicted_next_word <- predict_next_word_rf(input_text, combined_df)

if (!is.null(predicted_next_word)) {
  cat("Predicted next word:", predicted_next_word, "\n")
} else {
  cat("No prediction available\n")
}









