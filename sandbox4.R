library(tm)
library(stringi)
library(stringr)

# Function to read and process text from files
read_and_process_text <- function(file_path) {
  lines <- readLines(file_path, warn = FALSE, encoding = "UTF-8", skipNul = TRUE)
  corpus <- Corpus(VectorSource(lines))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, PlainTextDocument)
  Corpus(VectorSource(corpus))
}

# Function to create a document-term matrix and count words
create_dtm_and_count_words <- function(corpus) {
  dtm <- DocumentTermMatrix(corpus)
  dtm_matrix <- as.matrix(dtm)
  word_freq <- colSums(dtm_matrix)
  word_freq <- sort(word_freq, decreasing = TRUE)
  words <- names(word_freq)
  list(words, word_freq)
}

# Function to predict next words based on user input
predict_next_word <- function(input_word, lines, next_word_pattern) {
  matching_lines <- lines[grepl(input_word, lines, ignore.case = TRUE)]
  initial_chars <- str_extract(matching_lines, next_word_pattern)
  next_words <- stri_extract_last_words(initial_chars)
  
  df <- data.frame(next_word_is = next_words, stringsAsFactors = FALSE)
  return(head(df, 25))
}

# Read and process text data
blog_corpus <- read_and_process_text(paste(getwd(),
                                           "Coursera-SwiftKey/final/en_US/en_US.blogs.txt",
                                           sep = "/"))
news_corpus <- read_and_process_text(paste(getwd(),
                                           "Coursera-SwiftKey/final/en_US/en_US.news.txt",
                                           sep = "/"))
twitter_corpus <- read_and_process_text(paste(getwd(),
                                              "Coursera-SwiftKey/final/en_US/en_US.twitter.txt",
                                         sep = "/"))



# Combine all corpus data
all_corpus <- Corpus(VectorSource(c(blog_corpus, news_corpus, twitter_corpus)))

# Create a document-term matrix and count words
word_info <- create_dtm_and_count_words(all_corpus)

# Define next word pattern
next_word_pattern <- "[A-Za-z]+$"

# Predict next words based on user input
# You would replace "input_word" with the actual user input and
# choose the appropriate corpus variable (blog_corpus, news_corpus, twitter_corpus)
predicted_words <- predict_next_word("tragedy of", all_corpus, next_word_pattern)

# Display predicted words
print(predicted_words)
