library(tokenizers)
library(tm)
library(stringi)
library(stringr)
library(ngramr)
library(NLP)
library(RWeka)
library(SnowballC)



word_sample <- readLines(
  paste(getwd(),
        "Coursera-SwiftKey/final/en_US/en_US.corpus.txt",
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

corpus_text <- data.frame(text = unlist(
  sapply(word_sample_corpus, '[', "content")), stringsAsFactors = FALSE
)

corpus_filename <- paste(getwd(),"Coursera-SwiftKey/final/en_US/en_US.corpus.txt",sep = "/")
con <- file(corpus_filename, open = "w")
writeLines(corpus_text$text,con)
close(con)


corpus_path <- paste(
  getwd(),"Coursera-SwiftKey/final/en_US/en_US.corpus.txt",
  sep = "/"
)

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

 # Choose the n-gram order (e.g., 2 for bigrams)
unigram_list <- generate_ngrams(corpus_words, 1)
bigram_list <- generate_ngrams(corpus_words, 2)
trigram_list <- generate_ngrams(corpus_words, 3)




input_word <- readline("Enter a word for next word prediction: ")
predicted_next_word <- predict_next_word(input_word, trigram_list)

if (!is.null(predicted_next_word)) {
  cat("Predicted next word:", predicted_next_word, "\n")
} else {
  cat("No prediction available\n")
}


