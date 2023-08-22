library(stringi)
library(stringr)
library(tm)

# Read Blog lines
blogs <- file(
  paste(getwd(),'Coursera-SwiftKey/final/en_US/en_US.blogs.txt',sep = "/"),
  "r"
)
blog_lines <- readLines(blogs, warn = FALSE, encoding = "UTF-8", skipNul = TRUE)
close(blogs)

# Read News lines
news <- file(
  paste(getwd(),'Coursera-SwiftKey/final/en_US/en_US.news.txt',sep = "/"),
  "r"
)
news_lines <- readLines(news,warn = FALSE, encoding = "UTF-8", skipNul = TRUE)
close(news)

# Read Twitter Lines
twitter <- file(
  paste(getwd(),'Coursera-SwiftKey/final/en_US/en_US.twitter.txt',sep = "/"),
  "r"
)
twitter_lines <- readLines(twitter,warn = FALSE, encoding = "UTF-8", skipNul = TRUE)
close(twitter)

#####################################################################################

tokenmaker <- function(x) {
  corpus <- Corpus(VectorSource(x))
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, PlainTextDocument)
  #        corpus <- tm_map(corpus, stemDocument)
  corpus <- Corpus(VectorSource(corpus))
}  

####################################################################################

wordcounter <- function(x) {
  dtm<-DocumentTermMatrix(x)
  dtm_matrix <- as.matrix(dtm)
  word_freq <- colSums(dtm_matrix)
  word_freq <- sort(word_freq, decreasing = TRUE)
  words <- names(word_freq)
  return(list(words, word_freq))
}  
###################################################################################

next_word_is <- function(x,y){
  b_quest <- grepl(x, blog_lines, ignore.case = TRUE)
  b_docs <- blog_lines[b_quest]
  initial_char <- 'a'
  next_word_is <- 'a'
  i <- length(b_docs)
  if(i>0){
    for(i in 1:i){
      initial_char[i] <- str_extract(b_docs[i],y)
      next_word_is[i] <- stri_extract_last_words(initial_char[i])
    }
  }
  
  n_quest <- grepl(x, news_lines, ignore.case = TRUE)
  n_docs <- news_lines[n_quest]
  j = length(n_docs)
  if(j>0){
    for(j in 1:j){
      initial_char[i+j] <- str_extract(n_docs[j],y)
      next_word_is[i+j] <- stri_extract_last_words(initial_char[i+j])
    }
  }
  
  t_quest <- grepl(x, twitter_lines, ignore.case = TRUE)
  t_docs <- twitter_lines[t_quest]
  k = length(t_docs)
  if(k>0){
    for(k in 1:k){
      initial_char[i+j+k] <- str_extract(t_docs[k],y)
      next_word_is[i+j+k] <- stri_extract_last_words(initial_char[i+j+k] )
    }
  }
  
  bundle <- as.data.frame(
    next_word_is, stringsAsFactors = FALSE
  )
  summary(bundle)
  
  blog_token <- tokenmaker(bundle)
  blog_words <- wordcounter(blog_token)
  summary(nchar(bundle))
  head(bundle)
  
  tdm_blogs <- TermDocumentMatrix(blog_token)
  matrix_blogs <- as.matrix(tdm_blogs)
  
  df_blogs <- sort(
    rowSums(
      matrix_blogs
    ), decreasing = TRUE
  )
  
  df_blogs <- data.frame(
    word = names(df_blogs),
    freq = df_blogs
  )
  
  head(df_blogs, 25)
  return(list(head(df_blogs, 25)))
  
}

###############################################################

q1_result <- next_word_is(
  "a case of", "([Aa]+ +[Cc]ase+ +[Oo]f+ +[^ ]+ )"
)
q1_result

q2_result <- next_word_is(
  "would mean the", "(((999999"
)










