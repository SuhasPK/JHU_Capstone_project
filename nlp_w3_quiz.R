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

tokenmaker <- suppressWarnings( function(x) {
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
)
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

next_word_is <- suppressWarnings(function(x,y){
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
)
###############################################################

q1_result <- next_word_is(
  "a case of", "([Aa]+ +[Cc]ase+ +[Oo]f+ +[^ ]+ )"
)
q1_result
###############################################################
q2_result <- next_word_is(
  "would mean the ", "( [Ww]ould+ +[Mm]ean+ +[Tt]he+ +[^ ]+  )"
)
q2_result
###############################################################
q3_result <- next_word_is(
  "make me the ", "([Mm]ake+ +[Mm]e+ +[Tt]he+ +[^ ]+ )"
)
q3_result
###############################################################
q4a_result <- next_word_is(
  "struggling ", "([Ss]truggling+ +[^ ]+ +[^ ]+ +[^ ]+ )"
)
q4a_result
###############################################################
q4b_result <- next_word_is(
  "struggling ", "([Ss]truggling+ +[^ ]+ )"
)
q4b_result
###############################################################
q5_result <- next_word_is(
  "date at the ", "([Dd]ate+ +[Aa]t+ +[Tt]he+ +[^ ]+ )"
)
q5_result
###############################################################
q6_result <- next_word_is(
  "be on my ", "([Bb]e+ +[Oo]n+ +[Mm]y+ +[^ ]+ )"
)
q6_result
###############################################################
q7_result <- next_word_is(
  "quite some ", "([Qq]uite+ +[Ss]ome+ +[^ ]+ )" 
)
q7_result
###############################################################
q8_result <- next_word_is(
  "his little ", "([Hh]is+ +[Ll]ittle+ +[^ ]+ )"
)
q8_result
###############################################################
q9_result <- next_word_is(
  "during the ", "([Dd]uring+ +[TT]he+ +[^ ]+ )" 
)
q9_result
###############################################################
q10_result <- next_word_is(
  "must be ",  "([Mm]ust+ +[Bb]e+ +[^ ]+ )"
)
q10_result
###############################################################



if(!require(knitr)){
  install.packages("knitr")
  library(knitr)
}
if(!require(stringi)){
  install.packages("stringi")
  library(stringi)
}
if(!require(NLP)){
  install.packages("NLP")
  library(NLP)
}
if(!require(tm)){
  install.packages("tm")
  library(tm)
}
if(!require(RWeka)){
  install.packages("RWeka")
  library(RWeka)
}
if(!require(data.table)){
  install.packages("data.table")
  library(data.table)
}
if(!require(ggplot2)){
  install.packages("ggplot2")
  library(ggplot2)
}
if(!require(SnowballC)){
  install.packages("SnowballC")
  library(SnowballC)
}
if(!require(wordcloud)){
  install.packages("wordcloud")
  library(wordcloud)
}
if(!require(kableExtra)){
  install.packages("kableExtra")
  library(kableExtra)
}
if(!require(gridExtra)){
  install.packages("gridExtra")
  library(gridExtra)
}
if(!require(ggdark)){
  install.packages("ggdark")
  library(ggdark)
}
if(!require(RColorBrewer)){
  install.packages("RColorBrewer")
  library(RColorBrewer)
}

if(!require(ngram)){
  install.packages("ngram")
  library(ngram)
}
library(stringr)

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
##########################################################################################
set.seed(660067)

# assigning sample size
sampleSize = 0.01

# sampling all 3 datasets
sample_blogs <- sample(blog_lines, length(blog_lines)*sampleSize, replace = FALSE)
sample_news <- sample(news_lines, length(blog_lines)*sampleSize, replace = FALSE)
sample_twitter <- sample(twitter_lines, length(blog_lines)*sampleSize, replace = FALSE)

# Removing all non-English characters from the sample data.

sample_blogs <- iconv(sample_blogs, "latin1", "ASCII", sub="")
sample_news <- iconv(sample_news, "latin1", "ASCII", sub="")
sample_twitter <- iconv(sample_twitter, "latin1", "ASCII", sub="")

# Combining all 3 sample data sets into a single data set and writing to disk.

sample_data <- c(sample_blogs,sample_news,sample_twitter)
sample_data_filename <- paste(getwd(),    "Coursera-SwiftKey/final/en_US/en_US.sample.txt",sep = "/")
con <- file(sample_data_filename, open = "w")
writeLines(sample_data,con)
close(con)

# number of lines and words in the sample data set.
sample_data_lines <- length(sample_data)
sample_data_words <- sum(stri_count_words(sample_data))
sample_data_lines
sample_data_words

##########################################################################################################


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
  # Removing stopwords
  docs <- tm_map(docs, removeWords, stopwords("english"))
  # Removing punctuation marks.
  docs <- tm_map(docs, removePunctuation)
  # Removing white spaces.
  docs <- tm_map(docs, stripWhitespace)
  # Removing Plain text document
  docs <- tm_map(docs, PlainTextDocument)
  
  return(docs)
}

# Building corpus and writing to disk

corpus <- build_corpus(sample_data)
saveRDS(corpus, file = paste(
  getwd(),"Coursera-SwiftKey/final/en_US/en_US.corpus.rds", sep = "/"
))

# Converting the corpus to a data frame and writing the lines/word to disks.

corpus_text <- data.frame(text = unlist(
  sapply(corpus, '[', "content")), stringsAsFactors = FALSE
)

corpus_filename <- paste(getwd(),"Coursera-SwiftKey/final/en_US/en_US.corpus.txt",sep = "/")
con <- file(corpus_filename, open = "w")
writeLines(corpus_text$text,con)
close(con)

#####################################################################################################

df_corpus <- readRDS(paste(
  getwd(),"Coursera-SwiftKey/final/en_US/en_US.corpus.rds", sep = "/"
))

df_lines <- df_corpus
lines <- as.character(df_lines)



filtered_lines <- lines[
  str_count(lines, "\\s+") > 1
]


trigram <- ngram(filtered_lines, n = 3)

df <- get.phrasetable(trigram)
saveRDS(df, paste(getwd(),"Coursera-SwiftKey/final/en_US/co_3gram_en.rds", sep = "/"))

df <- readRDS(
  paste(getwd(),"Coursera-SwiftKey/final/en_US/co_3gram_en.rds", sep = "/")
)
head(df[grep("^case of", df[,1]),], 10)
df

head(df[grep("^case ", df$ngrams),])
head(df[grep("^mean the ", df$ngrams),])





