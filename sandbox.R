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
#########################################################
# Read bolg lines
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

# Read Twitter lines
twitter <- file(
  paste(getwd(),'Coursera-SwiftKey/final/en_US/en_US.twitter.txt',sep = "/"),
  "r"
)
twitter_lines <- readLines(twitter,warn = FALSE, encoding = "UTF-8", skipNul = TRUE)
close(twitter)

###################################################################
# number of lines 
num_lines <- sapply(list(blog_lines,news_lines,twitter_lines), length)

# number of characters
num_char <- sapply(list(nchar(blog_lines),nchar(news_lines),nchar(twitter_lines)),sum)

# number of words 
num_words <- sapply(list(blog_lines,news_lines,twitter_lines), stri_stats_latex)[4,]

# number of words per line
wpl <- lapply(list(blog_lines,news_lines,twitter_lines), function(x){stri_count_words(x)})

# summary 
wpl_gist <- sapply(list(blog_lines,news_lines,twitter_lines), function(x){
  summary(stri_count_words(x))[c('Min.', 'Mean', 'Max.')]})

rownames(wpl_gist) = c('wpl_min', 'wpl_mean', 'wpl_max')

gist <- data.frame(
  FileName = c("en_US.blogs.txt","en_US.news.txt","en_US.twitter.txt"),
  FileSize = sapply(
    list(blog_lines,news_lines,twitter_lines), function(x){format(object.size(x),"MB")}
  ),
  Lines = num_lines,
  Charaters = num_char,
  Words = num_words,
  t(rbind(round(wpl_gist)))
  
)



##########################################################################

set.seed(660067)

# assigning sample size
sampleSize = 0.001
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
sample_data_filename <- paste(getwd(),"Coursera-SwiftKey/final/en_US/en_US.sample.txt",sep = "/")
con <- file(sample_data_filename, open = "w")
writeLines(sample_data,con)
close(con)

# number of lines and words in the sample data set.
sample_data_lines <- length(sample_data)
sample_data_words <- sum(stri_count_words(sample_data))

##########################################################################################
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



################################################################################

# tdm <- TermDocumentMatrix(corpus_text)
# 
# 
# 
# freq <- sort(rowSums(as.matrix(tdm)), decreasing = TRUE)
# 
# 
# word_Freq <- data.frame(word = names(freq), freq = freq, row.names = NULL)
# 
# 
# # Plotting top 10 most frequent words.
# 
# plot_most_freq_word <- ggplot(
#   word_Freq[1:10,], aes(x = reorder(word_Freq[1:10,]$word, -word_Freq[1:10,]$freq),
#                         y = word_Freq[1:10,]$freq)
# )
# 
# plot_most_freq_word <- plot_most_freq_word+
#   geom_bar(stat = "Identity", fill = "#90EE90" ) + 
#   geom_text(aes(label = word_Freq[1:10,]$freq), vjust = -0.20, size=3) + xlab("Words")+ ylab("Word Frequencies")+theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
#                                                                                                                        axis.text.x = element_text(hjust = 0.5, vjust = 0.5, angle = 45),
#                                                                                                                        axis.text.y = element_text(hjust = 0.5, vjust = 0.5))+ggtitle("10 Most Frequent Words")+dark_theme_light()
# 
# plot_most_freq_word
# 
# # Word cloud
# suppressWarnings(
#   wordcloud(
#     words = word_Freq$word,
#     freq = word_Freq$freq,
#     min.freq = 1,
#     max.words = 100,
#     random.order = FALSE,
#     rot.per = 0.35,
#     colors = brewer.pal(10, "Dark2")
#   ) 
#   
#   
# )
# 
# ################################################################################
# unigram_tokenizer <- function(x){
#   NGramTokenizer(x, Weka_control(min=1, max = 1))
# }
# 
# bigram_tokenizer <- function(x){
#   NGramTokenizer(x, Weka_control(min=2,max=2))
# }
# 
# trigram_tokenizer <- function(x){
#   NGramTokenizer(x,
#                  Weka_control(min=3, max=3))}
################################################################################
# creating term document matrix for the corpus
#bigram_matrix <- TermDocumentMatrix(corpus_text, control = list(tokenize = bigram_tokenizer))

# Eliminating sparse terms for each N-gram and get frequencies of most common n-grams

# bigram_matrix_freq <- sort(rowSums(as.matrix(
#   removeSparseTerms(
#     bigram_matrix, 0.99
#   )
# )), decreasing = TRUE)
# 
# 
# bigram_matrix_freq <- data.frame(
#   word = names(bigram_matrix_freq),
#   freq = bigram_matrix_freq,
#   row.names = NULL
# )
# 
# head(bigram_matrix_freq,5)
# # Plotting histogram
# unigram_hist <- ggplot(
#   unigram_matrix_freq[1:20,],
#   aes(x = reorder(word, -freq), y = freq)
# ) + geom_bar(stat = "identity", fill = "#37BEB0")+ geom_text(
#   aes(label = freq), vjust = -0.20, size = 3) + xlab("Words") + ylab("Frequency")+
#   theme(plot.title = element_text(size = 14, hjust = 0.5, vjust = 0.5),
#         axis.text.x = element_text(hjust = 1.0, angle = 45),
#         axis.text.y = element_text(hjust = 0.5, vjust = 0.5))+ ggtitle("20 Most Common Unigrams")+dark_theme_light()
# 
# unigram_hist
# 
# rm(unigram_matrix, unigram_matrix_freq, unigram_hist)

################################################################################

####################################


unigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
bigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
trigramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))


bigramMatrix <- TermDocumentMatrix(corpus, control = list(tokenize = bigramTokenizer))

# eliminate sparse terms for each n-gram and get frequencies of most common n-grams
bigramMatrixFreq <- sort(rowSums(as.matrix(removeSparseTerms(bigramMatrix, 0.999))), decreasing = TRUE)
bigramMatrixFreq <- data.frame(word = names(bigramMatrixFreq), freq = bigramMatrixFreq)

bigramMatrixFreq
















