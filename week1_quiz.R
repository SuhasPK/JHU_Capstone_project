
getwd()

path <- "E:/my data science journey/john_hopkins_data_science/JHU_Capstone_project/Coursera-SwiftKey/final/"

# 1. 
size <- file.info("E:/my data science journey/john_hopkins_data_science/JHU_Capstone_project/Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
kb <- size$size/1024
mb <- kb/1024
mb

# 2.
twitter <- readLines(con <- file(paste(path,'en_US/en_US.twitter.txt',sep = "/")),
                     encoding = "UTF-8",
                     skipNul = TRUE)
length(twitter)

# 3.
blogs <- file(
  paste(getwd(),'Coursera-SwiftKey/final/en_US/en_US.blogs.txt',sep = "/"),
  "r"
)
blog_lines <- readLines(blogs)
close(blogs)
bl <- summary(nchar(blog_lines))

bl

news<-file(paste(path, 'en_US/en_US.news.txt',sep = "/")
           ,"r")
news_lines<-readLines(news)
close(news)
nl <- summary(nchar(news_lines))


twitter <- file(
  paste(path,'en_US/en_US.twitter.txt',sep = "/"),
  "r"
)
twitter_lines <- readLines(twitter)
close(twitter)
tl <- summary(nchar(twitter_lines))


# 4. 
love <- length(grep("love", twitter_lines))
love
hate <- length(grep("hate",twitter_lines))
hate
love/hate

# 5. 
grep("biostats", twitter_lines, value = TRUE)

# 6.
grep("hutchback", twitter_lines, value = TRUE)









