pf <- read.csv("profanity_en.csv")

head(pf)

library(readr)

profanity_words <- pf$text
text_file_path <- paste(
  getwd(),"Coursera-SwiftKey/final/en_US/en_US.profanity.txt",sep = "/"
)

con <- file(text_file_path,open = "w")
writeLines(profanity_words,con)
close(con)

rm(profanity_words)
