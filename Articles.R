install.packages("dplyr")
install.packages("tidytext")
install.packages("tm")
install.packages("stringr")
install.packages("cluster")
install.packages("xfun")
install.packages("factoextra")
install.packages("tidyverse")
install.packages("textclean")
install.packages("qdapRegex")
install.packages("pdftools")
install.packages("wordcloud")
library(wordcloud)
library(pdftools)
library(qdapRegex)
library(textclean)
library(tidyverse)
library(factoextra)
library(xfun)
library(cluster)
library(stringr)
library(tm)
library(dplyr)
library(tidytext)

titles <- Articles[,1]
authors <- Articles[,2]
summary <- Articles[,3]

clean <- function(data) {
    data <- tolower(data)
    data <- rm_city_state_zip(data)
    data <- rm_citation(data)
    data <- rm_email(data)
    data <- rm_url(data)
    data <- rm_non_words(data)
    data <- rm_non_ascii(data)
    data <- trimws(data)
    data <- removeWords(data, c(stopwords("english"), "stopwords.xlsx"))
    stopwords <- head(sort(unlist(stopwords), decreasing=TRUE), 705)
    stopwords <- removePunctuation(stopwords)
    data <- removeWords(data, stopwords)
    data <- stemDocument(data)
    data <- removePunctuation(data)
    data <- rm_nchar_words(data, 1)
    data <- rm_nchar_words(data, 2)
    data <- rm_nchar_words(data, 3)
    data <- removePunctuation(data)
    data <- str_remove_all(data, " \" ")
    data <- removeWords(data, "\"[A-za-z]+")
}

files <- list.files(pattern = "pdf$")
articles <- lapply(files, pdf_text)
for (i in 1:11) {
  articles[[i]] <- clean(articles[[i]])
}
articles <- Corpus(VectorSource(articles))
articles.dtm <- DocumentTermMatrix(articles)
dtm <- removeSparseTerms(articles.dtm, sparse = 0.98)
dtm.tfidf <- weightTfIdf(dtm)
tfidf.matrix <- t(as.matrix(dtm.tfidf))
f <- sort(rowSums(tfidf.matrix),decreasing=TRUE) 
fviz_nbclust(tfidf.matrix, kmeans, method = "wss")
set.seed(100)
clustering.kmeans <- kmeans(tfidf.matrix, centers = 4)
clustering.kmeans$size
result <- as.data.frame(clustering.kmeans$cluster)
result <- tibble::rownames_to_column(result, "word")
colnames(result) <- c("word","cluster")
I <- result %>%
  filter(cluster == 1)
I <- I$word
II <- result %>%
  filter(cluster == 2)
II <- II$word
III <- result %>%
  filter(cluster == 3)
III <- III$word
IV <- result %>%
  filter(cluster == 4)
IV <- IV$word

wordcloud(words = I, freq = f, max.words = 100)
wordcloud(words = II, freq = f, max.words = 100)
wordcloud(words = III, freq = f, max.words = 100)
wordcloud(words = IV, freq = f, max.words = 100)

