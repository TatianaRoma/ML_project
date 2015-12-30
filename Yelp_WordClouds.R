### Word clouds from yelp reviews by star

library(tm)
library(wordcloud)
library(SnowballC)

reviews <- read.csv("/Users/tatiana/reviews/rest_reviews_1.csv")
reviews$stars <- factor(reviews$stars)
rm_words <- c("food","like","place","one","got","get","can","good",
              "bad","just","will","restaurant","service","ordered",
              "really","chicken","great","time","came")
### One Star Cloud
one_star <- reviews[reviews$stars == "1",4:5]

mycorps <- Corpus(VectorSource(as.character(one_star[['text']])))

mycorps <- tm_map(mycorps, stripWhitespace, lazy = T)

mycorps <- tm_map(mycorps, content_transformer(tolower), lazy = T)

mycorps <- tm_map(mycorps, removeWords, stopwords("english"), lazy=T)

mycorps <- tm_map(mycorps, content_transformer(stemDocument))

mycorps <- tm_map(mycorps, removeWords, rm_words, lazy = T)

myDTM = TermDocumentMatrix(mycorps, control = list(minWordLength = 1))

m = as.matrix(myDTM)

v = sort(rowSums(m), decreasing = TRUE)
head(v,15)
set.seed(1985)

wordcloud(names(v), v, min.freq = 340, scale=c(2.3,0.2), 
          colors=brewer.pal(6, "Blues"), random.order=FALSE, 
          rot.per=0.35, max.words = 200)

### Two Stars Cloud
two_star <- reviews[reviews$stars == "2",4:5]

mycorps <- Corpus(VectorSource(as.character(two_star[['text']])))

mycorps <- tm_map(mycorps, stripWhitespace, lazy = T)

mycorps <- tm_map(mycorps, content_transformer(tolower), lazy = T)

mycorps <- tm_map(mycorps, removeWords, stopwords("english"), lazy=T)

mycorps <- tm_map(mycorps, content_transformer(stemDocument))

mycorps <- tm_map(mycorps, removeWords, rm_words, lazy = T)
myDTM = TermDocumentMatrix(mycorps, control = list(minWordLength = 1))

m = as.matrix(myDTM)

v = sort(rowSums(m), decreasing = TRUE)
head(v,15)
set.seed(1985)

wordcloud(names(v), v, min.freq = 440, scale=c(2.3,0.1), 
          colors=brewer.pal(8, "BuPu"), random.order=FALSE, 
          rot.per=0.35, max.words = 100)

### Three Stars Cloud
three_star <- reviews[reviews$stars == "3",4:5]

mycorps <- Corpus(VectorSource(as.character(three_star[['text']])))

mycorps <- tm_map(mycorps, stripWhitespace, lazy = T)

mycorps <- tm_map(mycorps, content_transformer(tolower), lazy = T)

mycorps <- tm_map(mycorps, removeWords, stopwords("english"), lazy=T)

mycorps <- tm_map(mycorps, content_transformer(stemDocument))

mycorps <- tm_map(mycorps, removeWords, rm_words, lazy = T)

myDTM = TermDocumentMatrix(mycorps, control = list(minWordLength = 1))

m = as.matrix(myDTM)

v = sort(rowSums(m), decreasing = TRUE)
set.seed(1985)

wordcloud(names(v), v, min.freq = 480, scale=c(2.3,0.2), 
          colors=brewer.pal(6, "Greens"), random.order=FALSE, 
          rot.per=0.35, max.words = 100)

### Four Stars Cloud
four_star <- reviews[reviews$stars == "4",4:5]

mycorps <- Corpus(VectorSource(as.character(four_star[['text']])))

mycorps <- tm_map(mycorps, stripWhitespace, lazy = T)

mycorps <- tm_map(mycorps, content_transformer(tolower), lazy = T)

mycorps <- tm_map(mycorps, removeWords, stopwords("english"), lazy=T)

mycorps <- tm_map(mycorps, content_transformer(stemDocument))

mycorps <- tm_map(mycorps, removeWords, rm_words, lazy = T)
myDTM = TermDocumentMatrix(mycorps, control = list(minWordLength = 1))

m = as.matrix(myDTM)

v = sort(rowSums(m), decreasing = TRUE)
set.seed(1985)

wordcloud(names(v), v, min.freq = 490, scale=c(2.3,0.2), 
          colors=brewer.pal(4, "YlOrBr"), random.order=FALSE, 
          rot.per=0.35, max.words = 100)


### Five Stars Cloud
five_star <- reviews[reviews$stars == "5",4:5]

mycorps <- Corpus(VectorSource(as.character(five_star[['text']])))

mycorps <- tm_map(mycorps, stripWhitespace, lazy = T)

mycorps <- tm_map(mycorps, content_transformer(tolower), lazy = T)

mycorps <- tm_map(mycorps, removeWords, stopwords("english"), lazy=T)

mycorps <- tm_map(mycorps, content_transformer(stemDocument))

mycorps <- tm_map(mycorps, removeWords,rm_words, lazy = T)

myDTM = TermDocumentMatrix(mycorps, control = list(minWordLength = 1))

m = as.matrix(myDTM)

v = sort(rowSums(m), decreasing = TRUE)
set.seed(1985)

wordcloud(names(v), v, min.freq = 480, scale=c(2.6,0.2), 
          colors=brewer.pal(6, "Reds"), random.order=FALSE, 
          rot.per=0.35, max.words = 100)




#######################




