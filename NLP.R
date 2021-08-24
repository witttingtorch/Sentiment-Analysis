Reviews <- read.csv("C:/Users/Liam/Desktop/Reviews.csv")
View(Reviews)
Reviews$Score <-as.factor(Reviews$Score)
str(Reviews)
str(Reviews[-10])
head(Reviews[,10],2)
# Randomly split data and use only 10% of the dataset
set.seed(90)
split =sample.split(Reviews$Score, SplitRatio =0.10)
food =subset(Reviews, split ==TRUE)
select_col <-c("Id","HelpfulnessNumerator","HelpfulnessDenominator",
               "Score","Summary","Text")
food_selected <-Reviews[,select_col]
food_selected[2,6]

library(lsa)
library(SnowballC)
summary(as.factor(food_selected[2,6]),k=1)
# for text mining
library(tm)
food_corpus <-VCorpus(VectorSource(food_selected$Text))
#Standardize the text - Pre-Processing
food_dtm <-DocumentTermMatrix(food_corpus, control
                                             =list(
                                               tolower =TRUE,
                                               removeNumbers =TRUE,
                                               stopwords =TRUE,
                                               removePunctuation =TRUE,
                                               stemming =TRUE
                                             ))
# save frequently-appearing terms( more than 500 times) to a character
food_freq <-findFreqTerms(food_dtm, 500)
# create DTMs with only the frequent terms
food_dtm <-food_dtm[ , food_freq]
tm::inspect(food_dtm[1:5,1:10])
#Create a tf-idf matrix
food_tfidf <-weightTfIdf(food_dtm, normalize
                                   =FALSE)
tm::inspect(food_tfidf[1:5,1:10])
# for text stemming
library(SnowballC)
library("RColorBrewer")
# word-cloud generator
library(wordcloud)
library(slam)
wc_tdm <- rollup(food_dtm,2,na.rm=TRUE,FUN=sum)
matrix_c <-as.matrix(wc_tdm)
wc_freq <-sort(rowSums(matrix_c))
wc_tmdata <-data.frame(words=names(wc_freq), wc_freq)
fine_food_data_corpus <-VCorpus(VectorSource(food_selected$Text))
fine_food_data_text_tdm <-TermDocumentMatrix(fine_food_data_corpus, control
                                             =list(
                                               tolower =TRUE,
                                               removeNumbers =TRUE,
                                               stopwords =TRUE,
                                               removePunctuation =TRUE,
                                               stemming =TRUE
                                             )) 
wc_tdm <-rollup(fine_food_data_text_tdm,2,na.rm=TRUE,FUN=sum)
matrix_c <-as.matrix(wc_tdm)
wc_freq <-sort(rowSums(matrix_c))
wc_tmdata <-data.frame(words=names(wc_freq), wc_freq)
wc_tmdata <-na.omit(wc_tmdata)
wordcloud (tail(wc_tmdata$words,30), tail(wc_tmdata$wc_freq,30),random.order =FALSE, colors=brewer.pal(8, "Dark2"))

library("RColorBrewer")
display.brewer.all()
wordcloud (tail(wc_tmdata$words,30), tail(wc_tmdata$wc_freq,30),random.order =FALSE, colors=brewer.pal(8, "Dark2"))