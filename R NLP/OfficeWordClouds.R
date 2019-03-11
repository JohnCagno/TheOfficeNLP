library(tidyverse)  
library(tidytext)  
library(topicmodels)  
library(tm)  
library(dplyr)
library(stringdist)
library(digest)
library(stringr)

#import data
setwd("C:/Users/Johnny/Documents/UNH Notes and Documents 2018/Spring 2018/NLP and Time Series Analysis")
office <- read.csv("OfficeNLP_Final2.csv")

#Creating a corpus
library(tm)
#making a corpus of a vector source 
office <- filter(office, office$Name == "dwight")
review_corpus <- VCorpus(VectorSource(office$Line))
print(review_corpus)



###############################################################

#Cleaning corpus - pre_processing
clean_corpus <- function(corpus){
  cleaned_corpus <- corpus
  cleaned_corpus <- tm_map(cleaned_corpus, removePunctuation)
  cleaned_corpus <- tm_map(cleaned_corpus, removeNumbers)
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  #custom_stop_words <- c()
  #cleaned_corpus <- tm_map(cleaned_corpus, removeWords, custom_stop_words)
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
}

#apply the clean_corpus function on your review_corpus 
cleaned_review_corpus <- clean_corpus(review_corpus)

########### TDM/DTM########
TDM_reviews <- TermDocumentMatrix(cleaned_review_corpus)
#TDM_reviews <- removeSparseTerms(TDM_reviews, sparse = 0.2)
TDM_reviews_m <- as.matrix(TDM_reviews)

# Term Frequency
term_frequency <- rowSums(TDM_reviews_m)
# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)

############Word Cloud
library(wordcloud)
# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=20,max.words=500,colors=brewer.pal(8, "Paired"))

##############bigrams and trigrams
#create a structure for bi-gram 
library(RWeka)
tokenizer <- function(x)
  NGramTokenizer(x,Weka_control(min=2,max=2))
bigram_tdm <- TermDocumentMatrix(cleaned_review_corpus,control = list(tokenize=tokenizer))
bigram_tdm_m <- as.matrix(bigram_tdm)

# Term Frequency
term_frequency <- rowSums(bigram_tdm_m)
# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)
############Word Cloud
# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=500,colors=brewer.pal(8, "Paired"))



#create a structure for tri-gram 
tokenizer <- function(x)
  NGramTokenizer(x,Weka_control(min= 3,max=3))
trigram_tdm <- TermDocumentMatrix(cleaned_review_corpus,control = list(tokenize=tokenizer))
trigram_tdm_m <- as.matrix(trigram_tdm)

# Term Frequency
term_frequency <- rowSums(trigram_tdm_m)
# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)
############Word Cloud
# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,max.words=500,colors=brewer.pal(8, "Paired"))



##########tf-idf weighting
tfidf_tdm <- TermDocumentMatrix(cleaned_review_corpus,control=list(weighting=weightTfIdf))
tfidf_tdm_m <- as.matrix(tfidf_tdm)
# Term Frequency
term_frequency <- rowSums(tfidf_tdm_m)
# Sort term_frequency in descending order
term_frequency <- sort(term_frequency,dec=TRUE)
############Word Cloud
library(wordcloud)
# Create word_freqs
word_freqs <- data.frame(term = names(term_frequency), num = term_frequency)
# Create a wordcloud for the values in word_freqs
wordcloud(word_freqs$term, word_freqs$num,min.freq=5,random.order = FALSE, max.words=3000,colors=brewer.pal(8, "Paired"))

