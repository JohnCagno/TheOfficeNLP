library(sentimentr)

setwd("C:/Users/Johnny/Documents/UNH Notes and Documents 2018/Spring 2018/NLP and Time Series Analysis")
mytext <- read.csv("OfficeNLP_Final2.csv")
mytext <- filter(mytext, mytext$Name == "dwight")
mytext <- data.frame(lapply(mytext, as.character), stringsAsFactors=FALSE)
mytext$Sentiment <- sentiment(mytext$Line)$sentiment 


library(radarchart)
library(tm)
library(dplyr)
library(tidyr)
library(tidytext)

#############################################3333333
clean_corpus <- function(cleaned_corpus){
  cleaned_corpus <- tm_map(cleaned_corpus, removeWords, stopwords("english"))
  cleaned_corpus <- tm_map(cleaned_corpus, stripWhitespace)
  return(cleaned_corpus)
}
##############################################3
review_corpus <- VCorpus(VectorSource(mytext$Line))
cleaned_review_corpus <- clean_corpus(review_corpus)

tidy_mytext <- tidy(TermDocumentMatrix(cleaned_review_corpus))

nrc_lex <- get_sentiments("nrc")
mytext_nrc <- inner_join(tidy_mytext, nrc_lex, by = c("term" = "word"))
mytext_nrc_noposneg <- mytext_nrc[!(mytext_nrc$sentiment %in% c("positive","negative")),]

emotion_summary <- mytext_nrc_noposneg %>%
  group_by(document,sentiment) %>%
  summarize(review_sentiment = sum(count)) %>%
  arrange(desc(review_sentiment))

emotion_overall_summary <- mytext_nrc_noposneg %>%
  group_by(sentiment) %>%
  summarize(review_sentiment = sum(count)) %>%
  arrange(desc(review_sentiment))

chartJSRadar(emotion_overall_summary)
