rm(list=ls())
install.packages("twitteR")
install.packages("RCurl")
api_key<- "************************"
api_secret<-"***********************"
access_token<- "********************"
access_token_secret<- "*******************"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)



twm<-laply(searchTwitter("world cup 2019", n=100), statusText)
names(twm) = "comments"
postCorpus = VCorpus(VectorSource(twm))
#load libraries
library(stringr)
library(tm)
library(wordcloud)
library(slam)
library(sentimentr)
writeLines(as.character(postCorpus[[1]]))

#case folding
postCorpus = tm_map(postCorpus, tolower)

#remove stop words
postCorpus = tm_map(postCorpus, removeWords, stopwords('english'))

#remove punctuation marks
postCorpus = tm_map(postCorpus, removePunctuation)

#remove numbers
postCorpus = tm_map(postCorpus, removeNumbers)

#remove unnecesary spaces
postCorpus = tm_map(postCorpus, stripWhitespace)

# #convert into plain text
postCorpus = tm_map(postCorpus, PlainTextDocument)
# 

tweetss<- postCorpus
# #create corpus
postCorpus = VCorpus(VectorSource(postCorpus))

#Build document term matrix
tdm = TermDocumentMatrix(postCorpus)
#tdm_min = TermDocumentMatrix(postCorpus, control=list(weighting=weightTfIdf, minWordLength=4, minDocFreq=10))

#Convert term document matrix into dataframe
TDM_data = as.data.frame(t(as.matrix(tdm))) 

##calculate the terms frequency
words_freq = rollup(tdm, 2, na.rm=TRUE, FUN = sum)

#Convert into matrix
words_freq = as.matrix(words_freq)

#Convert to proper dataframe
words_freq = data.frame(words_freq)

#Convert row.names into index
words_freq$words = row.names(words_freq)
row.names(words_freq) = NULL
words_freq = words_freq[,c(2,1)]
names(words_freq) = c("Words", "Frequency")

#Most frequent terms which appears in atleast 700 times
findFreqTerms(tdm, 100)

##wordcloud
postCorpus_WC = postCorpus
setwd("D:\\Data Science\\Advance Concepts\\Text_Mining")

pal2 = brewer.pal(8,"Dark2")
png("wordcloud_v3.png", width = 12, height = 8, units = 'in', res = 300)
wordcloud(postCorpus_WC, scale = c(5,.2), min.freq = 5, max.words = 150, random.order = FALSE, rot.per = .15, colors = pal2)
dev.off()

#Remove the defined stop words
postCorpus_WC = tm_map(postCorpus_WC, removeWords, c('will', 'also', 'can',
                                                     stopwords('english')))
postCorpus_WC = tm_map(postCorpus_WC, removeWords, stop_words$StopWords)
