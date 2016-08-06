# This is based on the tutorials of following links
# https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment
# http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/
# https://github.com/benmarwick/AAA2011-Tweets/blob/master/AAA2011.R

setwd("/home/manohar/Documents/Sentiment_Analysis/script")

library(wordcloud)
library(tm)
library(plyr)
library(ggplot2)
library(grid)
library(sentiment)
library(Rgraphviz)

# source("http://bioconductor.org/biocLite.R")
# biocLite("Rgraphviz")
# install.packages('tm')
# install.packages('wordcloud')
# download.file("http://cran.cnr.berkeley.edu/src/contrib/Archive/Rstem/Rstem_0.4-1.tar.gz", "Rstem_0.4-1.tar.gz")
# install.packages("Rstem_0.4-1.tar.gz", repos=NULL, type="source")
# download.file("http://cran.r-project.org/src/contrib/Archive/sentiment/sentiment_0.2.tar.gz", "sentiment.tar.gz")
# install.packages("sentiment.tar.gz", repos=NULL, type="source")

df <- read.table("../input/data.csv",sep=",",header=TRUE)
corp <- Corpus(VectorSource(df$Review)) 
corp <- tm_map(corp, tolower) 
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
# corp <- tm_map(corp, stemDocument, language = "english") 
corp <- tm_map(corp, removeWords, c("the", stopwords("english"))) 
corp <- tm_map(corp, PlainTextDocument)
corp.tdm <- TermDocumentMatrix(corp, control = list(minWordLength = 3)) 
corp.dtm <- DocumentTermMatrix(corp, control = list(minWordLength = 3)) 

wordcloud(corp, scale=c(5,0.5), max.words=100, random.order=FALSE, rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, 'Dark2'))

findFreqTerms(corp.tdm, lowfreq=30)
findAssocs(corp.tdm, 'claim', 0.2)

corp.tdm.df <- data.frame(inspect(corp.tdm))

corp.tdm.df <- sort(rowSums(corp.tdm.df),decreasing=TRUE) # populate term frequency and sort in decesending order
df.freq <- data.frame(word = names(corp.tdm.df),freq=corp.tdm.df) # Table with terms and frequency

# Set minimum term frequency value. The charts will be created for terms > or = to the minimum value that we set.
freqControl <- 100

# Frequency Plot
freqplotData <- subset(df.freq, df.freq$freq > freqControl)
freqplotData$word <- ordered(freqplotData$word,levels=levels(freqplotData$word)[unclass(freqplotData$word)])
freqplot <- ggplot(freqplotData,aes(reorder(word,freq), freq))
freqplot <- freqplot + geom_bar(stat="identity")
freqplot <- freqplot + theme(axis.text.x=element_text(angle=90,hjust=1)) + coord_flip() 
freqplot + xlim(rev(levels(freqplotData$word)))+ ggtitle("Frequency Plot")

# Wordcloud
# To change the proportion of words that are rotated by 90 degrees from the 20%, change option rot.per=0.2 appropriately
dark2 <- brewer.pal(6,"Dark2")
wordcloud(df.freq$word,df.freq$freq,min.freq=freqControl,random.order=FALSE, rot.per=0.2,colors=dark2,main="Corpus wordcloud")

# Correlation Plot
# 50 of the more frequent words has been chosen as the nodes and include links between words
# when they have at least a correlation of 0.5
# By default (without providing terms and a correlation threshold) the plot function chooses a
# random 20 terms with a threshold of 0.7
plot(corp.tdm,terms=findFreqTerms(corp.tdm,lowfreq=freqControl)[1:50],corThreshold=0.2, main="Correlation Plot")


# Paired-Terms wordcloud
# pick the top N most frequent words and extract associated words with strong correlation (say 70%). 
# Combine individual top N words with every associated word.
nFreqTerms <- findFreqTerms(corp.dtm,lowfreq=freqControl)
nFreqTermsAssocs <- findAssocs(corp.dtm, nFreqTerms, 0.3)
pairedTerms <- c()
for (i in 1:length(nFreqTermsAssocs)){
  if(length(names(nFreqTermsAssocs[[i]]))!=0) 
    lapply(names(nFreqTermsAssocs[[i]]),function(x) pairedTerms <<- c(pairedTerms,paste(names(nFreqTermsAssocs[i]),x,sep="-")))
}
wordcloud(pairedTerms,random.order=FALSE,colors=dark2,main="Paired Wordcloud")

hu.liu.pos = scan('../input/positive-words.txt', what = 'character',comment.char=';') 
hu.liu.neg = scan('../input/negative-words.txt',what = 'character',comment.char= ';') 
pos.words = c(hu.liu.pos)
neg.words = c(hu.liu.neg)
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use
  # "l" + "a" + "ply" = "laply":
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    # clean up sentences with R's regex-driven global substitute, gsub():
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence, '\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches= !is.na(pos.matches)
    neg.matches= !is.na(neg.matches)
    
    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}

review.scores<- score.sentiment(df$Review,pos.words,neg.words,.progress='text') 

ggplot(review.scores, aes(x=score)) + 
  geom_histogram(binwidth=1) + 
  xlab("Sentiment score") + 
  ylab("Frequency") + 
  theme_bw()  + 
  theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + 
  theme(axis.title.y=element_text(size = 14, angle=90, vjust = -0.25)) + 
  theme(plot.margin = unit(c(1,1,2,2), "lines"))

review.pos<- subset(review.scores,review.scores$score>= 2) 
review.neg<- subset(review.scores,review.scores$score<= -2)
claim <- subset(review.scores, regexpr("claim", review.scores$text) > 0) 
ggplot(claim, aes(x = score)) + geom_histogram(binwidth = 1) + xlab("Sentiment score for the token 'claim'") + ylab("Frequency") + theme_bw()  + theme(axis.title.x = element_text(vjust = -0.5, size = 14)) + theme(axis.title.y = element_text(size = 14, angle = 90, vjust = -0.25)) + theme(plot.margin = unit(c(1,1,2,2), "lines"))

#classify emotion
class_emo = classify_emotion(df$Review, algorithm="bayes", prior=1.0)
#get emotion best fit
emotion = class_emo[,7]
# substitute NA's by "unknown"
emotion[is.na(emotion)] = "unknown"

# classify polarity
class_pol = classify_polarity(df$Review, algorithm="bayes")

# get polarity best fit
polarity = class_pol[,4]

# data frame with results
sent_df = data.frame(text=df$Review, emotion=emotion, polarity=polarity, stringsAsFactors=FALSE)

# sort data frame
sent_df = within(sent_df, emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))

# plot distribution of emotions
ggplot(sent_df, aes(x=emotion)) +
  geom_bar(aes(y=..count.., fill=emotion)) +
  scale_fill_brewer(palette="Dark2") +
  labs(x="emotion categories", y="number of Feedback", 
       title = "Sentiment Analysis of Feedback about claim(classification by emotion)",
       plot.title = element_text(size=12))

# plot distribution of emotions
ggplot(sent_df, aes(x=polarity)) +
  geom_bar(aes(y=..count.., fill=polarity)) +
  scale_fill_brewer(palette="RdGy") +
  labs(x="emotion categories", y="number of Feedback", 
       title = "Sentiment Analysis of Feedback about claim(classification by emotion)",
       plot.title = element_text(size=12))

# separating text by emotion
emos = levels(factor(sent_df$emotion))
nemo = length(emos)
emo.docs = rep("", nemo)
for (i in 1:nemo)
{
  tmp = df$Review[emotion == emos[i]]
  emo.docs[i] = paste(tmp, collapse=" ")
}

# remove stopwords
emo.docs = removeWords(emo.docs, stopwords("english"))
# create corpus
corpus = Corpus(VectorSource(emo.docs))
tdm = TermDocumentMatrix(corpus)
tdm = as.matrix(tdm)
colnames(tdm) = emos

# comparison word cloud
comparison.cloud(tdm, colors = brewer.pal(nemo, "Dark2"),
                 scale = c(3,.5), random.order = FALSE, title.size = 1.5)
