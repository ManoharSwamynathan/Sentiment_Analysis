# Sentiment_Analysis

## Definition:

The process of computationally identifying and categorizing opinions expressed in a piece of text, especially in order to determine whether the writer's attitude towards a particular topic, product, etc. is positive, negative, or neutral

## Usecase:

Customer's online comments/feedback from an insurance companies website has been scrapped to run through the sentiment analysis

## Reference:

This is based on the tutorials from following links:

* https://sites.google.com/site/miningtwitter/questions/sentiment/sentiment
* http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/
* https://github.com/benmarwick/AAA2011-Tweets/blob/master/AAA2011.R

## Steps:

* Load data to R
* preprocess data
	+ convert to lower: this is to avoid distinguish between words simply on case
	+ remove punctuation: punctuation can provide grammatical context which supports understanding. Often for initial analyses we ignore the punctuation
	+ remove numbers: numbers may or may not be relevant to our analyses
	+ remove stopwords: stop words are common words found in a language. Words like for, of, are etc are common stop word
	+ create document term matrix: a document term matrix is simply a matrix with documents as the rows and terms as the columns and a count of the frequency of words as the cells of the matrix
	
* Insight through visualization
  + Word cloud
  + Frequency plot
  + Correlation plot
  + Paired word cloud

* Sentiment Score
  + Load Positive / Negative terms corpus
  + Calculate positive / negative score
  + Classify emotion
  + Classify polarity
  + Visualize
      + Distribution of overall score
      + Distribution of score for a given term
      + Distribution of emotion
      + Distribution of polarity
      + Text by emotion


Related Blog: https://manoharswamynathan.wordpress.com/2016/08/06/sentiment-analysis/
