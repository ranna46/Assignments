#########################tweets extraction############################

library("twitteR")
library("ROAuth")
library(base64enc)
library(httpuv)
#for sentiment analysis
library(rJava)
library(tm)		
library(SnowballC)
library(wordcloud)
library(RWeka)	
library(qdap)		
library(textir)
library(maptpx)
library(data.table)
library(stringr)
library(slam)
library(ggplot2)

cred<-OAuthFactory$new(consumerKey='FXTquJNbgDG2dH81XYVqNZFAb', # Consumer Key (API Key)
                       consumerSecret='3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO', #Consumer Secret (API Secret)
                       requestURL='https://api.twitter.com/oauth/request_token',
                       accessURL='https://api.twitter.com/oauth/access_token',
                       authURL='https://api.twitter.com/oauth/authorize')
#cred$handshake(cainfo="cacert.pem")
save(cred,file="twitter authentication.Rdata")
 
load ("twitter authentication.Rdata")

setup_twitter_oauth("FXTquJNbgDG2dH81XYVqNZFAb", # Consumer Key (API Key)
                    "3y0ALNFzJ8JKyxzFd0ba9FWSUpNSWhPisEIZOB6WCTtcGvP6SO", #Consumer Secret (API Secret)
                    "529590041-qOXLd769cQEUTbXg3iRqCd33pC1K6xoORrGOMJDh",  # Access Token
                    "WlqZJwXFQzf64IuojkbKh1jdT5cnSY8U44pqmz6Sc1d4A")  #Access Token Secret

#registerTwitterOAuth(cred)
Tweets <- userTimeline('@realDonaldTrump', n=1000,includeRts = T)
TweetsDF<- twListToDF(Tweets)
dim(TweetsDF)
View(TweetsDF)
write.csv(TweetsDF, "Trump.csv",row.names = F)
getwd()
########################################################################
#copy tweets from csv to txt file

####################word cloud###########################
# lOADING +VE AND -VE words  
pos.words=scan(file.choose(), what="character", comment.char=";")	# read-in positive-words.txt
neg.words=scan(file.choose(), what="character", comment.char=";") 	# read-in negative-words.txt
pos.words=c(pos.words,"wow", "kudos", "hurray") 			# including our own positive words to the existing list
neg.words = c(neg.words)
stopwdrds = readLines(file.choose())

# Loading the text file to be analysed 
x = readLines(file.choose()) 	

x <- stemDocument(x)

# Preparing corpus from the text document 
x1 = Corpus(VectorSource(x))  	# Constructs a source for a vector as input

x1 = tm_map(x1, stripWhitespace) 	# removes white space
x1 = tm_map(x1, tolower)		# converts to lower case
x1 = tm_map(x1, removePunctuation)	# removes punctuation marks
x1 = tm_map(x1, removeNumbers)		# removes numbers in the documents
x1 = tm_map(x1, removeWords, c(stopwords("english"),stopwdrds))

# Term document frequency matrix
tdm0 <- TermDocumentMatrix(x1)

# Term document matrix with inverse frequency 
tdm1 <- TermDocumentMatrix(x1,control = list(weighting = function(p) weightTfIdf(p,normalize = T)))#,stemming=T))
inspect(tdm1)
a0 <- NULL
a1 <- NULL
# getting the indexes of documents having count of words = 0
for (i1 in 1:ncol(tdm0))
{ if (sum(tdm0[, i1]) == 0) {a0 = c(a0, i1)} }
for (i1 in 1:ncol(tdm1))
{ if (sum(tdm1[, i1]) == 0) {a1 = c(a1, i1)} }

# Removing empty docs 
tdm0 <- tdm0[,-a0]
tdm1 <- tdm1[,-a1]

# Document term matrix 
dtm0 <- t(tdm0)
dtm1 <- t(tdm1)
######################functions#################################
makewordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  windows()
  wordcloud(freq.df$word[1:120], freq.df$freq[1:120],scale = c(4,.5),random.order = F, colors=1:10)
} 

# Making positive wordcloud function 
makeposwordc = function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  pos.matches = match(names(freq), c(pos.words,"approvals"))
  pos.matches = !is.na(pos.matches)
  freq_pos <- freq[pos.matches]
  names <- names(freq_pos)
  windows()
  wordcloud(names,freq_pos,scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}

# Making negatice wordcloud function
makenegwordc = function(x){	
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  # matching positive words
  neg.matches = match(names(freq), neg.words)
  neg.matches = !is.na(neg.matches)
  freq_neg <- freq[neg.matches]
  names <- names(freq_neg)
  windows()
  wordcloud(names[1:120],freq_neg[1:120],scale=c(4,.5),colors = brewer.pal(8,"Dark2"))
}


words_bar_plot <- function(x){
  freq = sort(rowSums(as.matrix(x)),decreasing = TRUE)
  freq.df = data.frame(word=names(freq), freq=freq)
  head(freq.df, 20)
  library(ggplot2)
  windows()
  ggplot(head(freq.df,50), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Words") + ylab("Frequency") +
    ggtitle("Most frequent words")
  
}

pos_words_bar_plot <- function(x){
  pos.matches = match(colnames(x), pos.words)
  pos.matches = !is.na(pos.matches)
  pos_words_freq = as.data.frame(apply(x, 2, sum)[pos.matches])
  colnames(pos_words_freq)<-"freq"
  pos_words_freq["word"] <- rownames(pos_words_freq)
  # Sorting the words in deceasing order of their frequency
  pos_words_freq <- pos_words_freq[order(pos_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(pos_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("Positive words") + ylab("Frequency") +
    ggtitle("Most frequent positive words")
}
neg_words_bar_plot <- function(x){
  neg.matches = match(colnames(x), neg.words)
  neg.matches = !is.na(neg.matches)
  neg_words_freq = as.data.frame(apply(x, 2, sum)[neg.matches])
  colnames(neg_words_freq)<-"freq"
  neg_words_freq["word"] <- rownames(neg_words_freq)
  # Sorting the words in deceasing order of their frequency
  neg_words_freq <- neg_words_freq[order(neg_words_freq$freq,decreasing=T),]
  windows()
  ggplot(head(neg_words_freq,30), aes(reorder(word,freq), freq)) +
    geom_bar(stat = "identity") + coord_flip() +
    xlab("words") + ylab("Frequency") +
    ggtitle("Most frequent negative words")
}
##################################################
# Word cloud - TF - Uni gram
makewordc(tdm0)
title(sub = "UNIGRAM - Wordcloud using TF")

# Frequency Bar plot - TF - Uni gram
words_bar_plot(tdm0)

# Word cloud - TFIDF - Unigram
makewordc(tdm1)

# Frequency Barplot - TFIDF - Unigram
words_bar_plot(tdm1)

# Positive word cloud - TF - Unigram
makeposwordc(tdm0)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TF")

# Frequency Barplot - Positive words - Unigram
pos_words_bar_plot(dtm0)

# Positive word cloud - Unigram - TFIDF
makeposwordc(tdm1)
title(sub = "UNIGRAM - POSITIVE Wordcloud using TFIDF")

# Frequency Barplot - Positive words - TFIDF - Unigram
pos_words_bar_plot(dtm1)

# Negative word cloud - TF - unigam
makenegwordc(tdm0) # doubts doubt 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TF")

# Frequency Barplot -negative words - Unigram - TF
neg_words_bar_plot(dtm0)

# Negative word cloud - Unigram - TFIDF
makenegwordc(tdm1) # doubts doubt 
title(sub = "UNIGRAM - NEGATIVE Wordcloud using TFIDF")

# Frequency Barplot - Negative words - TFIDF
neg_words_bar_plot(dtm1)

#####################sentiment analysis#################
install.packages("syuzhet")
library("syuzhet")
my_example_text <- readLines(file.choose())
s_v <- get_sentences(my_example_text)
class(s_v)
str(s_v)
head(s_v)
sentiment_vector <- get_sentiment(s_v, method = "bing")
head(sentiment_vector)
afinn_s_v <- get_sentiment(s_v, method = "afinn")
head(afinn_s_v)
nrc_vector <- get_sentiment(s_v, method="nrc")
head(nrc_vector)
sum(sentiment_vector)
mean(sentiment_vector)
summary(sentiment_vector)
# plot
plot(sentiment_vector, type = "l", main = "Plot Trajectory",
     xlab = "Narrative Time", ylab = "Emotional Valence")
abline(h = 0, col = "red")
# To extract the sentence with the most negative emotional valence
negative <- s_v[which.min(sentiment_vector)]
negative
# and to extract the most positive sentence
positive <- s_v[which.max(sentiment_vector)]
positive
# more depth
poa_v <- my_example_text
poa_sent <- get_sentiment(poa_v, method="bing")
plot(
  poa_sent, 
  type="h", 
  main="Example Plot Trajectory", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence"
)

# percentage based figures
percent_vals <- get_percentage_values(poa_sent)
plot(
  percent_vals, 
  type="l", 
  main="Throw the ring in the volcano Using Percentage-Based Means", 
  xlab = "Narrative Time", 
  ylab= "Emotional Valence", 
  col="red"
)
ft_values <- get_transformed_values(
  poa_sent, 
  low_pass_size = 3, 
  x_reverse_len = 100,
  scale_vals = TRUE,
  scale_range = FALSE
)
plot(
  ft_values, 
  type ="h", 
  main ="LOTR using Transformed Values", 
  xlab = "Narrative Time", 
  ylab = "Emotional Valence", 
  col = "red"
)
# categorize each sentence by eight emotions
nrc_data <- get_nrc_sentiment(s_v)
# subset
sad_items <- which(nrc_data$sadness > 0)
head(s_v[sad_items])
# To view the emotions as a barplot
barplot(sort(colSums(prop.table(nrc_data[, 1:8]))), horiz = T, cex.names = 0.7,
        las = 1, main = "Emotions", xlab = "Percentage",
        col = 1:8)

######analysis########
#Based on the analysis, there is a lot of trust shown in Trumps tweets followed by
#anticipation and fear amid the chinese virus pandemic