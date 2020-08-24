library(skimr)
sms_raw <- read.csv(choose.files())
View(sms_raw)
sms_raw$type <- factor(sms_raw$type)

# examine type variable more carefully
str(sms_raw$type)
table(sms_raw$type)
skim(sms_raw)
summary(sms_raw)

# build a corpus using tm package
library(tm)
library(SnowballC)
library(caret)
sms_corpus <- VCorpus(VectorSource(sms_raw$text))

# cleaning up corpus data using tm_map()
Corpus_clean <- tm_map(sms_corpus, tolower)
Corpus_clean <- tm_map(Corpus_clean, removeNumbers)
Corpus_clean <- tm_map(Corpus_clean, removeWords, stopwords())
Corpus_clean <- tm_map(Corpus_clean, removePunctuation)
Corpus_clean <- tm_map(Corpus_clean, stripWhitespace)
Corpus_clean <- tm_map(Corpus_clean, PlainTextDocument)

# create a document term matrix
sms_dtm <- DocumentTermMatrix(Corpus_clean)
sms_dtm

# creating training and test data sets
sms_raw_train <- sms_raw[1:4169, ]
sms_raw_test <- sms_raw[4170:5559, ]

sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test <- sms_dtm[4170:5559, ]

sms_corpus_train <- Corpus_clean[1:4169]
sms_corpus_test <- Corpus_clean[4170:5559]

# check proportion of spam is similar
prop.table(table(sms_raw_train$type))
prop.table(table(sms_raw_test$type))

# indicator feature for frequent words
sms_dict <- findFreqTerms(sms_dtm_train, 5)

sms_train <- DocumentTermMatrix(sms_corpus_train, list(dictionary = sms_dict))
sms_test <- DocumentTermMatrix(sms_corpus_test, list(dictionary = sms_dict))

# convert counts to a factor
convert_counts <- function(x) {
  x <- ifelse(x > 0,1,0)
  x <- factor(x, levels = c(0,1), labels = c("No", "Yes"))
}

# apply() convert_counts() to columns of train/test data
sms_train <- apply(sms_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_test, MARGIN = 2, convert_counts)

# training the model on data
library(e1071)
sms_classifier <- naiveBayes(sms_train, sms_raw_train$type)
sms_classifier

# evaluating model performance
sms_test_pred <- predict(sms_classifier, sms_test)

library(gmodels)
CrossTable(sms_test_pred, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))

confusionMatrix(sms_test_pred,sms_raw_test$type) #97.41%

###
sms_classifier2 <- naiveBayes(sms_train, sms_raw_train$type, laplace = 1)
sms_test_pred2 <- predict(sms_classifier2, sms_test)
CrossTable(sms_test_pred2, sms_raw_test$type,
           prop.chisq = FALSE, prop.t = FALSE, prop.r = FALSE,
           dnn = c('predicted', 'actual'))
confusionMatrix(sms_test_pred2,sms_raw_test$type) #97.55%








