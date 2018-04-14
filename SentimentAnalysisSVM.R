
library(devtools)
library(twitteR)
library(httr)
library(sentiment)
library(DT)
library(devtools)
library(wordcloud)

library(ggplot2)
library(rjson)
library(bigmemory)
library(biganalytics)
library(bigtabulate)
library(stringr)
library(jsonlite)
library(e1071)
library(caret)
library(RTextTools)
library(tm)
library(dplyr)
library(wordcloud)
library(bnlearn)
library(doMC)

# Execute below function to return scores of the tweets
score.sentiment = function(sentences, pos.words, neg.words, .progress='none')
{
  require(plyr)
  require(stringr)
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an “l” for us
  # we want a simple array (“a”) of scores back, so we use 
  # “l” + “a” + “ply” = “laply”:
  
  sentences = gsub('[[:punct:]]', '', sentences)
  sentences = gsub('[[:cntrl:]]', '', sentences)
  sentences = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', sentences)
  sentences = gsub('@\\w+', '', sentences)
  sentences = gsub('[[:punct:]]', '', sentences)
  sentences = gsub('[[:digit:]]', '', sentences)
  sentences = gsub('http\\w+', '', sentences)
  sentences = gsub('[ \t]{2,}', '', sentences)
  sentences = gsub('^\\s+|\\s+$', '', sentences)
  sentences = gsub('\\d+', '', sentences)
  try.error = function(x)
  {
    y = NA
    try_error = tryCatch(tolower(x), error=function(e) e)
    if (!inherits(try_error, 'error'))
      y = tolower(x)
    return(y)
  }
  sentences = sapply(sentences, try.error)
  sentences = sentences[!is.na(sentences)]
  names(sentences) = NULL
  # and convert to lower case:
  sentences = tolower(sentences)
  
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('(RT|via)((?:\\b\\W*@\\w+)+)', '', sentence)
    sentence = gsub('@\\w+', '', sentence)
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:digit:]]', '', sentence)
    sentence = gsub('http\\w+', '', sentence)
    sentence = gsub('[ \t]{2,}', '', sentence)
    sentence = gsub('^\\s+|\\s+$', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    try.error = function(x)
    {
      y = NA
      try_error = tryCatch(tolower(x), error=function(e) e)
      if (!inherits(try_error, 'error'))
        y = tolower(x)
      return(y)
    }
    sentence = sapply(sentence, try.error)
    sentence = sentence[!is.na(sentence)]
    names(sentence) = NULL
    # and convert to lower case:
    sentence = tolower(sentence)
    
    # split into words. str_split is in the stringr package
    word.list = str_split(sentence,'\\s+')
    # sometimes a list() is one level of hierarchy too much
    words = unlist(word.list)
    
    # compare our words to the dictionaries of positive & negative terms
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    
    # match() returns the position of the matched term or NA
    # we just want a TRUE/FALSE:
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)

    # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}




api_key <- "t4f2zQp9044P92l5J6PCz9wW2"
api_secret <- "cEKCFf9YSRYMnqMnKXizZ4RKB8dixPdGXgdtYmLYrY1xYY4HAV"
access_token <- "2717578724-rzBS0rZcNBg6xecEpFukQTFP4DVd4adeOgrhGHT"
access_token_secret <- "MeUkekEJ2Hu5f8uD9YECWBMhjJ9YdQ3mC9H7jbcbd00l0"
setup_twitter_oauth(api_key,api_secret,access_token,access_token_secret)

#Step1: Tweets Extraction
pos.words <- read.csv("positive-words.txt")
neg.words <- read.csv("negative-words.txt")

pos.words <- scan("positive-words.txt",what = 'character')
neg.words <- scan("negative-words.txt",what = 'character')

pos.words = c(pos.words, 'new','nice' ,'good', 'horizon')
neg.words = c(neg.words, 'behind','feels', 'ugly', 'back','worse' , 'shitty', 'bad', 'no','freaking','sucks','horrible')

modi_tweets <- searchTwitter("$narendramodi", n = 1000,lang='en')


require(plyr)
test <- ldply(modi_tweets,function(t) t$toDataFrame() )
result <- score.sentiment(test$text,pos.words,neg.words)
summary(result$score)
count(result$score)
qplot(result$score,xlab = "Score of tweets")

#store score of each tweet, polarity will be our classes for SVM and corresponding tweets in a data frame
namo_group_df=data.frame(score=result$score,polarity= result$score, tweets= result$text)

# SVM defined classes Neutral = 0, Negative = -1 and Positive = 1
# score 0 is class 0
# score 1 and above is class 1
# score less than 0 is class -1
for (i in 1:NROW(namo_group_df)){
eachScore= namo_group_df[i,"score"]
if(eachScore == 0){
  namo_group_df[i,"polarity"] = "0"
} else if(eachScore == 1){
  namo_group_df[i,"polarity"] = "1"
} else if(eachScore == 2){
  namo_group_df[i,"polarity"] = "1"
}
else if(eachScore == 3){
  namo_group_df[i,"polarity"] = "1"
}else if(eachScore == 4){
  namo_group_df[i,"polarity"] = "1"
}
else if(eachScore == -1){
  namo_group_df[i,"polarity"] = "-1"
} 
else if(eachScore == -2){
  namo_group_df[i,"polarity"] = "-1"
} 
else if(eachScore == -3){
  namo_group_df[i,"polarity"] = "-1"
}
else if(eachScore == -4){
  namo_group_df[i,"polarity"] = "-1"
}
}
qplot(namo_group_df$score, data=namo_group_df,color=namo_group_df$polarity)
x <- subset(namo_group_df, select=-namo_group_df$polarity)
y <- namo_group_df$polarity
namo_group_df
write.csv(namo_group_df,"NamoResults.csv")

# SVM Classficiation starts
# SVM classification needs classes, training set and testing set

#partition the data into two sets where 70% of the data will be assigned as training set and rest is testing set
intrain <- createDataPartition(y = namo_group_df$polarity, p= 0.7, list = FALSE)
#below is training set
training <- namo_group_df[intrain,]
#below is testing set
testing <- namo_group_df[-intrain,]

trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)

# polarity column in the namo_group_df is used for classes as mentioned above
# As there are three classes and one outlier which is -5 in the classes. SVM will have two hyper planes
svm_Linear <- train(polarity ~., data = training, method = "svmLinear",
                    trControl=trctrl,
                    preProcess = c("center", "scale"),
                    tuneLength = 10)

# View SVM results
print(svm_Linear)

# Now using the SVM analysis predict the data using testing data
test_pred <- predict(svm_Linear, newdata = testing)

# view prediction results
# this is used for comparing the polarity generated using Bayes algoritm
plot(test_pred)

#View predicted results against training classes
plot(testing$polarity,test_pred)
points(testing$polarity,test_pred,col="blue", pch=4)


