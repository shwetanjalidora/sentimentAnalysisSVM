# Sentiment analysis using SVM
## We use the tweets of Donald Trump to classify the polarity
### Here are the steps involved
- Download tweets from the twitter
- Clean the tweets
- Download postive words and negative words
- Score each tweet by matching number of words within the list of postive words and negative words
- Store all tweets with corresponding scores in a data frame
- Assign 1 to all positive score tweets
- Assign -1 to all negative score tweets
- Leave 0 as is
- Now we will have three classes available
#### Now that we prepared the data, lets proceed with SVM analysis
- Partition the data for training and test data
- Train the SVM using SVM linear method
- predict the data using test data
- Generate the confusion matrix to test the accuracy of the classifier

Confusion Matrix and Statistics generated based on SVM classifier.

          Reference
Prediction -1  0  1
        -1  0  0  0
        0   7 14  7
        1   0  0  1

In the above example, we have three classes: -1,0,1 and each corresponds to negative, nuetral and positive classes.
The above confusion matrix can be read like this.
1. SVM classified 7 tweets as neutral where as the actual data has 7 tweets as negative
2. SVM classified 14 tweets as nuetral where as the actual data has 14 tweets as neutral
3. SVM classified 1 tweet as positive where as the actual data has 1 tweet as positive

In summary, the values inside the confusion matrix diagonal is true positives and true negatives. 
Values outside the diagonal are false positives and false negatives and they are errors.
