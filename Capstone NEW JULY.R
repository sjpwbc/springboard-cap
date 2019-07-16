# To set this R script as your working directory:
setwd("~/Desktop/DATA SCIENCE BOOTCAMP/CAPSTONE PROJECT")

# To open the original csv file and rename it to save it as a clean version:
USvideos_new <- read.csv("USvideos.csv", header = TRUE, stringsAsFactors = FALSE)

# Packages
library(readr)
library(dplyr)
library(tidyr)
library(scales)
library(anytime)
library(tm)
library(SnowballC)
library(caTools)
library(rpart)
library(rpart.plot)
library(ROCR)
library(pROC)
library(caret)
library(e1071)
library(randomForest)
library(data.table)
library(treemap)
library(scales)
library(gplots)
library(ggplot2)

# To change the "category_id" varible to be more readable, simply change the id code numbers into the category names.
# Make a new "categories" column. 
USvideos_new <- mutate(USvideos_new, categories = 0)

# Then replace the 0 placeholder with the label name for the matching "category_id" variable.
USvideos_new <- USvideos_new %>%
  mutate(categories = ifelse(grepl("1", category_id), "Film & Animation", categories)) %>%
  mutate(categories = ifelse(grepl("2", category_id), "Autos & Vehicles", categories)) %>%
  mutate(categories = ifelse(grepl("10", category_id), "Music", categories)) %>%
  mutate(categories = ifelse(grepl("15", category_id), "Pets & Animals", categories)) %>%
  mutate(categories = ifelse(grepl("17", category_id), "Sports", categories)) %>%
  mutate(categories = ifelse(grepl("18", category_id), "Short Movies", categories)) %>%
  mutate(categories = ifelse(grepl("19", category_id), "Travel & Events", categories)) %>%
  mutate(categories = ifelse(grepl("20", category_id), "Gaming", categories)) %>%
  mutate(categories = ifelse(grepl("21", category_id), "Videoblogging", categories)) %>%
  mutate(categories = ifelse(grepl("22", category_id), "People & Blogs", categories)) %>%
  mutate(categories = ifelse(grepl("23", category_id), "Comedy", categories)) %>%
  mutate(categories = ifelse(grepl("24", category_id), "Entertainment", categories)) %>%
  mutate(categories = ifelse(grepl("25", category_id), "News & Politics", categories)) %>%
  mutate(categories = ifelse(grepl("26", category_id), "How To & Style", categories)) %>%
  mutate(categories = ifelse(grepl("27", category_id), "Education", categories)) %>%
  mutate(categories = ifelse(grepl("28", category_id), "Science & Technology", categories)) %>%
  mutate(categories = ifelse(grepl("29", category_id), "Nonprofits & Activism", categories)) %>%
  mutate(categories = ifelse(grepl("30", category_id), "Movies", categories)) %>%
  mutate(categories = ifelse(grepl("31", category_id), "Anime & Animation", categories)) %>%
  mutate(categories = ifelse(grepl("32", category_id), "Action/Adventure", categories)) %>%
  mutate(categories = ifelse(grepl("33", category_id), "Classics", categories)) %>%
  mutate(categories = ifelse(grepl("34", category_id), "Comedy", categories)) %>%
  mutate(categories = ifelse(grepl("35", category_id), "Documentary", categories)) %>%
  mutate(categories = ifelse(grepl("36", category_id), "Drama", categories)) %>%
  mutate(categories = ifelse(grepl("37", category_id), "Family", categories)) %>%
  mutate(categories = ifelse(grepl("38", category_id), "Foreign", categories)) %>%
  mutate(categories = ifelse(grepl("39", category_id), "Horror", categories)) %>%
  mutate(categories = ifelse(grepl("40", category_id), "Sci-Fi/Fantasy", categories)) %>%
  mutate(categories = ifelse(grepl("41", category_id), "Thriller", categories)) %>%
  mutate(categories = ifelse(grepl("42", category_id), "Shorts", categories)) %>%
  mutate(categories = ifelse(grepl("43", category_id), "Shows", categories)) %>%
  mutate(categories = ifelse(grepl("44", category_id), "Trailers", categories))
  
# To code the "trending_date" variable to separate the year, day, and month into their own columns:
USvideos_new <- separate(USvideos_new, trending_date, c("trending_year", "trending_day", "trending_month"))

# Add 20 to "trending_year" to match "publish_year".
USvideos_new <- USvideos_new %>%
  mutate(trending_year = ifelse(grepl("17", trending_year, ignore.case = TRUE), "2017", trending_year)) %>%
  mutate(trending_year = ifelse(grepl("18", trending_year, ignore.case = TRUE), "2018", trending_year))

# To code the "publish_time" variable, create a new column called "publish_date" (dataframe$columnname) that allows you to then use the separate function.
# Separate the year, month, and day into their own columns.
# Then you will have both trending and publish variables with year, day, and month in their own columns to compare them to each other. 
# Notice that 'time' is removed because it was only in the original "publish_time" variable and not in the original "trending_date" variable.
# Also note that the order for 'day' and 'month' are reversed for the 'trending' and 'publish' variables.
USvideos_new$publish_date <- anytime(USvideos_new$publish_time)

USvideos_new <- separate(USvideos_new, publish_date, c("publish_year", "publish_month", "publish_day"))

# Now recombine the "trending_year", "trending_month", "trending_day" into a single variable to compare to "publish_year", "publish_month", "publish_day" as a single variable.
USvideos_new$trend_date_combine <- with(USvideos_new, paste0(trending_year, trending_month, trending_day))

USvideos_new$trend_date_official <- anytime(USvideos_new$trend_date_combine)

USvideos_new$publish_date_combine <- with(USvideos_new, paste0(publish_year, publish_month, publish_day))

USvideos_new$publish_date_official <- anytime(USvideos_new$publish_date_combine)

# Create "popular" measure (to be used as output, dependent variable) by combining multiple factors into this single measure. 
# First, some videos had 0 comments and likes, because both can be disabled. Replace the disabled 0 counts with the average # for comment_count (8447) and the average for likes (74267). 
# Do this for videos where the comments and ratings were marked as disabled in the dataset ("comments_disabled", "ratings_disabled").
USvideos_new %>% summarise(avg_comment_count = mean(comment_count))

USvideos_new <- USvideos_new %>%
  mutate(comment_count = ifelse(grepl("TRUE", comments_disabled, ignore.case = TRUE), "8447", comment_count))

# Change back from character to integer
USvideos_new$comment_count <- as.integer(USvideos_new$comment_count)

USvideos_new %>% summarise(avg_likes = mean(likes))

USvideos_new <- USvideos_new %>%
  mutate(likes = ifelse(grepl("TRUE", ratings_disabled, ignore.case = TRUE), "74267", likes))

# Change back from character to integer
USvideos_new$likes <- as.integer(USvideos_new$likes)

# Second, to see if any of the factors ("views", "likes", "comment_count") need to be transformed to pull in any outliers, look at the shape of the data using a histogram.
hist(USvideos_new$views)
hist(USvideos_new$likes)
hist(USvideos_new$comment_count)
hist(USvideos_new$popular)
hist(USvideos_new$popular_norm, main = "Normalized Popularity Measure", xlab = "popularity", ylab = "frequency", border = "purple", col = "cyan")

# Because the "popular" variable results in a highly skewed metric, let's look at where the data was split at a threshold of 0.5
USvideos_new$popularity <- USvideos_new$popular_video
USvideos_new$popularity <- as.integer(USvideos_new$popularity)
hist(USvideos_new$popularity, main = "Popularity Measure at Threshold 0.5", xlab = "popularity", ylab = "frequency", border = "purple", col = "cyan")
table(USvideos_new$popularity) # FALSE (unpopular videos) = 25748 and TRUE (popular videos) = 15201, which are more reasonably balanced classes

# Now it's time to create a formula for "popular". To do this, sum "views", "likes", and "comment_count" together.
# A sensible weight must be chosen to assign to each factor in the total "popular" score. To determine weighting factors, use business logic derived from outside sources. 
# "likes" tend to be the most valuable indicator of popularity because a viewer is actively showing their approval of the video 
# "views" may be the second most valuable indicator of popularity because the more views a video receives, the more exposure it gets
# However, YouTube measures "total watch time"; therefore, it gives more weight to videos that a viewer watches for a longer period of time. 
# So even if Video A had twice as many views as Video B, if Video B was 5 times longer than Video A, Video B would still be ranked higher.
# "comment_count" is the least most valuable indicator of popularity because it may represent negative sentiments, which would signal a viewer's disapproval of the video
# Video comments have shown to have a very strong correlation with rankings, but not a significant relationship as views and likes do (See Backlinko article.)
# Therefore, this project will double the weight of "likes", keep "views" the same, and decrease the weight of "comment_count" by half to determine relative popularity for each video.
# Here is the formula:
USvideos_new$popular <- 1*USvideos_new$views + 2*USvideos_new$likes + 0.5*USvideos_new$comment_count

# Once the formula is built to generate the "popular" measure, then normalize so all observations are on the same scale between 0 to 100.
USvideos_new$popular_norm <- rescale(USvideos_new$popular, to = c(0, 100))

# To use the "popular" measure as a categorical, dependent variable for a classification model, apply a threshold to determine whether a video is popular or unpopular.
# The threshold is based on business logic. When looking at the data, if the cut off was 50, only 34 out of the total 40,949 videos would be considered popular.
# This is a very strict threshold and wouldn't provide enough useful data to build an accurate predictive model.
# Again, after looking at the data, a cut off of only 1 would show 8,712 videos as being popular. However, the majority of these videos have at least 2 million views, which is quite a bit.
# It's reasonable to think that popular videos could include those with at least 1 million views. Therefore, the threshold will be lowered to 0.5. 
# Now, when looking at the dataset, most videos at or above the applied threshold of 0.5 have 1 million "views", a 5-figure "like" count, and a 4-figure "comment_count".
# If a video is popular, it will have a score of 0.5 or above.
# If a video is unpopular, it will have a score of below 0.5.
# So, 15,201 videos are "popular" while 25,748 videos are "unpopular".
USvideos_new$popular_video = as.factor(USvideos_new$popular_norm >= 0.5)
table(USvideos_new$popular_video)

# To perform text analysis on the "tags" factor (independent variable), begin by pre-processing the data to use the "Bag of Words" technique.
# The "Bag of Words" technique transforms text into independent variables.
# First, remove the quotes.
USvideos_new$tags <- gsub('"', "", USvideos_new$tags)
# Second, remove the "|" symbol and replace with a blank space.
USvideos_new$tags <- gsub("\\|", " ", USvideos_new$tags)
# Third, remove the "/" symbol and replace with a blank space.
USvideos_new$tags <- gsub("\\/", " ", USvideos_new$tags)

# Now we need to convert our "tags" to a corpus (a collection of documents) for pre-processing. 
corpus = Corpus(VectorSource(USvideos_new$tags))

# Now we are ready to pre-process our data. Pre-processing is easy in "tm". Each operation like stemming or removing stop words can be done with one line in R using the "tm_map" function.

# Remove all capitalization and convert to all lowercase
corpus = tm_map(corpus, tolower)

# Remove all punctuation
corpus = tm_map(corpus, removePunctuation)

# Remove all English stop words (Ex: For the original tag line for video 1, "this" and "is" were removed.) Limitation: Some words are in other languages like Korean.
corpus = tm_map(corpus, removeWords, c(stopwords("english")))

# Lastly, stem the document to remove plurals and multiple endings of the same root word (Ex: For the original tag line for video 1, "recording" was changed to "record".)
corpus = tm_map(corpus, stemDocument)

# Now that we've completed the four pre-processing steps, we're now ready to extract the word frequencies to be used in our prediction problem.
# The "tm" package provides a function called "Document Term Matrix" that generates a matrix where the rows correspond to documents (videos) 
# and the columns correspond to words (tags) in those videos.
# The values in the matrix are the number of times a word (tag) appears in each document (video).
# Of the 40949 documents (videos) there are 19856 terms (tags)
frequencies <- DocumentTermMatrix(corpus)

# Let's inspect our matrix. Let's look at videos 1000 to 1005 and tags 505 to 515.
# This data is sparse. 
inspect(frequencies[1000:1005, 505:515])

# When data is sparse, meaning there are many zeros in the matrix, we should look at what the most frequent terms (or words or tags) are with the "findFreqTerms" function.
# The "lowfreq = " is the minimum number of times a term (tag) must appear in our matrix.
# This will allow us to remove the tags that don't appear too often in our dataset.
# So out of the 19856 tags, 2499 tags appeared at least 100 times or more in our videos. 
# This means that we will have a lot of tags that will be pretty useless for our prediction model.
# The number of terms (tags) is an issue for two main reasons: one is computational. More tags means more independent variables, which usually means it takes longer to build our models.
# Second, in building models, the ratio of independent variables to observations will affect how good the model will generalize.
findFreqTerms(frequencies, lowfreq = 100)

# So let's remove some tags that don't appear very often.
# To do this, use the "removeSparseTerms" function. The first argument is the name of our matrix (frequencies) and the second is the sparsity threshold.
# Here we set a threshold of the percentage of tags we keep. So, 0.995 would mean to only keep the tags that appear in 0.5% or more of the videos.
# We'll go ahead and use this sparcity threshold.
sparse <- removeSparseTerms(frequencies, 0.995)

# There are now only 740 tags in our sparse matrix, which is only 3.7% of the previous count of 19856 and a much more reasonable number.
sparse

# Now let's convert the sparse matrix into a dataframe (now called "tagsSparse") to use in our predictive models.
tagsSparse <- as.data.frame(as.matrix(sparse))

# Let's make sure no tags start with a number by running the "make.names" function. 
# This will convert all variables names to appropriate names before we build our predictive models. (For example, "2017" was changed to "X2017".)
# You should do this each time you build a dataframe using text analytics.
colnames(tagsSparse) <- make.names(colnames(tagsSparse))

# This dataframe will only include the frequencies of the tags that appeared in at least 0.5% of the videos.
# But in order to run our text analytics models, we also need to have the outcome variable, which is whether or not a video was popular (categorical, dependent variable). 
# So we need to add in this outcome variable.
# Now let's add a dependent variable (popular_video) to this dataset.
tagsSparse$popular_video <- USvideos_new$popular_video
str(tagsSparse) # Looking at our newly constructed dataframe, we see that there are 740 variables that are the frequencies of tags used for the videos and the last 1 is the outcome variable (popularity).

# Lastly, let's split our data into a training set and a testing set to then actually build a model. 
# Put 70% of the data in the training set.
set.seed(123) # Set the seed so our results are reproducible.

split <- sample.split(tagsSparse$popular_video, SplitRatio = 0.7) # Dependent variable = "tagsSparse$popular_video" 

# Now create training and test sets:
trainSparse <- subset(tagsSparse, split==TRUE)
testSparse <- subset(tagsSparse, split==FALSE)

# Our data is now ready and we can build our predictive models.

# BASELINE

# So how good is a simple baseline model that always predicts an unpopular video? To compute this, make a table of just the outcome variable.
# We can see here that in our test set we have 7724 observations that are unpopular and 4560 observations that are popular.
# So the accuracy of a baseline model that always predicts non-popular (unpopular videos) is 7724/(7724 + 4560) = 0.6287 or about 63%.
table(testSparse$popular_video)
table(trainSparse$popular_video)
(18024)/(18024 + 10641) # 0.6288 or about 63% on the training set as well 

####

# LOGISTIC REGRESSION

# Let's begin by using a logistic regression model to predict our categorical outcome (popular/unpopular)
# The logistic regression model computes probabilities that can be used to assess the confidence of the prediction
USvideosLOG <- glm(popular_video ~ ., data = trainSparse, family = binomial)
summary(USvideosLOG) # AIC is 27922, which is a measure of the quality of the logistic model that accounts for the number of variables used compared to the number of observations

# This gives us the probabilities of our predictions
predictLOG <- predict(USvideosLOG, type="response")
summary(predictLOG)

# Let's see if we are predicting higher probabilities for the actual popular videos as we expect.
tapply(predictLOG, trainSparse$popular_video, mean) # this will compute the average prediction for each of the TRUE outcomes
# We see that for all of the TRUE popular videos, we predict an average probability of about 0.59. 
# And for all of the FALSE popular videos (meaning unpopular), we predict an average probability of about 0.24.
# Good sign because it looks like we are predicting a higher probability for the actual popular videos.

# Select a threshold 
table(trainSparse$popular_video, predictLOG > 0.5)
6399/(4242 + 6399) # 0.6014 is the Sensitivity or the True Positive Rate (% of actual popular videos we classified correctly)
16042/(16042 + 1982) # 0.8900 is the Specificity or the True Negative Rate (% of actual unpopular videos we classified correctly)

# Because we are more concerned with having a high Sensitivity or high True Positive Rate, we pick a threshold that minimizes the False Positive Rate but has a very high True Positive Rate.
# Use a ROC curve to help you decide the threshold to use.
predLOG <- prediction(predictLOG, trainSparse$popular_video)
prefLOG <- performance(predLOG, "tpr", "fpr")
plot(prefLOG)
plot(prefLOG, main = "ROC Curve for Logistic Regression", colorize = TRUE, print.cutoffs.at=seq(0,1,0.1), text.adj=c(-0.2,1.7))

# Based on the ROC curve, we would want to use a threshold of 0.3 because we want to maximize the True Positive Rate while minimizing the False Positive Rate.
# Doing so will increase the Sensitivity or the percentage of actual popular videos we classified correctly.
# So, our Sensitivity went up while the Specificity went down
table(trainSparse$popular_video, predictLOG > 0.3)
9133/(1508 + 9133) # 0.8583 is the Sensitivity or the True Positive Rate (% of actual popular videos we classified correctly)
11456/(11456 + 6568) # 0.6356 is the Specificity of the True Negative Rate (% of actual unpopular videos we classified correctly)

# To get the AUC value on our training set
as.numeric(performance(predLOG, "auc")@y.values) # which gives an area under the curve of 0.8497 

# Now, let's use this logistic regression model to make predictions on our test set
predictLOG_test <- predict(USvideosLOG, type = "response", newdata = testSparse)

# Let's create a confusion matrix using the "table" function with a threshold of 0.3, which is an optimized threshold based on the training data
table(testSparse$popular_video, predictLOG_test > 0.3)
(4844 + 3872)/(4844 + 2880 + 688 + 3872) # 0.7095
# The accuracy of this model is about 71%, which is better than the baseline model of 63%.

# To get the AUC value on our testing set
predLOG_ROCR <- prediction(predictLOG_test, testSparse$popular_video)
as.numeric(performance(predLOG_ROCR, "auc")@y.values) # which gives an area under the curve of 0.8373
# So we have an AUC of about 84% on our test set, which means that the logistic model can differentiate between popular videos and unpopular videos pretty well.

####

# CART

# Besides a logistic regression model, we can use CART (Classification And Regression Trees) to predict a categorical outcome (in this case popularity)
# A CART model is very interpretable. A CART tree is a series of decision rules, which can easily be explained.
USvideosCART <- rpart(popular_video ~ ., data=trainSparse, method = "class")

# Now let's plot the model. 
# Our tree says if the tag "offici" is in our video tags, then predict TRUE or predict that the video is popular.
# If the tag "offici" is not in our video tags, but the tag "record" is, then again predict TRUE or predict that the video is popular.
# If the tags "offici" and "record" are not in our video tags, then predict FALSE or predict that the video is not popular.
# This intuitively makes sense because the top two video categories are "Entertainment" = 9964 videos and "Music" = 6472 videos.
prp(USvideosCART, main = "CART Tree", col = "purple")

# Now let's see how well our CART model does at predicting the cases in the test set.
# To do this, let's numerically evaluate the performance of the model by making predictions on our test set.
# We need to give a new argument when making predictions for our CART model (type = "class"). This will take each test set observation and classify it into the classes 0 or 1. 
# This is like having a threshold of 0.5 where it will pick the majority outcome.
predictCART <- predict(USvideosCART, newdata=testSparse, type = "class")

# Now let's compute the accuracy with the threshold of 0.5 (established in the previous type = "class" argument)
# To do this, let's build our confusion matrix using the "table" function 
table(testSparse$popular_video, predictCART)

# To compute the accuracy of our current CART model, 
# add up the number of cases we got correct (7249 + 795) and divide by the total number of observations in this table (7249 + 475 + 3765 + 795)
# This gives us: 8044/12284 = 0.6548
# We can see that the accuracy of our current model is about 65%.
# Quickly looking at the training set, we see we have an accuracy of 65% as well.
predictCARTTrain <- predict(USvideosCART, newdata=trainSparse, type = "class")
table(trainSparse$popular_video, predictCARTTrain)
(16802 + 1827)/(16802 + 1222 + 8814 + 1827) = 0.6499

# Generate the ROC curve for our CART model
# Now run the "predict" function without the option/argument (type = "class") so that we can pick any threshold value
predictROC <- predict(USvideosCART, newdata=testSparse)

# Let's see what this looks like
# This gives us output with two columns for each of our test set observations, labeled FALSE and TRUE
# The first column (FALSE) is the percentage of training set data in the same subset as that test set observation that had outcome FALSE
# The second column (TRUE) is the percentage of training set data in the same subset as that test set observation that had outcome TRUE
# You can interpret the second column (TRUE) as the probability that that test set observation has outcome TRUE
# We use this second column (TRUE) when thresholding
predictROC

# Now let's use the "prediction" and "performance" functions of the ROCR package to generate the ROC curve just as we do for logistic regression
# pred (give as arguments the second column "predictROC[,2]" which means to take all rows in the second column and then we give the true outcome "testSparse$popular_video")
# perf (give as arguments the output of the "prediction" function "pred" and then true positive rate "tpr" and false positive rate "fpr" to 
# determine what we want our ROC curve to have on the x-axis and y-axis)
pred <- prediction(predictROC[,2], testSparse$popular_video)
perf <- performance(pred, "tpr", "fpr")

# Now let's plot our ROC curve by plotting the output of the "performance" function
plot(perf, main = "ROC Curve for CART", col = 2, lwd = 2)

# To get the AUC value on our testing set
as.numeric(performance(pred, "auc")@y.values) # which gives an area under the curve of 0.5564
# So we have an AUC of about 56% on our test set, which means that the CART model doesn't differentiate between popular videos and unpopular videos too well.

# Let's see if we can improve the accuracy of our model by making sure we picked the proper cp. 
# Use the K-fold cross-validation method to properly select the parameter value
# This method averages the accuracy over the k-folds to determine the final parameter value to use
# If the parameter value is too small, then the accuracy is lower because the model is probably overfit to the training set
# If the parameter value is too large, the accuracy is also lower because the model is too simple

# When we use cross-validation in R, we use a parameter called cp (complexity parameter)
# The cp measures the trade-off between model complexity and accuracy on the training set
# A smaller cp value leads to a bigger tree, so a smaller cp value might overfit the model to the training set
# But a cp value that is too large might build a model that is too simple

# Let's use cross-validation to select the cp value for our CART tree
# First, define the number of folds (method="cv" is for the cross-validation method and number=10 is for 10 folds)
fitControl <- trainControl(method="cv", number=10)
# Second, pick the possible values for our parameter cp (this will use cp values from 0.01 through 0.5)
cartGrid <- expand.grid(.cp=(1:50)*0.01)

# Now we can perform cross-validation. To do this, use the "train" function and include the method ="rpart" because we want to validate our parameters for our CART tree
train(popular_video ~ ., data=trainSparse, method="rpart", trControl=fitControl, tuneGrid=cartGrid)

# The output shows us the accurary for different values of cp
# The first column is cp (our complexity parameter value)
# The second column is accuracy (the average accuracy on each of the folds for that complexity parameter value)
# The final value used for the model was cp = 0.02. This is the cp value we want to use for our CART model.
# Now let's create a new CART model using this value of cp we found through cross-validation.
USvideosCV <- rpart(popular_video ~ ., data=trainSparse, method = "class", control = rpart.control(cp=0.02))

# Now let's make predictions using this new model
predictCV <- predict(USvideosCV, newdata = testSparse, type = "class") # type = "class" applies a threshold of 0.5

# Let's create our confusion matrix using the "table" function
# The first argument is our TRUE values (testSparse$popular_video) and the second is our predictions (predictCV)
table(testSparse$popular_video, predictCV)

# Now let's compute the accuracy with our cross-validated model
(7249 + 795)/(7249 + 475 + 3765 + 795)
# This gives us an accuracy of 0.6548, which is the same as our accurary rate for our original CART model.
# Cross-validation will ensure that we are picking a good parameter value and often this will significantly increase the accuracy.
# In this case, our accuracy did not increase. Cross-validation is confirming that we are using a smart parameter value.

# So our current CART model is improving over the baseline (65% [CART] compared to 63% [baseline]) 
# But it did not perform better than the logistic regression model (65% [CART] compared to 71% [logistic]).
# This may be because CART models do not perform well on smaller datasets. Although one may argue this dataset is not particularly small, the sparcity of the data did not help.

####

# RANDOM FOREST

# How about a random forest model? How well will that do?
# The random forest model is similiar to CART and was designed to improve the prediction accuracy of CART and works by building a large number of CART trees.
set.seed(123)
USvideosRF <- randomForest(popular_video ~ ., data=trainSparse) ###

# Let's make predictions on our training set.
predictRFTrain <- predict(USvideosRF, newdata = trainSparse)

# Now let's make our confusion matrix using the "table" function to compute the accuracy
# First we give the actual outcome (trainSparse$popular_video) and then our predictions (predictRFTrain)
table(trainSparse$popular_video, predictRFTrain)

# To compute the accuracy of a random forest model, we sum the number of cases we got right and divide by the total number of cases in this table
(16972 + 9367)/(16972 + 1052 + 1274 + 9367) # So using the training set, our random forest model has an accuracy of 0.9189 or about 92%.

# Now that our model is done, let's make predictions on our test set.
predictRF <- predict(USvideosRF, newdata=testSparse)

# Now let's make our confusion matrix using the "table" function to compute the accuracy
# First we give the actual outcome (testSparse$popular_video) and then our predictions (predictRF)
table(testSparse$popular_video, predictRF)

# To compute the accuracy of a random forest model, we sum the number of cases we got right (7197 + 3937) and divide by the total number of cases in this table (7197 + 527 + 623 + 3937)
(11134)/(12284) # So using the test set, our random forest model has an accuracy of 0.9064 or about 90.6%.

# This is significantly better than our CART model (only 65%) and the baseline model (only 63%) and an improvement over our logistic regression model (71%).

# Generate the ROC curve for our random forest model
predictROC_RF <- predict(USvideosRF, type = "prob", newdata=testSparse)[,2]

# Now let's use the "prediction" and "performance" functions of the ROCR package to generate the ROC curve just as we do for logistic regression
predRF <- prediction(predictROC_RF, testSparse$popular_video)
perfRF <- performance(predRF, "tpr", "fpr")

# Now let's plot our ROC curve by plotting the output of the "performance" function
plot(perfRF, main="ROC Curve for Random Forest", col=2, lwd=2)
abline(a=0, b=1, lwd=2, lty=2, col="gray")

# To get the AUC value on our testing set
as.numeric(performance(predRF, "auc")@y.values) # which gives an area under the curve of 0.9234
# So we have an AUC of about 92% on our test set, which means that the random forest model differentiates between popular videos and unpopular videos very well.

# To interpret the ROC curve, recall that perfect classification happens at 100% True Positive Rate and 0% False Positive Rate. 
# So perfect classification happens at the upper left-hand corner of the graph. The closer our graph comes to that corner, the better we are at classification.
# The diagonal line represents random guess, so the distance of our graph over the distance line represents how much better we are doing than random guess.
# As we can see by the ROC curve, the random forest model is significantly better than random guess.

# To see which tags are important in classifying popularity based on our random forest model, let's use the "importance" and "varImpPlot" functions:
# The "importance" function gives a textual representation of how important the tag variables are in classifying popularity
importance(USvideosRF)

# The "varImpPlot" method gives a graphical representation of how important the tag variables are in classifying popularity
# We see from the Variable Importance Plot that the most important tags included: "offici", "news", "pop", "record", "music", "trailer", and "video"
# Mean Decrease in Gini is the average (mean) of a variable's total decrease in node impurity, weighted by the proportion of samples reaching that node
# in each individual decision tree in the random forest. A higher Mean Decrease in Gini indicates higher variable importance.
varImpPlot(USvideosRF, main = "Tags with Highest Importance", col = "purple")

# To see which tags are important in classifying popularity based on our random forest model for all categories
imptags <- varImpPlot(USvideosRF)

# Final Model

# Now, using the treshold and parameters set in our training set, which showed significant accuracy in our testing set as well, we will retrain the model to make predictions on all data
predictRFFM <- predict(USvideosRF, newdata = tagsSparse)

# Now let's make our confusion matrix using the "table" function to compute the accuracy of our final model on all of the data
# First we give the actual outcome (tagsSparse$popular_video) and then our predictions (predictRFFM)
table(tagsSparse$popular_video, predictRFFM)

# To compute the accuracy, we sum the number of cases we got right and divide by the total number of cases in this table
(24167 + 13304)/(24167 + 1581 + 1897 + 13304) # So using all of the data, our final random forest model has an accuracy of 0.9151 or about 91.5%.
# So the training set accuracy was 92%, the test set accuracy was 90.6%, and the model used on all data was 91.5%, which nicely falls between the two train/test data sets.

####

# CONCLUSION

# So, we can see here that we can reasonably pick popularity given our dataset
# The random forest model is the most accurate (91.5%) of all models tested to determine a video's popularity based on the tags used
# The random forest model differentiates between popular videos and unpopular videos very well (AUC = 92%)
# According to the random forest model, the most important tags to include are: "offici", "news", "pop", "record", "music", "trailer", and "video"

####

# CATEGORIES

# Looking at a table of the dataset, it's clear that the majority of videos represent only a handful of categories.
table(USvideos_new$categories)

# Because many of the videos represent only a couple of categories, this provides limited value for potential clients. 
# This is particularly true for those clients whose videos do not fall into these more popular categories ("Nonprofits & Activism" = 57 vs. "Entertainment" = 9964). 
# Thus, it's best to analyze the data by "category_id". 

# First, let's add our category variables to a dataset for all categories.
tagsCategories <- tagsSparse
tagsCategories$categories <- USvideos_new$categories
tagsCategories$category_id <- USvideos_new$category_id

# Next, to filter by "category_id", use the following code so that only those videos that match a specified "category_id" are included in their own unique dataset.
# We will analyze two categories to show how well our model's accuracy holds up when split into this subsets

# Category 26 ("How To & Style")
tagsCate_26 <- tagsCategories %>% filter(category_id == 26)

# Make predictions on category 26 data
predictRF26 <- predict(USvideosRF, newdata = tagsCate_26)

# Now let's make our confusion matrix using the "table" function to compute the accuracy of our final model on the category 26 data
table(tagsCate_26$popular_video, predictRF26)

# To compute the accuracy, we sum the number of cases we got right and divide by the total number of cases in this table
(2999 + 972)/(2999 + 95 + 80 + 972) # accuracy of 0.9578 or about 96% using only "How To & Style" data.

# Category 24 ("Entertainment")
tagsCate_24 <- tagsCategories %>% filter(category_id == 24)

# Make predictions on category 24 data
predictRF24 <- predict(USvideosRF, newdata = tagsCate_24)

# Now let's make our confusion matrix using the "table" function to compute the accuracy of our final model on the category 24 data
table(tagsCate_24$popular_video, predictRF24)

# To compute the accuracy, we sum the number of cases we got right and divide by the total number of cases in this table
(5949 + 3232)/(5949 + 362 + 421 + 3232) # accuracy of 0.9214 or about 92% using only "Entertainment" data.

####

# PLOTS

# Create a treemap for the most important tags to predict popularity

# Code to convert "imptags" object to a dataframe ("all_tags")
all_tags <- as.data.frame(imptags)
# To convert from from 1 variable to 2 variables so that: "rn" is the tag name and "MeanDecreaseGini" is the average (mean) of a variable's total decrease in node impurity
# The "setDT" function allows you to both remove row names and convert them to a column by reference using the "keep.rownames = TRUE" argument 
setDT(all_tags, keep.rownames = TRUE)

# A treemap of all 740 tags by their "Mean Decrease Gini" value
treemap_tags <- treemap(all_tags, index = "rn", vSize = "MeanDecreaseGini", palette = "PRGn") # This treemap contains too many tags to properly read, so look at condensed treemap below

# Let's look at a treemap of the top 100 Mean Decrease Gini tags
# To do this, we create a new dataset (all_tags_top) that filters out the tags with the highest "Mean Decrease Gini", which suggests they are the most important to use for popular videos
all_tags_top <- all_tags %>% filter(MeanDecreaseGini >= 21.3)
# Now, let's make our treemap of this new dataset
treemap_tags_top <- treemap(all_tags_top, index = "rn", vSize = "MeanDecreaseGini", title = "100 Most Important Tags to Predict Popularity", palette = "PRGn")

####

# Create a boxplot for views by category for all videos
VCate <- ggplot(USvideos_new, aes(categories, views)) + geom_boxplot(fill = "purple", color = "pink") + scale_y_log10() + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1))
print(VCate + ggtitle("Video Categories Ranked by View Count") + labs(y = "view count", x = "category")  + theme(plot.title = element_text(hjust = 0.5)))

# Create a boxplot for likes by category for all videos
LCate <- ggplot(USvideos_new, aes(categories, likes)) + geom_boxplot(fill = "purple", color = "blue") + scale_y_log10() + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1))
print(LCate + ggtitle("Video Categories Ranked by Like Count") + labs(y = "like count", x = "category")  + theme(plot.title = element_text(hjust = 0.5)))

# Create a boxplot for comment_count by category for all videos
CCate <- ggplot(USvideos_new, aes(categories, comment_count)) + geom_boxplot(fill = "purple", color = "green") + scale_y_log10() + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1))
print(CCate + ggtitle("Video Categories Ranked by Comment Count") + labs(y = "comment count", x = "category")  + theme(plot.title = element_text(hjust = 0.5)))

# Create a boxplot for popular_norm by category for all videos
PopCate <- ggplot(USvideos_new, aes(categories, popular_norm)) + geom_boxplot(fill = "pink", color = "blue") + scale_y_log10() + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1))
print(PopCate + ggtitle("Video Categories Ranked by Popularity") + labs(y = "popularity", x = "category")  + theme(plot.title = element_text(hjust = 0.5)))

# Create a boxplot for popular_norm by publish_year for all videos
PopPY <- ggplot(USvideos_new, aes(publish_year, popular_norm)) + geom_boxplot(fill = "blue", color = "green") + scale_y_log10() + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1))
print(PopPY + ggtitle("Publish Year Ranked by Popularity") + labs(y = "popularity", x = "publish year")  + theme(plot.title = element_text(hjust = 0.5)))

# Create a boxplot for popular_norm by trending_year for all videos
PopTY <- ggplot(USvideos_new, aes(trending_year, popular_norm)) + geom_boxplot(fill = "blue", color = "red") + scale_y_log10() + theme(text = element_text(size = 10), axis.text.x = element_text(angle = 90, hjust = 1))
print(PopTY + ggtitle("Trend Year Ranked by Popularity") + labs(y = "popularity", x = "trend year")  + theme(plot.title = element_text(hjust = 0.5)))

####

# Let's look at a bar plot of the top 10 Mean Decrease Gini tags
# To do this, we create a new dataset (all_tags_top10) that filters out the tags with the 10 highest "Mean Decrease Gini" scores, which suggests they are the most important to use for popular videos
all_tags_top10 <- all_tags %>% filter(MeanDecreaseGini >= 55)
# Now, let's make our bar plot of this new dataset
TagsPlot <- ggplot(all_tags_top10, aes(x = rn, fill = MeanDecreaseGini)) + geom_bar() 
print(TagsPlot + ggtitle("Top 10 Most Important Tags") + labs(y = "count", x = "tag name") + theme(plot.title = element_text(hjust = 0.5)))

# Time-series plot comparing 'publish' to 'trending' factors. This shows that the data included mostly videos published in 2017 or 2018 and only those that trended in 2017 and 2018.
ggplot(USvideos_new, aes(x = publish_date_official, y = trend_date_official)) + geom_point(position = "jitter", alpha = 0.5)
summary(USvideos_new$publish_date_official) # with a minimum of 2006-07-23 to a maximum of 2018-06-14
summary(USvideos_new$trend_date_official) # with a minimum of 2017-11-14 to a maximum of 2018-06-14