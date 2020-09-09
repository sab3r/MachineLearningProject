# Predicting fatality in an accident

library(ISLR)

accidents_df <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\Iowa_Road_Accidents\\v1_Data_Processing\\Vehicle_Accidents_in_Iowa_2015_2016_categorical.csv", header = TRUE)
names(accidents_df)

head(accidents_df)
summary(accidents_df)


#par(mfrow=c(1,1))

#hist(accidents_df$Property.Damage)

#boxplot(accidents_df$Property.Damage)

#library(Amelia)

#missmap(accidents_df)

sapply(accidents_df,function(x) sum(is.na(x)))
sapply(accidents_df, function(x) length(unique(x)))
sapply(accidents_df, function(x) is.factor(x))
#is.factor(accidents_df$DOT.Case.Number)
contrasts(accidents_df$Light.Conditions)


str(accidents_df)

# Remove unnecessary columns

accidents_df$DOT.Case.Number <- NULL
accidents_df$Law.Enforcement.Case.Number <- NULL
accidents_df$Crash.Date...Time <- NULL
accidents_df$City <- NULL
accidents_df$Crash.Month <- NULL
accidents_df$Work.Zone <- NULL
accidents_df$Literal.Description <- NULL
accidents_df$Report.Type <- NULL
accidents_df$Rest.Update <- NULL
accidents_df$District <- NULL
accidents_df$Environment <- NULL
accidents_df$Roadway <- NULL
accidents_df$County <- NULL
accidents_df$Route <- NULL
accidents_df$Crash.Day <- NULL
accidents_df$Crash.Time <- NULL
accidents_df$Route <- NULL
accidents_df$Crash.Location <- NULL


#install.packages('caTools')


# Encode target variable as factor

accidents_df$Fatality <- as.factor(accidents_df$Fatality)
accidents_df_backup <- accidents_df
class(accidents_df_backup$Occupants)


# Remove rows where occupants > 30 as these might be errors
accidents_df <- accidents_df_backup[accidents_df_backup$Occupants < 30,]
boxplot(accidents_df$Occupants)





## check Chi-square
str(accidents_df)
lapply(accidents_df[,c('Major.Cause','Crash.Manner','Crash.Severity','Surface.Conditions','Drug.Alcohol.Related','Light.Conditions', 'Weather.Conditions', 'Vehicles', 'Occupants', 'Property.Damage')], function(x) chisq.test(table(x,accidents_df$Fatality))$p.value)

######################
## Train Test Split ##
######################



require(caTools)  # loading caTools library
## Loading required package: caTools
set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(accidents_df,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 =subset(accidents_df,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test1=subset(accidents_df, sample==FALSE)

#########################
## Logistic Regression ##
#########################


logistic_model <- glm(Fatality ~Major.Cause+Crash.Manner+Surface.Conditions+Drug.Alcohol.Related+Light.Conditions+Light.Conditions+Weather.Conditions+Vehicles+Occupants+Property.Damage,family=binomial(link='logit'),data=train1)


summary(logistic_model)
library(caret)


fitted.results.probability <- predict(logistic_model,newdata=test1,type='response')
fitted.results <- ifelse(fitted.results.probability > 0.4,1,0)
misClasificError <- mean(fitted.results != test1$Fatality)
print(paste('Accuracy',1-misClasificError))

fitted.results

confusionMatrix(table(test1$Fatality, fitted.results), mode = "everything", positive = "1")

cm = table(test1$Fatality, fitted.results)

cm

logistic_recall <- diag(cm) / rowSums(cm)
logistic_recall


# ROC plot and F1 score

#install.packages('ROCR')
library(ROCR)
p <- predict(logistic_model, newdata=test1, type="response")
pr <- prediction(p, test1$Fatality)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

# F1 score using ROCR
logistic_f1_score <- performance(pr,"f")
logistic_f1_score

# Plotting logistic regression 

predicted.data <- data.frame(probability.of.fatality=fitted.results.probability, fatality=test1$Fatality)
predicted.data <- predicted.data[order(predicted.data$probability.of.fatality, decreasing = FALSE), ]
predicted.data$rank <- 1:nrow(predicted.data)
library(ggplot2)
library(cowplot)

#ggplot(data = predicted.data, aes(x=rank, y=probability.of.fatality)) +
#  geom_point(aes(color=fatality), alpha=1, shape=4, stroke=2) +
#  xlab("Index") +
#  ylab("Predicted probability of fatality")

#ggsave()


#######################################
### Logistic Regression with Weights ##
#######################################


# Assigning weights for the dependent variable in the training dataset

table(train1$Fatality)

weight <- as.numeric(as.character(train1$Fatality))
weight <- weight *29
weight <- weight + 1

###############################################################
## Weight of a fatality is 30 times more than a non-fatality ##
###############################################################

table(weight)

weighted_logistic_model <- glm(Fatality ~Major.Cause+Crash.Manner+Surface.Conditions+Drug.Alcohol.Related+Light.Conditions+Light.Conditions+Weather.Conditions+Vehicles+Occupants+Property.Damage,family=binomial(link='logit'),data=train1, weights = weight)

#varImp(weighted_logistic_model)
summary(weighted_logistic_model)



weighted_fitted.results.probability <- predict(weighted_logistic_model,newdata=test1,type='response')
weighted_fitted.results <- ifelse(weighted_fitted.results.probability > 0.4,1,0)
weighted_misClasificError <- mean(weighted_fitted.results != test1$Fatality)
print(paste('Accuracy',1-weighted_misClasificError))

weighted_fitted.results

table(test1$Fatality)
confusionMatrix(table(test1$Fatality, weighted_fitted.results), mode="everything", positive = "1")
weighted_cm = table(test1$Fatality, weighted_fitted.results)

weighted_cm

## Checking Recall for this model

weighted_logistic_recall <- diag(weighted_cm) / rowSums(weighted_cm)
weighted_logistic_recall

# Plotting logistic regression 

weighted_predicted.data <- data.frame(probability.of.fatality=weighted_fitted.results.probability, fatality=test1$Fatality)
weighted_predicted.data <- predicted.data[order(weighted_predicted.data$probability.of.fatality, decreasing = FALSE), ]
weighted_predicted.data$rank <- 1:nrow(weighted_predicted.data)
library(ggplot2)
library(cowplot)

head(weighted_predicted.data)

#ggplot(data = weighted_predicted.data, aes(x=rank, y=probability.of.fatality)) +
#  geom_point(aes(color=fatality), alpha=1, shape=4, stroke=2) +
#  xlab("Index") +
#  ylab("Predicted probability of fatality")

#ggsave()

#table(train1$Fatality, fitted.results > 0.5)
#names(fitted.results)

#install.packages('ROCR')
library(ROCR)
weighted_p <- predict(weighted_logistic_model, newdata=test1, type="response")
weighted_pr <- prediction(weighted_p, test1$Fatality)
weighted_prf <- performance(weighted_pr, measure = "tpr", x.measure = "fpr")
plot(weighted_prf)

# F1 score using ROCR
weighted_logistic_f1_score <- performance(pr,"f")
weighted_logistic_f1_score


#auc <- performance(pr, measure = "auc")
#auc <- auc@y.values[[1]]
#auc




############################
## Naive-Bayes Classifier ##
############################


require(caTools)  # loading caTools library

## Train Test Split

set.seed(123)   #  set seed to ensure you always have same random numbers generated
sample = sample.split(accidents_df,SplitRatio = 0.75) # splits the data in the ratio mentioned in SplitRatio. After splitting marks these rows as logical TRUE and the the remaining are marked as logical FALSE
train1 =subset(accidents_df,sample ==TRUE) # creates a training dataset named train1 with rows which are marked as TRUE
test1=subset(accidents_df, sample==FALSE)

#install.packages('e1071')
library('e1071')

## Training the classifier

class(train1$Fatality)
train1$Fatality <- as.factor(train1$Fatality)
test1$Fatality <- as.factor(test1$Fatality)

str(train1)
#classifier = naiveBayes(x = train1[-38], y = train1$Fatality)
str(train1[-21])
classifier = naiveBayes(x = train1[-21], y = train1$Fatality)



library(caret)
#varImp(classifier)

summary(classifier)

# Predicting values in the test dataset

naive_pred = predict(classifier, newdata = test1[-21])
#View(naive_pred)
naive_cm = table(test1$Fatality, naive_pred)
naive_cm
confusionMatrix(table(test1$Fatality, naive_pred), mode="everything", positive = "1")
#recall(table(test1$Fatality, naive_pred), relevant = levels(test1$Fatality)[2])
levels(test1$Fatality)

## Checking the Recall in Naive Bayes classifier

naive_recall <- diag(naive_cm) / colSums(naive_cm)
naive_recall

# plot naive bayes confusion matrix
plot(naive_cm)

# Plotting ROC for Naive Bayes

p_naive <- predict(classifier, newdata=test1[-38], type="raw")
class(p_naive)
table(p_naive)
p_naive[,2]
class(test1$Fatality)
pr_naive <- prediction(p_naive[,2], test1$Fatality)
prf_naive <- performance(pr_naive, measure = "tpr", x.measure = "fpr")
plot(prf_naive)


##########################################
### Naive Bayes with Laplace Smoothing ###
##########################################


classifier_laplace = naiveBayes(x = train1[-21], y = train1$Fatality, laplace = 1)



library(caret)
#varImp(classifier)

summary(classifier_laplace)

# Predicting values in the test dataset

naive_pred_laplace = predict(classifier_laplace, newdata = test1[-21])
#View(naive_pred)
naive_laplace_cm = table(test1$Fatality, naive_pred_laplace)
naive_laplace_cm
confusionMatrix(table(test1$Fatality, naive_pred_laplace), mode="everything", positive = "1")
#recall(table(test1$Fatality, naive_pred), relevant = levels(test1$Fatality)[2])
levels(test1$Fatality)

## Checking the Recall in Naive Bayes classifier

naive_laplace_recall <- diag(naive_laplace_cm) / colSums(naive_laplace_cm)
naive_laplace_recall

# plot naive bayes confusion matrix
plot(naive_laplace_cm)

# Plotting ROC for Naive Bayes

p_naive_laplace <- predict(classifier_laplace, newdata=test1[-38], type="raw")
class(p_naive_laplace)
table(p_naive_laplace)
p_naive_laplace[,2]
class(test1$Fatality)
pr_naive_laplace <- prediction(p_naive_laplace[,2], test1$Fatality)
prf_naive_laplace <- performance(pr_naive_laplace, measure = "tpr", x.measure = "fpr")
plot(prf_naive_laplace)




#######################################
## Support Vector Machine Classifier ##
#######################################

## Both Linear and Radial SVM models are implemented

library(e1071)
svm_classifier_radial = svm(formula = Fatality ~Major.Cause+Crash.Manner+Surface.Conditions+Drug.Alcohol.Related+Light.Conditions+Light.Conditions+Weather.Conditions+Vehicles+Occupants+Property.Damage, 
                     data = train1, 
                     type = 'C-classification',
                     kernel = 'radial')

svm_classifier_linear = svm(formula = Fatality ~Major.Cause+Crash.Manner+Surface.Conditions+Drug.Alcohol.Related+Light.Conditions+Light.Conditions+Weather.Conditions+Vehicles+Occupants+Property.Damage, 
                            data = train1, 
                            type = 'C-classification',
                            kernel = 'linear')


#plot(svm_classifier_linear, train1['Light.Conditions', 'Weather.Conditions', 'Fatality'])

y_pred_svm_radial = predict(svm_classifier_radial, newdata = test1[-21])

y_pred_svm_linear = predict(svm_classifier_linear, newdata = test1[-21])

cm_svm_radial = table(test1$Fatality, y_pred_svm_radial)

cm_svm_linear = table(test1$Fatality, y_pred_svm_linear)

cm_svm_radial
cm_svm_linear


# Confustion Matrix and Recall
caret_cm_svm_linear <- confusionMatrix(table(test1$Fatality, y_pred_svm_linear), mode = "everything", positive = "1")
caret_cm_svm_linear

# SVM Recall for Radial Model
svm_recall <- diag(cm_svm_radial) / rowSums(cm_svm_radial)
svm_recall

# SVM Recall for Linear Model
svm_recall <- diag(cm_svm_linear) / rowSums(cm_svm_linear)
svm_recall


# plot ROC for svm

p_svm <- predict(svm_classifier_linear, newdata=test1, type="response")
class(p_svm)
table(p_svm)
p_svm <- as.integer(p_svm)
class(test1$Fatality)
pr_svm <- prediction(p_svm, test1$Fatality)
prf_svm <- performance(pr_svm, measure = "tpr", x.measure = "fpr")
plot(prf_svm)










###############################
## Alcohol and Climate Study ##
###############################


alcohol_df <- read.csv("C:\\Users\\Soham More\\Documents\\GitHub\\dmml1\\Datasets\\final_merged_alcohol_weather_with_0_prcp_temp_cleaned.csv", header = TRUE)



# Check for NA values
sapply(alcohol_df, function(x) sum(is.na(x))) 

alcohol_df_backup <- alcohol_df

alcohol_df$AWND[is.na(alcohol_df$AWND)] <- 0

alcohol_df$TAVG <- NULL
x <- na.omit(alcohol_df)

# Splitting the dataset into the Training set and Test set


library(caTools)
set.seed(123)
split = sample.split(x$Sale..Dollars., SplitRatio = 0.75)
training_set = subset(x, split == TRUE)
test_set = subset(x, split == FALSE)



### Check correlation in the training data
library(corrplot)
head(training_set)
cor_mat <- cor(training_set[, c(3,4,8,9,10,11,12,13)], method = c('pearson'))
corrplot(cor_mat, method='number')

### Check outliers
boxplot(training_set$SNWD)
boxplot(training_set$SNOW)
boxplot(training_set$TMIN)
boxplot(training_set$TMAX)
boxplot(training_set$PRCP)

### Create multiple linear model

regressor = lm(formula = Sale..Dollars. ~ PRCP + AWND + SNOW + SNWD + TMAX + TMIN,
               data = training_set)

summary(regressor)

# diagnostic plots
#layout(matrix(c(1,2,3,4),2,2)) # optional 4 graphs/page
#plot(regressor)

y_pred = predict(regressor, newdata = test_set)

y_pred
#library(tidyverse)
library(caret)

# Model performance
# (a) Prediction error, RMSE
RMSE(y_pred, test_set$Sale..Dollars.)
# (b) R-square
R2(y_pred, test_set$Sale..Dollars.)





alcohol_df_without_NA <- x

# Applying k-Fold Cross Validation
folds = createFolds(alcohol_df_without_NA$Sale..Dollars., k = 10)
cv = lapply(folds, function(x) {
  training_fold = alcohol_df_without_NA[-x, ]
  test_fold = alcohol_df_without_NA
  regressor = lm(formula = Sale..Dollars. ~ PRCP + AWND + SNOW + SNWD + TMAX + TMIN,
                 data = training_fold)
  y_pred = predict(regressor, newdata = test_fold[-3])
  rsquare = R2(y_pred, test_fold$Sale..Dollars.)
  return(rsquare)
})
rsquare_with_k_fold = mean(as.numeric(cv))
rsquare_with_k_fold

## RMSE

folds = createFolds(alcohol_df_without_NA$Sale..Dollars., k = 10)
cv = lapply(folds, function(x) {
  training_fold = alcohol_df_without_NA[-x, ]
  test_fold = alcohol_df_without_NA
  regressor = lm(formula = Sale..Dollars. ~ PRCP + AWND + SNOW + SNWD + TMAX + TMIN,
                 data = training_fold)
  y_pred = predict(regressor, newdata = test_fold[-3])
  rsquare = RMSE(y_pred, test_fold$Sale..Dollars.)
  return(rsquare)
})
rmse_with_k_fold = mean(as.numeric(cv))
rmse_with_k_fold




#### Ridge regression 

library(glmnet)

independent_vars <- as.matrix(training_set[, c(8,9,10,11,12,13)])
dependent_vars <- training_set$Sale..Dollars.
lambda_seq <- 10^seq(2, -2, by = -.1)
#lambda_seq <- as.numeric(unlist(lambda_seq))
#dependent_vars <- as.numeric(unlist(dependent_vars))
lambda_seq

fit <- glmnet(independent_vars, dependent_vars, alpha = 0, lambda  = lambda_seq)
summary(fit)
ridge_cv <- cv.glmnet(independent_vars, dependent_vars, alpha = 0, lambda = lambda_seq)

# identifying best lamda

best_lambda <- ridge_cv$lambda.min
best_lambda
best_fit <- ridge_cv$glmnet.fit
head(best_fit)

# Rebuilding the model with best lamda value identified
best_ridge <- glmnet(independent_vars, dependent_vars, alpha = 0, lambda = 100)
coef(best_ridge)

# testing

test_independent_vars <- as.matrix(test_set[, c(8,9,10,11,12,13)])

pred <- predict(best_ridge, s = best_lambda, newx = test_independent_vars)
actual_sales <- test_set$Sale..Dollars.
R2(pred, actual_sales)
RMSE(pred, actual_sales)





# Lasso regression

lasso_x_vars <- as.matrix(training_set[, c(8,9,10,11,12,13)])
lasso_y_var <- training_set$Sale..Dollars.
lambda_seq <- 10^seq(2, -2, by = -.1)


# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(lasso_x_vars), nrow(lasso_x_vars)/2)
x_test = lasso_x_vars[-train,]
x_test
y_test = lasso_y_var[-train]
y_test
cv_output <- cv.glmnet(lasso_x_vars[train,], lasso_y_var[train],
                       alpha = 1, lambda = lambda_seq)

# identifying best lamda
best_lam <- cv_output$lambda.min
#best_lam


# Rebuilding the model with best lamda value identified
lasso_best <- glmnet(lasso_x_vars[train,], lasso_y_var[train], alpha = 1, lambda = best_lam)
lasso_pred <- predict(lasso_best, s = best_lam, newx = x_test)

# Finally, we combine the predicted values and actual values to see the two values side by side and then you can use the 
# R-Squared formula to check the model performance. Note - you must calculate the R-Squared values for both train and test dataset.

final <- cbind(y_test, lasso_pred)
# Checking the first six obs
head(final)
R2(y_test, lasso_pred)
RMSE(lasso_pred, y_test)



# Elastic Net regression

elastic_x_vars <- as.matrix(training_set[, c(8,9,10,11,12,13)])
elastic_y_var <- training_set$Sale..Dollars.
lambda_seq <- 10^seq(2, -2, by = -.1)


# Splitting the data into test and train
set.seed(86)
train = sample(1:nrow(elastic_x_vars), nrow(elastic_x_vars)/2)
elastic_net_x_test = elastic_x_vars[-train,]
elastic_net_y_test = elastic_y_var[-train]

elastic_cv_output <- cv.glmnet(elastic_x_vars[train,], elastic_y_var[train],
                               alpha = 1, lambda = lambda_seq)

# identifying best lamda
best_lam <- elastic_cv_output$lambda.min
#best_lam


# Rebuilding the model with best lamda value identified
elastic_best <- glmnet(elastic_x_vars[train,], elastic_y_var[train], alpha = 0.5, lambda = best_lam)
elastic_pred <- predict(elastic_best, s = best_lam, newx = elastic_net_x_test)

# Finally, we combine the predicted values and actual values to see the two values side by side and then you can use the 
# R-Squared formula to check the model performance. Note - you must calculate the R-Squared values for both train and test dataset.

final <- cbind(elastic_net_y_test, elastic_pred)
# Checking the first six obs
head(final)
RMSE(elastic_pred, elastic_net_y_test)
R2(elastic_net_y_test, elastic_pred)


#########
## SVR ##
#########

library(e1071)
library(caret)
svr_model <- svm(Sale..Dollars. ~ PRCP + AWND + SNOW + SNWD + TMAX + TMIN, training_set)

svr_y_pred <- predict(svr_model, test_set)

# Model performance
# (a) Prediction error, RMSE
RMSE(svr_y_pred, test_set$Sale..Dollars.)
# (b) R-square
R2(svr_y_pred, test_set$Sale..Dollars.)









