###################
###Create source###
###################

##########1st source
########descriptive_analytics_utils.R
## Utility functions
## data transformation
to.factors <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- as.factor(df[[variable]])
  }
  return(df)
}
## data analysis
# load dependencies
library(gridExtra) # grid layouts
library(pastecs) # details summary stats
library(ggplot2) # visualizations
library(gmodels) # build contingency tables
# analyze numerical variables
# summary statistics
get.numeric.variable.stats <- function(indep.var, detailed=FALSE){
  options(scipen=100)
  options(digits=2)
  if (detailed){
    var.stats <- stat.desc(indep.var)
  }else{
    var.stats <- summary(indep.var)
  }
  
  df <- data.frame(round(as.numeric(var.stats),2))
  colnames(df) <- deparse(substitute(indep.var))
  rownames(df) <- names(var.stats)
  
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  grid.table(t(df))
}
# visualizations
# histograms\density
visualize.distribution <- function(indep.var){
  pl1 <- qplot(indep.var, geom="histogram", fill=I('gray'), binwidth=5, col=I('black'))+ theme_bw()
  pl2 <- qplot(indep.var, geom="density", fill=I('gray'), binwidth=5, col=I('black'))+ theme_bw()
  
  grid.arrange(pl1,pl2, ncol=2)
}
# box plots
visualize.boxplot <- function(indep.var, dep.var){
  pl1 <- qplot(factor(0),indep.var, geom="boxplot", xlab = deparse(substitute(indep.var)), ylab="values") + theme_bw()
  pl2 <- qplot(dep.var,indep.var,geom="boxplot", xlab = deparse(substitute(dep.var)), ylab = deparse(substitute(indep.var))) + theme_bw()
  
  grid.arrange(pl1,pl2, ncol=2)
}
# analyze categorical variables
# summary statistics
get.categorical.variable.stats <- function(indep.var){
  
  feature.name = deparse(substitute(indep.var))
  df1 <- data.frame(table(indep.var))
  colnames(df1) <- c(feature.name, "Frequency")
  df2 <- data.frame(prop.table(table(indep.var)))
  colnames(df2) <- c(feature.name, "Proportion")
  
  df <- merge(
    df1, df2, by = feature.name
  )
  ndf <- df[order(-df$Frequency),]
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  grid.table(ndf)
}
# generate contingency table
get.contingency.table <- function(dep.var, indep.var, stat.tests=F){
  if(stat.tests == F){
    CrossTable(dep.var, indep.var, digits=1, prop.r=F, prop.t=F, prop.chisq=F)
  }else{
    CrossTable(dep.var, indep.var, digits=1, prop.r=F, prop.t=F, prop.chisq=F, chisq=T, fisher=T)
  }
}
# visualizations
# barcharts
visualize.barchart <- function(indep.var){
  qplot(indep.var, geom="bar", fill=I('gray'), col=I('black'), xlab = deparse(substitute(indep.var))) + theme_bw()
}
# mosaic plots
visualize.contingency.table <- function(dep.var, indep.var){
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  mosaicplot(dep.var ~ indep.var, color=T, main = "Contingency table plot")
}
##########2nd source
########performance_plot_utils.R
library(ROCR)
plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,col="black",lty=1, lwd=2, main=title.text, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
  abline(0,1, col="red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4,legend=c(paste0("AUC: ",auc)),cex=0.6,bty = "n",box.col = "white")
}
plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf,col="black",lty=1, lwd=2, main=title.text, cex.main=0.6, cex.lab=0.8, xaxs="i", yaxs="i")
}
########################Analize the Data#########################
source('descriptive_analytics_utils.R')
# Load the data
hr.df <- read.csv("C:/Users/my/Desktop/HR_comma_sep.csv", header = TRUE, sep = 
                    ",")
# Check data.frame
class(hr.df)
# Check the data
head(hr.df)
# Information about the dataset
str(hr.df)
# Check for missing values
sum(is.na(hr.df))
# Remove rows with missing values and check the dataset
sum(complete.cases(hr.df))
# Select variables to be converted to factor type
categorical.vars <- c( 'Department', 'salary', 'Work_accident', 'left', 
                       'promotion_last_5years')
# Change data type
hr.df <- to.factors(df = hr.df, variables = categorical.vars)
# Check the information of the modified dataset
str(hr.df)
## Data analysis
#load dependencies
library(car)
# Prepare the dataset variables for immediate use
attach(hr.df)
## Categorical data
# left stats
# A dummy variable-1: leave, 0: not leave
get.categorical.variable.stats(left)
# Visualize "left"
visualize.barchart(left)
# Work_accident stats and bar chart
# A dummy variable assessing whether(1) or not (0) they had an accident
get.categorical.variable.stats(Work_accident) #The majority of employees have no accidents
visualize.barchart(Work_accident)
# contingency table and mosaic plot
get.contingency.table(left, Work_accident, stat.tests = T)
#H0: There is no association between Work_accident and left.
#H1: There is an association between Work_accident and left.
#The p-value of 0.0005 from the Fisher test is much smaller than the significance level of 0.05, so we reject the null hypothesis.
#Therefore, there is an association between Work_accident and left.
#The p-value from the chi-squared test is also much smaller than the significance level of 0.05, so we reject the null hypothesis.
#Therefore, there is an association between Work_accident and left.
visualize.contingency.table(left, Work_accident)
# promotion_last_5years stats and bar chart
# 1: promoted, 0: not promoted
get.categorical.variable.stats(promotion_last_5years)
visualize.barchart(promotion_last_5years)
# contingency table and mosaic plot
get.contingency.table(left, promotion_last_5years, stat.tests = T)
visualize.contingency.table(left, promotion_last_5years)
#H0: There is no association between promotion_last_5years and left.
#H1: There is an association between promotion_last_5years and left.
#The p-value of 0.0005 from the Fisher test is much smaller than the significance level of 0.05, so we reject the null hypothesis.
#Therefore, there is an association between promotion_last_5years and left.
#The p-value from the chi-squared test is also much smaller than the significance level of 0.05, so we reject the null hypothesis.
#Therefore, there is an association between promotion_last_5years and left.

## Department stats and bar chart
get.categorical.variable.stats(Department)
visualize.barchart(Department)
# The distribution is particularly high in sales, support, and technical.
# View by the number of left employees, excluding the entire dataset.
library(dplyr)
ff <- hr.df%>%filter(hr.df$left==1)
gg <- hr.df%>%filter(hr.df$left==0)
hist(as.numeric(ff$Department))
#plot2
library(dplyr)
ff <- hr.df%>%filter(hr.df$left==1)
gg <- hr.df%>%filter(hr.df$left==0)
hist(as.numeric(ff$Department))
dp_data <- ff%>% group_by(left,Department) %>% summarise(total = n())
attach(dp_data)
dp_data_1 <- dp_data%>% group_by(left) %>% mutate(percent = round((total / 
                                                                     sum(total)),3))
dp_data_1
dp_data2 <- gg%>% group_by(left,Department) %>% summarise(total = n())
attach(dp_data)
dp_data_2 <- dp_data2%>% group_by(left) %>% mutate(percent = round((total / 
                                                                      sum(total)),3))
dp_data_2
df=rb
# Calculate the proportion by department for employees who left
dp_data <- hr.df %>% group_by(left,Department) %>% summarise(total = n())
dp_data_1 <- dp_data%>% group_by(left) %>% mutate(percent = total / sum(total))
dp_data_1
dp_data%>% group_by(Department)
#test
fisher.test(left,Department, simulate.p.value=TRUE)
chisq.test(left,Department)
# Check the association between 'left' and 'Department'
# Department categories: accounting, HR, IT, management, marketing, sales, 
# Product_mng (Production Management), RandD (Research and Development), support, technical.
# The IT department oversees the installation and maintenance of the company's computer network systems.
## salary stats and bar chart
# Salary variable with these 3 categories: low, medium, high
hr.df$salary <- factor(hr.df$salary, levels=c("high","medium","low"))
levels(hr.df$salary)
library(vcd)
c <- with(hr.df,table(left, salary))
c
mosaic(c, direction = "v")
attach(hr.df)
get.categorical.variable.stats(salary) #Salary frequency table
visualize.barchart(salary) # Salary histogram
# Low has the most employees, and high has the fewest.
# fisher/chisq test and mosaic plot
#Change the order of low, medium, and high by specifying it as a factor.
fisher.test(left, salary, simulate.p.value=TRUE)
# H0: There is no association between left and salary.
# H1: There is an association between left and salary.
# fisher.test (p=0.0005 < 0.05, reject the null hypothesis,
# there is an association between left and salary.
chisq.test(left, salary)
# chisq.test (p < 0.05, reject the null hypothesis,
# there is an association between left and salary.
visualize.contingency.table(left, salary)
# The proportion of left employees is low in the high salary group and high in the low salary group.
# In other words, employees with higher salaries are less likely to leave, while those with lower salaries are more likely to leave.
# When looking at the non-left employees, the proportions of low and medium salary groups are almost the same, 
# while the proportion is relatively lower in the high salary group.

###### Continuous Variables #######
library(ggplot2)

## Number of projects an employee works on during their employment
# number_project stats and bar chart
get.numeric.variable.stats(number_project)
# histogram\density plot
ggplot(hr.df,aes(x=number_project))+geom_histogram(binwidth=0.5)
d<-density(number_project)
plot(d)
ggplot(hr.df, aes(x = number_project, fill = left)) + geom_histogram(bins=6,alpha=0.7)
# box plot
p<-ggplot(hr.df, aes(x=left, y=number_project, fill=left)) +
  geom_boxplot()
p + geom_jitter(shape=1, alpha=0.3, position=position_jitter(0.2))

## Years of service
# time_spend_company stats and bar chart
get.numeric.variable.stats(time_spend_company)
# histogram\density plot
ggplot(hr.df,aes(x=time_spend_company))+geom_histogram(binwidth=0.5)
d<-density(time_spend_company)
plot(d)
ggplot(hr.df, aes(x = time_spend_company, fill = left)) + 
  geom_histogram(bins=9,alpha=0.7)
# box plot
p<-ggplot(hr.df, aes(x=left, y=time_spend_company, fill=left)) +
  geom_boxplot()
p + geom_jitter(shape=1, alpha=0.3, position=position_jitter(0.2))
# The higher the average years of service, the higher the turnover rate. The tail for employees who did not leave is longer.

## satisfaction_level analysis
# Employee satisfaction (0 to 1)
get.numeric.variable.stats(satisfaction_level)
# histogram\density plot
ggplot(hr.df,aes(x=satisfaction_level))+geom_histogram(binwidth=0.1)
d<-density(satisfaction_level)
plot(d)
ggplot(hr.df, aes(x = satisfaction_level, fill = left)) + 
  geom_histogram(bins=10,alpha=0.7)
# box plot
p<-ggplot(hr.df, aes(x=left, y=satisfaction_level, fill=left)) +
  geom_boxplot()
p + geom_jitter(shape=1, alpha=0.3, position=position_jitter(0.2))
# It can be observed that the lower the satisfaction level, the higher the number of employees who leave.

## last_evaluation
# Manager's rating (0 to 1)
get.numeric.variable.stats(last_evaluation)
# Maximum 1, minimum 0.36, average 0.72
# histogram\density plot
ggplot(hr.df,aes(x=last_evaluation))+geom_histogram(binwidth=0.1)
d<-density(last_evaluation)
plot(d)
ggplot(hr.df, aes(x = last_evaluation, fill = left)) + geom_histogram(bins=10,alpha=0.7)
# Employees who left tend to have lower scores, but some also received high scores.
# box plot
p<-ggplot(hr.df, aes(x=left, y=last_evaluation, fill=left)) +
  geom_boxplot()
p + geom_jitter(shape=1, alpha=0.3, position=position_jitter(0.2))
# The evaluation scores of employees who left have a wide range.

## Hours worked in a month
#average_monthly_hours
# average_montly_hours analysis
get.numeric.variable.stats(average_montly_hours)
# Maximum 310, minimum 96, average 201.05
# histogram\density plot
ggplot(hr.df,aes(x=average_montly_hours))+geom_histogram(binwidth=0.5)
d<-density(average_montly_hours)
plot(d)
ggplot(hr.df, aes(x = average_montly_hours, fill = left)) + 
  geom_histogram(bins=10,alpha=0.7)
# box plot
p<-ggplot(hr.df, aes(x=left, y=average_montly_hours, fill=left)) +
  geom_boxplot()
p + geom_jitter(shape=1, alpha=0.3, position=position_jitter(0.2))

## Normalization
# satisfaction_level, last_evaluation, average_monthly_hours, 
number_project,time_spend_company
# Five continuous variables
# Since they have different ranges, normalization is applied.
# Check other functions like scale
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}
# normalize variables
attach(hr.df)
numeric.vars <- c("satisfaction_level", "last_evaluation", "average_montly_hours", "number_project","time_spend_company")
hr.df <- scale.features(hr.df, numeric.vars)
attach(hr.df)
##### Continuous-Continuous Correlation ############
hr.df_cor=cor(hr.df[,c(1:5)])
par(mfcol=c(1,1))
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(hr.df_cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45, col=col(200), addCoef.col="black", order="AOE")
# No multicollinearity

###### Analyze the modified data using machine learning
#=====================================================
#=== Logistic Regression (LR) ========================
#=====================================================
attach(hr.df)
library(caret) # model training and evaluation
library(ROCR) # model evaluation
source("performance_plot_utils.R") #plotting metric results
# Split the data into 60% for training and the remaining 40% for testing
indexes <- sample(1:nrow(hr.df), size=0.6*nrow(hr.df))
train.data <- hr.df[indexes,]
test.data <- hr.df[-indexes,]
## separate feature and class variables
# Exclude dependent variable, include independent variables
test.feature.vars <- test.data[,-7]
test.class.var <- test.data[,7]
# Build a logistic regression model
# Try with all variables: formula.init <- "left ~ ."
formula.init <- as.formula(formula.init)
lr.model <- glm(formula=formula.init, data=train.data, family="binomial")
# view model details
summary(lr.model)
# perform and evaluate predictions
lr.predictions <- predict(lr.model, test.data, type="response")
lr.predictions <- as.factor(round(lr.predictions)) ##
confusionMatrix(data=lr.predictions, reference=test.class.var, positive='1')
#auc plot
lr.prediction.values <- predict(lr.model, test.feature.vars, type="response")
predictions <- prediction(lr.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="LR ROC Curve")
plot.pr.curve(predictions, title.text="LR Precision/Recall Curve")
## glm specific feature selection
formula <- "left ~ ."
formula <- as.formula(formula)
control <- trainControl(method="repeatedcv", number=10, repeats=2)
model <- train(formula, data=hr.df, method="glm", 
               trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance)
# build new model with selected features
formula.new <- "left ~ satisfaction_level +time_spend_company + Work_accident + 
salary +number_project"
formula.new <- as.formula(formula.new)
lr.model.new <- glm(formula=formula.new, data=hr.df, family="binomial")
# view model details
summary(lr.model.new)
# perform and evaluate predictions 
lr.predictions.new <- predict(lr.model.new, test.data, type="response") 
lr.predictions.new <- as.factor(round(lr.predictions.new)) ##
confusionMatrix(data=lr.predictions.new, reference=test.class.var, positive='1')
## model performance evaluations
# plot best model evaluation metric curves
#best model
lr.model.best <- lr.model.new
lr.prediction.values <- predict(lr.model.best, test.feature.vars, type="response")
predictions <- prediction(lr.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="LR ROC Curve")
plot.pr.curve(predictions, title.text="LR Precision/Recall Curve")

#=====================================================
#=== SVM =============================================
#=====================================================
# Split the data into 60% for training and the remaining 40% for testing
indexes <- sample(1:nrow(hr.df), size=0.6*nrow(hr.df))
train.data <- hr.df[indexes,]
test.data <- hr.df[-indexes,]
library(e1071) # svm model
library(caret) # model training\optimizations
library(kernlab) # svm model for hyperparameters
library(ROCR) # model evaluation
library(hexbin)
source("performance_plot_utils.R") # plot model metrics
## separate feature and class variables
# Exclude dependent variable, include independent variables
test.feature.vars <- test.data[,-7] 
test.class.var <- test.data[,7] 
## build initial model with training data
# Create an initial model with all variables
formula.init <- "left ~ ." 
# Convert input values into formula form
formula.init <- as.formula(formula.init) 
# Set the SVM model
# kernel: The kernel used for training and prediction, typically does not have a major effect in real problems
# gamma: The slope of the hyperplane
# cost: Degree of regularization to avoid overfitting
svm.model <- svm(formula=formula.init, data=train.data, 
                 kernel="radial", cost=100, gamma=1)
## view inital model details
# Summary of the model fitted using the svm() function
summary(svm.model)
## predict and evaluate results
# predict function: svm.model is the svm object, test.feature.vars is the object containing the new input data
svm.predictions <- predict(svm.model, test.feature.vars)
# ConfusionMatirx
# svm.predictions: Predicted values as a factor vector
# test.class.var: Actual values as a factor vector
confusionMatrix(data=svm.predictions, reference=test.class.var, positive="1")
## svm specific feature selection
formula.init <- "left ~ ."
formula.init <- as.formula(formula.init)
# trainControl() : Applies a consistent comparison method to each candidate
# Sampling method = repeated cross-validation / Number of cross-validation = 10 / Number of repetitions in cross-validation = 2
control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)

# Train the prediction model
model <- train(formula.init, data=train.data, method="svmRadial", 
               trControl=control)
# variables of importance
importance <- varImp(model, scale=FALSE)
# Select top 5 variables from the plot
plot(importance, cex.lab=0.5) 
## Build a new model with the selected features
# Create a new model using the variables identified from varimp
formula.new <- "left ~ satisfaction_level + time_spend_company + salary + Work_accident + number_project" 
formula.new <- as.formula(formula.new)
# Fit the SVM model
svm.model.new <- svm(formula=formula.new, data=train.data, 
                     kernel="radial", cost=10, gamma=0.25)
## predict results with new model on test data
svm.predictions.new <- predict(svm.model.new, test.feature.vars)
## new model performance evaluation
# ConfusionMatirx
confusionMatrix(data=svm.predictions.new, reference=test.class.var, positive="1")
## hyperparameter optimizations
# run grid search
cost.weights <- c(0.1, 10, 100)
gamma.weights <- c(0.01, 0.25, 0.5, 1)
# Apply cross-validation with different cost.weights and gamma.weights values
tuning.results <- tune(svm, formula.new, 
                       data = train.data, kernel="radial", 
                       ranges=list(cost=cost.weights, gamma=gamma.weights))
# view optimization results
print(tuning.results)
# plot results
plot(tuning.results, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i") #Figure 1 
# get best model and evaluate predictions
svm.model.best = tuning.results$best.model
svm.predictions.best <- predict(svm.model.best, test.feature.vars)
# ConfusionMatirx
confusionMatrix(data=svm.predictions.best, reference=test.class.var, positive="1")
# plot best model evaluation metric curves
svm.predictions.best <- predict(svm.model.best, test.feature.vars, decision.values = T)
# Check all the assigned attributes at once
svm.prediction.values <- attributes(svm.predictions.best)$decision.values
predictions <- prediction(svm.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="SVM ROC Curve")
plot.pr.curve(predictions, title.text="SVM Precision/Recall Curve") #Figure 2 
## model optimizations based on ROC
# ROC-based model optimization
# data transformation
transformed.train <- train.data
transformed.test <- test.data
for (variable in categorical.vars){
  new.train.var <- make.names(train.data[[variable]])
  transformed.train[[variable]] <- new.train.var
  new.test.var <- make.names(test.data[[variable]])
  transformed.test[[variable]] <- new.test.var
}
transformed.train <- to.factors(df=transformed.train, variables=categorical.vars)
transformed.test <- to.factors(df=transformed.test, variables=categorical.vars)
# Create variables excluding the dependent variable
transformed.test.feature.vars <- transformed.test[,-7] 
transformed.test.class.var <- transformed.test[,7] 
# view data to understand transformations
summary(transformed.train$left) 
# Build the optimal model based on AUC
# Quickly generate combinations of all vectors
grid <- expand.grid(C=c(1,10,100), 
                    sigma=c(0.01, 0.05, 0.1, 0.5, 1))
# Sampling method = cross_validation / Number of cross_validations = 10
ctr <- trainControl(method='cv', number=10, classProbs=TRUE, summaryFunction=twoClassSummary)
svm.roc.model <- train(formula.init, transformed.train, method='svmRadial', trControl=ctr, 
                       tuneGrid=grid, metric="ROC")
# predict and evaluate model performance
predictions <- predict(svm.roc.model, transformed.test.feature.vars)
# ConfusionMatirx
confusionMatrix(predictions, transformed.test.class.var, positive = "X1")
## plot model evaluation metric curves
svm.predictions <- predict(svm.roc.model, transformed.test.feature.vars, type="prob")
svm.prediction.values <- svm.predictions[,2]
predictions <- prediction(svm.prediction.values, test.class.var)
par(mfrow=c(1,2))
# ROC Cure
plot.roc.curve(predictions, title.text="SVM ROC Curve")
plot.pr.curve(predictions, title.text="SVM Precision/Recall Curve") #Figure 3 

#=============================================================== 
#=== Decision tree classifier ==================================
#===============================================================
# Split the data into 60% for training and the remaining 40% for testing
indexes <- sample(1:nrow(hr.df), size=0.6*nrow(hr.df))
train.data <- hr.df[indexes,]
test.data <- hr.df[-indexes,]
library(rpart)# tree models 
library(caret) # feature selection
library(rpart.plot) # plot dtree
library(ROCR) # model evaluation
library(e1071) # tuning model
source("performance_plot_utils.R") # plotting curves # plotting curves
## separate feature and class variables
# Create data without the dependent variable
test.feature.vars <- test.data[,-7]
test.class.var <- test.data[,7]
# Build a model with all variables
# Use the formula "left ~ ."
formula.init <- "left ~ ."
formula.init <- as.formula(formula.init)
# Set an appropriate cp value using dt. model$cptable / minsplit: minimum number of observations
dt.model <- rpart(formula=formula.init, method="class",data=train.data, 
                  control = rpart.control(minsplit=10, cp=0.05))
## predict and evaluate results
# Perform predictions with the model (use test.feature.vars, remove the dependent variable from test.data)
# Predict the class for each classification (type = "class")
dt.predictions <- predict(dt.model, test.feature.vars, type="class")
# Evaluate the results
confusionMatrix(data=dt.predictions, reference=test.class.var, positive="1")
str(hr.df)
# Perform predictions with the model (calculate AUC)
dt.predictions2 <- predict(dt.model, test.feature.vars, type="prob")
dt.prediction.values <- dt.predictions2[,2]
# Make predictions using test.class.var
predictions <- prediction(dt.prediction.values, test.class.var)
par(mfrow=c(1,2))
# ROC curve (AUC: 0.96)
plot.roc.curve(predictions, title.text="DT ROC Curve")
plot.pr.curve(predictions, title.text="DT Precision/Recall Curve")
## dt specific feature selection
formula.init <- "left ~ ."
formula.init <- as.formula(formula.init)
# Apply a consistent comparison method for evaluation
# Repeat k(number)-fold cross-validation 2 times (repeats)
# k(number)-fold cross-validation: Create folds based on the number and divide examples
control <- trainControl(method="repeatedcv", number=10, repeats=2)
# Train the prediction model (formula.init)
model <- train(formula.init, data=train.data, method="rpart", 
               trControl=control)
# Calculate feature importance
importance <- varImp(model, scale=FALSE)
# Visualization
plot(importance, cex.lab=0.5)
## build new model with selected features
# Extract variables based on importance => Create a new model
formula.new <- "left ~ satisfaction_level+average_montly_hours+time_spend_company+last_evaluation+numb
er_project"
formula.new <- as.formula(formula.new)
dt.model.new <- rpart(formula=formula.new, method="class",data=train.data, 
                      control = rpart.control(minsplit=10,cp=0.05), parms = list(prior = c(0.7, 0.3)))
## predict and evaluate results
# Predict the class for each classification (type = "class")
dt.predictions.new <- predict(dt.model.new, test.feature.vars, type="class")
# Calculate and evaluate the results
confusionMatrix(data=dt.predictions.new, reference=test.class.var, positive="1")
# Accuracy, Sensitivity, Specificity values increase
# View model details
# Set dt.model.new as the final model
dt.model.best <- dt.model.new
print(dt.model.best)
par(mfrow=c(1,1))
# Decision tree
prp(dt.model.new, type=1, extra=3, varlen=0, faclen=0)
## plot model evaluation metric curves
# Predict the probabilities for each class (type = "prob")
dt.predictions.best <- predict(dt.model.best, test.feature.vars, type="prob")
dt.prediction.values <- dt.predictions.best[,2]
# Make predictions using test.class.var
predictions <- prediction(dt.prediction.values, test.class.var)
par(mfrow=c(1,2))
# ROC curve (AUC: 0.96)
plot.roc.curve(predictions, title.text="DT ROC Curve")
plot.pr.curve(predictions, title.text="DT Precision/Recall Curve")
# Split the data into 60% for training and the remaining 40% for testing
indexes <- sample(1:nrow(hr.df), size=0.6*nrow(hr.df))
train.data <- hr.df[indexes,]
test.data <- hr.df[-indexes,]

#================================================== 
#===Random Forest==================================
#==================================================
#install.packages("randomForest")
library(randomForest) #rf model
library(caret) # feature selection
library(e1071) # model tuning
library(ROCR) # model evaluation
source("performance_plot_utils.R") # plot curves
## separate feature and class variables
test.feature.vars <- test.data[,-7]
test.class.var <- test.data[,7]
## build initial model with training data
formula.init <- "left ~ ."
formula.init <- as.formula(formula.init)
rf.model <- randomForest(formula.init, data = train.data, importance=T, 
                         proximity=T)
## view model details
print(rf.model)
## predict and evaluate results
rf.predictions <- predict(rf.model, test.feature.vars, type="class")
confusionMatrix(data=rf.predictions, reference=test.class.var, positive="1")
## build new model with selected features
formula.new <- "left ~ satisfaction_level+number_project+time_spend_company+average_montly_hours+last_
evaluation"
formula.new <- as.formula(formula.new)
rf.model.new <- randomForest(formula.new, data = train.data, 
                             importance=T, proximity=T)
## predict and evaluate results
rf.predictions.new <- predict(rf.model.new, test.feature.vars, type="class")
confusionMatrix(data=rf.predictions.new, reference=test.class.var, positive="1")
## hyperparameter optimizations
# run grid search
nodesize.vals <- c(2, 3, 4, 5)
ntree.vals <- c(200, 500, 1000, 2000)
tuning.results <- tune.randomForest(formula.new, 
                                    data = train.data,
                                    mtry=3, 
                                    nodesize=nodesize.vals,
                                    ntree=ntree.vals)
print(tuning.results)
# get best model and predict and evaluate performance 
rf.model.best <- tuning.results$best.model
rf.predictions.best <- predict(rf.model.best, test.feature.vars, type="class")
confusionMatrix(data=rf.predictions.best, reference=test.class.var, positive="1")
## plot model evaluation metric curves (full model)
rf.predictions.best <- predict(rf.model, test.feature.vars, type="prob")
rf.prediction.values <- rf.predictions.best[,2]
predictions <- prediction(rf.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="RF ROC Curve")
plot.pr.curve(predictions, title.text="RF Precision/Recall Curve")
## plot model evaluation metric curves (select 5)
rf.predictions.best <- predict(rf.model.new, test.feature.vars, type="prob")
rf.prediction.values <- rf.predictions.best[,2]
predictions <- prediction(rf.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="RF ROC Curve")
plot.pr.curve(predictions, title.text="RF Precision/Recall Curve")
## plot model evaluation metric curves (model 3)
rf.predictions.best <- predict(rf.model.best, test.feature.vars, type="prob")
rf.prediction.values <- rf.predictions.best[,2]
predictions <- prediction(rf.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="RF ROC Curve")
plot.pr.curve(predictions, title.text="RF Precision/Recall Curve")

#=============================================================== 
#=== NN classifier =============================================
#===============================================================
library(caret)
library(ROCR)
source("performance_plot_utils.R")
# separate feature and class variables
test.feature.vars <- test.data[,-7]
test.class.var <- test.data[,7]
# Data transformation
transformed.train <- train.data
transformed.test <- test.data
for (variable in categorical.vars){
  new.train.var <- make.names(train.data[[variable]])
  transformed.train[[variable]] <- new.train.var
  new.test.var <- make.names(test.data[[variable]])
  transformed.test[[variable]] <- new.test.var
}
transformed.train <- to.factors(df=transformed.train, variables=categorical.vars)
transformed.test <- to.factors(df=transformed.test, variables=categorical.vars)
transformed.test.feature.vars <- transformed.test[,-7]
transformed.test.class.var <- transformed.test[,7]
# Create a model with the training data
formula.init <- "left ~ ."
formula.init <- as.formula(formula.init)
nn.model <- train(formula.init, data = transformed.train, method="nnet")
# Check the model
print(nn.model)
# Make predictions and evaluate the results
nn.predictions <- predict(nn.model, transformed.test.feature.vars, type="raw")
confusionMatrix(data=nn.predictions, reference=transformed.test.class.var, 
                positive="X1")
# plot model evaluation metric curves
nn.predictions <- predict(nn.model, transformed.test.feature.vars, type="prob")
nn.prediction.values <- nn.predictions[,2]
predictions <- prediction(nn.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="NN ROC Curve")
plot.pr.curve(predictions, title.text="NN Precision/Recall Curve")
# Calculate the classification evaluation metrics for the entire model
TN <- 4415
FN <- 118
FP <- 129
TP <- 1338
FNR <- FN/(TP+FN)
FPR <- FP/(TN+FP)
Accuracy <- (TP+TN)/(TN+FN+FP+TP);Accuracy
error <- 1-Accuracy
Sensitivity <- TP/(FN+TP);Sensitivity
Specificity <- TN/(TN+FP);Specificity
Precision <- TP/(TP+FP);Precision
Recall <- TP/(TP+FN);Recall
Profit <- 0.4*Precision+0.4*Sensitivity+0.2*Accuracy;Profit
Loss <- 0.5*error+0.4*FNR+0.1*FPR ;Loss
Total_Gain <- Profit-Loss;Total_Gain
# Select specific variables
formula.init <- "left ~ ."
formula.init <- as.formula(formula.init)
control <- trainControl(method="repeatedcv", number=10, repeats=2)
model <- train(formula.init, data=transformed.train, method="nnet", 
               trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)
## Select the top 5
# Create a new model with the selected variables
formula.new <- "left ~ number_project + satisfaction_level + time_spend_company + 
promotion_last_5years +last_evaluation"
formula.new <- as.formula(formula.new)
nn.model.new <- train(formula.new, data=transformed.train, method="nnet")
# Make predictions and evaluate the results for the new model
nn.predictions.new <- predict(nn.model.new, transformed.test.feature.vars, 
                              type="raw")
confusionMatrix(data=nn.predictions.new, reference=transformed.test.class.var, 
                positive="X1")
# view hyperparameter optimizations
plot(nn.model.new, cex.lab=0.5)
# plot model evaluation metric curves
nn.model.best <- nn.model.new
nn.predictions.best <- predict(nn.model.best, transformed.test.feature.vars, 
                               type="prob")
nn.prediction.values <- nn.predictions.best[,2]
predictions <- prediction(nn.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="NN ROC Curve")
plot.pr.curve(predictions, title.text="NN Precision/Recall Curve")
#===== Visualize rf model
# Visualize the confusion matrix
library(ggplot2)
test <- hr.df[indexes,]
test$pred <- predict(rf.model.new, hr.df[indexes,]) # Input the predicted classification
ggplot(test, aes(left, pred, color = left)) + geom_jitter(width = 0.2, height = 0.1, 
                                                          size=2) +
  labs(title="Confusion Matrix", subtitle="Predicted vs. Observed from HR dataset", 
       y="Predicted", x="Truth")
# Plot the relative importance of variables
importance(rf.model.new)
varImpPlot(rf.model.new)
# Relative size (accuracy) when satisfaction level is 100%
rel.importance<-rf.model.new$importance[,3]/max(rf.model.new$importance[,3])*100
rel.importance.dec<-sort(rel.importance, decreasing=T)
barplot(rel.importance.dec,horiz=T,names=names(rel.importance.dec))
# Relative size (importance) when satisfaction level is 100%
rel.importance<-rf.model.new$importance[,4]/max(rf.model.new$importance[,4])*100
rel.importance.dec<-sort(rel.importance, decreasing=T)
barplot(rel.importance.dec,horiz=T,names=names(rel.importance.dec))