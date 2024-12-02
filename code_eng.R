###################
###Create Source files###
###################

##########Create the first source code
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
  pl1 <- qplot(indep.var, geom="histogram",
               fill=I('gray'), binwidth=5,
               col=I('black'))+ theme_bw()
  pl2 <- qplot(indep.var, geom="density",
               fill=I('gray'), binwidth=5,
               col=I('black'))+ theme_bw()
  
  grid.arrange(pl1,pl2, ncol=2)
}

# box plots
visualize.boxplot <- function(indep.var, dep.var){
  pl1 <- qplot(factor(0),indep.var, geom="boxplot",
               xlab = deparse(substitute(indep.var)),
               ylab="values") + theme_bw()
  pl2 <- qplot(dep.var,indep.var,geom="boxplot",
               xlab = deparse(substitute(dep.var)),
               ylab = deparse(substitute(indep.var))) + theme_bw()
  
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
    CrossTable(dep.var, indep.var, digits=1,
               prop.r=F, prop.t=F, prop.chisq=F)
  }else{
    CrossTable(dep.var, indep.var, digits=1,
               prop.r=F, prop.t=F, prop.chisq=F,
               chisq=T, fisher=T)
  }
}

# visualizations
# barcharts
visualize.barchart <- function(indep.var){
  qplot(indep.var, geom="bar",
        fill=I('gray'), col=I('black'),
        xlab = deparse(substitute(indep.var))) + theme_bw()
}

# mosaic plots
visualize.contingency.table <- function(dep.var, indep.var){
  if (names(dev.cur()) != "null device"){
    dev.off()
  }
  mosaicplot(dep.var ~ indep.var, color=T,
             main = "Contingency table plot")
}


##########Create the second source code
########performance_plot_utils.R
library(ROCR)


plot.roc.curve <- function(predictions, title.text){
  perf <- performance(predictions, "tpr", "fpr")
  plot(perf,col="black",lty=1, lwd=2,
       main=title.text, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i")
  abline(0,1, col="red")
  auc <- performance(predictions,"auc")
  auc <- unlist(slot(auc, "y.values"))
  auc <- round(auc,2)
  legend(0.4,0.4,legend=c(paste0("AUC: ",auc)),cex=0.6,bty = "n",box.col = "white")
  
}

plot.pr.curve <- function(predictions, title.text){
  perf <- performance(predictions, "prec", "rec")
  plot(perf,col="black",lty=1, lwd=2,
       main=title.text, cex.main=0.6, cex.lab=0.8, xaxs="i", yaxs="i")
}

########################Start analysis#########################
source('descriptive_analytics_utils.R')

# Load the data
hr.df <- read.csv("C:/Users/my/Desktop/HR_comma_sep.csv", header = TRUE, sep = ",")

# Check data.frame
class(hr.df)

# Check data
head(hr.df)

# dataset
str(hr.df)



# Check for missing values
sum(is.na(hr.df))

# Check the dataset after removing rows with missing values
sum(complete.cases(hr.df))

# Select variables to convert to factor
categorical.vars <- c( 'Department', 'salary', 'Work_accident', 'left', 'promotion_last_5years')


# Change data type
hr.df <- to.factors(df = hr.df, variables = categorical.vars)

# Check the modified dataset
str(hr.df)



## Data analysis

#load dependencies
library(car)
# Make the variables in the dataset directly accessible
attach(hr.df)


## Categorical Data

# left stats
# A dummy variable-1: leave, 0: not leave
get.categorical.variable.stats(left)

# visualize the Left variable
visualize.barchart(left)

# Work_accident stats and bar chart
# A dummy variable assessing whether(1) or not (0) they had an accident
get.categorical.variable.stats(Work_accident) #Mostly cases with no accidents
visualize.barchart(Work_accident)

# contingency table and mosaic plot
get.contingency.table(left, Work_accident, stat.tests = T)
# H0: There is no association between Work_accident and left.  
# H1: There is an association between Work_accident and left.  
# The p-value of 0.0005 obtained from the Fisher test is much smaller than the significance level of 0.05, so we reject the null hypothesis.  
# Therefore, there is an association between Work_accident and left.  
# The p-value obtained from the Chi-square test is much smaller than the significance level of 0.05, so we reject the null hypothesis.  
# Therefore, there is an association between Work_accident and left.
visualize.contingency.table(left, Work_accident)


# promotion_last_5years stats and bar chart
# 1: promoted, 0: not promoted
get.categorical.variable.stats(promotion_last_5years)
visualize.barchart(promotion_last_5years)

# contingency table and mosaic plot
get.contingency.table(left, promotion_last_5years, stat.tests = T)
visualize.contingency.table(left, promotion_last_5years)
# H0: There is no association between promotion_last_5years and left.  
# H1: There is an association between promotion_last_5years and left.  
# The p-value of 0.0005 obtained from the Fisher test is much smaller than the significance level of 0.05, so we reject the null hypothesis.  
# Therefore, there is an association between promotion_last_5years and left.  
# The p-value obtained from the Chi-square test is also much smaller than the significance level of 0.05, so we reject the null hypothesis.  
# Therefore, there is an association between promotion_last_5years and left.

## Department stats and bar chart
get.categorical.variable.stats(Department)
visualize.barchart(Department)

# There is a particularly large distribution in sales, support, and technical.

# Examine the "left" variable.
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
dp_data_1 <- dp_data%>% group_by(left) %>% mutate(percent = round((total / sum(total)),3))
dp_data_1
dp_data2 <- gg%>% group_by(left,Department) %>% summarise(total = n())
attach(dp_data)
dp_data_2 <- dp_data2%>% group_by(left) %>% mutate(percent = round((total / sum(total)),3))
dp_data_2
df=rb

# Calculate the proportion by department for employees who left.
dp_data <- hr.df %>% group_by(left,Department) %>% summarise(total = n())
dp_data_1 <- dp_data%>% group_by(left) %>% mutate(percent = total / sum(total))
dp_data_1
dp_data%>% group_by(Department)
#test
fisher.test(left,Department, simulate.p.value=TRUE)
chisq.test(left,Department)
# There is an association between left and Department.
visualize.contingency.table(left,Department)

#accounting, HR, IT, managemet, marketing, sales
#Prouduct_mng, RandD, support, technical

## salary stats and bar chart
# Three categories(low, medium, high)

hr.df$salary <- factor(hr.df$salary, levels=c("high","medium","low"))
levels(hr.df$salary)
library(vcd)
c <- with(hr.df,table(left, salary))
c
mosaic(c, direction = "v")

attach(hr.df)

get.categorical.variable.stats(salary) # Frequency table of salary

visualize.barchart(salary) # Histogram of salary
# "Low" is the most common, and "High" has the fewest.

# Fisher/Chi-square test and mosaic plot with the specified order of "low", "medium", and "high" as factors.
fisher.test(left, salary, simulate.p.value=TRUE)
# H0: There is no association between left and salary.  
# H1: There is an association between left and salary.  
# The p-value from fisher.test (p=0.0005 < 0.05) is smaller than the significance level, so we reject the null hypothesis.  
# Therefore, there is an association between left and salary.
chisq.test(left, salary)
# The p-value from chisq.test (p < 0.05) is smaller than the significance level, so we reject the null hypothesis.  
# Therefore, there is an association between left and salary.

visualize.contingency.table(left, salary)
# The proportion of employees who left is lower in the "high" salary group and higher in the "low" salary group.  
# In other words, employees with higher salaries are less likely to leave, while those with lower salaries are more likely to leave.  
# Looking at the proportion of employees who did not leave, the proportions in the "low" and "medium" salary groups are almost the same,  
# whereas the proportion is relatively lower in the "high" salary group.  
# Change the order of the salary factor.

######Continuous variable###########
library(ggplot2)
# number_project stats and bar chart
# The number of projects an employee works on during their employment.
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


## time_spend_company stats and bar chart
## Years of service
get.numeric.variable.stats(time_spend_company)

# histogram\density plot
ggplot(hr.df,aes(x=time_spend_company))+geom_histogram(binwidth=0.5)
d<-density(time_spend_company)
plot(d)
ggplot(hr.df, aes(x = time_spend_company, fill = left)) + geom_histogram(bins=9,alpha=0.7)


# box plot
p<-ggplot(hr.df, aes(x=left, y=time_spend_company, fill=left)) +
  geom_boxplot()
p + geom_jitter(shape=1, alpha=0.3, position=position_jitter(0.2))

# The higher the average years of service, the higher the turnover rate. The tail for employees who did not leave is longer.

## satisfaction_level(0 to 1) analysis
get.numeric.variable.stats(satisfaction_level)

# histogram\density plot
ggplot(hr.df,aes(x=satisfaction_level))+geom_histogram(binwidth=0.1)
d<-density(satisfaction_level)
plot(d)
ggplot(hr.df, aes(x = satisfaction_level, fill = left)) + geom_histogram(bins=10,alpha=0.7)

# box plot
p<-ggplot(hr.df, aes(x=left, y=satisfaction_level, fill=left)) +
  geom_boxplot()
p + geom_jitter(shape=1, alpha=0.3, position=position_jitter(0.2))



#It can be observed that the lower the satisfaction level, the higher the number of employees who leave.

## last_evaluation
# Manager's evaluation (0 to 1)
get.numeric.variable.stats(last_evaluation)
# The maximum is 1, the minimum is 0.36, and the average is 0.72.

# histogram\density plot
ggplot(hr.df,aes(x=last_evaluation))+geom_histogram(binwidth=0.1)
d<-density(last_evaluation)
plot(d)
ggplot(hr.df, aes(x = last_evaluation, fill = left)) + geom_histogram(bins=10,alpha=0.7)


# Employees who left have received both low and high scores.

# box plot
p<-ggplot(hr.df, aes(x=left, y=last_evaluation, fill=left)) +
  geom_boxplot()
p + geom_jitter(shape=1, alpha=0.3, position=position_jitter(0.2))

# The evaluation scores of employees who left have a wide range.

##average_monthly_hours

# average_montly_hours analysis
#Hours worked in a month
get.numeric.variable.stats(average_montly_hours)
# The maximum is 310, the minimum is 96, and the average is 201.05.

# histogram\density plot
ggplot(hr.df,aes(x=average_montly_hours))+geom_histogram(binwidth=0.5)
d<-density(average_montly_hours)
plot(d)
ggplot(hr.df, aes(x = average_montly_hours, fill = left)) + geom_histogram(bins=10,alpha=0.7)


# box plot
p<-ggplot(hr.df, aes(x=left, y=average_montly_hours, fill=left)) +
  geom_boxplot()
p + geom_jitter(shape=1, alpha=0.3, position=position_jitter(0.2))

## Normalization
# Five continuous variables  
# Since their ranges are different, normalization is performed.  
# Check other functions for scaling, such as `scale`.
scale.features <- function(df, variables){
  for (variable in variables){
    df[[variable]] <- scale(df[[variable]], center=T, scale=T)
  }
  return(df)
}

# normalize variables
attach(hr.df)
numeric.vars <- c("satisfaction_level", "last_evaluation", "average_montly_hours",
                  "number_project","time_spend_company")
hr.df <- scale.features(hr.df, numeric.vars)
attach(hr.df)

##### Continuous variables-Continuous variables Correlation############
hr.df_cor=cor(hr.df[,c(1:5)])
par(mfcol=c(1,1))
library(corrplot)
col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(hr.df_cor, method="shade", shade.col=NA, tl.col="black", tl.srt=45,
         col=col(200), addCoef.col="black", order="AOE")
# No multicollinearity

######## Logistic Regression (LR) ###########
attach(hr.df)
library(caret) # model training and evaluation
library(ROCR) # model evaluation
source("performance_plot_utils.R") #plotting metric results
# Split the data into 60% training data and the remaining 40% test data.
indexes <- sample(1:nrow(hr.df), size=0.6*nrow(hr.df))
train.data <- hr.df[indexes,]
test.data <- hr.df[-indexes,]

## separate feature and class variables
# Variance including only the independent variables, excluding the dependent variable.
test.feature.vars <- test.data[,-7]
test.class.var <- test.data[,7]

# build a logistic regression model
# Run it for all variables.
formula.init <- "left ~ ."
formula.init <- as.formula(formula.init)
lr.model <- glm(formula=formula.init, data=train.data, family="binomial")

# view model details
summary(lr.model)

# perform and evaluate predictions
lr.predictions <- predict(lr.model, test.data, type="response")
lr.predictions <- as.factor(round(lr.predictions))
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
formula.new <- "left ~ satisfaction_level +time_spend_company +  Work_accident + salary +number_project"
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

#=============================================================== 
#=== SVM =============================================
#===============================================================
# Split the data into 60% training data and the remaining 40% test data.
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
# Variance including only the independent variables, excluding the dependent variable.
test.feature.vars <- test.data[,-7] 
test.class.var <- test.data[,7] 

## build initial model with training data
# Create an initial model with all variables included.
formula.init <- "left ~ ." 
# Convert the input values into formula form.
formula.init <- as.formula(formula.init) 
# Fit the SVM model  
# Kernel: The kernel used for training and prediction, which typically has little effect in real-world problems  
# Gamma: The slope of the hyperplane  
# Cost: The degree to which overfitting is prevented
svm.model <- svm(formula=formula.init, data=train.data, 
                 kernel="radial", cost=100, gamma=1)

## view inital model details
summary(svm.model)

## predict and evaluate results
# predict function: `svm.model` is the SVM object, and `test.feature.vars` is the object containing the new input data.
svm.predictions <- predict(svm.model, test.feature.vars)
# ConfusionMatirx
# svm.predictions: Predicted values as a factor vector  
# test.class.var: Actual values as a factor vector
confusionMatrix(data=svm.predictions, reference=test.class.var, positive="1")

## svm specific feature selection
formula.init <- "left ~ ."
formula.init <- as.formula(formula.init)
# trainControl(): Applies a consistent comparison method to each candidate  
# Sampling method = repeated cross-validation / Number of cross-validation folds = 10 / Number of repetitions in repeated cross-validation = 2
control <- trainControl(method="repeatedcv", number=10, repeats=2)
# Train the prediction model.# Train the prediction model.
model <- train(formula.init, data=train.data, method="svmRadial", 
               trControl=control)
# variables of importance
importance <- varImp(model, scale=FALSE)
# Select the top 5 variables from the plot.
plot(importance, cex.lab=0.5) 

## build new model with selected features
# Create a new model using the variables selected based on varimp (variable importance).
formula.new <- "left ~ satisfaction_level + time_spend_company +
                            salary + Work_accident +
                            number_project"             
formula.new <- as.formula(formula.new)
# Fit the SVM model.
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
# Apply cross-validation (CV) for different values of cost.weights and gamma.weights.
tuning.results <- tune(svm, formula.new, 
                       data = train.data, kernel="radial", 
                       ranges=list(cost=cost.weights, gamma=gamma.weights))

# view optimization results
print(tuning.results)

# plot results
plot(tuning.results, cex.main=0.6, cex.lab=0.8,xaxs="i", yaxs="i") #그림1 

# get best model and evaluate predictions
svm.model.best = tuning.results$best.model
svm.predictions.best <- predict(svm.model.best, test.feature.vars)
# ConfusionMatirx
confusionMatrix(data=svm.predictions.best, reference=test.class.var, positive="1")

# plot best model evaluation metric curves
svm.predictions.best <- predict(svm.model.best, test.feature.vars, decision.values = T)
# Check all the assigned attributes at once.
svm.prediction.values <- attributes(svm.predictions.best)$decision.values
predictions <- prediction(svm.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="SVM ROC Curve")
plot.pr.curve(predictions, title.text="SVM Precision/Recall Curve") #Figure 2

## model optimizations based on ROC

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
# Variance including only the independent variables, excluding the dependent variable.
transformed.test.feature.vars <- transformed.test[,-7] 
transformed.test.class.var <- transformed.test[,7] 

# view data to understand transformations
summary(transformed.train$left) 

# build optimal model based on AUC

# Quickly generate all combinations of vectors.
grid <- expand.grid(C=c(1,10,100), 
                    sigma=c(0.01, 0.05, 0.1, 0.5, 1))
# Sampling method = cross-validation / Number of cross-validation folds = 10
ctr <- trainControl(method='cv', number=10,
                    classProbs=TRUE,
                    summaryFunction=twoClassSummary)
svm.roc.model <- train(formula.init, transformed.train,
                       method='svmRadial', trControl=ctr, 
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
plot.pr.curve(predictions, title.text="SVM Precision/Recall Curve")  #그림3 

#=============================================================== 
#=== Decision tree classifier =============================================
#===============================================================

# Create train.data and test.data (60% of the total data for training).
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
# Create a dataset without the dependent variable.
test.feature.vars <- test.data[,-7]
test.class.var <- test.data[,7]

## build initial model with all variables
formula.init <- "left ~ ."
# "left~ ."
formula.init <- as.formula(formula.init)
# dt.model$cptable: Set an appropriate cp (complexity parameter) value  
# minsplit: Minimum number of data points required to split a node.
dt.model <- rpart(formula=formula.init, method="class",data=train.data, 
                  control = rpart.control(minsplit=10, cp=0.05))

## predict and evaluate results
# Perform prediction with the created model (using `test.feature.vars`), which is the dataset with the dependent variable removed from `test.data`.  
# Predict the class for each category (type = "class").
dt.predictions <- predict(dt.model, test.feature.vars, type="class")
# Evaluate the results.
confusionMatrix(data=dt.predictions, reference=test.class.var, positive="1")
str(hr.df)

# Predict using the created model (to calculate AUC).
dt.predictions2 <- predict(dt.model, test.feature.vars, type="prob")
dt.prediction.values <- dt.predictions2[,2]
# Predict using `test.class.var`.
predictions <- prediction(dt.prediction.values, test.class.var)
par(mfrow=c(1,2))
# ROC curve (AUC:0.96)
plot.roc.curve(predictions, title.text="DT ROC Curve")
plot.pr.curve(predictions, title.text="DT Precision/Recall Curve")



## dt specific feature selection
formula.init <- "left ~ ."
formula.init <- as.formula(formula.init)
# Apply a consistent comparison method for evaluation  
# Perform k-fold cross-validation with 2 repeats  
# k-fold cross-validation: Divide the examples into k folds, where the model is trained on k-1 folds and tested on the remaining fold
control <- trainControl(method="repeatedcv", number=10, repeats=2)
# Train the prediction model (formula.init).
model <- train(formula.init, data=train.data, method="rpart", 
               trControl=control)
# Calculate the importance.
importance <- varImp(model, scale=FALSE)
# Visualization
plot(importance, cex.lab=0.5)


## build new model with selected features
# Extract variables based on importance and create a new model.
formula.new <- "left ~ satisfaction_level+average_montly_hours+time_spend_company+last_evaluation+number_project"
formula.new <- as.formula(formula.new)
dt.model.new <- rpart(formula=formula.new, method="class",data=train.data, 
                      control = rpart.control(minsplit=10,cp=0.05),
                      parms = list(prior = c(0.7, 0.3)))

## predict and evaluate results
# Predict the class for each category (type = "class").
dt.predictions.new <- predict(dt.model.new, test.feature.vars, type="class")
# Evaluate the results.
confusionMatrix(data=dt.predictions.new, reference=test.class.var, positive="1")
# Accuracy, Sensitivity, and Specificity values are increasing.


# view model details
# Decide `dt.model.new` as the final model.
dt.model.best <- dt.model.new
print(dt.model.best)
par(mfrow=c(1,1))
# Decision Tree
prp(dt.model.new, type=1, extra=3, varlen=0, faclen=0)


## plot model evaluation metric curves
# Predict the probabilities for each category (type = "prob").
dt.predictions.best <- predict(dt.model.best, test.feature.vars, type="prob")
dt.prediction.values <- dt.predictions.best[,2]
# Predict using `test.class.var`
predictions <- prediction(dt.prediction.values, test.class.var)
par(mfrow=c(1,2))
# ROC curve (AUC:0.96)
plot.roc.curve(predictions, title.text="DT ROC Curve")
plot.pr.curve(predictions, title.text="DT Precision/Recall Curve")

# Split the dataset into training and test datasets with a 60:40 ratio.
indexes <- sample(1:nrow(hr.df), size=0.6*nrow(hr.df))
train.data <- hr.df[indexes,]
test.data <- hr.df[-indexes,]

#=============================================================== 
#===RF=============================================
#===============================================================

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
rf.model <- randomForest(formula.init, data = train.data, importance=T, proximity=T)

## view model details
print(rf.model)

## predict and evaluate results
rf.predictions <- predict(rf.model, test.feature.vars, type="class")
confusionMatrix(data=rf.predictions, reference=test.class.var, positive="1")


## build new model with selected features
formula.new <- "left ~ satisfaction_level+number_project+time_spend_company+average_montly_hours+last_evaluation"
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

# Transform the data (excluding the dependent variable).
test.feature.vars <- test.data[,-7]
test.class.var <- test.data[,7]

# Transform the data
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

# Create a model using the training data.
formula.init <- "left ~ ."
formula.init <- as.formula(formula.init)
nn.model <- train(formula.init, data = transformed.train, method="nnet")

# Check the model.
print(nn.model)


# Make predictions and evaluate the results.
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

# Calculate the classification evaluation metrics for the entire model.
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


# Select specific variables.
formula.init <- "left ~ ."
formula.init <- as.formula(formula.init)
control <- trainControl(method="repeatedcv", number=10, repeats=2)
model <- train(formula.init, data=transformed.train, method="nnet", 
               trControl=control)
importance <- varImp(model, scale=FALSE)
plot(importance, cex.lab=0.5)

## Select the top 5 most important variables  
# Create a new model using the selected variables.
formula.new <- "left ~ number_project + satisfaction_level + time_spend_company + promotion_last_5years +last_evaluation"
formula.new <- as.formula(formula.new)
nn.model.new <- train(formula.new, data=transformed.train, method="nnet")

# Make predictions and evaluate the results of the new model.
nn.predictions.new <- predict(nn.model.new, transformed.test.feature.vars, type="raw")
confusionMatrix(data=nn.predictions.new, reference=transformed.test.class.var, 
                positive="X1")


# view hyperparameter optimizations
plot(nn.model.new, cex.lab=0.5)


# plot model evaluation metric curves
nn.model.best <- nn.model.new
nn.predictions.best <- predict(nn.model.best, transformed.test.feature.vars, type="prob")
nn.prediction.values <- nn.predictions.best[,2]
predictions <- prediction(nn.prediction.values, test.class.var)
par(mfrow=c(1,2))
plot.roc.curve(predictions, title.text="NN ROC Curve")
plot.pr.curve(predictions, title.text="NN Precision/Recall Curve")

# Visualize the RF (Random Forest) model  
# Visualize the confusion matrix.
library(ggplot2)
test <- hr.df[indexes,]
test$pred <- predict(rf.model.new, hr.df[indexes,]) #예측된 분류 입력하기
ggplot(test, aes(left, pred, color = left)) + geom_jitter(width = 0.2, height = 0.1, size=2) +
  labs(title="Confusion Matrix", subtitle="Predicted vs. Observed from HR dataset", y="Predicted", x="Truth")

# Plot the relative importance of variables.
importance(rf.model.new)
varImpPlot(rf.model.new)

# The relative size (accuracy) when the satisfaction level is set to 100%.
rel.importance<-rf.model.new$importance[,3]/max(rf.model.new$importance[,3])*100
rel.importance.dec<-sort(rel.importance, decreasing=T)
barplot(rel.importance.dec,horiz=T,names=names(rel.importance.dec))

# The relative importance when the satisfaction level is set to 100%.
rel.importance<-rf.model.new$importance[,4]/max(rf.model.new$importance[,4])*100
rel.importance.dec<-sort(rel.importance, decreasing=T)
barplot(rel.importance.dec,horiz=T,names=names(rel.importance.dec))
