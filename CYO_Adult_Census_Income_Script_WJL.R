
##########################################################
# Create data set "adult" by read.csv and load libraries
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(ggthemes)) install.packages("ggthemes", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(plyr)) install.packages("plyr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(caTools)) install.packages("caTools", repos = "http://cran.us.r-project.org")
if(!require(ROSE)) install.packages("ROSE", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(rpart.plot)) install.packages("rpart.plot", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(ggplot2)
library(plyr)
library(dplyr)
library(ggthemes)
library(caret)
library(data.table)
library(corrplot)
library(caTools)
library(ROSE)
library(rpart)
library(rpart.plot)
library(randomForest)

adult <- read.csv('../projects/Adult-Census-Income/input/adult.csv', stringsAsFactors = FALSE, header = TRUE, strip.white = TRUE)

##   Data Exploration

##########################################################
# Explore basic statistics, Split it into train set adult_train and validation test set adult_test
##########################################################

# take a first glance of data set "adult" to understand total observations, variables and what are they.
str(adult)
head(adult)
dim(adult)

# get the basic statistics of data set "adult"
summary(adult)
# check any variables with null value
anyNA(adult)

unique(adult$age)
# from output we can see "age" is a continuous variable
unique(adult$workclass)
# from output we can see "workclass" is a categorical variable which contains invalid value "?"
unique(adult$education)
# from output we can see "education" is a categorical variable
unique(adult$education.num)
# from output we can see "education.num" is a categorical variable
unique(adult$marital.status)
# from output we can see "marital.status" is a categorical variable
unique(adult$occupation)
# from output we can see "occupation" is a categorical variables which contains invalid value "?"
unique(adult$relationship)
# from output we can see "relationship" is a categorical variable
unique(adult$race)
# from output we can see "race" is a categorical variables
unique(adult$sex)
# from output we can see "sex" is a categorical variables
unique(adult$capital.gain)
# from output we can see "capital.gain" is a continuous variable
unique(adult$capital.loss)
# from output we can see "capital.loss" is a continuous variable
unique(adult$hours.per.week)
# from output we can see "hours.per.week" is a continuous variable
unique(adult$native.country)
# from output we can see "native.country" is a categorical variable which contains invalid value "?"
unique(adult$income)
# from output we can see "income" is a categorical variable

#Count the invalid value "?"
colSums(adult =="?")

sum(adult$workclass == "?")/nrow(adult)
sum(adult$occupation == "?")/nrow(adult)

# convert "?" to "NA"
adult[adult == "?"] <- NA

# Omitting NA values
adult <- na.omit(adult)
# Check again to make sure all observations are valid
colSums(adult =="?")
anyNA(adult)

# Convert all character variables to categorical factor : workclass, education, marital-status, occupation, relationship, race, sex, native-country.

adult$workclass <- as.factor(adult$workclass)
adult$education <- as.factor(adult$education)
adult$marital.status <- as.factor(adult$marital.status)
adult$occupation <- as.factor(adult$occupation)
adult$relationship <- as.factor(adult$relationship)
adult$race <- as.factor(adult$race)
adult$sex <- as.factor(adult$sex)
adult$native.country <- as.factor(adult$native.country)

#split data set "adult" into "adult_train" and "adult_test" with percentage 80% and 20%
set.seed(20)
split <- sample.split(adult, SplitRatio = 0.8) # 80:20
adult_train <- subset(adult, split == TRUE)
adult_test <- subset(adult, split == FALSE)

str(adult_train)
str(adult_test)

##    Data Pre-processing

# First check the correlation between continous variables "age", "fnlwgt",  "capital.gain", "capital.loss", "hours.per.week" and "income". 
# Before we check the correlation, we need convert "income" to numeric variable

adult_train$income<-ifelse(adult_train$income=='<=50K',0,1)

# list the correlations between continous variables "age", "fnlwgt",  "capital.gain", "capital.loss", "hours.per.week" and "income"

continous_factors_cor <- cor(adult_train %>% select_if(is.numeric))
as.matrix(round(continous_factors_cor,3))

# visualize the correlation between continous variables and income

columns <- c(1, 3, 5, 11, 12, 13, 15)
corrplot(cor(adult_train[,columns]))

# drop variable "fnlwgt" from adult_train
adult_train <- adult_train[,-3]

adult_train$income <- mapvalues(adult_train$income, from = c(0,1), to = c('<=50K','>50K'))
adult_train$income <- as.factor(as.character(adult_train$income))

##   Data Analysis

# Explore target "income" distribution
sum(adult_train$income == "<=50K")/nrow(adult_train)
sum(adult_train$income == ">50K")/nrow(adult_train)

#hist(adult_train$income)

ggplot(data = adult_train) +
  aes(x=income) +
  geom_bar(fill="blue") + theme(axis.text.x = element_text(angle = 45)) + ggtitle("Income Distribution") + theme(plot.title = element_text(hjust = 0.5))

#Further analysis on correlation between continuous variables and target "income"
# 1. Education.num
summary(adult_train$education.num)

# Visualize education.num distribution
ggplot(data = adult_train) +
  aes(x=education.num) +
  geom_bar(fill="blue") + ggtitle("Education.num Distribution") + theme(plot.title = element_text(hjust = 0.5))

# Visualize amounts of "education.num VS income"
ggplot(data = adult_train) + aes(x=education.num,fill=income) + geom_histogram(binwidth =2) + scale_fill_manual(values=c("blue","green")) +
scale_x_continuous(breaks=seq(0, 20, by= 1)) + ggtitle("Education.num VS Income Distribution") + theme(plot.title = element_text(hjust = 0.5))

# Visualize frequency of "education.num VS income"
ggplot(data = adult_train) +
  aes(x = education.num, fill = income) +
  geom_histogram(binwidth=2, position="fill") + scale_fill_manual(values=c("blue","green")) + 
  scale_x_continuous(breaks=seq(0, 20, by= 1)) +
  labs(x="Education.num", y="Frequency") + ggtitle("Education.num VS Income Frequency") + theme(plot.title = element_text(hjust = 0.5))

# 2. Age
summary(adult_train$age)

# Visualize the distribution of continuous variable "age"
ggplot(data = adult_train) +
  aes(x=age) +
  geom_bar(fill="blue") + ggtitle("Age Distribution") + theme(plot.title = element_text(hjust = 0.5))

# Visualize amounts of "age VS income"
ggplot(data = adult_train) + aes(x=age,fill=income) + geom_histogram(binwidth =3) + scale_fill_manual(values=c("blue","green")) + scale_x_continuous(breaks=seq(15, 90, by= 5)) +  ggtitle("Age VS Income Distribution") + theme(plot.title = element_text(hjust = 0.5))

# Visualize frequency of "age VS income"
ggplot(data = adult_train) +
  aes(x = age, fill = income) +
  geom_histogram(binwidth=5, position="fill") + scale_fill_manual(values=c("blue","green")) + scale_x_continuous(breaks=seq(15, 90, by= 5)) + labs(x="Age", y="Frequency") + ggtitle("Age VS Income Frequency") + theme(plot.title = element_text(hjust = 0.5))

# 3. Hours.per.week
summary(adult_train$hours.per.week)

# Visualize the distribution of continuous variable "hours.per.week"
ggplot(data = adult_train) +
  aes(x=hours.per.week) +
  geom_bar(fill="blue") + ggtitle("Hours.per.week Distribution") + theme(plot.title = element_text(hjust = 0.5))

# Visualize amounts of "hours.per.week VS income"
ggplot(data = adult_train) + aes(x=hours.per.week,fill=income) + geom_histogram(binwidth =3) + scale_fill_manual(values=c("blue","green")) + scale_x_continuous(breaks=seq(1, 100, by= 10)) +  ggtitle("Hours.per.week VS Income Distribution") + theme(plot.title = element_text(hjust = 0.5))

# Visualize frequency of "hours.per.week VS income"
ggplot(data = adult_train) +
  aes(x = hours.per.week, fill = income) +
  geom_histogram(binwidth=5, position="fill") + scale_fill_manual(values=c("blue","green")) + scale_x_continuous(breaks=seq(1, 100, by= 10)) + labs(x="Hours.per.week", y="Frequency") + ggtitle("Hours.per.week VS Income Frequency")  + theme(plot.title = element_text(hjust = 0.5)) 

# 4. capital.gain & capital.loss & capital.gain - capital.loss
summary(adult_train$capital.gain)

#Visualize the distribution of continuous variable "capital.gain"
hist(adult_train$capital.gain, col="blue", main="Capital Gain")

# Visualize frequency of "capital.gain VS income"
ggplot(data = adult_train) +
  aes(x = capital.gain, fill = income) +
  geom_histogram(binwidth=5, position="fill") + scale_fill_manual(values=c("blue","green")) +
  scale_x_continuous(breaks=seq(0, 100000, by= 10000)) +
  labs(x="Capital.gain", y="Frequency") + ggtitle("Capital.gain VS Income Frequency") + theme(plot.title = element_text(hjust = 0.5))  

ggplot(adult_train, aes(x=capital.gain))+ geom_boxplot()

summary(adult_train$capital.loss)

# Visualize the distribution of continuous variable "capital.loss"

# Visualize frequency of "capital.loss VS income"
ggplot(data = adult_train) +
  aes(x = capital.loss, fill = income) +
  geom_histogram(binwidth=5, position="fill") + scale_fill_manual(values=c("blue","green")) +
  #scale_x_continuous(breaks=seq(0, 100000, by= 10000)) +
  labs(x="Capital.loss", y="Frequency") + ggtitle("Capital.loss VS Income Frequency") + theme(plot.title = element_text(hjust = 0.5))  

#Boxplot of capital.loss

ggplot(adult_train, aes(x=capital.loss))+ geom_boxplot()

#Net Capital
hist(adult_train$capital.gain-adult_train$capital.loss, col="blue", main="Net Capital")

adult_train$income <- mapvalues(adult_train$income, from = c(0,1), to = c('<=50K','>50K'))

# 5. workclass
#plot workclass distribution
ggplot(data = adult_train) +
  aes(x=workclass) +
  geom_bar(fill="blue") + ggtitle("Workclass Distribution") + theme(plot.title = element_text(hjust = 0.5))

table(adult_train$workclass, adult_train$income)

#Plot Workclass VS Income from amount point of view
ggplot(adult_train, aes(x = workclass, fill = income)) + geom_bar() +  scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 45)) + ggtitle("Workclass VS Income") + theme(plot.title = element_text(hjust = 0.5))

#Plot Workclass VS Income from frequency point of view
ggplot(adult_train, aes(x = workclass, fill = income)) + geom_bar(position = "fill") + ylab("Frequency") + scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 45)) + ggtitle("Workclass VS Income") + theme(plot.title = element_text(hjust = 0.5))

#6. Occupation

#plot occupation distribution
ggplot(data = adult_train) +
  aes(x=occupation) +
  geom_bar(fill="blue") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Occupation Distribution") + theme(plot.title = element_text(hjust = 0.5))

table(adult_train$occupation, adult_train$income)

#Plot occupation VS Income from amount point of view
ggplot(adult_train, aes(x = occupation, fill = income)) + geom_bar() +  scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Occupation VS Income") + theme(plot.title = element_text(hjust = 0.5))

#Plot occupation VS Income from frequency point of view
ggplot(adult_train, aes(x = occupation, fill = income)) + geom_bar(position = "fill") + ylab("Frequency") + scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Occupation VS Income") + theme(plot.title = element_text(hjust = 0.5))

#7. education

#plot education distribution
ggplot(data = adult_train) +
  aes(x=education) +
  geom_bar(fill="blue") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Education Distribution") + theme(plot.title = element_text(hjust = 0.5))
# we can see that the most of people are with education level of HS-grad followed by level of Some-college and Bachelors

table(adult_train$education, adult_train$income)

#Plot Education VS Income from amount points of view
ggplot(adult_train, aes(x = education, fill = income)) + geom_bar() + scale_fill_manual(values=c("blue","green")) + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Education VS Income") + theme(plot.title = element_text(hjust = 0.5))

#Plot Education VS Income from frequency points of view
ggplot(adult_train, aes(x = education, fill = income)) + geom_bar(position = "fill") + ylab("Frequency") + scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Education VS Income") + theme(plot.title = element_text(hjust = 0.5))

#8. marital.status

#plot marital.status distribution
ggplot(data = adult_train) +
  aes(x=marital.status) +
  geom_bar(fill="blue") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Marital.status Distribution") + theme(plot.title = element_text(hjust = 0.5))

table(adult_train$marital.status, adult_train$income)

#Plot marital.status VS Income from amount point of view
ggplot(adult_train, aes(x = marital.status, fill = income)) + geom_bar() + scale_fill_manual(values=c("blue","green")) + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Marital.status VS Income") + theme(plot.title = element_text(hjust = 0.5))

#Plot marital.status VS Income from frequency point of view
ggplot(adult_train, aes(x = marital.status, fill = income)) + geom_bar(position = "fill") + ylab("Frequency") + scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Marital.status VS Income") + theme(plot.title = element_text(hjust = 0.5))

#9. Relationship
#plot relationship distribution
ggplot(data = adult_train) +
  aes(x=relationship) +
  geom_bar(fill="blue") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Relationship Distribution") + theme(plot.title = element_text(hjust = 0.5))

table(adult_train$relationship, adult_train$income)
#Plot relationship VS Income from both amount and frequency angles
ggplot(adult_train, aes(x = relationship, fill = income)) + geom_bar() + scale_fill_manual(values=c("blue","green")) + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Relationship VS Income") + theme(plot.title = element_text(hjust = 0.5))

ggplot(adult_train, aes(x = relationship, fill = income)) + geom_bar(position = "fill") + ylab("Frequency") + scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Relationship VS Income") + theme(plot.title = element_text(hjust = 0.5))

#10. Race
#plot race distribution
ggplot(data = adult_train) +
  aes(x=race) +
  geom_bar(fill="blue") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Race Distribution") + theme(plot.title = element_text(hjust = 0.5))

table(adult_train$race, adult_train$income)

#Plot race VS Income from both amount and frequency points of view
ggplot(adult_train, aes(x = race, fill = income)) + geom_bar() + scale_fill_manual(values=c("blue","green")) + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Race VS Income") + theme(plot.title = element_text(hjust = 0.5))

ggplot(adult_train, aes(x = race, fill = income)) + geom_bar(position = "fill") + ylab("Frequency") + scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Race VS Income") + theme(plot.title = element_text(hjust = 0.5))

#11. Gender
#plot gender distribution
ggplot(data = adult_train) +
  aes(x=sex) +
  geom_bar(fill="blue") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Gender Distribution")  + theme(plot.title = element_text(hjust = 0.5))

table(adult_train$sex, adult_train$income)

ggplot(adult_train, aes(x = sex, fill = income)) + geom_bar(position = "fill") + ylab("Frequency") + scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Gender VS Income") + theme(plot.title = element_text(hjust = 0.5))

#12. Native country
#plot native.country distribution
ggplot(data = adult_train) +
  aes(x=native.country) +
  geom_bar(fill="blue") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Native.country Distribution") + theme(plot.title = element_text(hjust = 0.5))

table(adult_train$native.country, adult_train$income)

#Plot native.country VS Income from both amount and frequency angles
ggplot(adult_train, aes(x = native.country, fill = income)) + geom_bar() + scale_fill_manual(values=c("blue","green")) + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Native.country VS Income") + theme(plot.title = element_text(hjust = 0.5))

ggplot(adult_train, aes(x = native.country, fill = income)) + geom_bar(position = "fill") + ylab("Frequency") + scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Native.country VS Income") + theme(plot.title = element_text(hjust = 0.5))

# Relationship + Marital.status VS Income
ggplot(data = adult_train) +
  aes(x=relationship ,fill=income) + 
  geom_bar() + 
  facet_wrap(~marital.status)+ 
  scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Relationship + Marital.status VS Income") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = adult_train) +
  aes(x=relationship ,fill=income) + 
  geom_bar(position="fill") + ylab("Frequency") +
  facet_wrap(~marital.status)+ 
  scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Relationship + Marital.status VS Income") + theme(plot.title = element_text(hjust = 0.5))

# Workclass + Gender VS Income
ggplot(data = adult_train) +
  aes(x=sex ,fill=income) + 
  geom_bar() + 
  facet_wrap(~workclass)+ 
  scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Workclass + Gender VS Income") + theme(plot.title = element_text(hjust = 0.5))

ggplot(data = adult_train) +
  aes(x=sex ,fill=income) + 
  geom_bar(position="fill") + ylab("Frequency") +
  facet_wrap(~workclass)+ 
  scale_fill_manual(values=c("blue","green")) +
  theme(axis.text.x = element_text(angle = 90)) + ggtitle("Workclass + Gender VS Income") + theme(plot.title = element_text(hjust = 0.5))

#Modeling

#drop irrelative "fnlwgt" from adult_test

adult_test <- adult_test[,-3]
adult_test$income <- as.factor(adult_test$income)

#Model 1: Logistic Regression

glmfit <- glm(income~., data=adult_train, family=binomial)
pred<- predict(glmfit,newdata=adult_test,type = 'response')
pred_lgr<- ifelse(pred>0.5,">50K","<=50K")
confusionMatrix(factor(pred_lgr),adult_test$income,positive = ">50K")
Lgr_Accu <-confusionMatrix(factor(pred_lgr),adult_test$income,positive = ">50K")$overall["Accuracy"]

#resample train set to get a balanced one
adult_train_balanced <- ovun.sample(income~.,data=adult_train,method = "both")$data

glmfit_balanced <- glm(income~., data=adult_train_balanced, family=binomial)
pred_balanced<- predict(glmfit_balanced,newdata=adult_test,type = 'response')
pred_lgr_balanced<- ifelse(pred_balanced>0.5,">50K","<=50K")
confusionMatrix(factor(pred_lgr_balanced),adult_test$income,positive = ">50K")
Lgr_Accu_Balanced <- confusionMatrix(factor(pred_lgr_balanced),adult_test$income,positive = ">50K")$overall["Accuracy"]

#Model 2: Classification (Decision) Tree

Dctfit <- rpart(income ~., data = adult_train, method = "class")
Pred_Dct<- predict(Dctfit,newdata = adult_test,type = 'class')
confusionMatrix(Pred_Dct,adult_test$income,positive = ">50K")
Dct_Accu <- confusionMatrix(Pred_Dct,adult_test$income,positive = ">50K")$overall["Accuracy"]

#Visualize the decision tree
rpart.plot(Dctfit)

#Model 3: Random Forest

RfFit<- randomForest(income~.,data= adult_train, importance = TRUE)
Pred_Rf<- predict(RfFit,newdata = adult_test, type = 'class')
confusionMatrix(Pred_Rf,adult_test$income,positive = ">50K")
Rf_Accu <- confusionMatrix(Pred_Rf,adult_test$income,positive = ">50K")$overall["Accuracy"]

RfFit$importance

rf_imp = data.table(RfFit$importance,names = rownames(RfFit$importance))[order(-MeanDecreaseGini)]

rf_imp

varImpPlot(RfFit)

Accuracy_Comp<-data.frame(Model=c('Logistic Regression','Logistic Regression With Balanced Sample','Decision Tree','Random Forest'),Accuracy=c(Lgr_Accu,Lgr_Accu_Balanced,Dct_Accu,Rf_Accu))

Accuracy_Comp