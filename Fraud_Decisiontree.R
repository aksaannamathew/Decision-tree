library(readr)
library(DataExplorer)
library(rpart)
library(rpart.plot)
library(C50)
library(tree)
library(gmodels)
library(rattle)
library(caret)
library(party)

#Importing Data
Fraud_data <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\4 - Decision Tree\\Fraud_check.csv")
attach(Fraud_data)
View(Fraud_data)
head(Fraud_data)

#EDA
sum(is.na(Fraud_data))
summary(Fraud_data)
str(Fraud_data)

#Graphical Representation
plot_histogram(Fraud_data)
pairs(Fraud_data)
plot_bar(Fraud_data)

#Converting into Categorical Data
Taxincome_fac <- ifelse(Fraud_data$Taxable.Income<=30000, "RISKY", "GOOD")
FRAUD_FAC <- data.frame(Taxincome_fac, Fraud_data)
head(FRAUD_FAC)
FRAUD_FAC <- FRAUD_FAC[,-4]
FRAUD_FAC <- data.frame(FRAUD_FAC)

#Data Splitting
set.seed(123)
split <- sample(1:nrow(FRAUD_FAC), size = nrow(FRAUD_FAC)*0.75, replace = F) 
train_fraud <- FRAUD_FAC[split,]
test_fraud <- FRAUD_FAC[-split,]
head(train_fraud)

#Model Building 1
model_fraud <- rpart(Taxincome_fac~., data = train_fraud, method = "class")
model_fraud
plot(model_fraud)
text(model_fraud)
fancyRpartPlot(model_fraud)

#Evaluation of Model Building 1
set.seed(100)
pred1 <- predict(model_fraud, newdata = test_fraud, type = "class")
pred1
confusionMatrix(table(pred1, test_fraud$Taxincome_fac)) #Accuracy=72.67% 
CrossTable(pred1, test_fraud$Taxincome_fac)

#Model Building 2
model_fraud2 <- C5.0(Taxincome_fac~., data = train_fraud, trails = 100)
model_fraud2
plot(model_fraud2)

#Evaluation of Model Building 2
pred2 <- predict(model_fraud2, test_fraud)
pred2
confusionMatrix(table(pred2, test_fraud$Taxincome_fac)) #Accuracy=74.67%
CrossTable(pred2, test_fraud$Taxincome_fac)

#Bagging Method
acc <- c()
for (i in 1:100) {
  print(i)
  split <- sample(1:nrow(FRAUD_FAC), size = nrow(FRAUD_FAC)*0.75, replace = F)
  train_fraud <- FRAUD_FAC[split,]
  test_fraud <- FRAUD_FAC[-split,]
  model3 <- rpart(Taxincome_fac~., data = train_fraud, method = "class")
  pred4 <- predict(model3, test_fraud, type = "class")
  a <- table(pred4, test_fraud$Taxincome_fac)
  acc <- c(acc, sum(diag(a))/sum(a))
}
acc
summary(acc)
