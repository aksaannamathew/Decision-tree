install.packages("RGtk2")
library(readr)
library(party)
library(rpart)
library(tree)
library(C50)
library(rpart.plot)
library(caret)
library(rattle)
library(RGtk2)

#Importing the Dataset
company <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\4 - Decision Tree\\Company_Data.csv")
attach(company)
View(company)

#EDA and Statistical Analysis
str(company)
summary(company)

sales_factor <- ifelse(company$Sales>7.5, "high", "low")
company_sale <- data.frame(company, sales_factor)
company_sales <- company_sale[, -1]
head(company_sales)
View(company_sales)
table(company_sales$sales_factor)

#Data Spliting
set.seed(123)
split <- sample(1:nrow(company_sales), size = nrow(company_sales)*0.75, replace = F)
train_companysales <- company_sales[split,]
test_companysales <- company_sales[-split,]
head(train_companysales)

#Model Building
model_1 <- rpart(sales_factor~CompPrice+Income+Advertising+Population+Price+ShelveLoc+Age+Education+Urban+US, data = train_companysales)
model_1
plot(model_1)
text(model_1)

#Graphical Representation
rpart.plot(model_1)
prp(model_1,faclen = 0, cex = 0.8, extra = 1)

rattle()
fancyRpartPlot(model_1)
printcp(model_1)
plotcp(model_1)
rpart.plot(model_1)
bestcp <- model_1$cptable[which.min(model_1$cptable[,"xerror"]), "CP"]

#Pruning
model_1_pruned <- prune(model_1, cp=bestcp)
prp(model_1_pruned, faclen = 0, cex = 0.8, extra = 1)

#Evaluation
pred_1 <- predict(model_1, newdata = test_companysales, type = "class")
pred_1
confusionMatrix(pred_1, test_companysales$sales_factor) #Accuracy=86%

#------------------------------------------------------------------------------------------------------------------
#Bagging Technique
acc <- c()
for (i in 1:100) {
  print(i)
  split <- sample(1:nrow(company_sales), size = nrow(company_sales)*0.75, replace = F)
  train_companysales <- company_sales[split,]
  test_companysales <- company_sales[-split,]
  model_2 <- rpart(sales_factor~., data = train_companysales, method = "class")
  pred_2 <- predict(model_2, newdata = test_companysales, type = "class")
  a <- table(pred_2, test_companysales$sales_factor)
  acc <- c(acc, sum(diag(a))/sum(a))
}

acc
summary(acc) 
#------------------------------------------------------------------------------------------------------------------
#Model on Regression Tree

#Importing the Data
company_reg <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\4 - Decision Tree\\Company_Data.csv")
attach(company_reg)
View(company_reg)
head(company_reg)

#EDA
str(company_reg)
summary(company_reg)

#Data Splitting
set.seed(222)
split_reg <- sample(1:nrow(company_reg), size = nrow(company_reg)*0.75, replace = F)
train_reg <- company_reg[split_reg,]
test_reg <- company_reg[-split_reg,]
head(train_reg)

#Model Building
model_reg <- rpart(Sales~.,data = train_reg, method = "anova")
model_reg

#Graphiocal Representation
plot(model_reg)
text(model_reg)
rattle()
fancyRpartPlot(model_reg)
printcp(model_reg)
plotcp(model_reg)
rpart.plot(model_reg)

bestcp <- model_reg$cptable[which.min(model_reg$cptable[,"xerror"]), "CP"]

#Pruning
reg_pruned <- prune(model_reg, cp = bestcp)
prp(reg_pruned, faclen = 0, cex = 0.8, extra = 1)
#Evaluation
pred_3 <- predict(model_reg, newdata = test_reg)
RMSE(pred = pred_3, test_reg$Sales) #RMSE=2.252219
