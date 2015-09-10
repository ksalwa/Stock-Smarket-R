#Fitiing logistic regression on Smarket dataset

#Load data
library(ISLR)
attach(Smarket)

#summary
summary(Smarket)
cor(Smarket[,-9])
pairs(Smarket[,-9])

#split the data
train = {Year<2005}
train_data = Smarket[train,]

test = !train
test_data = Smarket[test,]

Direction_test = Direction[test]

#fit logistic regression model
stock_model = glm(Direction ~ Lag1+Volume,data = train_data,family = binomial)
summary(stock_model)

#predictions
pred_prob = predict(stock_model,test_data,type = "response")
pred_direc = rep("Down",252)
pred_direc[pred_prob>0.5] = "Up"

#confusion matrix
table(pred_direc,Direction_test)
mean(pred_direc!=Direction_test)
