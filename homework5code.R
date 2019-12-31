rm(list = ls())

###Q 1
library(h2o)
library(ElemStatLearn)
library(neuralnet)
library(gam)
data(spam)
spam$type = ifelse(spam$type == "spam",1,0)
spam = spam[1:4600,]
train_index = sample(1:nrow(spam), 0.8*nrow(spam))
train = spam[train_index,]
test = spam[-train_index,]

n<- names(spam)
formulae <- as.formula(paste("type ~", paste(n[!n %in% "type"], collapse = " + ")))

localh2o = h2o.init(ip='localhost', port = 54321, max_mem_size = '6g',nthreads = 1)
train.hex = as.h2o(train)
test.hex = as.h2o(test)
#check how many neurons is best 
error_train = c()
error_test = c()
for(i in seq(2,20,2)){
  NN_model = h2o.deeplearning(x = setdiff(colnames(train.hex), "type"), 
                              y = "type", training_frame = train.hex,
                              activation = "RectifierWithDropout",
                              hidden = c(i), epochs = 100)
  prediction_train = round(as.data.frame(h2o.predict(NN_model, newdata = train.hex[1:57]))$predict)
  error_train = append(error_train,abs(sum(train$type-prediction_train!=0))/length(prediction_train) )
  prediction_test = round(as.data.frame(h2o.predict(NN_model, newdata = test.hex[1:57]))$predict)
  accuracy = MLmetrics::Accuracy(prediction_test, test$type)
  error_test = append(error_test, 1-accuracy)
  print(i)
}
m = seq(2,20,2)
results<-cbind(m,error_train,error_test)
results
error_train
error_test
min(error_train)
min(error_test)
#select hidden = 18 or 20
which(min(error_train) == error_train)
neural_network <- neuralnet(formulae,data=train,hidden=20,err.fct='ce', linear.output=FALSE,
                            threshold=0.5)
neural_network_predict <- compute(neural_network,test[,1:57])
test_label <- test$type
predi<- round(neural_network_predict$net.result)
NN_error <- abs(sum(test_label-predi!=0))/length(predi) 
NN_error
#table(test_label,predi)

additive_model <- gam(formulae,data=train,family=binomial)
summary(additive_model)
additive_model_predict <- predict(additive_model,newdata=test)
pred_additive <- round(additive_model_predict)
additive_error <- (sum(test_label-pred_additive!=0))/length(pred_additive) 
additive_error 
  #high error rates, low accuracy


###Q2
library(h2o)

  # use spam data above
origin_train = train
outlier_train = train
testset = test
test_label <- test$type
head(outlier_train)
summary(outlier_train$our)
  # create a univariate outlier
  # change learningset$A.5[2] = 20
outlier_train$our[2] = 20
set.seed(123)
boxplot(train$our, main = "Boxplot")
boxplot(outlier_train$our, main = "Boxplot")
  # an obvious oulier there


localh2o = h2o.init(ip='localhost', port = 54321, max_mem_size = '6g',nthreads = 1)
train.hex = as.h2o(origin_train)
test.hex = as.h2o(testset)
#check how many neurons is best 
origin_error_train = c()
origin_error_test = c()
for(i in seq(1,20,1)){
  NN_model = h2o.deeplearning(x = setdiff(colnames(train.hex), "type"), 
                              y = "type", training_frame = train.hex,
                              activation = "RectifierWithDropout",
                              hidden = c(i), epochs = 100)
  prediction_train = round(as.data.frame(h2o.predict(NN_model, newdata = train.hex[1:57]))$predict)
  origin_error_train = append(origin_error_train,abs(sum(train$type-prediction_train!=0))/length(prediction_train) )
  prediction_test = round(as.data.frame(h2o.predict(NN_model, newdata = test.hex[1:57]))$predict)
  accuracy = MLmetrics::Accuracy(prediction_test, test$type)
  origin_error_test = append(origin_error_test, 1-accuracy)
  print(i)
}
origin_error_train
origin_error_test
which(min(origin_error_test) == origin_error_test)
min(origin_error_train)#12
min(origin_error_test)#20

outlier_train.hex = as.h2o(outlier_train)
outlier_error_train = c()
outlier_error_test = c()
for(i in seq(1,20,1)){
  NN_model = h2o.deeplearning(x = setdiff(colnames(outlier_train.hex), "type"), 
                              y = "type", training_frame = outlier_train.hex,
                              activation = "RectifierWithDropout",
                              hidden = c(i), epochs = 100)
  prediction_train = round(as.data.frame(h2o.predict(NN_model, newdata = outlier_train.hex[1:57]))$predict)
  outlier_error_train = append(outlier_error_train,abs(sum(train$type-prediction_train!=0))/length(prediction_train) )
  prediction_test = round(as.data.frame(h2o.predict(NN_model, newdata = test.hex[1:57]))$predict)
  accuracy = MLmetrics::Accuracy(prediction_test, test$type)
  outlier_error_test = append(outlier_error_test, 1-accuracy)
  print(i)
}
outlier_error_train
outlier_error_test
which(min(outlier_error_test) == outlier_error_test)
min(outlier_error_train)#20
min(outlier_error_test)#20


outlier_train = train
error_test = c()
for(i in seq(0,20,2)){
  outlier_train$our[2] = i
  outlier_train.hex = as.h2o(outlier_train)
  NN_model = h2o.deeplearning(x = setdiff(colnames(outlier_train.hex), "type"), 
                              y = "type", training_frame = outlier_train.hex,
                              activation = "RectifierWithDropout",
                              hidden = 20, epochs = 100)
  prediction_test = round(as.data.frame(h2o.predict(NN_model, newdata = test.hex[1:57]))$predict)
  accuracy = MLmetrics::Accuracy(prediction_test, test$type)
  error_test = append(error_test, 1-accuracy)
  print(i)
}
error_test


##Q3
library(ISLR)
library(e1071)
library(tuneR)
data("OJ")
OJ = data.frame(OJ)
str(OJ)
  #change data type 
OJ$StoreID = as.factor(as.character(OJ$StoreID))
OJ$SpecialCH = as.factor(as.character(OJ$SpecialCH))
OJ$SpecialMM = as.factor(as.character(OJ$SpecialMM))
OJ$STORE = as.factor(as.character(OJ$STORE))
set.seed(1234)
train_index = sample(1:nrow(OJ), 0.8*nrow(OJ))
OJ_train = OJ[train_index,]
OJ_test = OJ[-train_index,]

######## A
OJ_train_errors = c()
OJ_test_errors = c()
cost_list = c(0.01,0.05,0.1,0.5,1,5,10)
for(i in cost_list){
  tune.model.linear<-tune(svm,Purchase~.,data=OJ_train,kernel="linear",ranges=list(cost=i))
  y.hat.test.linear<-predict(tune.model.linear$best.model,newdata=OJ_test)
  y.ture.test<-OJ_test$Purchase
  
  test.error<-length(which(y.hat.test.linear!=y.ture.test))/length(y.ture.test)
  OJ_test_errors<-c(OJ_test_errors,test.error)
  
  y.hat.train.linear<-predict(tune.model.linear$best.model,newdata=OJ_train)
  y.true.train<-OJ_train$Purchase
  
  train.error<-length(which(y.hat.train.linear!=y.true.train))/length(y.true.train)
  OJ_train_errors<-c(OJ_train_errors,train.error)
}
linear<-cbind(cost,OJ_train_errors,OJ_test_errors)
linear

cost = cost_list
quartz()
plot(cost,OJ_test_errors,type="b",lty=5,col = "black",xlab = "cost", ylim = c(min(OJ_train_errors,OJ_test_errors),max(OJ_train_errors,OJ_test_errors)), main="Linear SVM",ylab="train & test error rates")
lines(cost,OJ_train_errors,type="b",lty=6,col="purple",xlab = "cost")
legend("topright",c("test_error_linear","train_error_linear"),lty=c(5,6),col=c("black","purple"))


######## B
#radial kernel
OJ_train_errors1 = c()
OJ_test_errors1 = c()
for(i in cost_list){
  tune.model.linear<-tune(svm,Purchase~.,data=OJ_train,kernel="radial",ranges=list(cost=i))
  y.hat.test.linear<-predict(tune.model.linear$best.model,newdata=OJ_test)
  y.ture.test<-OJ_test$Purchase
  
  test.error<-length(which(y.hat.test.linear!=y.ture.test))/length(y.ture.test)
  OJ_test_errors1<-c(OJ_test_errors1,test.error)
  
  y.hat.train.linear<-predict(tune.model.linear$best.model,newdata=OJ_train)
  y.true.train<-OJ_train$Purchase
  
  train.error<-length(which(y.hat.train.linear!=y.true.train))/length(y.true.train)
  OJ_train_errors1<-c(OJ_train_errors1,train.error)
}
radial<-cbind(cost,OJ_train_errors1,OJ_test_errors1)
radial

#polynomial kernel
OJ_train_errors2 = c()
OJ_test_errors2 = c()
for(i in cost_list){
  tune.model.linear<-tune(svm,Purchase~.,data=OJ_train,degree=2, kernel="polynomial",ranges=list(cost=i))
  y.hat.test.linear<-predict(tune.model.linear$best.model,newdata=OJ_test)
  y.ture.test<-OJ_test$Purchase
  
  test.error<-length(which(y.hat.test.linear!=y.ture.test))/length(y.ture.test)
  OJ_test_errors2<-c(OJ_test_errors2,test.error)
  
  y.hat.train.linear<-predict(tune.model.linear$best.model,newdata=OJ_train)
  y.true.train<-OJ_train$Purchase
  
  train.error<-length(which(y.hat.train.linear!=y.true.train))/length(y.true.train)
  OJ_train_errors2<-c(OJ_train_errors2,train.error)
}
polynomial<-cbind(cost,OJ_train_errors2,OJ_test_errors2)
polynomial

lower = min(OJ_train_errors2,OJ_test_errors2,OJ_train_errors,OJ_test_errors,OJ_train_errors1,OJ_test_errors1)
upper = max(OJ_train_errors2,OJ_test_errors2,OJ_train_errors,OJ_test_errors,OJ_train_errors1,OJ_test_errors1)
quartz()
plot(cost,OJ_test_errors2,type="b",lty=1,col = "red",ylim = c(lower,upper), xlab = "cost",main="Comparison",ylab="train & test error rates")
lines(cost,OJ_train_errors2,type="b",lty=2,col="orange")
lines(cost,OJ_test_errors1,type="b",lty=3,col = "brown",xlab = "cost")
lines(cost,OJ_train_errors1,type="b",lty=4,col="blue",xlab = "cost")
lines(cost,OJ_test_errors,type="b",lty=5,col = "black",xlab = "cost")
lines(cost,OJ_train_errors,type="b",lty=6,col="purple",xlab = "cost")

legend("topright",c("test_error_polynomial", "train_error_polynomial","test_error_radial","train_error_radial","test_error_linear","train_error_linear"),lty=c(1,2,3,4,5,6),col=c("red","orange","brown","blue","black","purple"))

