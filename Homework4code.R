rm(list = ls())
###Q1
library(ElemStatLearn)
library(glmnet)
library(pls)
library(leaps)

set.seed(5000)
#AIC & BIC
data <- prostate
best_subset1 = regsubsets(train~.,data = prostate,method="exhaustive")
reg_summary1=summary(best_subset1)

select <- reg_summary1$outmat
aic_list <- c()
bic_list <- c()
errors_lm <- c()

for(i in 1:8){
  asterisk <- which(select[i,]=="*")
  data_select <- data[,c(9,asterisk)]
  select.fit<-lm(data$train~.,data=data_select)
  aic<-AIC(select.fit)
  bic<-BIC(select.fit)
  aic_list<-c(aic_list,aic)
  bic_list<-c(bic_list,bic)
  predict_lm<-predict(select.fit,newdata=data)
  errors<-sum((predict_lm-data$train)^2)/length(data$train)
  errors_lm<-c(errors_lm,errors)
}

errors_lm
# 0.2059847 0.2034065 0.1966227 0.1952737 0.1932049 0.1929553 0.1927423 0.1925755
aic_list
# 130.0186 130.7968 129.5066 130.8388 131.8057 133.6803 135.5732 137.4892

bic_list
# 140.3174 143.6704 144.9549 148.8618 152.4034 156.8527 161.3203 165.8110


upper= max(aic_list,bic_list)
lower= min(aic_list,bic_list)

quartz()
plot(aic_list,type="o",lty=1,col = "red",ylim = c(lower-5,upper+5),xlab = "k",main="AIC & BIC",ylab="Value")
lines(bic_list,type="o",lty=2,col="blue")
legend("topright",c("AIC", "BIC"),lty=c(1,2),col=c("red","blue"))


#5-fold
train_5fold = sample(1:nrow(prostate), nrow(prostate)*0.8)
test_5fold = -train_5fold
train_5fold_data = prostate[train_5fold, ]
test_5fold_data = prostate[test_5fold, ]
best_sub2 = regsubsets(train~.,data = train_5fold_data,method="exhaustive")
train_errors_5fold = rep(NA,8)
test_errors_5fold = rep(NA,8)
train_label = train_5fold_data$train
test_label = test_5fold_data$train
train_pred_matrix_5fold = model.matrix(train~., data = train_5fold_data)
test_pred_matrix_5fold = model.matrix(train~., data = test_5fold_data)
for (i in 1:8) {
  coefi = coef(best_sub2, id = i)
  pred_train <- train_pred_matrix_5fold[,names(coefi)] %*% coefi
  train_errors_5fold[i] = mean((train_label - pred_train)^2)
  pred_test <- test_pred_matrix_5fold[,names(coefi)] %*% coefi
  test_errors_5fold[i] = mean((test_label - pred_test)^2)
} 
#train_errors_5fold
test_errors_5fold
# 0.2394922 0.2717072 0.2615755 0.2645233 0.2635547 0.2657428 0.2691912 0.2692452
# the first is the best

#10-fold
train_10fold = sample(1:nrow(prostate), nrow(prostate)*0.9)
test_10fold = -train_10fold
train_10fold_data = prostate[train_10fold, ]
test_10fold_data = prostate[test_10fold, ]
best_sub3 = regsubsets(train~.,data = train_10fold_data,method="exhaustive")
train_errors_10fold = rep(NA,8)
test_errors_10fold = rep(NA,8)
train_label = train_10fold_data$train
test_label = test_10fold_data$train
train_pred_matrix_10fold = model.matrix(train~., data = train_10fold_data)
test_pred_matrix_10fold = model.matrix(train~., data = test_10fold_data)
for (i in 1:8) {
  coefi = coef(best_sub3, id = i)
  pred_train <- train_pred_matrix_10fold[,names(coefi)] %*% coefi
  train_errors_10fold[i] = mean((train_label - pred_train)^2)
  pred_test <- test_pred_matrix_10fold[,names(coefi)] %*% coefi
  test_errors_10fold[i] = mean((test_label - pred_test)^2)
}

#train_errors_10fold
test_errors_10fold
# 0.2571633 0.2633224 0.2744674 0.2833720 0.2829434 0.2780088 0.2766591 0.2726940
# the first is the best


#boot
library(boot)
"bootpred" <- function(x,y,nboot,theta.fit,theta.predict,err.meas,...) {
  call <- match.call()
  x <- as.matrix(x)
  n <- length(y)
  saveii <- NULL
  fit0 <- theta.fit(x,y,...)
  yhat0 <- theta.predict(fit0,x)
  app.err <- mean(err.meas(y,yhat0))
  err1 <- matrix(0,nrow=nboot,ncol=n)
  err2 <- rep(0,nboot)
  for(b in 1:nboot){
    ii <- sample(1:n,replace = TRUE)
    saveii <- cbind(saveii,ii)
    fit <- theta.fit(x[ii,],y[ii],...)
    yhat1 <- theta.predict(fit,x[ii,])
    yhat2 <- theta.predict(fit,x)  
    err1[b,] <- err.meas(y,yhat2)
    err2[b] <- mean(err.meas(y[ii],yhat1))
  }
  
  optim <- mean( apply(err1,1,mean)-err2)
  
  junk <- function(x,i){sum(x==i)}
  e0 <- 0
  for(i in 1:n){
    o <- apply(saveii,2,junk,i)
    if( sum(o==0)==0)
      cat("increase nboot for computation of the .632 estimator",
          fill = TRUE)
    e0 <- e0+ (1/n)*sum(err1[o==0,i])/sum(o==0)
  }
  err.632 <- .368*app.err + .632*e0
  return(list(app.err, 
              optim, 
              err.632, 
              call=call))
}

best_sub = regsubsets(train~.,data = prostate,method="exhaustive")
reg_summary=summary(best_sub)
select = reg_summary$outmat

beta.fit <- function(X,Y){
  lsfit(X,Y)	
}

beta.predict <- function(fit, X){
  cbind(1,X)%*%fit$coef
}

sq.error <- function(Y,Yhat){
  (Y-Yhat)^2
}

errors_boot <- c()
for (i in 1:8){
  # Pull out the model
  asterisk <- which(select[i,] == "*")
  
  res <- bootpred(prostate[,asterisk], prostate$train, nboot = 50, theta.fit = beta.fit, theta.predict = beta.predict, err.meas = sq.error) 
  errors_boot <- c(errors_boot, res[[3]])
  
}
errors_boot
# 0.2155809 0.2182957 0.2174019 0.2168557 0.2178529 0.2159487 0.2301553 0.2337875

quartz()
plot(test_errors_5fold,type="o",lty=2,col = "blue",ylim = c(0,1),xlab = "k",ylab="error",main="Error")
lines(test_errors_10fold,type="o",lty=1,col="red")
lines(errors_boot,type="o",lty=3,col="green")
lines(errors_lm,type="o",lty=4,col="black")
legend("topright",c("5-fold", "10-fold","bootstrap","lm"),lty=c(2,1,3,4),col=c("blue","red","green","black"))

###Q2 
library(tree)
library("rpart") #install.packages("rpart")
library(MASS)
set.seed(10000)
setwd("/Users/sunchengzhe/Desktop/Graduate School/2018 Fall/EAS 506/Homework/HW4")
wine_data = read.csv('wine_data.txt',header = FALSE)
wine_train = sample(1:nrow(wine_data), nrow(wine_data)*0.8)
wine_test = -wine_train
wine_train_data = wine_data[wine_train, ]
wine_test_data = wine_data[wine_test, ]

#Tree for full dataset
model.control <- rpart.control(minsplit = 10, xval = 5, cp = 0)
wine_model <- rpart(V1~., data = wine_data, method = "class", control = model.control)
plot(wine_model,branch = 0.5, uniform = T, compress = T,  main = "Tree for Full Dataset")
text(wine_model,use.n = T, all = T, cex = .6)

#Tree for train dataset
wine_train_model <- rpart(V1~., data = wine_train_data, method = "class", control = model.control)

#plot(wine_train_model,branch = 0.5, uniform = T, compress = T,  main = "Full Tree of test dataset: without pruning")
#text(wine_train_model, use.n = T, all = T, cex = .6)

#plot(fit.wine.data.train$cptable[,4], main = "Cp for model selection", ylab = "cv error")

#prun trees
min_cp = which.min(wine_train_model$cptable[,4])
pruned_model <- prune(wine_train_model, cp = wine_train_model$cptable[min_cp,1])

plot(pruned_model, branch = .3,uniform = T,  compress=T, main = "Pruned Tree")
text(pruned_model, use.n = T, all = T, cex = .5)

plot(wine_train_model, branch = .3, uniform = T, compress=T, main = "Full Tree")
text(wine_train_model, use.n = T, all = T, cex = .5)


tree_pred = predict(pruned_model, wine_test_data, type = "class")
mean(tree_pred != wine_test_data$V1)
#0.08333333 pretty good fit
#pruned_model$frame
#to obtain node information 
table(wine_test_data$V1,tree_pred)

###Q4
library(ElemStatLearn)
library(neuralnet)
library(gam)
library(kernlab)
library(ggplot2)
library(randomForest)
library(MLmetrics)
data(spam)
#sum(is.na(spam))
#spam$spam = ifelse(spam$spam == "spam",1,0)
spam = spam[1:4600,]
set.seed(1234)
train_index = sample(1:nrow(spam), 0.8*nrow(spam))
train = spam[train_index,]
test = spam[-train_index,]

trails = c()
OOB_errors = rep(0,100)
accuracy=c()
for(i in seq(2,20,2)){
  rf.model = randomForest(type~., spam, ntree = 100, mtry = i)
  model_predictions = predict(rf.model, test)
  OOB_errors = cbind(OOB_errors, rf.model$err.rate[,c(1)])
  accuracy = append(accuracy, MLmetrics::Accuracy(model_predictions, test$type))
  trails = append(trails, i)
  print(i)
}

quartz()
plot_dataframe = data.frame(trails, 1-accuracy)
names(plot_dataframe) = c("mtry", "test_error")
ggplot(plot_dataframe, aes(x = mtry, y = test_error)) + geom_line() + geom_point() +
  ggtitle("m vs errors") + theme(plot.title = element_text(hjust = 0.5))


OOBlist = data.frame(OOB_errors)[,-c(1)]
names(OOBlist) = c("mtry_2", "mtry_4","mtry_6","mtry_8","mtry_10")
OOBlist$NTrees = seq(1, 100)
ggplot(OOBlist, aes(NTrees)) + 
  geom_line(aes(y = mtry_2, colour = "mtry_2")) + geom_point(aes(y = mtry_2), size = 0.3) + 
  geom_line(aes(y = mtry_4, colour = "mtry_4")) + geom_point(aes(y = mtry_4), size = 0.3) + 
  geom_line(aes(y = mtry_6, colour = "mtry_6")) + geom_point(aes(y = mtry_6), size = 0.3) + 
  geom_line(aes(y = mtry_8, colour = "mtry_8")) + geom_point(aes(y = mtry_8), size = 0.3) + 
  geom_line(aes(y = mtry_10, colour = "mtry_10")) + geom_point(aes(y = mtry_10), size = 0.3) + 
  
  ggtitle("Out of Bag Errors.") + xlab("No of features") + ylab("OOB Error") +
  theme(plot.title = element_text(hjust = 0.5))

