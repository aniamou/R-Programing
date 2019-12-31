rm(list = ls())
install.packages("glmnet")
install.packages("ISLR")
install.packages("pls")
library("pls")
library("glmnet")
library("ISLR")
college <- College
set.seed(123)
#### A
sample <- sample.int(n = nrow(college), size = floor(.70*nrow(college)), replace = F)

train = college[sample,]
test = college[-sample,]

lin_mod = lm(Apps~.,data=train)
summary(lin_mod)
quartz()
plot(lin_mod)

prediction = predict(lin_mod,newdata=test)

MSE=mean((test$Apps-prediction)^2)

print(MSE)

### B
xtrain=model.matrix (Apps~.,train)[,-1]
ytrain=train$Apps
xtest=model.matrix (Apps~.,test)[,-1]
ytest=test$Apps



set.seed (123)
cros_val =cv.glmnet (xtrain,ytrain,alpha =0)
quartz()
plot(cros_val)




bestlam= cros_val$lambda.min
#Creating training model using ridge regression
cros_val1 =glmnet(xtrain,ytrain,alpha=0,lambda=bestlam)
#Printing out the logistic model
cros_val1$beta




pred1 = predict(ridg_mod,s=bestlam ,newx=xtest)
#Calculating Accuracy
MSE=mean((pred1 - ytest)^2)
#Printing MSE
print(MSE)


#### D

set.seed (123)
crosval2=cv.glmnet (xtrain,ytrain,alpha =1)
plot(cv.out)


bestlam=crosval2$lambda.min
#Creating training model using lasso regression
laso_mod =glmnet(xtrain,ytrain,alpha=1,lambda=bestlam)
#Printing out the logistic model
laso_mod$beta




pred2=predict(laso_mod,s=bestlam ,newx=xtest)
#Calculating Accuracy
MSE2=mean((pred2-ytest)^2)
#Printing MSE
print(MSE2)



las_mod_co=predict(laso_mod,type="coefficients",s=bestlam)[1:length(laso_mod$beta),]
#Listing the non zero coefficients
las_mod_co[las_mod_co!=0]

quartz()
plot(las_mod_co)


##### E

pcr_fit = pcr(Apps~.,data = train, scale = TRUE, validation = "CV")
quartz()
validationplot(pcr_fit, val.type = "MSEP")



pcr_pred = predict(pcr_fit, test, ncomp = 10)
mean((test[,"Apps"]- pcr_pred)^2)

####  F

pls_fit<-plsr(Apps~.,data= train, scale = TRUE, validation= "CV")
quartz()
validationplot(pls_fit,val.type = "MSEP")


pls_pred<-predict(pls_fit,test, ncomp=10)
mean((pls_pred-test$Apps)^2)

###Q2
setwd("/Users/abdulayeniamou/Desktop/R-studio")
rm(list =  ls())
set.seed(123)
train_data <-read.table('trainingset.txt')
test_data <-read.table('testset.txt')
train_train = sample(1:nrow(train_data), nrow(train_data)*0.80)
train_test = -train_train
#train data
train_train_data = train_data[train_train, ]
#test data
train_test_data = train_data[train_test, ]


lm_fit = lm(V86~., data = train_train_data)
lm_pred = predict(lm_fit, train_train_data)
#very high false negative rate, using logistic regression is much better in training (10) still 0 in testing
#a<-ifelse(lm_pred > 0.5,1,0) 
#
#mouge sb
#forward,backward fit
library(leaps)
regfit_fwd <- regsubsets(V86~., data=train_train_data, method = "forward",nvmax = 85)
regfit_bwd <- regsubsets(V86~., data=train_train_data, method = "backward",nvmax = 85)

#find best fit
test_matrix = model.matrix(V86 ~., data=train_test_data)
fwd_true_pos = rep(NA,84)
bwd_true_pos = rep(NA,84)
for (i in 1:84) {
  coef_fwd = coef(regfit_fwd, id = i)
  coef_bwd = coef(regfit_bwd, id = i)
  pred_fwd<-test_matrix[,names(coef_fwd)]%*%coef_fwd
  pred_bwd<-test_matrix[,names(coef_bwd)]%*%coef_bwd
  pred_fwd_bool <- ifelse(pred_fwd>0.5,1,0)
  pred_bwd_bool <- ifelse(pred_bwd>0.5,1,0)
  fwd_true_pos[i] = length(which(pred_fwd_bool == train_test_data$V86 & pred_fwd_bool == 1))
  bwd_true_pos[i] = length(which(pred_bwd_bool == train_test_data$V86 & pred_bwd_bool == 1))  
  

#ridge lasso
ridge_fit = cv.glmnet(as.matrix(train_train_data)[,1:85], train_train_data$V86, alpha=0)
lasso_fit = cv.glmnet(as.matrix(train_train_data)[,1:85], train_train_data$V86, alpha=1)
ridge_fit$lambda.min
# > ridge_fit$lambda.min
# [1] 0.1908452
#test
pred_ridge = predict(ridge_fit, newx=as.matrix(train_test_data)[,1:85], s=ridge_fit$lambda.min)
pred_lasso = predict(lasso_fit, newx=as.matrix(train_test_data)[,1:85], s=lasso_fit$lambda.min)

pred_ridge_bool <- ifelse(pred_ridge>0.5,1,0)
pred_lasso_bool <- ifelse(pred_lasso>0.5,1,0)
length(which(pred_ridge_bool==train_test_data$V86 & pred_ridge_bool ==1))
# > length(which(pred_ridge_bool==train_test_data$V86 & pred_ridge_bool ==1))
# [1] 0
length(which(pred_lasso_bool==train_test_data$V86 & pred_lasso_bool ==1))
# > length(which(pred_lasso_bool==train_test_data$V86 & pred_lasso_bool ==1))
# [1] 1
#both test 0

###Q3

set.seed(1)
p = 20
n = 1000
x = matrix(rnorm(n*p),n,p)
beta <- rnorm(p)
beta[3] <- 0
beta[4] <- 0
beta[9] <- 0
beta[10] <- 0
beta[19] <- 0
epsilon <- rnorm(p)
y <- x%*%beta + epsilon

# Split your data set into a training set containing 100 observations and a test set containing 900 observations.
data <- data.frame(y,x)
n.dat<-dim(data)[1]
set.seed(5)
rows<-sample(1:n.dat,n.dat/10)
test<-data[rows,]
dim(test)

train<-data[-rows,]
dim(train)

# Perform best subset selection on the training set, and plot the training set MSE associated with the best model of each size.
# Plot the test set MSE associated with the best model of each size.
library("leaps")
library("data.table")
regfit.model = regsubsets(y ~ ., train,nvmax = p)
val.errors = rep(NA, p)
col.name = colnames(data)
for (i in 1:p) {
  coefi = coef(regfit.model, id = i)
  pred = as.matrix(train[, col.name %in% names(coefi)]) %*% coefi[names(coefi) %in%                                                                  col.name]
  val.errors[i] = mean((train$y - pred)^2)
}
library(ggplot2)
dt.val.errors <- data.table(val.errors,seq(1:20))
ggplot(dt.val.errors, aes(x=V2,y=val.errors)) + geom_line()
which.min(val.errors)

#Plot the test set MSE associated with the best model of each size.

val.test.errors = rep(NA, p)
for (i in 1:p) {
  coefi = coef(regfit.model, id = i)
  pred = as.matrix(test[, col.name %in% names(coefi)]) %*% coefi[names(coefi) %in%                                                               col.name]
  val.test.errors[i] = mean((test$y - pred)^2)
}
dt.val.test.errors <- data.table(seq(1:20),val.test.errors)
ggplot(dt.val.test.errors, aes(V1,val.test.errors)) + geom_line()

# For which model size does the test set MSE take on its minimum value? Comment on your results. If it takes on its minimum value for a model containing only an intercept or a model containing all of the features, then play around with the way that you are generating the data in (a) until you come up with a scenario in which the test set MSE is minimized for an intermediate model size.
which.min(val.test.errors)

# How does the model at which the test set MSE is minimized compare to the true model used to generate the data? Comment on the coefficient values.
coef(regfit.model, id =19)

val.errors.g = rep(NA, p)
a = rep(NA, p)
b = rep(NA, p)
col.name <- col.name[-c(1)]
for (i in 1:p) {
  coefi = coef(regfit.model, id = i)
  a[i] = length(coefi) - 1
  b[i] = sqrt(sum((beta[col.name %in% names(coefi)] - coefi[names(coefi) %in% col.name])^2) + 
                sum(beta[!(col.name %in% names(coefi))])^2)
}

a.b <- data.table(a,b,keep.rownames = TRUE)
ggplot(a.b, aes(a,b)) + geom_point() + geom_line()
which.min(a.b$b)


