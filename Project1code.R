rm(list =  ls())
### quarts() or x11() to open another window
### set directory
setwd("/Users/abdulayeniamou/Desktop/R-studio")

###read data file from directory
cereal_data = read.csv("/Users/abdulayeniamou/Desktop/R-studio/cereal.csv", sep = ",", header = TRUE)


### subset of data to only include numeric values
new_cereal <- cereal_data[c(4:16)] #### only keep columns with numeric values
###  plot Histograms
par(mfrows = c(1,2))
attach(cereal_data)
hist(rating, xlab = "Nutritional Ratings (Miles/gallon)")

########### plot density function
## density proximation
dens <-density(rating)
xlim <-range(dens$x)
ylim <-range(dens$y)

hist(rating, xlab = "Nutritional Ratings (Miles/gallon)", probability = TRUE, xlim = xlim, ylim = ylim)
lines(dens)

### giving an idea of the distribution

### export graphs
saveeps("density_plots")



########## stem and leaf plots
### get the rattings

stem(cereal_data$rating)


####### results of stem and leaf plots
# 1 | 8
# 2 | 0223478899
# 3 | 0000111234445666677789999
# 4 | 000011111224566779
# 5 | 00112233355899
# 6 | 013588
# 7 | 34
# 8 | 
#   9 | 4


### has to be less than 3 outliers
######### Box plots
boxplot(new_cereal$rating, plot= FALSE)$out
#### OUTLIER  [1] 93.70491

outliers <- boxplot(new_cereal$rating, plot= FALSE)$out

new_cereal[which(new_cereal$rating %in% outliers),]
new_cereal <- new_cereal[-which(new_cereal$rating %in% outliers),]

#### var2

outliers <- boxplot(new_cereal$calories, plot= FALSE)$out

new_cereal[which(new_cereal$calories %in% outliers),]
#### shows too many rows of the observation as outliers

##VAR 3
outliers <- boxplot(new_cereal$protein, plot= FALSE)$out

new_cereal[which(new_cereal$protein %in% outliers),]



### OUTLIERS [1] 6 5 6


### VAR 4
outliers <- boxplot(new_cereal$fat, plot= FALSE)$out

new_cereal[which(new_cereal$fat %in% outliers),]
new_cereal <- new_cereal[-which(new_cereal$fat %in% outliers),]
### NO OUTLiERs


###VAR 5

boxplot(new_cereal$soduim, horizontal = TRUE, plot= FALSE)$out

outliers <-boxplot(new_cereal$fiber, plot= FALSE)$out
new_cereal[which(new_cereal$fiber %in% outliers),]
new_cereal <- new_cereal[-which(new_cereal$fiber %in% outliers),]

boxplot(new_cereal$fiber, horizontal = TRUE, plot= FALSE)$out
# Outliers [1] 10  9

outliers <- boxplot(new_cereal$carbo, plot= FALSE)$out

new_cereal[which(new_cereal$carbo %in% outliers),]
new_cereal <- new_cereal[-which(new_cereal$carbo %in% outliers),]

#### outlier [1] -1

boxplot(new_cereal$sugars, horizontal = TRUE, plot= FALSE)$out


boxplot(new_cereal$potass, horizontal = TRUE, plot= FALSE)$out


boxplot(new_cereal$vitamins, horizontal = TRUE, plot= FALSE)$out


boxplot(cereal_data$shelf, horizontal = TRUE, plot= FALSE)$out


boxplot(cereal_data$weight, horizontal = TRUE, plot= FALSE)$out

outliers <- boxplot(new_cereal$cups, plot= FALSE)$out

new_cereal[which(new_cereal$cups %in% outliers),]
new_cereal <- new_cereal[-which(new_cereal$cups %in% outliers),]

#### OUTLIERS [1] 1.5

### alternative

###########bwplot(~rating, data = cereal_data)



### looks like anything higher than 75 is an outlier

######## Patterns in Bivariate 
quartz()

### subset of data to only include numeric values


xyrange <- range(new_cereal)

plot(rating ~ calories, data = new_cereal, xlim = xyrange, ylim = xyrange)
rug(new_cereal$rating)
rug(new_cereal$calories)
abline()

quartz()
pairs(rating ~. , data = new_cereal)


quartz()
pairs(rating ~. , data = new_cereal)

####### eliminate rows with a -1 as missing data
###### eliminate outliers
##### finding outliers

boxplot(new_cereal$calories)$out



####new_cereal <- subset(new_cereal, names(cereal_data) !=-1)
##### add a linear model for simple regression
#pairs(~rating + calories + protein + fat + sodium + fiber + carbo + sugars + potass + vitamins + shelf + weight+ cups, data = new_cereal)


##### eliminate the nutrients with a value of -1 which is missing observation


################# QUESTION 2A 3 STARTS ARE SIGNIFICANT
model <- lm(rating ~., data = new_cereal)
summary(model)

### significant 
# calories, protein, fat, sodim, fibers, carbo, sugar, potass, vitamins, 

# Residuals:
#   Min         1Q     Median         3Q        Max
# -5.243e-07 -2.577e-07  4.643e-08  2.264e-07  5.657e-07

# Coefficients:
#   Estimate Std. Error    t value Pr(>|t|)
# (Intercept)  5.493e+01  3.630e-07  1.513e+08   <2e-16 ***
#   calories    -2.227e-01  5.663e-09 -3.933e+07   <2e-16 ***
#   protein      3.273e+00  5.092e-08  6.428e+07   <2e-16 ***
#   fat         -1.691e+00  6.226e-08 -2.717e+07   <2e-16 ***
#   sodium      -5.449e-02  4.962e-10 -1.098e+08   <2e-16 ***
#   fiber        3.443e+00  4.309e-08  7.992e+07   <2e-16 ***
#   carbo        1.092e+00  1.743e-08  6.268e+07   <2e-16 ***
#   sugars      -7.249e-01  1.819e-08 -3.986e+07   <2e-16 ***
#   potass      -3.399e-02  1.473e-09 -2.307e+07   <2e-16 ***
#   vitamins    -5.121e-02  1.928e-09 -2.657e+07   <2e-16 ***
#   shelf       -3.721e-08  5.285e-08 -7.040e-01    0.484
# weight      -4.298e-07  5.206e-07 -8.260e-01    0.412
# cups         1.379e-07  1.924e-07  7.170e-01    0.476
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1



############ ANSWER TO QUESTION 2B
#########When every other predictor held constant, the mpg value increases with each year that passes. Specifically, mpg increase by 1.43 each year.-7.249e-01



model = lm(rating ~.-shelf+calories:protein, data = new_cereal)
summary(model)


model = lm(rating ~.-shelf-cups-weight+calories+protein+fat+sodium+fiber+carbo+potass+vitamins+sugars, data=new_cereal)
summary(model)




model = lm(rating ~.-name+calories+protein+fat+sodium+fiber+carbo+potass+vitamins+sugars, data=new_cereal)
summary(model)



###Q3
###a
library(MASS)
head(Boston)
#pairs(Boston[, -1], gap = 0, pch = ".")
str(Boston)
Boston$chas <- as.numeric(Boston$chas)
Boston$rad <- as.numeric(Boston$rad)
quartz()
pairs(Boston)
## Question b##
large_model_Boston <- lm(crim ~ ., data = Boston)
summary(large_model_Boston)

cor(Boston$crim,Boston$zn)
cor(Boston$crim,Boston$indus)
cor(Boston$crim,Boston$chas)
cor(Boston$crim,Boston$nox)
cor(Boston$crim,Boston$rm)
cor(Boston$crim,Boston$age)
cor(Boston$crim,Boston$dis)
cor(Boston$crim,Boston$rad)
cor(Boston$crim,Boston$tax)
cor(Boston$crim,Boston$ptratio)
cor(Boston$crim,Boston$black)
cor(Boston$crim,Boston$lstat)
cor(Boston$crim,Boston$medv)

###c
summary(Boston$crim)
summary(Boston$tax)
summary(Boston$ptratio)
library(ggplot2)
quartz()
qplot(Boston$crim, binwidth=5 , xlab = "Crime rate", ylab="Numbers of Suburbs" )
qplot(Boston$tax, binwidth=50 , xlab = "Full-value property-tax rate per $10,000", ylab="Number of Suburbs")
qplot(Boston$ptratio, binwidth=5, xlab ="Pupil-teacher ratio by town", ylab="Number of Suburbs")
selection <- subset( Boston, crim < 10)
nrow(selection)/ nrow(Boston)
selection <- subset( Boston, crim > 25)
nrow(selection)/ nrow(Boston)
selection <- subset( Boston, crim > 50)
nrow(selection)/ nrow(Boston)
selection <- subset( Boston, crim > 75)
nrow(selection)/ nrow(Boston)
selection <- subset( Boston, tax< 500)
nrow(selection)/ nrow(Boston)
selection <- subset( Boston, tax> 600) 
nrow(selection)/ nrow(Boston)
selection <- subset( Boston, ptratio< 18)
nrow(selection)/ nrow(Boston)
selection <- subset( Boston, ptratio> 18) 
nrow(selection)/ nrow(Boston)

###d
rm_over_7 <- subset(Boston, rm>7)
nrow(rm_over_7)  
rm("rm_over_7")
rm_over_8 <- subset(Boston, rm>8)
nrow(rm_over_8) 
summary(rm_over_8)


###Q4
install.packages("ElemStatLearn")
library(ElemStatLearn)
require(class)
#load data (2's and 3's only)
train = data.frame(zip.train)
train <- train[c(which(train[,1] == 2), which(train[,1] == 3)),]
test = data.frame(zip.test)
test <- test[c(which(test[,1] == 2), which(test[,1] == 3)),]

#linear regression classification
formula_lm <- lm(formula = train[,1] ~ ., data = train[,2:257])
prediction.train <-predict(formula_lm,train[,2:257])
train.error <- 1-length(which(train[,1]==round(prediction.train)))/length(train[,1])
print(train.error) ###training errors###
prediction.test <-predict(formula_lm,test[,2:257])
test.error <- 1-length(which(test[,1]==round(prediction.test)))/length(test[,1])
print(test.error) ###test errors###

#knn classification
knn.train <- train[,2:257]
knn.test <- test[,2:257]
train.label <- train[,1]
test.label <- test[,1]
for (i in c(1,3,5,7,9,11,13,15)){
  prediction.knn <- knn(knn.train,knn.train,train.label,k=i)
  print(1-length(which(train.label==prediction.knn))/length(train.label))#accuracy
}

###test errors###
for (i in c(1,3,5,7,9,11,13,15)){
  prediction.knn <- knn(knn.train,knn.test,train.label,k=i)
  print(1-length(which(test.label==prediction.knn))/length(test.label))#accuracy
}

