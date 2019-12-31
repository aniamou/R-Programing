library(ISLR)
library(MASS)
install.packages('knitr')
install.packages('caret', dependencies = TRUE)
library(caret)
require(knitr)

setwd("/Users/abdulayeniamou/Desktop/R-studio")
attach(Boston)
str(Boston)
summary(Boston)

# > summary(Boston)
# crim                zn             indus            chas              nox               rm       
# Min.   : 0.00632   Min.   :  0.00   Min.   : 0.46   Min.   :0.00000   Min.   :0.3850   Min.   :3.561  
# 1st Qu.: 0.08204   1st Qu.:  0.00   1st Qu.: 5.19   1st Qu.:0.00000   1st Qu.:0.4490   1st Qu.:5.886  
# Median : 0.25651   Median :  0.00   Median : 9.69   Median :0.00000   Median :0.5380   Median :6.208  
# Mean   : 3.61352   Mean   : 11.36   Mean   :11.14   Mean   :0.06917   Mean   :0.5547   Mean   :6.285  
# 3rd Qu.: 3.67708   3rd Qu.: 12.50   3rd Qu.:18.10   3rd Qu.:0.00000   3rd Qu.:0.6240   3rd Qu.:6.623  
# Max.   :88.97620   Max.   :100.00   Max.   :27.74   Max.   :1.00000   Max.   :0.8710   Max.   :8.780  
# age              dis              rad              tax           ptratio          black            lstat      
# Min.   :  2.90   Min.   : 1.130   Min.   : 1.000   Min.   :187.0   Min.   :12.60   Min.   :  0.32   Min.   : 1.73  
# 1st Qu.: 45.02   1st Qu.: 2.100   1st Qu.: 4.000   1st Qu.:279.0   1st Qu.:17.40   1st Qu.:375.38   1st Qu.: 6.95  
# Median : 77.50   Median : 3.207   Median : 5.000   Median :330.0   Median :19.05   Median :391.44   Median :11.36  
# Mean   : 68.57   Mean   : 3.795   Mean   : 9.549   Mean   :408.2   Mean   :18.46   Mean   :356.67   Mean   :12.65  
# 3rd Qu.: 94.08   3rd Qu.: 5.188   3rd Qu.:24.000   3rd Qu.:666.0   3rd Qu.:20.20   3rd Qu.:396.23   3rd Qu.:16.95  
# Max.   :100.00   Max.   :12.127   Max.   :24.000   Max.   :711.0   Max.   :22.00   Max.   :396.90   Max.   :37.97  
# medv      
# Min.   : 5.00  
# 1st Qu.:17.02  
# Median :21.20  
# Mean   :22.53  
# 3rd Qu.:25.00  
# Max.   :50.00 

#### create new column for crim above the median
### creat a new response variable
Boston$resp <- "No"
Boston$resp[crim > median(crim)] <- 'Yes'
Boston$resp <-factor(Boston$resp)
table(Boston$resp)

# > table(Boston$resp)

# No Yes 
# 253 253

# Drop old crim variable
Boston <- Boston[-drop(1)]

### create training and test set\
seed(123)

for_training <- createDataPartition(y = Boston$resp, p = 0.8, list = FALSE)

train <- Boston[for_training,]
test <- Boston[-for_training,]


### find the correlations
correlation <- cor(train[,-14])

correlation



# > correlation
# zn      indus        chas        nox          rm        age        dis         rad         tax
# zn       1.00000000 -0.5424525 -0.04560424 -0.5157733  0.32253664 -0.5658022  0.6607403 -0.31879027 -0.32272483
# indus   -0.54245246  1.0000000  0.10114963  0.7629540 -0.39617129  0.6410099 -0.7097631  0.61002679  0.69743573
# chas    -0.04560424  0.1011496  1.00000000  0.1384542  0.08652719  0.1124080 -0.1350406  0.03796217  0.02592708
# nox     -0.51577331  0.7629540  0.13845422  1.0000000 -0.31765262  0.7343501 -0.7707763  0.60251138  0.66158830
# rm       0.32253664 -0.3961713  0.08652719 -0.3176526  1.00000000 -0.2407078  0.2294036 -0.22885723 -0.28948100
# age     -0.56580221  0.6410099  0.11240796  0.7343501 -0.24070779  1.0000000 -0.7407607  0.45408240  0.49901460
# dis      0.66074031 -0.7097631 -0.13504056 -0.7707763  0.22940359 -0.7407607  1.0000000 -0.49590779 -0.53434412
# rad     -0.31879027  0.6100268  0.03796217  0.6025114 -0.22885723  0.4540824 -0.4959078  1.00000000  0.93258906
# tax     -0.32272483  0.6974357  0.02592708  0.6615883 -0.28948100  0.4990146 -0.5343441  0.93258906  1.00000000
# ptratio -0.41114690  0.3743772 -0.09415178  0.1902672 -0.34801227  0.2471389 -0.2419634  0.49146907  0.46047857
# black    0.17707847 -0.3664854  0.06108062 -0.3767049  0.16414102 -0.2697625  0.2972137 -0.44240885 -0.44570286
# lstat   -0.41112224  0.5850224 -0.05059740  0.5791651 -0.61513285  0.5821771 -0.4772606  0.47623773  0.51290205
# medv     0.35241262 -0.4507214  0.17452105 -0.4108707  0.69324484 -0.3517494  0.2367083 -0.36929748 -0.43137969
# ptratio       black      lstat       medv
# zn      -0.41114690  0.17707847 -0.4111222  0.3524126
# indus    0.37437720 -0.36648538  0.5850224 -0.4507214
# chas    -0.09415178  0.06108062 -0.0505974  0.1745210
# nox      0.19026720 -0.37670493  0.5791651 -0.4108707
# rm      -0.34801227  0.16414102 -0.6151329  0.6932448
# age      0.24713891 -0.26976253  0.5821771 -0.3517494
# dis     -0.24196342  0.29721372 -0.4772606  0.2367083
# rad      0.49146907 -0.44240885  0.4762377 -0.3692975
# tax      0.46047857 -0.44570286  0.5129020 -0.4313797
# ptratio  1.00000000 -0.17096216  0.3660110 -0.4821966
# black   -0.17096216  1.00000000 -0.3861227  0.3365310
# lstat    0.36601099 -0.38612268  1.0000000 -0.7410390
# medv    -0.48219655  0.33653102 -0.7410390  1.0000000



### the highest correlations are between indus and rm, nox and chaos, nox and rad,

highest_Corr <- findCorrelation(correlation, cutoff = 0.75)
highest_Corr
### columns 2 industrial areas, 4 nox, and 9 property tax.

#logical regression
log_reg <- train(resp~., data=train, 
               method='glm', family=binomial(link='logit'),
               preProcess=c('scale', 'center'))

summary(log_reg)
log_reg

# > log_reg
# Generalized Linear Model 
# 
# 406 samples
# 13 predictor
# 2 classes: 'No', 'Yes' 
# 
# Pre-processing: scaled (13), centered (13) 
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 406, 406, 406, 406, 406, 406, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.8827026  0.7647523

# > summary(log_reg)
# 
# Call:
#   NULL
# 
# Deviance Residuals: 
#   Min       1Q   Median       3Q      Max  
# -2.0624  -0.2026  -0.0012   0.0062   3.3351  
# 
# Coefficients:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)  3.57935    0.93931   3.811 0.000139 ***
#   zn          -1.45356    0.80185  -1.813 0.069870 .  
# indus       -0.57772    0.34811  -1.660 0.096999 .  
# chas         0.41348    0.24506   1.687 0.091563 .  
# nox          5.29232    0.95852   5.521 3.36e-08 ***
#   rm          -0.06321    0.58592  -0.108 0.914096    
# age          0.57814    0.38114   1.517 0.129304    
# dis          1.21631    0.50499   2.409 0.016014 *  
#   rad          4.29565    1.44720   2.968 0.002995 ** 
#   tax         -0.62257    0.57686  -1.079 0.280484    
# ptratio      0.94072    0.33828   2.781 0.005422 ** 
#   black       -5.31402    1.71959  -3.090 0.002000 ** 
#   lstat        0.08910    0.38905   0.229 0.818850    
# medv         1.40214    0.75141   1.866 0.062040 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# (Dispersion parameter for binomial family taken to be 1)
# 
# Null deviance: 562.84  on 405  degrees of freedom
# Residual deviance: 157.86  on 392  degrees of freedom
# AIC: 185.86
# 
# Number of Fisher Scoring iterations: 9


###KNN Model
knnGrid <- expand.grid(.k=c(2))
KNN <- train(x=train[,-14], method='knn',
             y=train$resp, 
             preProcess=c('center', 'scale', 'pca'), 
             tuneGrid = knnGrid)
KNN

# > KNN
# k-Nearest Neighbors 
# 
# 406 samples
# 13 predictor
# 2 classes: 'No', 'Yes' 
# 
# Pre-processing: centered (13), scaled (13), principal component signal extraction (13) 
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 406, 406, 406, 406, 406, 406, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.8727355  0.7444523
# 
# Tuning parameter 'k' was held constant at a value of 2

train_cor <- train[,-drop(c(2,9))]
test_cor <- test[,-drop(c(2,9))]


LDA <- train(resp~., data=train_cor,
             method='lda', 
             preProcess=c('scale', 'center'))


LDA

# > LDA
# Linear Discriminant Analysis 
# 
# 406 samples
# 11 predictor
# 2 classes: 'No', 'Yes' 
# 
# Pre-processing: scaled (11), centered (11) 
# Resampling: Bootstrapped (25 reps) 
# Summary of sample sizes: 406, 406, 406, 406, 406, 406, ... 
# Resampling results:
#   
#   Accuracy   Kappa    
# 0.8613871  0.7216788


###Q2
setwd("/Users/abdulayeniamou/Desktop/R-studio")
my_data <- read.delim("https://astro.temple.edu/~alan/DiabetesAndrews36_1.txt", sep = "", header=FALSE)
head(my_data)
install.packages('MASS')
library(MASS)
### A
data_2 = my_data[ ,which(names(my_data) %in% c("V5","V6","V7","V8","V9","V10"))]
pairs(data_2[1:5])
cols <- character(nrow(data_2))
cols[]<-"red"
cols[data_2$V10 == 3] <- "green"
cols[data_2$V10 == 2] <- "blue"
jpeg('rplot2.jpg')
pairs(data_2[1:5],col=cols)
dev.off()
### B

lda_mod = lda(V10 ~ . , data = data_2)
lda_pred=predict(lda_mod,data_2)
mean(lda_pred$class == data_2$V10)
# 0.9034483

qda_mod = qda(V10 ~ . , data = data_2)
qda_pred=predict(qda_mod,data_2)
mean(qda_pred$class == data_2$V10)
# 0.9517241

#without CV qda is better

### C
V5=c(0.98)
V6=c(122)
V7=c(544)
V8=c(186)
V9=c(184)
df=data.frame(V5,V6,V7,V8,V9)
predict(qda_mod,df)
# $class
# [1] 2
# Levels: 1 2 3
# 
# $posterior
# 1         2            3
# 1 0.2934095 0.7065893 1.209066e-06

predict(lda_mod,df)

# > predict(lda_mod,df)
# $class
# [1] 3
# Levels: 1 2 3
# 
# $posterior
# 1         2         3
# 1 0.001240273 0.4731416 0.5256181
# 
# $x
# LD1         LD2
# 1 -0.007191535 -0.01192259

###Q4
set.seed(1)
x <- rnorm(100)
y <- x - 2 * x^2 + rnorm(100)

setwd("/Users/abdulayeniamou/Desktop/R-studio")

install.packages('boot')
library(boot)
### A1
set.seed(1)
Data <- data.frame(x, y)
glm_1 <- glm(y ~ x)
cv.glm(Data, glm_1)$delta[1]
### 8.855222

### A2

glm_2 <- glm(y ~ poly(x, 2))
cv.glm(Data, glm_2)$delta[1]
### 1.175313

### A3

glm_3 <- glm(y ~ poly(x, 3))
cv.glm(Data, glm_3)$delta[1]
### 1.198569

### A4

glm_4 <- glm(y ~ poly(x, 4))
cv.glm(Data, glm_4)$delta[1]
### 1.184311
jpeg('rplot.jpg')
plot(x,y)
dev.off()

### c
summary(glm_4)









