#<<<<<<< HEAD
#TEXT TEXT TEX


#what is the even Seb

library(lme4)


a <- 1:100
b <- sqrt(a)
plot(a,b)

library(tidyverse)
tsd <- read.csv('songs_to_classify.csv',header=T)
tsd<-as.tibble(tsd)


dat <- read.csv('training_data.csv',header=T)

dat<-as.tibble(dat)
dat$key<-as.factor(dat$key)
dat$mode<-as.factor(dat$mode)
dat$time_signature<-as.factor(dat$time_signature)
set.seed(9806543)
samp<-sample(1:500,100)
testdat<-dat[samp,]
traindat<-dat[-samp,]
##############################descriptives
summary(traindat)

summary(testdat)

pairs(traindat)


#####################################Always guess like
pred<-rep("like",100)
table(testdat$label, pred)
mean(pred == testdat$label)

#####################################Logistic with all cov


glm.fit <- glm(formula = label ~ ., data = traindat, family = binomial)

glm.probs <- predict(object = glm.fit, newdata = testdat, type = "response")
glm.pred <- rep("like", length(glm.probs))
glm.pred[glm.probs < 0.5] <- "dislike"
table(testdat$label, glm.pred)
mean(glm.pred == testdat$label)



#################################### Linear disc. analysis
library(MASS)

lda.fit <- lda(formula = label ~ ., data = traindat)
lda.testdata <- predict(object = lda.fit, newdata = testdat)
lda.pred <- lda.testdata$class
table(testdat$label, lda.pred)
mean(lda.pred == testdat$label)


#################################### quad disc. analysis

qda.fit <- qda(formula = label ~ danceability+ energy+loudness+ speechiness+ acousticness +instrumentalness+ liveness+ valence +  tempo+ duration, data = traindat)
qda.testdata <- predict(object = qda.fit, newdata = testdat)
qda.pred <- qda.testdata$class
table(testdat$label, qda.pred)
mean(qda.pred == testdat$label)


################################### k-Nearest Neig, k=1

library(class)

train.KNN <- scale(data.matrix(traindat[,c(-3,-5,-13,-14)]))
test.KNN <- scale(data.matrix(testdat[,c(-3,-5,-13,-14)]))
knn.pred <- knn(train = train.KNN, test = test.KNN,
                cl = as.matrix(traindat["label"]), k = 1)
table(testdat$label, knn.pred)
mean(knn.pred == testdat$label)



################################### various k

misclassification <- c()
for (kt in 1:50) {

  knn.pred <- knn(train = train.KNN, test = test.KNN,
                  cl = as.matrix(traindat["label"]), k = kt)
  misclassification[kt] = mean(knn.pred != testdat$label)
}
plot(x = 1:50, y = misclassification, type="l",xlab="k")



################################ ROC curve for log pred with diff thressholds
falsepositiverate <- c()
truepositiverate <- c()
N <- sum(testdat$label == "dislike")
P <- sum(testdat$label == "like")
for (i in 1:999) #try threshold 0.01, 0.02, ..., 0.99
{
  glm.pred <- rep("dislike", length(glm.probs))
  glm.pred[glm.probs > (i/1000)] <- "like"
  FP <- sum((glm.pred == "like")*(testdat$label == "dislike"))
  TP <- sum((glm.pred == "like")*(testdat$label == "like"))
  falsepositiverate[i] <- FP/N
  truepositiverate[i] <- TP/P
}
plot(x=falsepositiverate, y=truepositiverate,type="l",main="ROC curve")



########################################################################################### model selection


X<-scale(data.matrix(dat[,c(-3,-5,-13,-14)]))
y<-dat$label
N.K <- 100


#a

error.training <- c()
for (kt in 1:N.K)
{
  pred <- knn(train=X,test=X,cl=y,k=kt)
  error.training[kt] <- 1-mean(pred==y)
}
plot(x=1:N.K,y=error.training,type="l",xlab="k",ylab="training error",main="training error for kNN")
#b



training.indices <- sample(nrow(dat), size = 400, replace = FALSE)
X.training <- X[training.indices,]
y.training <- y[training.indices]
X.validation <- X[-training.indices,]
y.validation <- y[-training.indices]
error.validation <- c()
for (kt in 1:N.K)
{
  pred <- knn(train=X.training,test=X.validation,cl=y.training,k=kt)
  error.validation[kt] <- 1-mean(pred==y.validation)
}
plot(x=1:N.K,y=error.validation,type="l",xlab="k",ylab="validation error",main="validation set error for kNN")

c(1:N.K)[which(error.validation==min(error.validation))]
#c

N.CV <- 100 #repeat 10 times
error.crossvalidation1 <- matrix(0,N.K,N.CV)
for (i in 1:N.CV)
{
  training.indices <- sample(nrow(dat), size = 400, replace = FALSE)
  X.training <- X[training.indices,]
  y.training <- y[training.indices]
  X.validation <- X[-training.indices,]
  y.validation <- y[-training.indices]
  for (kt in 1:N.K)
  {
    pred <- knn(train=X.training,test=X.validation,cl=y.training,k=kt)
    error.crossvalidation1[kt,i] <- 1-mean(pred==y.validation)
  }
  print(i)
}
plot(x=1:N.K,y=rowMeans(error.crossvalidation1),type="l",xlab="k",
     ylab="validation error",main="cross validation error for kNN")

bestk<-which(rowMeans(error.crossvalidation1)==min(rowMeans(error.crossvalidation1)))


############################################################################################################
ER <- function(y, yhat){
  r <- 1-mean(y==yhat)
  return(r)
}

N.cv <- 100 # number of crossvalidation
N.CV <- 100
ER.CV <- data.frame(log.reg=double(), # intialize a data frame for storing results
                   lda=double(),
                   qda=double(),
                   kNN=double())


randomize.indices <- sample(nrow(dat), size = nrow(dat), replace = FALSE)
dat.rand <- dat[randomize.indices,]
for (i in 1:N.cv){
    start.index = (i-1)*ceiling(nrow(dat)/N.CV)+1
  end.index = min(i*ceiling(nrow(dat)/N.CV),nrow(dat))
  validation.indices <- seq(from = start.index, to = end.index, by = 1)
  validation.data <- dat.rand[validation.indices,]
  training.data <- dat.rand[-validation.indices,]
  glm.model <- glm(formula = label ~ ., data = training.data, family = binomial)
  glm.probs <- predict(object = glm.model, newdata = validation.data, type="response")
  glm.predictions <- rep("dislike",nrow(validation.data))
  glm.predictions[glm.probs>.5] <- "like"
  glm.ER <- ER(y = validation.data$label, yhat = glm.predictions)
  
  lda.model <- lda(formula = label ~ ., data = training.data)
  lda.predictions <- predict(object = lda.model,newdata=validation.data)
  lda.ER <- ER(y = validation.data$label, yhat = lda.predictions$class)
  qda.model <- qda(formula = label ~ danceability+ energy+loudness+ speechiness+ acousticness +instrumentalness+ liveness+ valence +  tempo+ duration, data = training.data)
  qda.predictions <- predict(object = qda.model,newdata=validation.data)
  qda.ER <- ER(y = validation.data$label, yhat = qda.predictions$class)
  
  kNN.predictions <- knn(train = scale(data.matrix(training.data[,c(-3,-5,-13,-14)])),
                         test = scale(data.matrix(validation.data[,c(-3,-5,-13,-14)])),
                         cl=training.data$label, k=15)
  kNN.ER <- ER(y = validation.data$label, yhat = kNN.predictions)
  ER.CV[nrow(ER.CV)+1,] <-c(glm.ER, lda.ER, qda.ER, kNN.ER)
}


boxplot(ER.CV)

=======
#TEXT TEXT TEX

library(tidyverse)
tsd <- read.csv('songs_to_classify.csv',header=T)
tsd<-as.tibble(tsd)


dat <- read.csv('training_data.csv',header=T)

dat<-as.tibble(dat)
dat$key<-as.factor(dat$key)
dat$mode<-as.factor(dat$mode)
dat$time_signature<-as.factor(dat$time_signature)
set.seed(9806543)
samp<-sample(1:500,100)
testdat<-dat[samp,]
traindat<-dat[-samp,]
##############################descriptives
summary(traindat)

summary(testdat)

pairs(traindat)


#####################################Always guess like
pred<-rep("like",100)
table(testdat$label, pred)
mean(pred == testdat$label)

#####################################Logistic with all cov


glm.fit <- glm(formula = label ~ ., data = traindat, family = binomial)

glm.probs <- predict(object = glm.fit, newdata = testdat, type = "response")
glm.pred <- rep("like", length(glm.probs))
glm.pred[glm.probs < 0.5] <- "dislike"
table(testdat$label, glm.pred)
mean(glm.pred == testdat$label)



#################################### Linear disc. analysis
library(MASS)

lda.fit <- lda(formula = label ~ ., data = traindat)
lda.testdata <- predict(object = lda.fit, newdata = testdat)
lda.pred <- lda.testdata$class
table(testdat$label, lda.pred)
mean(lda.pred == testdat$label)


#################################### quad disc. analysis

qda.fit <- qda(formula = label ~ danceability+ energy+loudness+ speechiness+ acousticness +instrumentalness+ liveness+ valence +  tempo+ duration, data = traindat)
qda.testdata <- predict(object = qda.fit, newdata = testdat)
qda.pred <- qda.testdata$class
table(testdat$label, qda.pred)
mean(qda.pred == testdat$label)


################################### k-Nearest Neig, k=1

library(class)

train.KNN <- scale(data.matrix(traindat[,c(-3,-5,-13,-14)]))
test.KNN <- scale(data.matrix(testdat[,c(-3,-5,-13,-14)]))
knn.pred <- knn(train = train.KNN, test = test.KNN,
                cl = as.matrix(traindat["label"]), k = 1)
table(testdat$label, knn.pred)
mean(knn.pred == testdat$label)



################################### various k

misclassification <- c()
for (kt in 1:50) {

  knn.pred <- knn(train = train.KNN, test = test.KNN,
                  cl = as.matrix(traindat["label"]), k = kt)
  misclassification[kt] = mean(knn.pred != testdat$label)
}
plot(x = 1:50, y = misclassification, type="l",xlab="k")



################################ ROC curve for log pred with diff thressholds
falsepositiverate <- c()
truepositiverate <- c()
N <- sum(testdat$label == "dislike")
P <- sum(testdat$label == "like")
for (i in 1:999) #try threshold 0.01, 0.02, ..., 0.99
{
  glm.pred <- rep("dislike", length(glm.probs))
  glm.pred[glm.probs > (i/1000)] <- "like"
  FP <- sum((glm.pred == "like")*(testdat$label == "dislike"))
  TP <- sum((glm.pred == "like")*(testdat$label == "like"))
  falsepositiverate[i] <- FP/N
  truepositiverate[i] <- TP/P
}
plot(x=falsepositiverate, y=truepositiverate,type="l",main="ROC curve")



########################################################################################### model selection


X<-scale(data.matrix(dat[,c(-3,-5,-13,-14)]))
y<-dat$label
N.K <- 100


#a

error.training <- c()
for (kt in 1:N.K)
{
  pred <- knn(train=X,test=X,cl=y,k=kt)
  error.training[kt] <- 1-mean(pred==y)
}
plot(x=1:N.K,y=error.training,type="l",xlab="k",ylab="training error",main="training error for kNN")
#b



training.indices <- sample(nrow(dat), size = 400, replace = FALSE)
X.training <- X[training.indices,]
y.training <- y[training.indices]
X.validation <- X[-training.indices,]
y.validation <- y[-training.indices]
error.validation <- c()
for (kt in 1:N.K)
{
  pred <- knn(train=X.training,test=X.validation,cl=y.training,k=kt)
  error.validation[kt] <- 1-mean(pred==y.validation)
}
plot(x=1:N.K,y=error.validation,type="l",xlab="k",ylab="validation error",main="validation set error for kNN")

c(1:N.K)[which(error.validation==min(error.validation))]
#c

N.CV <- 100 #repeat 10 times
error.crossvalidation1 <- matrix(0,N.K,N.CV)
for (i in 1:N.CV)
{
  training.indices <- sample(nrow(dat), size = 400, replace = FALSE)
  X.training <- X[training.indices,]
  y.training <- y[training.indices]
  X.validation <- X[-training.indices,]
  y.validation <- y[-training.indices]
  for (kt in 1:N.K)
  {
    pred <- knn(train=X.training,test=X.validation,cl=y.training,k=kt)
    error.crossvalidation1[kt,i] <- 1-mean(pred==y.validation)
  }
  print(i)
}
plot(x=1:N.K,y=rowMeans(error.crossvalidation1),type="l",xlab="k",
     ylab="validation error",main="cross validation error for kNN")

bestk<-which(rowMeans(error.crossvalidation1)==min(rowMeans(error.crossvalidation1)))


############################################################################################################
ER <- function(y, yhat){
  r <- 1-mean(y==yhat)
  return(r)
}

N.cv <- 100 # number of crossvalidation
N.CV <- 100
ER.CV <- data.frame(log.reg=double(), # intialize a data frame for storing results
                   lda=double(),
                   qda=double(),
                   kNN=double())


randomize.indices <- sample(nrow(dat), size = nrow(dat), replace = FALSE)
dat.rand <- dat[randomize.indices,]
for (i in 1:N.cv){
    start.index = (i-1)*ceiling(nrow(dat)/N.CV)+1
  end.index = min(i*ceiling(nrow(dat)/N.CV),nrow(dat))
  validation.indices <- seq(from = start.index, to = end.index, by = 1)
  validation.data <- dat.rand[validation.indices,]
  training.data <- dat.rand[-validation.indices,]
  glm.model <- glm(formula = label ~ ., data = training.data, family = binomial)
  glm.probs <- predict(object = glm.model, newdata = validation.data, type="response")
  glm.predictions <- rep("dislike",nrow(validation.data))
  glm.predictions[glm.probs>.5] <- "like"
  glm.ER <- ER(y = validation.data$label, yhat = glm.predictions)
  
  lda.model <- lda(formula = label ~ ., data = training.data)
  lda.predictions <- predict(object = lda.model,newdata=validation.data)
  lda.ER <- ER(y = validation.data$label, yhat = lda.predictions$class)
  qda.model <- qda(formula = label ~ danceability+ energy+loudness+ speechiness+ acousticness +instrumentalness+ liveness+ valence +  tempo+ duration, data = training.data)
  qda.predictions <- predict(object = qda.model,newdata=validation.data)
  qda.ER <- ER(y = validation.data$label, yhat = qda.predictions$class)
  
  kNN.predictions <- knn(train = scale(data.matrix(training.data[,c(-3,-5,-13,-14)])),
                         test = scale(data.matrix(validation.data[,c(-3,-5,-13,-14)])),
                         cl=training.data$label, k=15)
  kNN.ER <- ER(y = validation.data$label, yhat = kNN.predictions)
  ER.CV[nrow(ER.CV)+1,] <-c(glm.ER, lda.ER, qda.ER, kNN.ER)
}


boxplot(ER.CV)

>>>>>>> 07d1f7bae07c043800a9c948db7a81fb11c7b900
