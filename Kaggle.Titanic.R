library(DMwR); library(plyr);library(magrittr); library(ggplot2);library(WDI);library(caret);library(e1071);library(gbm)

train <- read.csv( "titanic_traning.csv", header = T )
test <- read.csv( "titanic_test.csv", header = T )

str(data)
train$survived <- as.factor(train$survived)

#dt = sort(sample(nrow(data), nrow(data)*.7))
#train<-data[dt,]
#test<-data[-dt,]

### remove ID coloumn
train <- train[,c(-7)]
test <- test[,c(-7)]


str(train)

train[,8] <- as.factor((train[,8]))


### Distribution of survived vs not survived
cbind(freq=table(train$survived), percentage=prop.table(table(train$survived))*100)


##################### Which coloumns have NA values ##################### 
colnames(train)[colSums(is.na(train)) > 0]
colnames(test)[colSums(is.na(test)) > 0]

##################### use KNN to replace NA's##################### 

train <- knnImputation(train,k=7, scale = T, meth = 'median', distData = NULL)

test <- knnImputation(test,k=7, scale = T, meth = 'median', distData = NULL)

anyNA(train)
anyNA(test)

##################### Plot gender survival##################### 
barplot(table(train$survived, train[,2]))
legend("topleft", legend = c("Died", "Survived"), fill=c("black","grey"))

##################### 10-fold cross validation ##################### 
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"
set.seed(7)
grid <- expand.grid(.sigma=c(0.025, 0.05, 0.1, 0.15), .C=seq(1, 10, by=1))
fit.svm <- train(survived~., data=train, method="svmRadial", metric=metric, tuneGrid=grid,
                 preProc=c("BoxCox"), trControl=trainControl)
print(fit.svm)

# 10-fold cross validation with 3 repeats
trainControl <- trainControl(method="repeatedcv", number=10, repeats=3)
metric <- "Accuracy"


# Random Forest
set.seed(7)
RF <- train(survived~., data=train, method="rf", metric=metric, preProc=c("BoxCox"),
                trControl=trainControl)

set.seed(7)
fit.c50 <- train(survived~., data=train, method="C5.0", metric=metric, preProc=c("BoxCox"),
                 trControl=trainControl)


# Compare results
results <- resamples(list( RF=RF, C50=fit.c50))
summary(results)

summary(results)
dotplot(results)


##################### SVM Model ##################### 
str(test)

#prepare parameters for data transform
set.seed(7)
model <- svm(survived ~ ., data = train)
preprocessParams <- preProcess(testData, method=c("BoxCox"))
testData$Age[is.na(testData$Age)] <- 0
testData$Fare[is.na(testData$Fare)] <- 0
testData <- predict(preprocessParams, testData)


predictions <- predict(model, testData, type="class")
submit <- data.frame(PassengerId = datasetTest$PassengerId, Survived = predictions)

write.csv(predictions, file = "svm.csv", row.names = FALSE)

##################### SVM Model 2 with gama  ##################### 

svm.model <- svm(survived ~ ., data = train, gamma = 1,na.action = na.exclude)

head(svm.model)
svm.model$nSV
svm.model$SV
svm.model$x.scale
svm.model$scaled
svm.model$coefs
# Obtain feature weights
w = t(svm.model$coefs) %*% svm.model$SV
w

str(test)

#test <- test[!(is.na(test$embarked) | test$embarked==""), ]
#test$embarked <- factor(test$embarked)

svm.pred  <- predict(svm.model, test)

##################### Final Choice for Predicting ##################### 


svm.pred  <- predict(fit.c50, test)

write.csv(svm.pred,"titanic.csv") 


