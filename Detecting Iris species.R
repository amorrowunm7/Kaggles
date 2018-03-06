rm( list = ls() )

library( data.table ); library( caret );library(NbClust)

train = fread( "iris_traning.csv", header = T )
test = fread( "iris_test.csv", header = T )

version
normalize <- function(x){
  
  return ((x-min(x))/(max(x)-min(x)))
  
}

data<- as.data.frame(lapply(train[,2:5],normalize))

standard <- function(x) {
  return ((x - mean(x))/sd(x))
}

data<- as.data.frame(lapply(train[,2:5],standard))


nb <- NbClust(data, distance = "euclidean", min.nc = 2,
              max.nc = 20, method = "kmeans")


trctrl <- trainControl(method = "repeatedcv", number = 7, repeats = 3)

set.seed(123)

knn_fit <- train(Species ~., data = train, method = "knn",
                 trControl=trctrl,
                 preProcess = c("center", "scale"),
                 tuneLength = 7)

test_pred <- predict(knn_fit, newdata = test )

write.csv(test_pred,"final.csv") 

