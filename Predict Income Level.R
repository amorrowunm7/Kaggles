install.packages("rockchalk")


library(plyr)
library(magrittr)
#library(ggplot2)
library(WDI)

install.packages("WDI")

library(rockchalk);library( train.table ); library( caret );library(NbClust);library(ggplot2);library(caTools);library(e1071);library(magrittr)

train =fread( "census_traning.csv", header = T )
test = fread( "census_test.csv", header = T )



str(train)

#Convert "?" to NA's and omit NA's
train = train[ complete.cases( train ) ]

l <- train
sum(is.na(l))

str(train)

#Visulization of distrubution
histogram(train$age, breaks = 10)
histogram(train$hours.per.week, breaks = 10)
histogram(train$education.num, breaks = 10)


ggplot(train, aes(x = native.country, fill = Class)) + geom_bar(position="fill") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Workclass")

table(train$native.country, train$Class)


ggplot(train, aes(x = , fill = Class)) + geom_bar(position="fill") + theme(axis.text.x = element_text(angle = 90)) + ggtitle("Education")


## ommit education because education.num and education are the same,combine sex and matrial status, omit naive country
head(train)

train$relationship <- combineLevels(train$relationship, levs=c("Husband", "Wife"), "Spouse")

train <- train[, -c(5)]

# Make binary white and not white
train$race.binary <- ifelse(train$race == "White", 1,0)
table(train$race.binary)
train$race.strings<- train$race
race_dummies<- model.matrix(~race.strings -1, train)
train<- cbind(race_dummies, train)
colnames(train)


#Binary for occupations.
occs <- c("Exec-managerial", "Prof-specialty", "Protective-serv", "Sales", "Tech-support")
train$binary.occs <- ifelse(train$occupation %in% occs, 1,0)
train$sex.binary <- ifelse(train$sex == "Female", 1, 0)


#Binary developed or developing countries
dev_countries <- c("Canada", "England", "France", "Germany", "Italy", "Ireland", 
                   "Japan", "Portugal", "Taiwan", "India", "Holand-Netherlands", "China", "United-Sates")
train$dev.country.binary <- ifelse(train$native.country %in% dev_countries, 1,0)

#developed countries
train$native.country.continents <- countrycode::countrycode(sourcevar = train$native.country, origin = "country.name", destination = "region")

#train$native.country.continents <- as.factor(x = train$native.country.continents)
sum(is.na(train$native.country.continents))
table(train$native.country.continents)

# Remvoe rows with South as country name
train <- train[train$native.country != "South",]
# SOuth taken out



train$native.country.continents <- ifelse(train$native.country == 'Colombia', "South America", 
      ifelse(train$native.country == 'England', "Western Europe",
        ifelse(train$native.country == 'Hong', "Eastern Asia",
          ifelse(train$native.country == 'Yugoslavia', "Eastern Europe",
                                ifelse(train$native.country == 'Scotland', "Western Europe", train$native.country.continents)))))
train$native.country.continents <-as.factor(train$native.country.continents)
sum(is.na(train$native.country.continents))




train$ID <- as.factor(train$ID)
train$workclass <- as.factor(train$workclass)
train$fnlwgt <- as.factor(train$fnlwgt)
train$education.num <- as.factor(train$education.num)
train$marital.status <- as.factor(train$marital.status)
train$occupation <- as.factor(train$occupation)
train$relationship <- as.factor(train$relationship)
train$race <- as.factor(train$race)
train$sex <- as.factor(train$sex)
train$hours.per.week <- as.factor(train$hours.per.week)
train$Class <- as.factor(train$Class)


NB_model <- naiveBayes(Class ~., data=train)
NB_p <- predict(NB_model, test)

str(train)

nb <- naiveBayes(Class ~ ., data = train)


test_pred <- predict(nb, test )

write.csv(NB_p,"Final_HMW_4.csv") 


