library(titanic)
plot(Titanic)
data_titanic <- titanic_train
require(rpart)
titanic_rpart <- rpart(Survived ~ Pclass + Age + SibSp + Parch + Fare, data = data_titanic)
plot(titanic_rpart) 
text(titanic_rpart) 


require(party)

titanic_ctree<-ctree(Survived ~ Pclass + Age + SibSp + Parch + Fare, data = data_titanic)
plot(titanic_ctree)


library(kknn)
titanic_kknn <- train.kknn(Survived ~ Pclass + Age + SibSp + Parch + Fare, titanic_train, distance = 1,kmax = 25,
                     kernel = c("triangular", "epanechnikov", "biweight", "triweight", "cos", "inv", "gaussian", "rank", "optimal"))
summary(titanic_kknn)

require(randomForest)
titanic_randomForest <- randomForest(Survived ~ Pclass + Age + SibSp + Parch + Fare, data = data_titanic)
print(titanic_randomForest) 	
importance(titanic_randomForest) 
