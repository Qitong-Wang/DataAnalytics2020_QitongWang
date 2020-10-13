library(rpart)
library(rpart.plot)
library(ggplot2)

data(msleep)

str(msleep)
help("msleep") 
str(data)

mSleepDF1 <- msleep[,c(3,6,10,11)] 
str(mSleepDF1)
head(mSleepDF1)
help("rpart") 
sleepModel_1 <- rpart(sleep_total ~ ., data=mSleepDF1, method = "anova")
sleepModel_1
help("rpart.plot")
rpart.plot(sleepModel_1, type = 3, fallen.leaves = TRUE)
rpart.plot(sleepModel_1, type = 3,digits = 3, fallen.leaves = TRUE) 
rpart.plot(sleepModel_1, type = 3,digits = 4, fallen.leaves = TRUE)

require(C50)
data("iris")
head(iris)
str(iris)
table(iris$Species)
set.seed(9850)
grn <-runif(nrow(iris))
irisrand <-iris[order(grn),]
str(irisrand)
classificationmodel1 <-C5.0(irisrand[1:100,-5], irisrand[1:100,5])
classificationmodel1
summary(classificationmodel1)
prediction1 <- predict(classificationmodel1,irisrand[101:150,])
prediction1
table(irisrand[101:150,5],prediction1)
plot(classificationmodel1)


require(rpart)
Swiss_rpart <- rpart(Fertility ~ Agriculture + Education + Catholic, data = swiss)
plot(swiss_rpart) # try some different plot options
text(swiss_rpart) # try some different text options

require(party)

treeSwiss<-ctree(Species ~ ., data=iris)
plot(treeSwiss)

cforest(Species ~ ., data=iris, controls=cforest_control(mtry=2, mincriterion=0))

treeFert<-ctree(Fertility ~ Agriculture + Education + Catholic, data = swiss)

cforest(Fertility ~ Agriculture + Education + Catholic, data = swiss, controls=cforest_control(mtry=2, mincriterion=0))


library(tree)
tr <- tree(Species ~ ., data=iris)
tr
tr$frame
plot(tr)
text(tr)


fit2M <- ctree(Mileage~Price + Country + Reliability + Type, data=na.omit(cu.summary))
summary(fit2M)
plot(fit2M, uniform=TRUE, main="CI Tree Tree for Mileage ")


fitK <- ctree(Kyphosis ~ Age + Number + Start, data=kyphosis)
plot(fitK, main="Conditional Inference Tree for Kyphosis")
plot(fitK, main="Conditional Inference Tree for Kyphosis",type="simple")



