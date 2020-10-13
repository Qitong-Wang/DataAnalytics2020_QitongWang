library("readxl")
EPI_data <- read_xls("2010EPI_data.xls",sheet = "EPI2010_onlyEPIcountries")
View(EPI_data)
summary(EPI_data)
tf<- is.na(EPI_data$EPI)
sum(tf)
summary(EPI_data$EPI)
fivenum(EPI_data$EPI)
stem(EPI_data$EPI)
hist(EPI_data$EPI, seq(30., 95., 2.0), prob=TRUE)
lines(density(EPI_data$EPI,na.rm=TRUE,bw=1.))
rug(EPI_data$EPI)

#Exercise 1
tf <- is.na(EPI_data$ENVHEALTH)
ENVHEALTH <- EPI_data$ENVHEALTH[!tf]
summary(ENVHEALTH)
qqplot(EPI_data$EPI,ENVHEALTH)

tf <- is.na(EPI_data$ECOSYSTEM)
ECOSYSTEM <- EPI_data$ECOSYSTEM[!tf]
summary(ECOSYSTEM)

tf <- is.na(EPI_data$DALY)
DALY <- EPI_data$DALY[!tf]
summary(DALY)
boxplot(DALY,ECOSYSTEM,names=c("DALY","ECOSYSTEM"))

tf <- is.na(EPI_data$AIR_H)
AIR_H <- EPI_data$AIR_H[!tf]
summary(AIR_H)

tf <- is.na(EPI_data$WATER_H)
WATER_H <- EPI_data$WATER_H[!tf]
summary(WATER_H)
qqplot(AIR_H,WATER_H)


tf <- is.na(EPI_data$BIODIVERSITY)
BIODIVERSITY <- EPI_data$BIODIVERSITY[!tf]
summary(BIODIVERSITY)
plot(ecdf(BIODIVERSITY), do.points=FALSE, verticals=TRUE)


#Exercise2
EPILand<-EPI_data$EPI[!EPI_data$Landlock]
Eland <- EPILand[!is.na(EPILand)]
hist(Eland)
hist(Eland, seq(30., 95., 1.0), prob=TRUE)

