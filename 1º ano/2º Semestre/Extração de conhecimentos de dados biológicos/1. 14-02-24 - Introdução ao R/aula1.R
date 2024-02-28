###############################
#  ECDB - Aula 1              # 
###############################

# loading iris dataset

data(iris)
dim(iris)
names(iris)
head(iris)
unlist(lapply(iris,class))

# loading ecoli dataset

fileUrl = "http://archive.ics.uci.edu/ml/machine-learning-databases/ecoli/ecoli.data"
download.file(fileUrl, destfile="ecoli.csv")
ecoli = read.table("ecoli.csv")
dim(ecoli)
head(ecoli)

# loading earthquakes dataset

ficheiro = "http://darwin.di.uminho.pt/cursoAnaliseDados/earthquakeData.csv"
download.file(ficheiro, destfile = "earthquakes.csv")
eData = read.csv("earthquakes.csv")
dim(eData)
names(eData)
head(eData)

# verifying data structure

dim(eData)
nrow(eData)
ncol(eData)
names(eData)
class(eData)
unlist(lapply(eData, class))
unlist(lapply(eData, typeof))

# correcting/ changing data structure

names(eData)
names(eData)[5]="Latitude"
names(eData)[6]="Longitude"
names(eData)
row.names(eData)
row.names(eData) = paste("id-", row.names(eData), sep="")
row.names(eData)

# filters - examples with earthsquakes

eData$Latitude[1:10] > 40
any(eData$Lat[1:10] > 40)
all(eData$Lat[1:10] > 40)
which(eData$Lat[1:10] > 37)
sum(eData$Lat > 60)
eData[eData$Latitude > 38 & eData$Longitude > -119, c("Latitude","Longitude")]

subData = subset(eData, Longitude < -125 | Latitude > 40)
dim(subData)

# summarization

mean(eData$Magnitude)
median(eData$Magnitude)
sd(eData$Magnitude)
mad(eData$Magnitude)
quantile(eData$Magnitude)
summary(eData$Magnitude)

cfseal = read.csv("dataset-cfseal.csv")
summary(cfseal$lung, na.rm=T)

summary(iris)
rowMeans(iris[,-5])
colMeans(iris[,-5])
colSums(iris[-5])

mean(iris$Petal.Length)
sd(iris$Sepal.Width)
sapply(iris[,1:4], IQR)
sapply(iris[,1:4], sd)
quantile(iris[,1])

table(iris$Species)
unique(iris$Species)

unique(eData$Src)
table(eData$Src)
table(eData$Src,eData$Version)  


# discretization

range(eData$Magnitude)
md = cut(eData$Magnitude, c(0,1.5,2.5,4,7), labels = c("VL","L","M","H"))
levels(md)
table(md)
md[1:10]
eData$MagnitudeFactor = md
class(eData$MagnitudeFactor)


# missing values


names(cfseal)
cfseal$lung
sum(is.na(cfseal$lung))
cfseal$lung[cfseal$lung>2000]
cfseal$lung[cfseal$lung>2000 & !is.na(cfseal$lung)]
mean(cfseal$lung)
mean(cfseal$lung, na.rm=T)

table(c(0,1,2,3,NA,3,3,2,2,3))
table(c(0,1,2,3,NA,3,3,2,2,3), useNA="ifany")

# handling missing values

sum(is.na(cfseal$lung))
mean(cfseal$lung)
m = mean(cfseal$lung, na.rm=T)
m
cfseal$lung[is.na(cfseal$lung)] = m
sum(is.na(cfseal$lung))
mean(cfseal$lung)

cfn = na.exclude(cfseal)
sum(is.na(cfn$lung))

for (i in 1:ncol(cfseal)) { 
  m = mean(cfseal[,i], na.rm = TRUE) 
  cfseal[is.na(cfseal[,i]),i] = m 
}
sapply(cfseal, function(x) sum(is.na(x)) )

data(airquality)
complete.cases(airquality)
dim(airquality)
aqn = airquality[complete.cases(airquality),]
dim(aqn)
sapply(airquality, function(x) sum(is.na(x)))
sapply(aqn, function(x) sum(is.na(x)))

# data standardization

mean(iris$Sepal.Length)
sd(iris$Sepal.Length)
iris.st = as.data.frame(scale(iris[,1:4]))
mean(iris.st$Sepal.Length) 
sd(iris.st$Sepal.Length)
