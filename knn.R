library(class)

setwd("/home/danilo/R/workspace/projetoVunerabilidades/")

if(!exists("foo", mode="function")) source("carregarDados.R")
if(!exists("foo", mode="function")) source("avaliarDados.R")

stringQueryCountFUNCTIONS_derby <- "SELECT COUNT(*) FROM software.FUNCTIONS_derby;"
stringQueryFUNCTIONS_derby <- "SELECT * FROM software.FUNCTIONS_derby;"

fKnn <- function(stringQueryFUNCTIONS, stringQueryCountFUNCTIONS){
listTrainTestAndLabels <- getTrainTestAndLabels(stringQueryFUNCTIONS, stringQueryCountFUNCTIONS)

train <- as.data.frame(listTrainTestAndLabels[1])
test <- as.data.frame(listTrainTestAndLabels[2])
labelTrain <- as.data.frame(listTrainTestAndLabels[3])[,1]
labelTest <- as.data.frame(listTrainTestAndLabels[4])[,1]

###### split train
train20 <- as.data.frame(splitDatesInTrainAndTest(train, .2)[1])
train40 <- as.data.frame(splitDatesInTrainAndTest(train, .4)[1])
train60 <- as.data.frame(splitDatesInTrainAndTest(train, .6)[1])
train80 <- as.data.frame(splitDatesInTrainAndTest(train, .8)[1])

labelTrain20 <- sample(labelTrain, size = dim(train20)[1])
labelTrain40 <- sample(labelTrain, size = dim(train40)[1])
labelTrain60 <- sample(labelTrain, size = dim(train60)[1])
labelTrain80 <- sample(labelTrain, size = dim(train80)[1])
##### split trains

###### split test
test20 <- as.data.frame(splitDatesInTrainAndTest(test, .2)[1])
test40 <- as.data.frame(splitDatesInTrainAndTest(test, .4)[1])
test60 <- as.data.frame(splitDatesInTrainAndTest(test, .6)[1])
test80 <- as.data.frame(splitDatesInTrainAndTest(test, .8)[1])

labelTest20 <- sample(labelTest, size = dim(test20)[1])
labelTest40 <- sample(labelTest, size = dim(test40)[1])
labelTest60 <- sample(labelTest, size = dim(test60)[1])
labelTest80 <- sample(labelTest, size = dim(test80)[1])
##### split test

data20 <- knn(train = train20, test = test20, cl = labelTrain20, k = 3)
valuesCrossTable20 <- getValuesCrossTable(labelTest20, data20)
fMeasureResult20 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTable20)[3])

data40 <- knn(train = train40, test = test40, cl = labelTrain40, k = 3)
valuesCrossTable40 <- getValuesCrossTable(labelTest40, data40)
fMeasureResult40 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTable40)[3])

data60 <- knn(train = train60, test = test60, cl = labelTrain60, k = 3)
valuesCrossTable60 <- getValuesCrossTable(labelTest60, data60)
fMeasureResult60 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTable60)[3])

data80 <- knn(train = train80, test = test80, cl = labelTrain80, k = 3)
valuesCrossTable80 <- getValuesCrossTable(labelTest80, data80)
fMeasureResult80 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTable80)[3])

data100 <- knn(train = train, test = test, cl = labelTrain, k = 3)
valuesCrossTable100 <- getValuesCrossTable(labelTest, data100)
fMeasureResult100 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTable100)[3])

allFMeasures <- c(fMeasureResult20, fMeasureResult40, fMeasureResult60, fMeasureResult80, fMeasureResult100)
#barplot(allFMeasures, names.arg=c("20", "40", "60", "80", "100"), main="F-measure KNN", xlab="Porcentagem do conjunto de teste", ylab ="F-measure")

return(allFMeasures)
}
#fKnn(stringQueryFUNCTIONS_derby, stringQueryCountFUNCTIONS_derby)

