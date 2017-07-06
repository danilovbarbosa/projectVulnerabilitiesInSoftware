library(C50)

setwd("/home/danilo/R/workspace/projetoVunerabilidades/")

if(!exists("foo", mode="function")) source("carregarDados.R")
if(!exists("foo", mode="function")) source("avaliarDados.R")

stringQueryCountFUNCTIONS_derby <- "SELECT COUNT(*) FROM software.FUNCTIONS_derby;"
stringQueryFUNCTIONS_derby <- "SELECT * FROM software.FUNCTIONS_derby;"

fC50 <- function(stringQueryFUNCTIONS, stringQueryCountFUNCTIONS){
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


classifier20 <-  C5.0(train20, labelTrain20)
predict20 <- predict(classifier20, test20)
valuesCrossTableBayes20 <- getValuesCrossTable(labelTest20, predict20)
fMeasureResult20 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTableBayes20)[3])

classifier40 <-  C5.0(train40, labelTrain40)
predict40 <- predict(classifier40, test40)
valuesCrossTableBayes40 <- getValuesCrossTable(labelTest40, predict40)
fMeasureResult40 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTableBayes40)[3])

classifier60 <-  C5.0(train60, labelTrain60)
predict60 <- predict(classifier60, test60)
valuesCrossTableBayes60 <- getValuesCrossTable(labelTest60, predict60)
fMeasureResult60 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTableBayes60)[3])

classifier80 <-  C5.0(train80, labelTrain80)
predict80 <- predict(classifier80, test80)
valuesCrossTableBayes80 <- getValuesCrossTable(labelTest80, predict80)
fMeasureResult80 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTableBayes80)[3])

classifier100 <-  C5.0(train, labelTrain)
predict100 <- predict(classifier100, test)
valuesCrossTableBayes100 <- getValuesCrossTable(labelTest, predict100)
fMeasureResult100 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTableBayes100)[3])

allFMeasures <- c(fMeasureResult20, fMeasureResult40, fMeasureResult60, fMeasureResult80, fMeasureResult100)
#a <- barplot(allFMeasures, names.arg=c("20", "40", "60", "80", "100"), main="F-measure C5.0", xlab="Porcentagem do conjunto de teste", ylab ="F-measure")

return(allFMeasures)
}

#fC50(stringQueryFUNCTIONS_derby, stringQueryCountFUNCTIONS_derby)
