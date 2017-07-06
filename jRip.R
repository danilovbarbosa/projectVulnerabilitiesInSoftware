##Dando erro

#sudo R CMD javareconf
java_h <- Sys.getenv("JAVA_HOME")
Sys.setenv(JAVA_HOME=java_h)

install.packages("RWeka")
library(RWeka)

setwd("/home/danilo/R/workspace/projetoVunerabilidades/")

if(!exists("foo", mode="function")) source("carregarDados.R")
if(!exists("foo", mode="function")) source("avaliarDados.R")

stringQueryCountFUNCTIONS_derby <- "SELECT COUNT(*) FROM software.FUNCTIONS_derby;"
stringQueryFUNCTIONS_derby <- "SELECT * FROM software.FUNCTIONS_derby;"

listTrainTestAndLabels <- getTrainTestAndLabels(stringQueryFUNCTIONS_derby, stringQueryCountFUNCTIONS_derby)

train <- as.data.frame(listTrainTestAndLabels[1])
test <- as.data.frame(listTrainTestAndLabels[2])
labelTrain <- as.data.frame(listTrainTestAndLabels[3])[,1]
labelTest <- as.data.frame(listTrainTestAndLabels[4])[,1]

######
test20 <- as.data.frame(splitDatesInTrainAndTest(test, .2)[1])
test40 <- as.data.frame(splitDatesInTrainAndTest(test, .4)[1])
test60 <- as.data.frame(splitDatesInTrainAndTest(test, .6)[1])
test80 <- as.data.frame(splitDatesInTrainAndTest(test, .8)[1])

labelTest20 <- sample(labelTest, size = dim(test20)[1])

labelTest40 <- sample(labelTest, size = dim(test40)[1])
labelTest60 <- sample(labelTest, size = dim(test60)[1])
labelTest80 <- sample(labelTest, size = dim(test80)[1])
#####

classifier100 <- naiveBayes(train, labelTrain)


predict20 <- predict(classifier100, test20)
valuesCrossTableBayes20 <- getValuesCrossTable(labelTest20, predict20)
fMeasureResult20 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTableBayes20)[3])

predict40 <- predict(classifier100, test40)
valuesCrossTableBayes40 <- getValuesCrossTable(labelTest40, predict40)
fMeasureResult40 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTableBayes40)[3])

predict60 <- predict(classifier100, test60)
valuesCrossTableBayes60 <- getValuesCrossTable(labelTest60, predict60)
fMeasureResult60 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTableBayes60)[3])

predict80 <- predict(classifier100, test80)
valuesCrossTableBayes80 <- getValuesCrossTable(labelTest80, predict80)
fMeasureResult80 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTableBayes80)[3])

predict100 <- predict(classifier100, test)
valuesCrossTableBayes100 <- getValuesCrossTable(labelTest, predict100)
fMeasureResult100 <- as.numeric(getPrecisonRecallAndFMeasure(valuesCrossTableBayes100)[3])

allFMeasures <- c(fMeasureResult20, fMeasureResult40, fMeasureResult60, fMeasureResult80, fMeasureResult100)
a <- barplot(allFMeasures, names.arg=c("20", "40", "60", "80", "100"), main="F-measure Naive Bayes", xlab="Porcentagem do conjunto de teste", ylab ="F-measure")
