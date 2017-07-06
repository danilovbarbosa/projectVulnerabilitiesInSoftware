library(RMySQL)
library(caTools)

#https://eden.dei.uc.pt/nmsa/metrics-dataset

splitDatesInTrainAndTest <- function(data, rate){
  #Separando base de trainamento e teste
  indexData <- sample.split(data,SplitRatio=rate)
  trainSet <- data[indexData==TRUE,]
  testSet <- data[indexData==FALSE,]
  
  return(list(trainSet, testSet))
}

limparConsultaBD <- function(rs){
  dbHasCompleted(rs)
  dbClearResult(rs)
}

fecharConnBD <- function(con){
  dbDisconnect(con)
  on.exit(dbDisconnect(con))
}

consultaBD <- function(con, stringQuery, numeroDeLinhasNoResultado){
  resultadoDaConsulta <- dbSendQuery(con, stringQuery)
  data <- fetch(resultadoDaConsulta, n=numeroDeLinhasNoResultado)
  limparConsultaBD(resultadoDaConsulta)
  
  return(data)
}

balancedDatas <- function(amostra1, amostra2, tamanhoAmostra, porcentagemParaSlitTrain){
  #Balanceamento, igualando os valores entre os afetados e uma amostra dos não afetados
  #sampleBalancedNoAffected <- noAffected[sample(nrow(noAffected),size = 1461),]
  sampleBalancedNoAffected <- amostra1[sample(nrow(amostra1),size = tamanhoAmostra),]
  dim(sampleBalancedNoAffected)
  #Igualando o nome das colunas e unindo os data.frames
  colnames(sampleBalancedNoAffected) <- colnames(amostra2)
  dataBalanced <- rbind(amostra2, sampleBalancedNoAffected)
  #Dividindo em treimaneto e teste
  dataBalanced$X1.Affected <- factor(dataBalanced$X1.Affected)
  #dataSplitTrainAndTest <- splitDatesInTrainAndTest(dataBalanced, .7)
  dataSplitTrainAndTest <- splitDatesInTrainAndTest(dataBalanced, porcentagemParaSlitTrain)
  
  return(dataSplitTrainAndTest)
}

getTrainTestAndLabels <- function(stringQuery, stringQueryCount){
  con <- dbConnect(MySQL(),
                   user="root", password="12345",
                   dbname="software", host="localhost")
  
  numeroLinhas <- as.numeric(consultaBD(con, stringQueryCount, 1))
  numeroLinhas
  
  data <- consultaBD(con, stringQuery, numeroLinhas)
  summary(data)
  dim(data)
  data <- data[-1:-11]
  dim(data)
  
  #Split nos dados
  dataSplitPorFactor <- split(data, data$Affected)
  noAffected <- as.data.frame(dataSplitPorFactor[1])
  dim(noAffected)
  affected <- as.data.frame(dataSplitPorFactor[2])
  dim(affected)
  
  
  dataSplitTrainAndTest <- balancedDatas(noAffected, affected, dim(affected)[1],.7)
  train <- as.data.frame(dataSplitTrainAndTest[1])
  test <- as.data.frame(dataSplitTrainAndTest[2])
  labelTrain <- train[,1]
  labelTest <- test[,1]
  
  #fecharConnBD(con)
  return(list(train, test, labelTrain, labelTest))
}

#stringQueryCountPATCHES <- "SELECT COUNT(*) FROM software.PATCHES;"
#stringQueryPATCHES <- "SELECT * FROM software.PATCHES;"

#stringQueryCountFUNCTIONS_derby <- "SELECT COUNT(*) FROM software.FUNCTIONS_derby;"
#stringQueryFUNCTIONS_derby <- "SELECT * FROM software.FUNCTIONS_derby;"

#test <- getTrainTestAndLabels(stringQueryFUNCTIONS_derby, stringQueryCountFUNCTIONS_derby)

