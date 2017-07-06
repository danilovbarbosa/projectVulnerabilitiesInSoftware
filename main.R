setwd("/home/danilo/R/workspace/projetoVunerabilidades/")

if(!exists("foo", mode="function")) source("knn.R")
if(!exists("foo", mode="function")) source("bayes.R")
if(!exists("foo", mode="function")) source("randomForest.R")
if(!exists("foo", mode="function")) source("c50.R")
if(!exists("foo", mode="function")) source("svm.R")

querys <- read.csv("querys.csv", stringsAsFactors = FALSE)

resultTableCSV = list()

for (numerolinha in 7:7){
  #print(paste(querys$query[numerolinha], querys$countQuery[numerolinha]))
  resultKnn <- fKnn(querys$query[numerolinha], querys$countQuery[numerolinha])
  resultNaiveBayes <- fNaiveBayes(querys$query[numerolinha], querys$countQuery[numerolinha])
  resultRandomForest <- fRandomForest(querys$query[numerolinha], querys$countQuery[numerolinha])
  resultC50 <- fC50(querys$query[numerolinha], querys$countQuery[numerolinha])
  resultSvm <- fSvm(querys$query[numerolinha], querys$countQuery[numerolinha])
  
  lineCSV = list(resultKnn, resultNaiveBayes, resultRandomForest, resultC50, resultSvm, querys$query[numerolinha])
  print(lineCSV)
}

#write.table(linhaCSV, file = "resultados.csv",row.names=FALSE, na="", sep=",")

#print(fKnn(querys$query[1], querys$countQuery[1]))

