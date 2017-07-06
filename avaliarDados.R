library(gmodels)

accuracy <- function(tp, tn, fp, fn){ return((tp + tn)/(tp + tn + fp + fn)) }

errorRate <- function(tp, tn, fp, fn){ return(1 - accuracy(tp, tn, fp, fn)) } 

precision <- function(tp, fp){ return(tp/(tp + fp)) }

recall <- function(tp, fn){ return(tp/(tp + fn)) }

fMeasure <- function(tp, fp, fn){ return((2 * tp) / (2 * tp + fp + fn))}

getValuesCrossTable <- function(labelTest, dataClassified){
  crossTableKnn <- CrossTable(x = labelTest, y = dataClassified, prop.chisq=FALSE)[1]
  tp <- as.data.frame(crossTableKnn)[4,3]
  tn <- as.data.frame(crossTableKnn)[1,3]
  fp <- as.data.frame(crossTableKnn)[3,3]
  fn <- as.data.frame(crossTableKnn)[2,3]
  
  return(list(tp, tn, fp, fn))
}

getPrecisonRecallAndFMeasure <- function(valuesCrossTable){
  tp <- as.numeric(valuesCrossTable[1])
  tn <- as.numeric(valuesCrossTable[2])
  fp <- as.numeric(valuesCrossTable[3])
  fn <- as.numeric(valuesCrossTable[4])
  
  p <- precision(tp, fp)
  r <- recall(tp, fn)
  f <- fMeasure(tp, fp, fn)
  
  return(list(p, r, f))
}