#' ---	
#' title: "HW7_B66084"	
#' author: "Luis Gerson Noboa Martillo"	
#' date: "7 de abril de 2017"	
#' output: pdf_document	
#' ---	
#' 	
#' 	
knitr::opts_chunk$set(echo = TRUE)	
library(ggplot2)	
library(pROC)	
#' 	
#' 	
#' #Exercise 1	
#' 	
#' ## Functions for calculating scores	
#' 	
#' 	
precision <- function(table){	
  a = table[1,1]	
  c = table[2,1]	
  	
  ret = a / (a * (a+c))	
  return(ret)	
}	
	
recall <- function(table){	
  a = table[1,1]	
  b = table[1,2]	
  	
  ret = a/(a+b)	
  return(ret)	
}	
	
accuracy <- function(table){	
  a = table[1,1]	
  b = table[1,2]	
  c= table[2,1]	
  d = table[2,2]	
  	
  ret = (a+d) / (a+b+c+d)	
  return(ret)	
}	
	
f1Score <- function(table){	
  p = precision(table)	
  r = recall(table)	
  	
  upper = 2 * (p * r)	
  lower = p + r	
  	
  return(upper/lower)	
}	
#' 	
#' 	
tableA <- matrix(c(225, 175, 100, 100), ncol=2)	
colnames(tableA) <- c("Predicted 1", "Predicted 0")	
rownames(tableA) <- c("Actual 1", "Actual 0")	
print("Table A")	
tableA	
	
paste("Precision", precision(tableA))	
paste("Recall", recall(tableA))	
paste("Accuracy", accuracy(tableA))	
paste("F1-Score", f1Score(tableA))	
#' 	
#' 	
#' 	
tableB <- matrix(c(180, 75, 145, 200), ncol=2)	
colnames(tableB) <- c("Predicted 1", "Predicted 0")	
rownames(tableB) <- c("Actual 1", "Actual 0")	
print("Table B")	
tableB	
	
paste("Precision", precision(tableB))	
paste("Recall", recall(tableB))	
paste("Accuracy", accuracy(tableB))	
paste("F1-Score", f1Score(tableB))	
#' 	
#' 	
#' 	
tableC <- matrix(c(100, 200, 50, 650), ncol=2)	
colnames(tableC) <- c("Predicted 1", "Predicted 0")	
rownames(tableC) <- c("Actual 1", "Actual 0")	
print("Table C")	
tableC	
	
paste("Precision", precision(tableC))	
paste("Recall", recall(tableC))	
paste("Accuracy", accuracy(tableC))	
paste("F1-Score", f1Score(tableC))	
#' 	
#' 	
#' 	
tableD <- matrix(c(120, 350, 30, 500), ncol=2)	
colnames(tableD) <- c("Predicted 1", "Predicted 0")	
rownames(tableD) <- c("Actual 1", "Actual 0")	
print("Table D")	
tableD	
	
paste("Precision", precision(tableD))	
paste("Recall", recall(tableD))	
paste("Accuracy", accuracy(tableD))	
paste("F1-Score", f1Score(tableD))	
#' 	
#' 	
#' Precision is the likelihood that a specific item from the set is correct. Here we can see that B has the highest value, followed by C, A and D. This means that B is the one that is more likely to be correct among the four classifiers. A value extracted from B has more likelihood of having being correctly identified than from the other samples.	
#' 	
#' Recall is the measurement that given a true example, how likely is it for the prediction to detect it as true and not as false. For this, the best score is by D, then A, then C and then B. This means that given a true example in D, the likelihood of the prediction to get it right is bigger than for the rest of the classifiers.	
#' 	
#' For accuracy, the percentage points out the amount of data predicted correctly. For this measurement, C gets the highest score, with 75% of the data predicted correctly. Then B, D and A follow.	
#' 	
#' F-score combines precision and recall into one single metric. With this, we can determine how recall and precision interact with each other and reach a middle point between both. This displays similar values to precision, in which the other is B, C, A and D. Thus, B reaches a better compromise between precision and recall.	
#' 	
#' With all of this analysis, I think that the most relevant is accuracy, so I would determine that C is the best model of them all, followed by B, D and A. Even though C is not the first in the other scores, it is never at the last spot, and it isn't that far off from the top performers. B does a good job in precision and f-score, while D does a good job in recall. A performs good on none of them, probably explaining its low score on accuracy.	
#' 	
#' #Exercise 2	
#' 	
#' 	
tpr = function(table){	
  return(recall(table))	
}	
	
fpr = function(table){	
  c = table[2,1]	
  d = table[2,2]	
  	
  return(c/(c+d))	
}	
	
#' 	
#' 	
info = data.frame(name=c("A","B","C","D"), tpr=c(tpr(tableA), tpr(tableB), tpr(tableC), tpr(tableD)), fpr=c(fpr(tableA), fpr(tableB), fpr(tableC), fpr(tableD)))	
	
info	
#' 	
#' 	
#' 	
ggplot(info, aes(x=fpr, y=tpr, color=name)) +	
  geom_point() +	
  xlim(0,1) +	
  ylim(0,1)	
	
#' 	
#' 	
#' ## Literal A	
#' From this information, the classifier C is the best positioned in the ROC space. It has a high TPR and the lowerst FPR of them all. A is the worst, with almost the same TPR as C but with much higher FPR. One could argue that D might be as good as C, but that's what the measurements in the first exercise are for, and there it was determined that C was better than D. This goes in line with what was found in the first exercise, primarily to the accuracy rating, which gave is the same results. Also we see that A performs the worst in this plot, in line with what we determined from the first exercise. It seems like accuracy has a big role in the position in the ROC curve.	
#' 	
#' ## Literal B	
#' 	
#' We can see that the values with the best position in the ROC space are the ones that feature the highest numbers for correct predicted values (that means, both TP and TN). Even if the number of true negatives is higher in both cases, it still means that the predictions are more accurate, and thus, perform better in the ROC space. C is the best of them because it has the least amount of falsely predicted values (FP and FN) compared to its values predicted correctly. 	
#' 	
#' When results are more equally distributed, then the position in the ROC space suffers, and thus this lets us conclude that the predictions are wrong and the model is not good to work with. Even if models A and B have actually less falsely predicted values, they also have less correctly predicted values so that's why their position is not good.	
#' 	
#' In conclusion, as the correctly predicted values tend to increase (TP and TN) and be better higher in numbers than its incorrectly predicted values (FP and FN), the position in the ROC curve tends to be better. Likewise, when incorrectly predicted values are predominant, position in the ROC curve suffers.	
#' 	
#' #Exercise 3	
#' 	
#' 	
rocExample = read.csv("ROC_example_new.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")	
#' 	
#' 	
rocExample$ROC1order = as.numeric(scan(text=as.character(rocExample$ROC1order), dec=",", sep="."))	
rocExample$ROC2order = as.numeric(scan(text=as.character(rocExample$ROC2order), dec=",", sep="."))	
rocExample$ROC3order = as.numeric(scan(text=as.character(rocExample$ROC3order), dec=",", sep="."))	
#' 	
#' 	
#' 	
rocExample	
#' 	
#' 	
#' Here the data is extracted. The commas were replaced by periods because if not, my R Studio would determine that they were factors and not numbers, thus bringing issues to the ordering of the data	
#' 	
#' ##Calculate ROC Curve Function	
#' 	
#' 	
getROCValues <- function(data){	
  positives = nrow(subset(data, isTrue == TRUE))	
  negatives = nrow(data) - positives	
  	
  positiveFraction = 1 / positives	
  negativeFraction = 1 / negatives	
  	
  	
  for(index in 1:nrow(data)){	
    if (index == 1){	
      if (data[index,2] == TRUE) data[index, 4] = positiveFraction else data[index, 5] = negativeFraction	
      	
    }else{	
      if (data[index,2] == TRUE){	
        data[index, 4] = data[index - 1, 4] + positiveFraction	
        data[index, 5] = data[index - 1, 5]	
      }  else{	
        data[index, 4] = data[index - 1, 4]	
        data[index, 5] = data[index - 1, 5] + negativeFraction	
      } 	
    }	
  }	
  	
  return(data)	
  	
}	
#' 	
#' 	
#' For calculating the values, the ordered data is received as a parameter and then the fraction that each axis increases with teach TP/FP is determined. Then the FPR and TPR are calculated and stored in the initial data frame.	
#' ##ROC Curves	
#' 	
roc1 = data.frame(ID=rocExample$ID, isTrue=rocExample$TRUE., order=rocExample$ROC1order, tpr=rep(0,nrow(rocExample)), fpr=rep(0,nrow(rocExample)))	
roc1 = roc1[order(roc1$order),]	
roc1 = getROCValues(roc1)	
	
roc2 = data.frame(ID=rocExample$ID, isTrue=rocExample$TRUE., order=rocExample$ROC2order, tpr=rep(0,nrow(rocExample)), fpr=rep(0,nrow(rocExample)))	
roc2 = roc2[order(roc2$order),]	
roc2 = getROCValues(roc2)	
	
roc3 = data.frame(ID=rocExample$ID, isTrue=rocExample$TRUE., order=rocExample$ROC3order, tpr=rep(0,nrow(rocExample)), fpr=rep(0,nrow(rocExample)))	
roc3 = roc3[order(roc3$order),]	
roc3 = getROCValues(roc3)	
	
#' 	
#' 	
#' 	
ggplot() +	
  geom_line(data=roc1, aes(x=fpr, y=tpr, color="Value 1")) +	
  geom_line(data=roc2, aes(x=fpr, y=tpr, color="Value 2")) +	
  geom_line(data=roc3, aes(x=fpr, y=tpr, color="Value 3")) +	
  xlim(0,1.01) +	
  ylim(0,1.01)	
  	
	
#' 	
#' 	
#' We can see that for all three examples, it is pretty easy to determine with a simple inspection which is the one that has the highest ROC (value 1, red line). Since the red one gets closer to the corner, this means that it has a higher AUC. However, if especially the first and third value were more similar, it could turn very difficult to determine without calculating the AUC.	
#' 	
#' ## Area under the curve	
#' 	
#' 	
	
library(pROC)	
	
paste("AUC Value 1:", auc(as.numeric(roc1$isTrue), roc1$order))	
paste("AUC Value 2:", auc(as.numeric(roc2$isTrue), roc2$order))	
paste("AUC Value 3:", auc(as.numeric(roc3$isTrue), roc3$order))	
#' 	
#' 	
#' The ideal value for AUC is 1, since this means that the curve is a perfect corner. Where we can see that the first value has the highest score for AUC. This means that the first curve has a higher positive rank. This means that the mean of all the positive ranks is higher than for the second and third values. A rank is defined as the amount of negative values to the right of the chosen value in an ordered set. So, as the AUC gets bigger, this means that the data gets closer to the ideal value of a ROC curve, so the predicted that is more accurate. The predicted data for the first value (called ROC1order in the data frame) is the one who gets closer to the ideal 1.	
#' 	
#' #Exercise 4	
#' 	
#' 	
getCost <- function(data, fpv, fnv){	
  positives = nrow(subset(data, isTrue == TRUE))	
  negatives = nrow(data) - positives	
  	
  positiveFraction = 1 / positives	
  negativeFraction = 1 / negatives	
  	
  dataWithROC = getROCValues(data)	
	
  	
  for(index in 1:nrow(dataWithROC)){	
    	
    shouldBePositive = dataWithROC[1:index,]	
    countFn = 0	
    for(posIndex in 1:nrow(shouldBePositive)){	
      if (shouldBePositive[posIndex, 2] != TRUE){	
        countFn = countFn + 1	
      }	
    }	
    	
    	
    lowerBound = min(nrow(dataWithROC), index + 1)	
    shouldBeNegative = dataWithROC[lowerBound:nrow(dataWithROC),]	
    countFp = 0	
    for(negIndex in 1:nrow(shouldBeNegative)){	
      if (shouldBeNegative[negIndex, 2] != FALSE){	
        countFp = countFp + 1	
      }	
    }	
    	
    dataWithROC$sum[index] = (countFn * fnv) + (countFp * fpv)	
    	
  }	
  	
  return(dataWithROC)	
  	
}	
#' 	
#' 	
#' To get the cost, the data frame is being traversed through a for loop, and for each index, a cutoff is generated. Values are divided into two frames, and from the indext upwards, positive values are expected. From the index downwards, negative values are expected. With this, the false positives and false negatives are counted and then multiplied against the cost.	
#' 	
#' ## Cost for Value 1	
#' 	
roc1Cost = data.frame(ID=rocExample$ID, isTrue=rocExample$TRUE., order=rocExample$ROC1order, tpr=rep(0,nrow(rocExample)), fpr=rep(0,nrow(rocExample)))	
roc1Cost = roc1Cost[order(roc1Cost$order),]	
r1_10_100 = getCost(roc1Cost, 10, 100)	
r1_100_10 = getCost(roc1Cost, 100, 10)	
r1_40_60 = getCost(roc1Cost, 40, 60)	
r1_60_40 = getCost(roc1Cost, 60, 40)	
	
optimalr1c1 = r1_10_100[which(r1_10_100$sum == min(r1_10_100$sum)), ]	
optimalr1c2 = r1_10_100[which(r1_100_10$sum == min(r1_100_10$sum)), ]	
optimalr1c3 = r1_10_100[which(r1_40_60$sum == min(r1_40_60$sum)), ]	
optimalr1c4 = r1_10_100[which(r1_60_40$sum == min(r1_60_40$sum)), ]	
	
paste("Optimal for 10 FP, 100 FN -> TPR:", optimalr1c1$tpr,", FPR:", optimalr1c1$fpr, ", SUM:", optimalr1c1$sum)	
paste("Optimal for 100 FP, 10 FN -> TPR:", optimalr1c2$tpr,", FPR:", optimalr1c2$fpr, ", SUM:", optimalr1c2$sum)	
paste("Optimal for 40 FP, 60 FN -> TPR:", optimalr1c3$tpr,", FPR:", optimalr1c3$fpr, ", SUM:", optimalr1c3$sum)	
paste("Optimal for 60 FP, 40 FN -> TPR:", optimalr1c4$tpr,", FPR:", optimalr1c3$fpr, ", SUM:", optimalr1c4$sum)	
	
ggplot() +	
  geom_line(data=roc1, aes(x=fpr, y=tpr, color="Value 1")) +	
  geom_point(data = optimalr1c1, aes(x=fpr, y= tpr, color="10  FP, 100 FN")) +	
  geom_point(data = optimalr1c2, aes(x=fpr, y= tpr, color="100  FP, 10 FN")) +	
  geom_point(data = optimalr1c3, aes(x=fpr, y= tpr, color="40  FP, 60 FN")) +	
  geom_point(data = optimalr1c4, aes(x=fpr, y= tpr, color="60  FP, 40 FN")) +	
  xlim(0,1.01) +	
  ylim(0,1.01)	
	
#' 	
#' 	
#' ## Cost for Value 2	
#' 	
#' 	
roc2Cost = data.frame(ID=rocExample$ID, isTrue=rocExample$TRUE., order=rocExample$ROC2order, tpr=rep(0,nrow(rocExample)), fpr=rep(0,nrow(rocExample)))	
roc2Cost = roc2Cost[order(roc2Cost$order),]	
r2_10_100 = getCost(roc2Cost, 10, 100)	
r2_100_10 = getCost(roc2Cost, 100, 10)	
r2_40_60 = getCost(roc2Cost, 40, 60)	
r2_60_40 = getCost(roc2Cost, 60, 40)	
	
optimalr2c1 = r2_10_100[which(r2_10_100$sum == min(r2_10_100$sum)), ]	
optimalr2c2 = r2_10_100[which(r2_100_10$sum == min(r2_100_10$sum)), ]	
optimalr2c3 = r2_10_100[which(r2_40_60$sum == min(r2_40_60$sum)), ]	
optimalr2c4 = r2_10_100[which(r2_60_40$sum == min(r2_60_40$sum)), ]	
	
paste("Optimal for 10 FP, 100 FN -> TPR:", optimalr2c1$tpr,", FPR:", optimalr2c1$fpr, ", SUM:", optimalr2c1$sum)	
paste("Optimal for 100 FP, 10 FN -> TPR:", optimalr2c2$tpr,", FPR:", optimalr2c2$fpr, ", SUM:", optimalr2c2$sum)	
paste("Optimal for 40 FP, 60 FN -> TPR:", optimalr2c3$tpr,", FPR:", optimalr2c3$fpr, ", SUM:", optimalr2c3$sum)	
paste("Optimal for 60 FP, 40 FN -> TPR:", optimalr2c4$tpr,", FPR:", optimalr2c3$fpr, ", SUM:", optimalr2c4$sum)	
	
ggplot() +	
  geom_line(data=roc2, aes(x=fpr, y=tpr, color="Value 2")) +	
  geom_point(data = optimalr2c1, aes(x=fpr, y= tpr, color="10  FP, 100 FN")) +	
  geom_point(data = optimalr2c2, aes(x=fpr, y= tpr, color="100  FP, 10 FN")) +	
  geom_point(data = optimalr2c3, aes(x=fpr, y= tpr, color="40  FP, 60 FN")) +	
  geom_point(data = optimalr2c4, aes(x=fpr, y= tpr, color="60  FP, 40 FN")) +	
  xlim(0,1.01) +	
  ylim(0,1.01)	
#' 	
#' 	
#' ## Cost for Value 3	
#' 	
roc3Cost = data.frame(ID=rocExample$ID, isTrue=rocExample$TRUE., order=rocExample$ROC3order, tpr=rep(0,nrow(rocExample)), fpr=rep(0,nrow(rocExample)))	
roc3Cost = roc3Cost[order(roc3Cost$order),]	
r3_10_100 = getCost(roc3Cost, 10, 100)	
r3_100_10 = getCost(roc3Cost, 100, 10)	
r3_40_60 = getCost(roc3Cost, 40, 60)	
r3_60_40 = getCost(roc3Cost, 60, 40)	
	
optimalr3c1 = r3_10_100[which(r3_10_100$sum == min(r3_10_100$sum)), ]	
optimalr3c2 = r3_10_100[which(r3_100_10$sum == min(r3_100_10$sum)), ]	
optimalr3c3 = r3_10_100[which(r3_40_60$sum == min(r3_40_60$sum)), ]	
optimalr3c4 = r3_10_100[which(r3_60_40$sum == min(r3_60_40$sum)), ]	
	
paste("Optimal for 10 FP, 100 FN -> TPR:", optimalr3c1$tpr,", FPR:", optimalr3c1$fpr, ", SUM:", optimalr3c1$sum)	
paste("Optimal for 100 FP, 10 FN -> TPR:", optimalr3c2$tpr,", FPR:", optimalr3c2$fpr, ", SUM:", optimalr3c2$sum)	
paste("Optimal for 40 FP, 60 FN -> TPR:", optimalr3c3$tpr,", FPR:", optimalr3c3$fpr, ", SUM:", optimalr3c3$sum)	
paste("Optimal for 60 FP, 40 FN -> TPR:", optimalr3c4$tpr,", FPR:", optimalr3c3$fpr, ", SUM:", optimalr3c4$sum)	
	
ggplot() +	
  geom_line(data=roc3, aes(x=fpr, y=tpr, color="Value 3")) +	
  geom_point(data = optimalr3c1, aes(x=fpr, y= tpr, color="10  FP, 100 FN")) +	
  geom_point(data = optimalr3c2, aes(x=fpr, y= tpr, color="100  FP, 10 FN")) +	
  geom_point(data = optimalr3c3, aes(x=fpr, y= tpr, color="40  FP, 60 FN")) +	
  geom_point(data = optimalr3c4, aes(x=fpr, y= tpr, color="60  FP, 40 FN")) +	
  xlim(0,1.01) +	
  ylim(0,1.01)	
#' 	
#' 	
#' ##Explanation	
#' 	
#' We can see that for all values, there are some tendencies emerging. When the false positives are given a higher cost, the ideal cutoff tends to appear in the upper part of the graph. This is probably because, when we are in the higher part of the graph, there's a big tendency of having a lot of false negatives instead of having false positives, so the FN value is lower, thus the cost is lower. Likewise, we can see that for the values that feature a high value for false negative, they tend to be on the lower part of the ROC curve, for the same reason as explained abouve but in this case, false negatives are more expensive, thus, it's better to have less false negatives and more false positives.	
#' 	
#' #Exercise 5	
#' 	
#' ##Ensemble 1: Sum of three scores	
#' 	
#' 	
roc4 = data.frame(ID=rocExample$ID, isTrue=rocExample$TRUE., order= (rocExample$ROC1order + rocExample$ROC2order + rocExample$ROC3order), tpr=rep(0,nrow(rocExample)), fpr=rep(0,nrow(rocExample)))	
roc4 = roc4[order(roc4$order),]	
roc4 = getROCValues(roc4)	
#' 	
#' 	
#' ##Ensemble 2: Convert scores to ranks	
#' 	
#' 	
rocTemp = rocExample	
	
rocTemp = rocTemp[order(rocTemp$ROC1order),]	
rocTemp$rank1 = 1:(nrow(rocTemp))	
rocTemp = rocTemp[order(rocTemp$ROC2order),]	
rocTemp$rank2 = 1:(nrow(rocTemp))	
rocTemp = rocTemp[order(rocTemp$ROC3order),]	
rocTemp$rank3 = 1:(nrow(rocTemp))	
	
roc5 = data.frame(ID=rocTemp$ID, isTrue=rocTemp$TRUE., order= (rocTemp$rank1 + rocTemp$rank2 + rocTemp$rank3), tpr=rep(0,nrow(rocTemp)), fpr=rep(0,nrow(rocTemp)))	
roc5 = roc5[order(roc5$order),]	
roc5 = getROCValues(roc5)	
#' 	
#' 	
#' ##Graph and AUC values	
#' 	
#' 	
ggplot() +	
  geom_line(data=roc1, aes(x=fpr, y=tpr, color="Value 1")) +	
  geom_line(data=roc2, aes(x=fpr, y=tpr, color="Value 2")) +	
  geom_line(data=roc3, aes(x=fpr, y=tpr, color="Value 3")) +	
  geom_line(data=roc4, aes(x=fpr, y=tpr, color="Ensemble 1")) +	
  geom_line(data=roc5, aes(x=fpr, y=tpr, color="Ensemble 2")) +	
  xlim(0,1.01) +	
  ylim(0,1.01)	
#' 	
#' 	
#' 	
paste("AUC Value 1:", auc(as.numeric(roc1$isTrue), roc1$order))	
paste("AUC Value 2:", auc(as.numeric(roc2$isTrue), roc2$order))	
paste("AUC Value 3:", auc(as.numeric(roc3$isTrue), roc3$order))	
paste("AUC Ensemble 1:", auc(as.numeric(roc4$isTrue), roc4$order))	
paste("AUC Ensemble 2:", auc(as.numeric(roc5$isTrue), roc5$order))	
#' 	
#' 	
#' The ranking of the three scores and subsequent addition outputs the best graph, since it has a very high AUC, as can be seen from simple inspection and with actual AUC values (Emsemble 2). We can even see that the Ensemble 2 has an almost perfect beginning of the curve, with values very close to the X axis and almost no deviation from the Y axis, snething that is not seen in any of the other four curves. Also, emsemble 1 performs almost as poorly as Value 2, so it doesn't make sense to use this emsemble if what we're looking is a better AUC.	
