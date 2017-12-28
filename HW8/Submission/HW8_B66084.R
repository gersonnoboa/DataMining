#' ---	
#' title: "HW8_B66084"	
#' author: "Luis Gerson Noboa Martillo"	
#' date: "16 de abril de 2017"	
#' output: html_document	
#' ---	
#' 	
#' 	
knitr::opts_chunk$set(echo = TRUE)	
library(ggplot2)	
library(plyr)	
#' 	
#' 	
#' # Exercise 1	
#' 	
#' 	
outlook = c("Sunny", "Sunny", "Overcast", "Rainy", "Rainy", "Rainy", "Overcast", "Sunny", "Sunny", "Rainy", "Sunny", "Overcast", "Overcast", "Rainy")	
temp = c("Hot", "Hot", "Hot", "Mild", "Cool", "Cool", "Cool", "Mild", "Cool", "Mild", "Mild", "Mild", "Hot", "Mild")	
humidity = c("High", "High", "High", "High", "Normal", "Normal", "Normal", "High", "Normal", "Normal", "Normal", "High", "Normal", "High")	
windy = c("False", "True", "False", "False", "False", "True", "True", "False", "False", "False", "True", "True", "False", "True")	
play = c("No", "No", "Yes", "Yes", "Yes", "No", "Yes", "No", "Yes", "Yes", "Yes", "Yes", "Yes", "No")	
	
info = data.frame(outlook, temp, humidity, windy, play)	
info	
#' 	
#' 	
#' 	
#' Formula for calculating entropy	
#' 	
#' 	
	
entropySingle <- function(data, column){	
  frequencies = count(data, column)	
  totalCount = sum(frequencies$freq)	
  entropy = 0	
  	
  for (i in 1:nrow(frequencies)){	
    row = frequencies[i,]	
    probability = row$freq / totalCount	
    entropy = ((- probability) * log2(probability)) + entropy	
  }	
  return(entropy)	
}	
	
entropyNumbers <- function(n1, n2){	
  total = n1 + n2	
  prob1 = n1/total	
  prob2 = n2/total	
  entropy = ((- prob1) * log2(prob1)) + ((- prob2) * log2(prob2))	
  	
  if (is.nan(entropy)){	
    entropy = 0	
  }	
  return(entropy)	
}	
	
entropyColumns <- function(data){	
  entropy = 0	
  total = sum(data)	
  for (i in 1:nrow(data)){	
    partialTotal = sum(data[i,])	
    	
    row = data[i,]	
    n1 = as.numeric(row[1])	
    n2 = as.numeric(row[2])	
    probability = partialTotal / total	
    partEntropy = entropyNumbers(n1, n2)	
    entropy = (probability * partEntropy) + entropy	
  }	
  	
  return(entropy)	
}	
	
#' 	
#' 	
#' ## First level	
#' 	
#' First, the calculation of the play entropy is done.	
#' 	
#' 	
playEntropy = entropySingle(info, "play")	
paste("Play entropy:", playEntropy)	
#' 	
#' 	
#' Then the combination of play with the other columns is performed.	
#' 	
#' ### Outlook	
#' 	
#' 	
outlookPlay = matrix(c(3,0,2,2,4,3), ncol=2)	
colnames(outlookPlay) <- c("No", "Yes")	
rownames(outlookPlay) <- c("Sunny", "Overcast", "Raining")	
outlookPlay	
	
outlookPlayEntropy = entropyColumns(outlookPlay)	
paste("Entropy:", outlookPlayEntropy)	
paste("Gain: ", playEntropy - outlookPlayEntropy)	
	
#' 	
#' 	
#' ### Temperature	
#' 	
#' 	
tempPlay = matrix(c(2,2,1,2,4,3), ncol=2)	
colnames(tempPlay) <- c("No", "Yes")	
rownames(tempPlay) <- c("Hot", "Mild", "Cool")	
tempPlay	
	
tempPlayEntropy = entropyColumns(tempPlay)	
paste("Entropy:", tempPlayEntropy)	
paste("Gain: ", playEntropy - tempPlayEntropy)	
	
#' 	
#' 	
#' ### Humidity	
#' 	
#' 	
humidityPlay = matrix(c(4,1,3,6), ncol=2)	
colnames(humidityPlay) <- c("No", "Yes")	
rownames(humidityPlay) <- c("High", "Normal")	
humidityPlay	
	
humidityPlayEntropy = entropyColumns(humidityPlay)	
paste("Entropy:", humidityPlayEntropy)	
paste("Gain: ", playEntropy - humidityPlayEntropy)	
#' 	
#' 	
#' ### Windy	
#' 	
#' 	
windyPlay = matrix(c(2,3,6,3), ncol=2)	
colnames(windyPlay) <- c("No", "Yes")	
rownames(windyPlay) <- c("High", "Normal")	
windyPlay	
	
windyPlayEntropy = entropyColumns(windyPlay)	
paste("Entropy:", windyPlayEntropy)	
paste("Gain: ", playEntropy - windyPlayEntropy)	
#' 	
#' 	
#' We can see that outlook has the biggest gain, so it should be at the top of the tree.	
#' 	
#' ## Second level	
#' 	
#' 	
knitr::include_graphics("ex1a.jpg")	
#' 	
#' 	
#' As we can see, there's a tendency in the data, in which, in the case of sunny, the output is No every time humidity is High and Yes if humidity is Normal. In the case of overcast, it is Yes regardless of what happens with the other choices. Also, for rainy, Yes is selected if wind is false, and No if wind is true.	
#' 	
#' Therefore, we can make the tree in this way:	
#' 	
#' 	
knitr::include_graphics("ex1b.jpg")	
#' 	
#' 	
#' 	
#' With this arrangement, the example:	
#' 	
#' Sunny	Cool High TRUE	
#' 	
#' Would yield Play: No.	
#' 	
#' # Exercise 2	
#' 	
#' To calculate this, we use the formula in order to get the NB value for Yes when the conditions of the example happen, and then with no. Then it's only a matter of getting the probabilities and calculating the values through simple math.	
#' 	
#' 	
knitr::include_graphics("ex2.jpg")	
#' 	
#' 	
#' 	
#' Because the No value yields a much higher score, then we can conclude that Naive Bayes predicts that the value will be No, and that the probability that it is true is a little more than 94%.	
#' 	
#' # Exercise 3	
#' 	
#' For this exercise, values were transformed to numeric scores. If the value was the same as the example we have to predict, then we changed it to 0. Else, we changed it to one. I tried to change it to one and two, depending on the column (ex: Sunny 0, Overcast 1, Rainy 2), but results got very messy then.	
#' 	
#' 	
knitr::include_graphics("ex3.png")	
#' 	
#' 	
#' # Exercise 4	
#' 	
#' 	
library(rpart)	
library(caret)	
library(e1071)	
library(FSelector)	
	
bank = read.csv("bank-full.csv", header = TRUE, stringsAsFactors = TRUE, sep = ";")	
	
splits = split(bank, sample(1:5, nrow(bank), replace=T))	
split1 = as.data.frame(splits[1])	
split2 = as.data.frame(splits[2])	
split3 = as.data.frame(splits[3])	
split4 = as.data.frame(splits[4])	
split5 = as.data.frame(splits[5])	
	
names(split1) <- c("age", "job", "marital", "education", "default", "balance", "housing", "loan", "contact", "day", "month", "duration", "campaign", "pdays", "previous", "poutcome", "y")	
names(split2) <- names(split1)	
names(split3) <- names(split1)	
names(split4) <- names(split1)	
names(split5) <- names(split1)	
	
treeGeneration <- function(train, test){	
  print("Information gain")	
  print(information.gain(y ~ ., data = train))	
  	
  tree <- rpart(y ~ ., data = train)	
  print("Tree")	
  print(tree)	
  predicted = predict(tree, test, type = "class")	
  	
  print("Confusion Matrix for Tree")	
  print(confusionMatrix(predicted, test$y))	
  	
}	
	
nbGeneration <- function(train, test){	
  model <- naiveBayes(y ~ ., data = train)	
  print("NB")	
  print(model)	
  predictedNB = predict(model, test)	
  print("Confusion Matrix for NB")	
  print(confusionMatrix(predictedNB, test$y))	
}	
#' 	
#' 	
#' ## Split 1	
#' 	
train1 = rbind(split2, split3, split4, split5)	
test1 = split1	
#' 	
#' 	
#' ### Decision Tree	
#' 	
#' 	
treeGeneration(train1, test1)	
#' 	
#' 	
#' ### Naive Bayes	
#' 	
#' 	
nbGeneration(train1, test1)	
#' 	
#' 	
#' 	
#' ## Split 2	
#' 	
train2 = rbind(split1, split3, split4, split5)	
test2 = split2	
#' 	
#' 	
#' ### Decision Tree	
#' 	
#' 	
treeGeneration(train2, test2)	
#' 	
#' 	
#' ### Naive Bayes	
#' 	
#' 	
nbGeneration(train2, test2)	
#' 	
#' 	
#' ## Split 3	
#' 	
train3 = rbind(split1, split2, split4, split5)	
test3 = split3	
#' 	
#' 	
#' ### Decision Tree	
#' 	
#' 	
treeGeneration(train3, test3)	
#' 	
#' 	
#' ### Naive Bayes	
#' 	
#' 	
nbGeneration(train3, test3)	
#' 	
#' 	
#' ## Split 4	
#' 	
train4 = rbind(split1, split2, split3, split5)	
test4 = split4	
#' 	
#' 	
#' ### Decision Tree	
#' 	
#' 	
treeGeneration(train4, test4)	
#' 	
#' 	
#' ### Naive Bayes	
#' 	
#' 	
nbGeneration(train4, test4)	
#' 	
#' 	
#' ## Split 5	
#' 	
train5 = rbind(split1, split2, split3, split4)	
test5 = split5	
#' 	
#' 	
#' ### Decision Tree	
#' 	
#' 	
treeGeneration(train5, test5)	
#' 	
#' 	
#' ### Naive Bayes	
#' 	
#' 	
nbGeneration(train5, test5)	
#' 	
#' 	
#' In the case of decision trees, we can see that they all report good accuracy, with the second one surpassing 90% mark, with 90.11%, and the fifth one reaching 90% exactly. They also have very low P-values. They also seem to converge in the fact that this Y column mostly has No values, with more than 7700 of the values being No, out of the approximately 9000 values each test data has. Based on accuracy and P-value, we can conclude that split 2 is the best out of them all, since it has a very low P-value combined with the highest accuracy out of all the classifiers.	
#' 	
#' On Naive Bayes, we see similar results, but we see values dropping a lot. While in decision trees, we got accuracy values that almost reached 90%, here they didn't reach 88%, with the second one again taking the highest score. We see that P-values have gotten a lot bigger. Frankly, I am not sure of which P-value is the one I should look at but I suppose it is the Mcnemar's one because if it was for the other P-value (the one that says ACC > NIR), all of these data had to be thrown away because it almost reaches one. But using the Mcnemar's P-value we can also see an increase on them, probably indicating that decision trees have done a better job in predicting data than Naive Bayes. We see a decrease in correct responses and an increase in incorrect responses from this model. In this case, it is a little harder to determine which one is the best, since the set 2 has the highest accuracy, but the set 1's P-value is much lower than all of them, with the P-value of set 2 being the highest of all, so it would have to be one of them. Probably set 1 would be a better choice because it displays a strong accuracy and a very low P-value.	
#' 	
#' # Exercise 5	
#' 	
#' 	
library(randomForest)	
library(ROCR)	
	
set.seed(25)	
train_idx = sample(nrow(bank), nrow(bank)*0.8)		
train = bank[train_idx,]		
test = bank[-train_idx,]	
	
fit <- randomForest(y ~ ., train,ntree=100)	
summary(fit)	
	
predictedWithForest = predict(fit,test)	
confusionMatrix(predictedWithForest, test$y)	
	
predictionNormalization <- ifelse(predictedWithForest=="no", 1, 0)	
testNormalization <- ifelse(test$y=="no", 1, 0)	
	
pred <- prediction(predictionNormalization, testNormalization)	
perf <- performance(pred, "tpr", "fpr")	
plot(perf)	
	
paste("AUC:", as.numeric(performance(pred, "auc")@y.values))	
	
#' 	
#' 	
#' As far as parameters in the confusion matrix go, we can see that they're higher than the ones displayed in decision trees, and especially in the Naive Bayes algorithm. With random forests, values pass the 90% mark of accuracy, displaying even better performance than decision trees, while also having a very low P-score (on both of them). Also, random forest outputs that the error rate of 9.31%.	
#' 	
#' As for the ROC, we can see that it doesn't display that good of a curve, with the beginning of the curve being very diagonal instead of straight. We can see that the AUC suffers because of this, with only 0.72.	
#' 	
