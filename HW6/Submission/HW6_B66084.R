#' ---	
#' title: "HW6_B66084"	
#' author: "Luis Gerson Noboa Martillo"	
#' date: "1 de abril de 2017"	
#' output:	
#'   html_document: default	
#'   pdf_document: default	
#' ---	
#' 	
#' 	
knitr::opts_chunk$set(echo = TRUE)	
library(ggplot2)	
library(knitr)	
opts_chunk$set(tidy.opts=list(width.cutoff=60),tidy=TRUE)	
#' 	
#' 	
#' ## Exercise 1	
#' 	
#' 	
hw6 = read.table("HW6_exercise1.txt", header = TRUE, stringsAsFactors = F, sep = "\t")	
#' 	
#' 	
ggplot(hw6, aes(x=x, y=y)) +	
  geom_point() +	
  ggtitle("y = x")	
	
hw6_15 = 1.5 * (hw6$x)	
ggplot(hw6, aes(x=hw6_15, y=y)) +	
  geom_point() +	
  ggtitle("y = 1.5x")	
	
hw6_2 = 2 * hw6$x	
ggplot(hw6, aes(x=hw6_2, y=y)) +	
  geom_point() +	
  ggtitle("y = 2x")	
#' 	
#' 	
#' In order to calculate RMSE, the following function was defined:	
#' 	
#' 	
##help from https://heuristically.wordpress.com/2013/07/12/calculate-rmse-and-mae-in-r-and-sas/	
	
rmse <- function(e){	
  return(sqrt(mean(e ^ 2)))	
}	
#' 	
#' 	
#' Then, errors were calculated in order to get the RMSE:	
#' 	
#' 	
error1 = hw6$x - hw6$y	
paste("Error for y=X:", rmse(error1))	
	
error15 = hw6_15 - hw6$y	
paste("Error for y=1.5X:", rmse(error15))	
	
error2 = hw6_2 - hw6$y	
paste("Error for y=2X:", rmse(error2))	
	
#' 	
#' 	
#' 	
#' As explained by the Wikipedia page we were linked for in the homework website, the root mean square deviation is used to measure the difference between the actual values versus the predicted. Root mean square error gives a lot of weight to values that are very wrong. There's another measure, the mean absolute error, that gives equal weight to all errors, so by analyzing RMSE, we can see how deviated are the predicted values from the actual values, and how off are the biggest values of the vector.	
#' 	
#' Obviously, a big value means that the predictions are completely off, and that big errors are very common. A small value means that errors are not that small, so the predicted data is a good indicator of the real life scenario.	
#' 	
#' In our datasets, we can see that the smallest RMSE comes when the X value is multiplied by 1.5. It displays a value of approximately 4. Other values display higuer values, reaching 4.27 and 4.30. This means that errors for these sets are bigger than for the 1.5 set.	
#' 	
#' ## Exercise 2	
#' 	
#' 	
wine = read.table("winequality.csv", header = TRUE, stringsAsFactors = F, sep = ";")	
#' 	
#' 	
reg1 = lm(formula = quality ~ alcohol, data = wine)	
summary(reg1)	
#' 	
#' 	
#' For the first relationship, there were four values that had the lowest amount of p-value: volatile acidity, citric acid, sulphates, and alcohol. This was done by testing manually, no other fancy methods. The value for them were all displayed like < 2.2e-16 by R. All of the other variables had a higher p-value.	
#' 	
#' Important to note is that residual sugar's p-value is 0.4082, making it the biggest p-value out of all the combinations, so that one will be very hard to use on the subsequent models. Also, free sulfur dioxide displays a value of 0.0513, so it will probably not be useful for the model either.	
#' 	
#' At first, my first reaction was to choose alcohol because I think it is a better, more understandable comparison to make initially than volatile acidity, for example. It is easier to explain that quality has a close relation to alcohol, since that's something everyone knows, rather than using volatile acidity or sulphates. Fortunately, alcohol is the one of the four that displays the least standard error and the highest t-value, making it the ideal variable to start the analysis.	
#' 	
#' 	
summary(lm(formula = quality ~ alcohol + residual.sugar, data = wine))	
#' 	
#' 	
#' Just as an example, since the p-value for alcohol is very low, it makes the overall p-value to be low too, but the p-value for residual sugar individually is very, very high. As predicted, we won't be able to use it for the model.	
#' 	
#' 	
reg2 = lm(formula = quality ~ alcohol + volatile.acidity, data = wine)	
summary(reg2)	
#' 	
#' 	
#' We can see very low values of error, t and p values if we mix alcohol and volatile acidity, indicating that they are closely related to the quality of the wine. Thus the next examinations are made with these two values. Volatile acidity was chosen instead of, for example, sulphates or citric acid because it had the highest absolute t-value and the second highest std error.	
#' 	
#' 	
reg3 = lm(formula = quality ~ alcohol + volatile.acidity + sulphates, data = wine)	
summary(reg3)	
#' 	
#' 	
#' Out of all the remaining values, sulphates had the least p-value of them, so it was chosen for the next variable. A thing to not was that p-values start to get higher and higher, so there's less values to choose from because they exceed the 0.05 value threshold. We also see t values dropping, although std error keeps the same. R-squared values are also increasing.	
#' 	
#' A curious thing was that citric acid, which had a small p-value on its own, now displays a 0.478 value, making it impossible to use now.	
#' 	
#' 	
reg4 = lm(formula = quality ~ alcohol + volatile.acidity + sulphates + fixed.acidity, data = wine)	
summary(reg4)	
#' 	
#' 	
#' At this level, only fixed acidity and pH are elegible for choosing, because all of the other variables have high p-values. However, we start seeing p-values closer to 0.05, unlike the previous steps in which p-values reached 10 to the power of -16 and similar. I have no idea why the std. error is so low, when it would probably have to get higher with each step. T-value is indeed getting lower, as we can see from the increase in p-value. This is the first observation that doesn't have three stars all over the variables, with fixed acidity having two only.	
#' 	
#' 	
reg5 = lm(formula = quality ~ alcohol + volatile.acidity + sulphates + fixed.acidity + chlorides, data = wine)	
summary(reg5)	
#' 	
#' 	
#' By adding chlorides, it somehow decreased the p-value for sulphates and and fixed acidity. It also maintained kind of the samet-value, but standard error is now increasing significantly. R square values are also increasing.	
#' 	
#' 	
reg6 = lm(formula = quality ~ alcohol + volatile.acidity + sulphates + fixed.acidity + chlorides + total.sulfur.dioxide, data = wine)	
summary(reg6)	
#' 	
#' 	
#' Total sulfure dioxide was successfully added to the model without disrupting other values that much or by increased t-value. Standard error is surprisingly small for this value, even smaller than alcohol, which is the variable that had the best numbers in relation to quality.	
#' 	
#' 	
reg7 = lm(formula = quality ~ alcohol + volatile.acidity + sulphates + fixed.acidity + chlorides + total.sulfur.dioxide + density, data = wine)	
summary(reg7)	
#' 	
#' 	
#' By adding density, we see that the p-value is barely lower than 0.05, but it also makes the value from the intercept receive. T-value is getting really low now, but standard error decreased for all values.	
#' 	
#' ### Final model	
#' 	
reg8 = lm(formula = quality ~ alcohol + volatile.acidity + sulphates + fixed.acidity + chlorides + total.sulfur.dioxide + density + residual.sugar, data = wine)	
summary(reg8)	
#' 	
#' 	
#' Adding residual sugar somehow worked here, when at the beginning it didn't. P-value is just above threshold with 0.0149, while t-value keeps betting lower. This is also the final model, because adding pH, citric acid, or free sulfur dioxide ends up in getting values greater than 0.05.	
#' 	
#' Since adding density, the standard error values are very low, indicating that the values are closer to the regression line, which means that this model behaves pretty accurately. Then t-values have gotten smaller and smaller since the start of the creation of the model, while p-values have gotten higher. Since they're closely related, it makes sense that p-value increases while t-value decreases.	
#' 	
#' The estimates can be used in the following formula:	
#' 	
#' quality = 6.021e+01 + (2.347e-01 x alcohol) - (1.067e+00 x volatile.acidity) + (9.023e-01 x sulphates) + (6.493e-02 x fixed.acidity) - (1.730e+00 x chlorides) - (2.392e-03 x total.sulfur.dioxide) - (5.762e+01 x density) + (3.755e-02 x residual.sugar)	
#' 	
#' Also, as we can see, the R-squared values are pretty small, indicating that the regression is pretty close to the actual values.  	
#' 	
#' 	
rmse(reg8$residuals)	
help(lm)	
#' 	
#' 	
#' RMSE is also small for the regression, so we can be confident that this model is very accurate compared to the real values that were delivered to us.	
#' 	
#' ## Exercise 3	
#' 	
#' ### Hypothesis	
#' 	
#' The T test will confirm that as Iris versicolor petal width increases, Iris setosa petal width also increases.	
#' 	
#' ### T-test method	
#' 	
#' 	
iris = read.table("iris.data", header = FALSE, stringsAsFactors = F, sep = ",")	
	
irisSubset = subset(iris, V5 != "Iris-virginica")	
autoTest= t.test(irisSubset$V3~irisSubset$V5)	
autoTest	
	
autoTest$p.value	
	
#The same as doing this	
#versicolor = subset(iris, V5 == "Iris-versicolor")	
#setosa = subset(iris, V5 == "Iris-setosa")	
#t.test(versicolor$V3, setosa$V3)	
#' 	
#' 	
#' ### Manual t-test	
#' 	
#' 	
versicolor = subset(iris, V5 == "Iris-versicolor")	
setosa = subset(iris, V5 == "Iris-setosa")	
	
#adapted from http://stats.stackexchange.com/questions/30394/how-to-perform-two-sample-t-tests-in-r-by-inputting-sample-statistics-rather-tha 	
mean1 = mean(versicolor$V3)	
mean2 = mean(setosa$V3)	
sd1 = sd(versicolor$V3)	
sd2 = sd(setosa$V3)	
values = 50	
	
se = sqrt((sd1^2/values) + (sd2^2/values))	
df = ( (sd1^2/values + sd2^2/values)^2 ) /((sd1^2/values)^2/(values-1) + (sd2^2/values)^2/(values-1) )	
	
t = 2*pt(-abs((mean1-mean2)/se ),df)	
t	
	
#' 	
#' 	
#' We can see that both p-values are the same. This small p-value means that the hypothesis was correct and petal widths for both plants increase at the same ratio.	
#' 	
#' 	
ggplot(versicolor, aes(x=V3)) + 	
  geom_density()	
	
ggplot(setosa, aes(x=V3)) + 	
  geom_density()	
#' 	
#' 	
#' We can see from the desity plots that both graphs are actually not that similar. we see that the graph for versicolor seems more big towards the left, toward higher values. The graph for setosa seems more centered and according to a normal distribution. This is strange to see, since the tests clearly show a close dependency. I don't know how to explain this.	
#' 	
#' ## Exercise 4	
#' 	
#' 	
diamonds = read.table("diamonds.csv", header = TRUE, stringsAsFactors = F, sep = ",")	
train_idx = sample(nrow(diamonds), nrow(diamonds)*0.8)	
train = diamonds[train_idx,]	
test = diamonds[-train_idx,]	
  	
perform <- function(case){	
  r = getModel(case)	
  	
  test$predictions = predict(newdata=test, r)	
  MSE = sqrt((1/nrow(test))*sum((test$price - test$predictions)^2))	
  #e = test$price - test$predictions	
  #rmse(e)	
  	
  return(MSE)	
}	
	
getModel <- function(case){	
  r = 0	
  if (case == 1){	
    r = lm(formula = price ~ carat + cut + clarity + depth + table + x + y + z, data = train)    	
  } else if (case == 2){	
    r = lm(formula = price ~ carat + cut + clarity + depth + table + x + y + z + poly(carat, 2) + poly(depth, 2), data = train)	
  } else if (case == 3){	
    r = lm(formula = price ~ carat + cut + clarity + depth + table + x + y + z + carat^3 + carat^2 + depth^3 + depth^2, data = train)	
  } else{	
    r = lm(formula = price ~ carat + cut + clarity + depth + table + x + y + z + carat^3 + carat^2 + depth^3 + depth^2 + poly(x, 2) + poly(y, 2) +	
             poly(z, 2), data = train)	
  }	
  	
  return(r)	
}	
	
#' 	
#' 	
rmseValues = c(perform(1), perform(2), perform(3), perform(4))	
	
firstModel = getModel(1)	
paste("RMSE for first model:", rmse( firstModel$residuals), ", prediction:", rmseValues[1])	
	
secondModel = getModel(2)	
paste("RMSE for second model:", rmse( secondModel$residuals), ", prediction:", rmseValues[2])	
thirdModel = getModel(3)	
paste("RMSE for third model:", rmse( thirdModel$residuals), ", prediction:", rmseValues[3])	
	
fourthModel = getModel(4)	
paste("RMSE for fourth model", rmse( fourthModel$residuals), ", prediction", rmseValues[4])	
	
#' 	
#' 	
#' 	
test$predictions = predict(newdata=test, getModel(1))	
ggplot(test, aes(x=test$price, y= test$predictions)) +	
  geom_point()	
	
#' 	
#' 	
#' I didn't understand the instructions so I have no idea of how to proceed.	
#' 	
#' ## Exercise 5	
#' 	
#' 	
test = read.table("test.csv", header = TRUE, stringsAsFactors = F, sep = ",")	
train = read.table("train.csv", header = TRUE, stringsAsFactors = F, sep = ",")	
	
model <- lm(target ~ connections + days + usage1 + usage4 + agegroup + usage3, data=train)	
	
test$predictions <- predict(newdata=test, model)	
	
final <- data.frame(target=numeric(4000), stringsAsFactors=FALSE) 	
final$target = test$predictions	
	
write.csv(final, file = "submission.csv")	
#' 	
#' 	
#' Submission was made under the name Gerson Noboa.	
#' 	
#' I used the same method used for exercise 2, just add as many variables as I could without disrupting the p-values that much. When I saw that adding a value made the p-values increase or overflow after 0.05, then I just removed them. I kept adding values until I had no other values remaining that didn't affect my p-values. Finally, I considered that this solution was the most appropriate considering the p-values and that I was in a hurry. Then, I made the CSV file with the write.csv function, but since it added an ID to it automatically, I had to edit this csv manually so that it included the ID header correctly.	
