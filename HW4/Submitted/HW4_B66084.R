library(ggplot2)

#EX2
children = read.table("ncmp_1516_final_non_disclosive_rand_id_published.csv", header = TRUE, stringsAsFactors = F, sep = ",")
validWH = subset(children, weight > 0 & height > 0)
ggplot(validWH, aes(weight)) +
  geom_density()

randomRecordNumber = nrow(children) * 0.05
set.seed(14)
randomData = subset(children[sample(nrow(children), randomRecordNumber),], weight > 0 & height > 0)

ggplot(randomData, aes(weight)) +
  geom_density()

ggplot(validWH, aes(height)) +
  geom_density()

ggplot(randomData, aes(height)) +
  geom_density()

ggplot(randomData, aes(x=ageinmonths, y=bmi, color=ageinmonths)) +
  geom_point(shape=1)

ggplot(randomData, aes(x=height, y=bmi, color=schoolindexofmultipledepriv)) +
  geom_point(shape=1)

#EX3
dataset = matrix(c(8,4,7,11,3,12,8,8), ncol = 8)
colnames(dataset) <- c("A", "B", "C", "D", "E", "F", "G", "H")
rownames(dataset) <- c("Count")
as.table(dataset)

datasetTwo = matrix(c(5,6,6,4,3,5,5,4,3,8,6,6,8,6,3), ncol = 15)
colnames(datasetTwo) <- c("AC", "AD", "AF", "AG", "AH", "CD", "CF", "CG", "CH", "DF", "DG", "DH", "FG", "FH", "GH")
rownames(datasetTwo) <- c("Count")
as.table(datasetTwo)

datasetThree = matrix(c(4,3,4,3,4,6,4,2,3), ncol = 9)
colnames(datasetThree) <- c("ACD", "ACF", "ADF", "CDF", "DFH", "DFG", "DFH", "DGH", "FGH")
rownames(datasetThree) <- c("Count")
as.table(datasetThree)

#EX4
a = c("NA","NA","1","1","1","NA","1","1","1","NA","1","NA","NA","NA","1") 
b = c("1","NA","NA","1","NA","NA","1","NA","NA","NA","NA","1","NA","NA","NA")
c = c("NA","1","NA","1","1","NA","NA","NA","1","NA","1","NA","NA","1","1")
d = c("1","1","1","1","NA","1","NA","1","1","1","1","NA","1","NA","1")
e = c("NA","NA","NA","NA","NA","NA","1","NA","NA","NA","1","1","NA","NA","NA")
f = c("1","1","1","NA","1","NA","1","1","1","1","NA","1","1","1","1")
g = c("NA","1","1","NA","1","NA","NA","1","1","1","NA","NA","1","1","NA")
h = c("1","NA","NA","1","NA","1","NA","1","NA","1","NA","1","NA","1","1")
df = data.frame(a,b,c,d,e,f,g,h)
df

library(arules)
aprioriRules = apriori(df, appearance = list(lhs=c("d=1", "f=1", "g=1"), default = "rhs"))
inspect(aprioriRules)

aprioriRules2 = apriori(df, appearance = list(lhs=c("a=1", "c=1", "d=1"), default = "rhs"))
inspect(aprioriRules2)

aprioriRules3 = apriori(df, appearance = list(lhs=c("f=1", "g=1"), default = "rhs"))
inspect(aprioriRules3)

totalItems = 15
totalSupportDFG = 6
totalSupportDF = 8
totalSupportG = 8

#"Support DFG"
totalSupportDFG/totalItems

#"Confidence {DF} -> {G}"
totalSupportDFG/totalSupportDF

#"Lift {DF} -> {G}"
(totalSupportDFG/totalItems)/((totalSupportDF/totalItems) * (totalSupportG/totalItems))

totalSupportADF = 4
totalSupportAD = 6
totalSupportF = 12

#"Support ADF"
totalSupportADF/totalItems

#"Confidence {AD} -> {F}"
totalSupportADF/totalSupportAD

#"Lift {AD} -> {F}"
(totalSupportADF/totalItems)/((totalSupportAD/totalItems) * (totalSupportF/totalItems))

totalSupportDF = 8
totalSupportD = 11
totalSupportF = 12

#"Support DF"
totalSupportDF/totalItems

#"Confidence {D} -> {F}"
totalSupportDF/totalSupportD

#"Lift {D} -> {F}"
(totalSupportDF/totalItems)/((totalSupportD/totalItems) * (totalSupportF/totalItems))

#EX5
titanic = read.table("titanic.txt", header = TRUE, stringsAsFactors = T, sep = ",")

#load package for frequent set mining
library(arules)

#run apriori algorithm with default settings
rules = apriori(titanic)

#inspection of the result
inspect(rules)

#now let us assume, we want to see only those rules that have rhs as survived:
rules = apriori(titanic,appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"))
inspect(rules)

#let us relax the default settings for the rules we are looking for
rules = apriori(titanic,parameter = list(minlen=2, supp=0.05, conf=0.8),appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"))
library(arulesViz)

plot(rules, method="graph", control=list(type="items"))

#EX6
ggplot(randomData, aes(x=pupilschooldistancebanded, color=pupilschooldistancebanded)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot(randomData, aes(x=schoolindexofmultipledepriv, color=schoolindexofmultipledepriv)) +
  geom_bar() +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot(randomData, aes(x=pupilschooldistancebanded, y=weight, color=pupilschooldistancebanded)) +
  geom_violin() + 
  geom_boxplot(width=.1) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot(randomData, aes(x=genderdescription, y=sqrt(height * (weight * 35.274))/60, color=genderdescription)) +
  geom_violin() + 
  geom_boxplot(width=.1)

ggplot(randomData, aes(x=height, color=genderdescription)) +
  geom_density()

ggplot(randomData, aes(x=weight, color=genderdescription)) +
  geom_density()

ggplot(subset(randomData, ageinmonths < 100), aes(x=schoolindexofmultipledepriv, y=bmi, color=schoolindexofmultipledepriv)) +
  geom_jitter()