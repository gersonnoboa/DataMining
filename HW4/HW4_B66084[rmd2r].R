#' ---	
#' title: "HW4_B66084"	
#' author: "Luis Gerson Noboa Martillo"	
#' date: "10 de marzo de 2017"	
#' output: html_document	
#' ---	
#' 	
#' 	
knitr::opts_chunk$set(echo = TRUE)	
library(ggplot2)	
#' 	
#' 	
#' ## Exercise 1	
#' 	
#' Tamara is talking about visualization principless. She starts by defining visualization as providing visual representations of datasets intended to help people carry out tasks more effectively.	
#' 	
#' As humans can't get information or formulate hypothesis without visual representations, then good visualization is needed and numerous experiments have been done in order to get to the best alternatives.	
#' 	
#' She then defines different types of data:	
#' 	
#' * Tabular	
#'   + Categorical (non-implicit order)	
#'   + Ordered	
#'     - Ordinal (like S, M, L)	
#'     - Quantitative (17 inches, 23 inches)	
#' * Relational	
#' * Spatial	
#' 	
#' Then, the image theory is introduced, which focuses on marks and visual channels. These visual channels are divided in types that describe what/where, how much and grouping. The point of knowing this is for the data analyst to encode the most improtant attributes with the highest ranked channels.	
#' 	
#' Tamara also introduces the Stevens Psychophysical Power Law, which describes the effect changes like color saturation, length and brighness affect a human being.	
#' 	
#' Some recommendations are given by her, like don't try to mix channels in visualization or else it would be difficult for a human being to see. She also emphasizes on the difficulties we have seeing 3D and thus recommends against it unless there's no other way.	
#' 	
#' Finally, she mentions that small multiple examples are better than animations, and that when composing visualizations, you have to validate against the right threat, whether it is a problem, abstraction, enconding or algorithm.	
#' 	
#' ## Exercise 2	
#' 	
#' To avoid manipulating all of the data at once, random subsetting of the data is used. To determine if the subset is representative, we first choose the weight attribute of all the data and plot its density. I made a subset of weight values that are lower than 0 because it is impossible to have negative weight.	
#' 	
#' 	
children = read.table("ncmp_1516_final_non_disclosive_rand_id_published.csv", header = TRUE, stringsAsFactors = F, sep = ",")	
validWH = subset(children, weight > 0 & height > 0)	
#' 	
#' 	
ggplot(validWH, aes(weight)) +	
  geom_density()	
#' 	
#' 	
#' Now the subset is generated.	
#' 	
#' 	
randomRecordNumber = nrow(children) * 0.05	
set.seed(14)	
randomData = subset(children[sample(nrow(children), randomRecordNumber),], weight > 0 & height > 0)	
	
ggplot(randomData, aes(weight)) +	
  geom_density()	
#' 	
#' 	
#' As we can see, using 10% of the data yields a very similar density graph of weight. A small issue is that the biggest peak reaches higher than 0.08 of density in the first graph, but barely reaches 0.08 on the subset graph. In order to make sure that this is right, the height is also analyzed.	
#' 	
#' 	
ggplot(validWH, aes(height)) +	
  geom_density()	
#' 	
#' 	
#' This is the graph for the random subset:	
#' 	
#' 	
ggplot(randomData, aes(height)) +	
  geom_density()	
#' 	
#' 	
#' Both graph still show the same pattern, so we can conclude that data subsetting has been effective here and that the random values are representative of most of the data present on the main dataset.	
#' 	
#' 	
#' For the analysis, first I chose the age in months of the children and plotted it against the BMI.	
#' 	
ggplot(randomData, aes(x=ageinmonths, y=bmi, color=ageinmonths)) +	
  geom_point(shape=1)	
#' 	
#' 	
#' In my opinion, this is valuable since I was expecting to see a continuous increase throughout the X axis, but it seems like age is only captured on kids within certain range of age in months. Anyways, it is interesting to see how the average BMI gets much higher when the child gets older. According to some research I made, the BMI optimum value varies a lot depending on the age, so, even though a BMI of 15-20 is low for adults, it is not for children 50-60 months old.	
#' 	
#' The case with children of approximately 125 months old is more severe, since they should be a little bit higher in the BMI scale, but, as we can see, there's a lot of children still on the 15-20 region, when they're supposed to be higher, since they have twice the age. This might indicate a small deficiency in nutrition with these children, that is not present on younger children. However, there's also a considerable amount of children that has a higher BMI than the first group.	
#' 	
#' Next, I analyzed the weight against the BMI, with the school deprivation index as a color dimension.	
#' 	
#' 	
ggplot(randomData, aes(x=height, y=bmi, color=schoolindexofmultipledepriv)) +	
  geom_point(shape=1)	
	
#' 	
#' 	
#' We can see here a tendency of the BMI to hover on the 15-25 BMI mark. The gap between both groups can be attributed to the age gap that we saw on the first graph, since height can be directly related to height. The second group also displays this tendency of having a greater amount of children with higher BMIs. There are also many children that are separated from the first group, as we can see from the 30 BMI mark onward. This doesn't happen on the first group, which can, as the first graph, indicate some kind of deficiency in which children are getting overweight after some height.	
#' 	
#' Also, we can see that the school deprivation index doesn't really say anything. There's no more deprivation as the BMI increases or another tendency. Both big main groups and data outside of the main groups are equally represented in the school deprivation index.	
#' 	
#' 	
#' ## Exercise 3	
#' 	
#' To generate the itemsets, fist all individual values were identified and counted.	
#' 	
#' 	
dataset = matrix(c(8,4,7,11,3,12,8,8), ncol = 8)	
colnames(dataset) <- c("A", "B", "C", "D", "E", "F", "G", "H")	
rownames(dataset) <- c("Count")	
as.table(dataset)	
#' 	
#' 	
#' As we can see, values B and E appear less than the requested 5 support value, so, according to the Apriori principle, their combinations will be also very infrequent, and thus, they were discarded. 	
#' 	
#' Next, with the remaining values, all possible combinations were generated without the B and E values. To get them all, I first chose the first letter and then combined it with the remaining ones. So, I picked A and combined it like AC, AD, AF, AG, and AH. Then I moved to the second one (C) and combined it like CD, CF, CG, and CH, and so on. This way, all of the possible combinations between letters was used.	
#' 	
#' 	
datasetTwo = matrix(c(5,6,6,4,3,5,5,4,3,8,6,6,8,6,3), ncol = 15)	
colnames(datasetTwo) <- c("AC", "AD", "AF", "AG", "AH", "CD", "CF", "CG", "CH", "DF", "DG", "DH", "FG", "FH", "GH")	
rownames(datasetTwo) <- c("Count")	
as.table(datasetTwo)	
#' 	
#' 	
#' Here, thanks to the Apriori principle, we can discard AG, AH, CG, CH and GH because their support is less than five.	
#' 	
#' To generate the itemsets with three values, the same principle was used as with the ones with two values. Combinations were made by manually combining them and inspecting if every possible combination. So, AC was combined with AD to create ACD, AC was combined with AF to create ACF, and so on.	
#' 	
#' 	
datasetThree = matrix(c(4,3,4,3,4,6,4,2,3), ncol = 9)	
colnames(datasetThree) <- c("ACD", "ACF", "ADF", "CDF", "DFH", "DFG", "DFH", "DGH", "FGH")	
rownames(datasetThree) <- c("Count")	
as.table(datasetThree)	
#' 	
#' 	
#' Since the only value that exceeds 5 is DFG, all of the other values are discarded. Thus, there's no other values to combine to create sets with 4 items, so the combinations are left there, since whatever letter we combine with DFG will have a support lower than 5 and will be discarded by the Apriori principle.	
#' 	
#' ## Exercise 4	
#' 	
#' First, the data was put in a table that indicates, for each letter, if the letter was used or not.	
#' 	
#' 	
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
#' 	
#' 	
#' With this arrangment, the Apriori rules were generated.	
#' 	
#' 	
library(arules)	
aprioriRules = apriori(df, appearance = list(lhs=c("d=1", "f=1", "g=1"), default = "rhs"))	
#' 	
#' 	
inspect(aprioriRules)	
#' 	
#' 	
#' This shows us the rules for the biggest created on the previous exercise using the Apriori principle. As discussed then, since B and E are very scarce in the dataset, having the biggest set of items associated with B and E yields a confidence value of 1. This is a signal that the Apriori results that I got on the previous exercises were correct, since I could determine that the biggest dataset possible was DFG (that's why it is included in this stage of the analysis) and that B and E are very infrequent.	
#' 	
#' 	
aprioriRules2 = apriori(df, appearance = list(lhs=c("a=1", "c=1", "d=1"), default = "rhs"))	
#' 	
#' 	
inspect(aprioriRules2)	
#' 	
#' 	
#' Here we can also conclude that the analysis done previously was correct, since Apriori decided to remove ACD, since it has a low support level, but it didn't remove DFG, since its support value is much higher. We can also see how Apriori deals primarily with values E and B, because it is very common to have these values being NA. So, all the confidence values, even though they're not 1, are quite high, since it is improbable to have a B or an E mixed with these data sets.	
#' 	
#' 	
aprioriRules3 = apriori(df, appearance = list(lhs=c("f=1", "g=1"), default = "rhs"))	
#' 	
#' 	
inspect(aprioriRules3)	
#' 	
#' 	
#' Here we can see that Apriori doesn't want to give us values that doesn't deal with B and E, because they're very common. Thus, it would be better to calculate values manually.	
#' 	
#' Support for {DF} -> {G}	
#' 	
#' 	
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
#' 	
#' 	
#' We can see a high lift in the case of the biggest set determined by Apriori, which was DFG. This means that it is very likely to see DFG in case we see DF in our data set. Also, a high confidence for {DF} -> {G} means that in 75% of the cases, when DF is present, then DFG is also present.	
#' 	
#' We will next analyze one of the other frequent values with 3 items, ADF, which encapsulates the three letters with the biggest support.	
#' 	
#' 	
totalSupportADF = 4	
totalSupportAD = 6	
totalSupportF = 12	
	
#"Support ADF"	
totalSupportADF/totalItems	
	
#"Confidence {AD} -> {F}"	
totalSupportADF/totalSupportAD	
	
#"Lift {AD} -> {F}"	
(totalSupportADF/totalItems)/((totalSupportAD/totalItems) * (totalSupportF/totalItems))	
	
#' 	
#' 	
#' Values here are much lower than on the DFG case, so we can see why on the Apriori algorithm, they didn't get to the threshold of 5 or more support. We can see a very low lift value, meaning that it is unlikely to see AD associated with F. Confidence is also not that high but still at 66%, it could be worse.	
#' 	
#' I will now analyze DF, which is the value that appears the most in the two item data set, and that encapsulates the two letters with the biggest amount of appearances.	
#' 	
#' 	
	
totalSupportDF = 8	
totalSupportD = 11	
totalSupportF = 12	
	
#"Support DF"	
totalSupportDF/totalItems	
	
#"Confidence {D} -> {F}"	
totalSupportDF/totalSupportD	
	
#"Lift {D} -> {F}"	
(totalSupportDF/totalItems)/((totalSupportD/totalItems) * (totalSupportF/totalItems))	
	
#' 	
#' 	
#' We can see that even though they're very common, that doesn't mean that they are supposed to be common in associations. A lift of 0.90 means that it is not likely to see them together, and a confidence of 0.72 is not that high if we consider that D appears in 11 out of the 15 cases and F appears in 12.	
#' 	
#' ## Exercise 5	
#' 	
#' First, the data is inspected in order to determine the combinations.	
#' 	
#' 	
titanic = read.table("titanic.txt", header = TRUE, stringsAsFactors = T, sep = ",")	
	
#load package for frequent set mining	
library(arules)	
	
#run apriori algorithm with default settings	
rules = apriori(titanic)	
	
#' 	
#' 	
#inspection of the result	
inspect(rules)	
	
#' 	
#' 	
#' From this data we can see some very amusing facts. Starting with support, we see that the amount of adults that had a 3rd class ticket, was, kind of unsurprisingly, bigger than the other classes. For some reason, there were actually more combinations of adults in first class than in second class, something that is not common, since normally first class has the least amount of passengers.	
#' 	
#' Another interesting piece of information is the huge difference between male and female. Male adults reach a support of 0.7573, while adult women reached only 0.1930. The number of children was so little that the Apriori principle directly removed it.	
#' 	
#' Going into more somber data, it is amazing to see that the amount of male people that didn't survive reaches 0.62 support, with adults dying reaching 0.65 support. We can infer that the amount of adults surviving is 0.35, which is an unfortunate low number.	
#' 	
#' Also, here we also have information about the crew. We can see that the support for crew adult male members reaches 0.391 support, and the ones who died reaches 0.3044, indicating that very few crew members managed to survive, probably in the form of controlling lifeboats.	
#' 	
#' Moving into confidence, we can also see some very sad facts. For example, a 0.82 confidence that if you find a 3rd class male passenger, you will find that he didn't survive the accident.	
#' 	
#' The other sets that associate something with age=Adult are almost irrelevant because, since children has been completely removed because of its low amount, you can infer that whatever set we have involves adults.	
#' 	
#' Interesting data can be extracted from the crew class again, since the confidence of an adult crew member being a male is 0.97, which indicates how little women employees the Titanic had. Also, with 0.995 confidence in rule 27, we can infer that practically all crew members who died were male.	
#' 	
#' Rule 25 has something interesting too, saying that the confidence of a 3rd class male adult not surviving was an unfortunate 0.837. Rule 22 also say that there's a confidence of 0.923 that an adult that didn't survive was a male. It is very clear that the crew opted for saving women and children first.	
#' 	
#' Finally, some data can be gathered thanks to the lift parameter. For example, rule 25 yields a high lift, meaning that it is likely for a 3rd class male adult to not survive the accident. Also, rule 27 has the same effect but for crew members. No association has a strikingly low value, with the lowest being the chance of a 3rd class passenger being also an adult.	
#' 	
#' 	
#' 	
#now let us assume, we want to see only those rules that have rhs as survived:	
rules = apriori(titanic,appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"))	
inspect(rules)	
	
#let us relax the default settings for the rules we are looking for	
rules = apriori(titanic,parameter = list(minlen=2, supp=0.05, conf=0.8),appearance = list(rhs=c("Survived=No", "Survived=Yes"),default="lhs"))	
library(arulesViz)	
#' 	
#' 	
plot(rules, method="graph", control=list(type="items"))	
#' 	
#' 	
#' The graph shows primarily a fact that can also be seen in the previous rules: a huge amount of people dying. Also, this graph shows nicely the disproportionate amount of third class passengers, which is much bigger than the rest.	
#' 	
#' ## Exercise 6 (Bonus)	
#' 	
#' First, just to get an idea, I plotted a bar graph that shows us the amount of children that lives close or far to their school.	
#' 	
#' 	
ggplot(randomData, aes(x=pupilschooldistancebanded, color=pupilschooldistancebanded)) +	
  geom_bar() +	
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))	
#' 	
#' 	
#' As is expected, a lot of children live close to their school, with the values decreasing very quickly by the D sector, which signals a distance between 2 and 3 kms. We can see that the amount of children living 20km or more away from their schools is almost negligible.	
#' 	
#' I also plotted the school deprivation index to get an overview of the environment in which these children are.	
#' 	
#' 	
ggplot(randomData, aes(x=schoolindexofmultipledepriv, color=schoolindexofmultipledepriv)) +	
  geom_bar() +	
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))	
#' 	
#' 	
#' We can see a huge amount of children with high school deprivation indexes (here, a low value means the child is more deprived), which is very unfortunate. In the second exercise, we saw that the school deprivation index was normally throughout BMI, so it was not a matter of lack of nutrition or stuff.	
#' 	
#' Next, the weight of the children is plotted against the distance from their house to their school.	
#' 	
#' 	
ggplot(randomData, aes(x=pupilschooldistancebanded, y=weight, color=pupilschooldistancebanded)) +	
  geom_violin() + 	
  geom_boxplot(width=.1) +	
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))	
#' 	
#' 	
#' We can infer that when the weight is low, probably the kid is very young, and as it grows, the weight keeps increasing. Thus, when the child is young (has a low weight), parents opt for educating them in places that are very close to their houses. This shouldn't be a surprise but it is very apparent here how parents from these children are very cautious. 	
#' 	
#' We then see that as the distance keeps increasing, the weight is also more dispersed, it is not all concentrated in the lower part of the violin plot anymore. This means that, as kids grow (and thus gain weight), parents opt to shift from safety of a school nearby to what probably they think that is best for their child. 	
#' 	
#' This is also indicated by the box plots inside the violin, with increasing values as the distance from house to school keeps increasing.	
#' 	
#' It is curious to see that even though this is the tendency, we also see an increasing amount of children with a lot of weight studying nearby. It is not as common as other cases, but it is still important to know that there are a respectable amount of students that don't follow the tendency.	
#' 	
#' Next, the sex is calculated against the body surface area.	
#' 	
#' 	
ggplot(randomData, aes(x=genderdescription, y=sqrt(height * (weight * 35.274))/60, color=genderdescription)) +	
  geom_violin() + 	
  geom_boxplot(width=.1)	
#' 	
#' 	
#' We can see that male children have a bigger surface area than females, so either woman are shorter and/or smaller, or boys are bigger/fatter. We can find out.	
#' 	
#' 	
ggplot(randomData, aes(x=height, color=genderdescription)) +	
  geom_density()	
#' 	
#' 	
#' 	
ggplot(randomData, aes(x=weight, color=genderdescription)) +	
  geom_density()	
#' 	
#' 	
#' From both graphs we can conclude that, almost in all cases, male are both fatter and taller than women in this sample, thus, this is why the body surface area is greater in male children than in female. Around the 40 mark, we see a slight increase in female weights compared to female, but this is not the tendency. 	
#' 	
#' We can also see two peaks followed by a very pronounced drop. This is because children in the dataset are of two very distinct ages, there's a region in which no children has been recorded, so instad of progressively increasing, it just falls where there are almost no children on record and then increases again.	
#' 	
#' Finally, it would be interesting to see the school deprivation by age, to see if there's a tendency.	
#' 	
#' 	
ggplot(subset(randomData, ageinmonths < 100), aes(x=schoolindexofmultipledepriv, y=bmi, color=schoolindexofmultipledepriv)) +	
  geom_jitter()	
#' 	
#' 	
#' Even though the second exercise and this one identified that the BMI doesn't have a correlation, in this graph, in which only the younger children are used, we can see that, even though in the center (from 15-20), the amount of children stay the same throughout the graph, there's more children on the lower part of these high-density areas as the school deprivation index is lower. This can mean that the children that are more deprived from school are probably living in worse conditions and have bad nutrition habits, which is directly related to the BMI score.	
#' 	
#' However, we also see a lot of children on the upper part of the high-density area for more deprived children, much more than in the high deprived area. This is related to the second graph of this exercise, which said that there's more children deprived from schools. As the deprivation index increases (and thus, the deprivation decreases), there's less variation in the data, but this is also because there's less data to work with.	
