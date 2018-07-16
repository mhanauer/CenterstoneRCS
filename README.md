---
title: "CCPEBaseline"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
The first step is setting the working directory to the location of the data with setwd
The next step is reading the data, which I do using read.csv.  I use header = TRUE so that the first column in the data is treated as the variable name.

Then I subset the data with only the items that I want, which are the items and the following demographics: age, gender, ethnicity, sexual orientation.

Then I write the itemsOnly dataset as a csv, because I want to reupload.  When I reupload it using read.csv, I can specificy the values that I want to treat as NA. 

Then I go through and change the words (i.e. Strongly Agree, Agree) into numbers.

Finally, I write itemsOnly dataset as a csv and reupload it in order for R to read the values as integers (needed for later analysis below).

I also use the summary function to check if there any other funky numbers (i.e. negative numbers).

Also, I am subetting only the variables that I want to include to ensure that my demographics are based on items that I am including in the sample.  So I don't include all the items just the items in the analysis, which I decided upon by comparing to the BARC
```{r}
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
library(ltm)
library(prettyR)
library(semTools)
library(GPArotation)
library(Amelia)
#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/RCS/Data")
#dat = read.csv("AAC_RCS_Intake_Clean.csv", header = TRUE)
head(dat)
# Grabbing just demographics that I want
itemsOnly = dat[,c(4, 7:10)]
head(itemsOnly)
itemsOnly = cbind(itemsOnly, dat[c("V33", "V32", "V27", "V31", "V7", "V29", "V4", "V15", "V21", "V35")])

write.csv(itemsOnly, "itemsOnly.csv", row.names = FALSE)
itemsOnly = read.csv("itemsOnly.csv", header= TRUE, na.strings = c("Don't Know", "Not Applicable", "Refused", "#N/A", "-999", "-888", "-777"))

summary(itemsOnly)

itemsOnly = data.frame(apply(itemsOnly, 2, function(x){ifelse(x == "Strongly Agree", 5,ifelse(x == "Agree", 4,ifelse(x == "Sometimes", 3, ifelse(x == "Disagree", 2, ifelse(x == "Strongly Disagree",1, x)))))}))


write.csv(itemsOnly, "itemsOnly.csv", row.names = FALSE)
itemsOnly = read.csv("itemsOnly.csv", header = TRUE)
head(itemsOnly)
```
Need to get demographics for those with total data.
Get age on its own, because that is just the mean.  Then get gender, sexual orientation, and race for counts and percentages.

First getting the number of people in the sample with total data and those without total data.  You are fine with the later analysis that does not get rid of data, because you are using fiml, which means you want to include the total set.

We are having 32 be item one and 33 be item 2 and then rest of the item numbers falls into the pattern below (V27 is item three and V31 is item four).

```{r}
dim(itemsOnly)
itemsOnlyDescribe = na.omit(itemsOnly)
dim(itemsOnlyDescribe)
itemsOnlyDescribe = subset(itemsOnlyDescribe,G1..Gender. <=2)
dim(itemsOnlyDescribe)
dim(itemsOnlyDescribe)[1] /dim(itemsOnly)[1]

range(itemsOnlyDescribe$age)
round(mean(itemsOnlyDescribe$age),3)
round(sd(itemsOnlyDescribe$age),3)

itemsOnlyDescribeDemo = data.frame(Gender = itemsOnlyDescribe$G1..Gender.,Sexual.Orientation =  itemsOnlyDescribe$G1a..Sexual.Orientation., Race = itemsOnlyDescribe$G3..Race.)  

round(describe.factor(itemsOnlyDescribeDemo$Gender),3)
round(describe.factor(itemsOnlyDescribeDemo$Sexual.Orientation),3)
round(describe.factor(itemsOnlyDescribeDemo$Race),3)

itemsOnlyDescribeItems = itemsOnlyDescribe[c("V33", "V32", "V27", "V31", "V7", "V29", "V4", "V15", "V21", "V35")]

describe(itemsOnlyDescribeItems)
```


Here I am getting the reliablity by subsetting only the items in the data set (I don't want to include the demographics in the reliability calculation).  The I use the cronbach.alpha function which calculates Cronbach's alpha.    
```{r}

itemsOnlyAlpha = itemsOnly[c("V33", "V32", "V27", "V31", "V7", "V29", "V4", "V15", "V21", "V35")]
itemsOnlyAlpha = data.frame(itemsOnlyAlpha)
write.csv(itemsOnlyAlpha, "itemsOnlyAlpha.csv", row.names = FALSE)
itemsOnlyAlpha = read.csv("itemsOnlyAlpha.csv", header = TRUE)
cronbach.alpha(itemsOnlyAlpha, na.rm = TRUE,CI =TRUE)


```
Now I am going to run the EFA for the reviewer
```{r}
parallel = fa.parallel(itemsOnlyAlpha, fa= "fa")
parallel$fa.values

unrotated4 <- efaUnrotate(itemsOnlyAlpha, nf=4, estimator="mlr", missing = "ML")
summary(unrotated4, std=TRUE)
inspect(unrotated4, "std")
3.22689192/0.40033788 

anova(unrotated1, unrotated4)

```
Now I am going to get the percentage of missing data for each item using the Amelia package for the reviewer.
```{r}
itemsOnlyMissing= amelia(itemsOnly)
summary(itemsOnlyMissing)
```




Here we are running the CFA.  We want to create a model that says Recovery Capital equals the following items.  Once we develop the model we can use it in the cfa function, which does the actual CFA analysis.  See the paper for explainations of the additional arugements.

Finally, we want to get a summary of the results so we use the summary function and also ask for additional fit measures
```{r}
model1 = 'RCA =~ V33 + V32 + V27 + V31 + V7 + V29 + V4 + V15 + V21 + V35'
fit1 = cfa(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = itemsOnly)
summary(fit1, fit.measures = TRUE)
```
Here we are doing the measurement invariance and assess whether the constuct is similar across different demographics.  First, because there are only three people who did not identifty as male or female we excluded them, because that is enough people to have their group.  Then we run the model using the function measurement invariance.  The only new item here is group and that is where you specifcy the grouping variable.
```{r}
#Gender
itemsOnly$G1..Gender. = ifelse(itemsOnly$G1..Gender. == 3, NA, itemsOnly$G1..Gender.)
count(itemsOnly, "G1..Gender.")
modelMI1 = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = itemsOnly, group = "G1..Gender.", strict = TRUE)
summary(modelMI1, fit.measure = TRUE)

```
Model for white versus black
```{r}
itemsOnlyWB = subset(itemsOnly, G3..Race. == 1 | G3..Race. == 2)
modelMIWB = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = itemsOnlyWB, group = "G3..Race.", strict = TRUE)
summary(modelMIWB, fit.measure = TRUE)

```
Model for white versus hispanic
```{r}
itemsOnlyWH = subset(itemsOnly, G3..Race. == 1 | G3..Race. == 3)
dim(itemsOnlyWH)
modelMIWH = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = itemsOnlyWH, group = "G3..Race.", strict = TRUE)
summary(modelMIWH, fit.measure = TRUE)
```
Model for white versus other ethnic identity
```{r}
itemsOnlyWO = subset(itemsOnly, G3..Race. == 1 | G3..Race. > 3)
dim(itemsOnlyWO)
itemsOnlyWO$G3..Race. = ifelse(itemsOnlyWO$G3..Race. > 3, 1, 0)
dim(itemsOnlyWO)
modelMIWO = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = itemsOnlyWO, group = "G3..Race.", strict = TRUE)
summary(modelMIWO, fit.measure = TRUE)

```
For the sexual orientation model, to increase power, we created two categories any who is and is not heterosexual.  Therefore, anything greater than 1 (i.e. 2,3,4) become 2's.
```{r}
itemsOnly$G1a..Sexual.Orientation. = ifelse(itemsOnly$G1a..Sexual.Orientation.>1, 2,itemsOnly$G1a..Sexual.Orientation.)
levels(as.factor(itemsOnly$G1a..Sexual.Orientation.))
modelMI4 = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = itemsOnly, group = "G1a..Sexual.Orientation.", strict = TRUE)
summary(modelMI4, fit.measure = TRUE)


```
Here we are going to try some concurrent validity with the PHQ-9
Need to rename the ID's so that we can merge them
Then I need to merge them
Then I need to create total scores
Then I need to get correlation between total scores of both assessments.

Here getting the id with itemsOnly
```{r}

#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/RCS/Data")
#datPHQ9 = read.csv("PHQ9 - Intake and Discharge Final - 8-22-17.csv", header = TRUE)

itemsOnly = dat[,c(4, 7:10, 26:60)]
head(itemsOnly)
write.csv(itemsOnly, "itemsOnly.csv", row.names = FALSE)
itemsOnly = read.csv("itemsOnly.csv", header= TRUE, na.strings = c("Don't Know", "Not Applicable", "Refused", "#N/A", "-999", "-888", "-777"))


itemsOnly = data.frame(apply(itemsOnly, 2, function(x){ifelse(x == "Strongly Agree", 5,ifelse(x == "Agree", 4,ifelse(x == "Sometimes", 3, ifelse(x == "Disagree", 2, ifelse(x == "Strongly Disagree",1, x)))))}))

itemsOnlyAlpha = itemsOnly[c("V33", "V32", "V27", "V31", "V7", "V29", "V4", "V15", "V21", "V35")]

write.csv(itemsOnlyAlpha, "itemsOnlyAlpha.csv", row.names = FALSE)
itemsOnlyAlpha = read.csv("itemsOnlyAlpha.csv", header = TRUE)


itemsOnlyCon = data.frame(apply(itemsOnlyAlpha, 1, sum))
colnames(itemsOnlyCon) = c("RCSTotalScore")
RCSTotalScore = data.frame(itemsOnlyCon, dat$X)
colnames(RCSTotalScore) = c("RCSTotalScore", "ID")
head(RCSTotalScore)
RCSTotalScore
```
Now alter ID for PHQ-9 subset and get total score
```{r}
datPHQ9Con = data.frame(datPHQ9$Intake.MR.., datPHQ9$Intake.Score)
colnames(datPHQ9Con) = c("ID", "PHQ9TotalScore")
concur = merge(datPHQ9Con, RCSTotalScore, all = TRUE)
dim(concur)
concur
sum(is.na(concur))

concur = na.omit(concur)
dim(concur)

cor.test(concur$PHQ9TotalScore, concur$RCSTotalScore)
```
Ok now trying with the FAD
```{r}
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/RCS/Data")
datFAD = read.csv("FAD Intake and Discharge Final - 8-23-17.csv", header = TRUE)
datFAD =  data.frame(datFAD$Intake.MR.., datFAD$Intake)
colnames(datFAD) = c("ID", "FADTotalScore")
concurFAD_RCS = merge(RCSTotalScore, datFAD, all = TRUE)
concurFAD_RCS = na.omit(concurFAD_RCS)
dim(concurFAD_RCS)

cor.test(concurFAD_RCS$RCSTotalScore, concurFAD_RCS$FADTotalScore)
```



