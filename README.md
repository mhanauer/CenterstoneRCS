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

Finally, I write itemsOnly dataset as a csc and reupload it in order for R to read the values as integers (needed for later analysis below).
```{r}
library(lavaan)
library(psych)
library(semTools)
library(dplyr)
#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/RCS/Data")
#dat = read.csv("AAC_RCS_Intake_Clean.csv", header = TRUE)
head(dat)
itemsOnly = dat[,c(4, 7:10, 26:60)]
head(itemsOnly)
write.csv(itemsOnly, "itemsOnly.csv", row.names = FALSE)
itemsOnly = read.csv("itemsOnly.csv", header= TRUE, na.strings = c("Don't Know", "Not Applicable", "Refused", "#N/A", "-999", "-888", "-777"))


itemsOnly = data.frame(apply(itemsOnly, 2, function(x){ifelse(x == "Strongly Agree", 5,ifelse(x == "Agree", 4,ifelse(x == "Sometimes", 3, ifelse(x == "Disagree", 2, ifelse(x == "Strongly Disagree",1, x)))))}))


write.csv(itemsOnly, "itemsOnly.csv", row.names = FALSE)
itemsOnly = read.csv("itemsOnly.csv", header = TRUE)
head(itemsOnly)
```
Here I am getting the reliablity by subsetting only the items in the data set (I don't want to include the demographics in the reliability calculation).  The I use the alpha function which calculates Cronbach's alpha.    
```{r}
itemsOnlyAlpha = itemsOnly[c("V33", "V32", "V27", "V31", "V7", "V29", "V4", "V15", "V21", "V35")]
alpha(itemsOnlyAlpha)
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
modelMI1 = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = itemsOnly, group = "G1..Gender.")
summary(modelMI1, fit.measure = TRUE)

```
Model for ethnicity
```{r}
itemsOnly$G3..Race. = ifelse(itemsOnly$G3..Race. > 3, 4, itemsOnly$G3..Race.)

modelMI2 = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = itemsOnly, group = "G3..Race.")
summary(modelMI2, fit.measure = TRUE)

```
Model for age.  We put them into the census bins: 17 and under, 18-24, 25-44, 45-64, 65 and older.  So everyone who is under 18 gets a 1, everyone who is between 18-24 gets a 2 and so one.
```{r}
itemsOnly$age = ifelse(itemsOnly$age < 18, 1, ifelse(itemsOnly$age < 25, 2, ifelse(itemsOnly$age < 45,3, ifelse(itemsOnly$age < 65, 4, 5))))
itemsOnly$age 

modelMI3 = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = itemsOnly, group = "age")
summary(modelMI3, fit.measure = TRUE)

```
For the sexual orientation model, to increase power, we created two categories any who is and is not heterosexual.  Therefore, anything greater than 1 (i.e. 2,3,4) become 2's.
```{r}
itemsOnly$G1a..Sexual.Orientation. = ifelse(itemsOnly$G1a..Sexual.Orientation.>1, 2,itemsOnly$G1a..Sexual.Orientation.)
levels(as.factor(itemsOnly$G1a..Sexual.Orientation.))
modelMI4 = measurementInvariance(model1, estimator = "MLR", missing = "fiml", std.lv = TRUE, data = itemsOnly, group = "G1a..Sexual.Orientation.")
summary(modelMI3, fit.measure = TRUE)

```





