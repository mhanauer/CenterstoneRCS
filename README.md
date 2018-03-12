---
title: "CCPEBaseline"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Get the data clean.  Change the ones you know and let the rest be the same then look at the range of values

The first step is setting the working directory to the location of the data with setwd
The next step is reading the data, which I do using read.csv.  I use header = TRUE so that the first column in the data is treated as the variable name.

Then I subset the data with only the items, because I only need the items for the analysis, which I call itemsOnly.

Then I write the itemsOnly dataset as a csv, because I want to reupload.  When I reupload it using read.csv, I can specificy the values that I want to treat as NA. 

Then I go through and change the words (i.e. Strongly Agree, Agree) into numbers.

Finally, I write itemsOnly dataset as a csc and reupload it in order for R to read the values as integers (needed for later analysis below)
```{r}
library(lavaan)
library(psych)
#setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/RCS/Data")
#dat = read.csv("AAC_RCS_Intake_Clean.csv", header = TRUE)
itemsOnly = dat[,26:60]
write.csv(itemsOnly, "itemsOnly.csv", row.names = FALSE)
itemsOnly = read.csv("itemsOnly.csv", header= TRUE, na.strings = c("Don't Know", "Not Applicable", "Refused"))


itemsOnly = data.frame(apply(itemsOnly, 2, function(x){ifelse(x == "Strongly Agree", 5,ifelse(x == "Agree", 4,ifelse(x == "Sometimes", 3, ifelse(x == "Disagree", 2, ifelse(x == "Strongly Disagree",1, x)))))}))


write.csv(itemsOnly, "itemsOnly.csv", row.names = FALSE)
itemsOnly = read.csv("itemsOnly.csv", header = TRUE)


```
Here I am assuming that all 35 items load on to one model.
```{r}
model1 = 'RCS =~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35'

alpha(itemsOnly)

fit1 = cfa(model1, estimator = "MLR", missing="fiml", std.lv = TRUE, data = itemsOnly)
 
summary(fit1, fit.measures = TRUE)
```
Ok let us try it with multiple constructs
identity construction
social networks 
place 
material capital 
```{r}
library(lavaan)
library(semTools)
library(GPArotation)

unrotated <- efaUnrotate(itemsOnly, nf=5, varList=paste0("x", 1:35), estimator="mlr", missing = "ML")
summary(unrotatedTest, std=TRUE)
inspect(unrotatedTest, "std")

# Rotated by Quartimin
rotatedTest <- oblqRotate(unrotatedTest, method="quartimin")
summary(rotatedTest)
```

