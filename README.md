---
title: "CCPEBaseline"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Get the data clean.  Change the ones you know and let the rest be the same then look at the range of values  
```{r}
library(lavaan)
setwd("S:/Indiana Research & Evaluation/Matthew Hanauer/RCS/Data")
dat = read.csv("AAC_RCS_Intake_Clean.csv", header = TRUE)
head(dat)
itemsOnly = dat[,26:60]
head(itemsOnly)
dim(itemsOnly)
itemsOnly = na.omit(itemsOnly)
dim(itemsOnly)
library(psych)
levels(itemsOnly$V2)

itemsOnly = data.frame(apply(itemsOnly, 2, function(x){ifelse(x == "Strongly Agree", 5,ifelse(x == "Agree", 4,ifelse(x == "Sometimes", 3, ifelse(x == "Disagree", 2, ifelse(x == "Strongly Disagree",1, x)))))}))

itemsOnly = data.frame(apply(itemsOnly, 2, function(x){ifelse(x == "Don't Know", -99, ifelse(x == "Not Applicable", -99, ifelse(x == "Refused", -99, x)))}))


levels(itemsOnlyTest$V1)
```
Let us just try it with one construct
```{r}
model1 = 'RCS =~ V1+V2+V3+V4+V5+V6+V7+V8+V9+V10+V11+V12+V13+V14+V15+V16+V17+V18+V19+V20+V21+V22+V23+V24+V25+V26+V27+V28+V29+V30+V31+V32+V33+V34+V35'

fit1 = cfa(model1, data = itemsOnly, estimator = "MLR")


```



Figure out which items might go with which construct

Try with four constructs

Try with one construct


