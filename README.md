---
title: "Test1"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```
Need to explain how to download R, RStudio, and RMarkdown.

Creating fake data
```{r}
id = 1:10000
genderSamp = c("Male", "Female", "Other_Identity")
ethSamp = c("White", "African_American", "Asian", "Hispanic", "Other_Ethnic_Identity")
SESSamp = c("Very_Low", "Low", "Middle", "High")
set.seed(12345)
preScore = abs(rnorm(10000, 50, 10))
postScore = abs(rnorm(10000, 60, 10))
dat = data.frame(id, Gender = sample(genderSamp, 10000, replace = TRUE, prob = c(.40, .30, .30)), Ethnicity = sample(ethSamp, 10000, replace = TRUE, prob = c(rep(.2, 5))), SES = sample(SESSamp, 10000, replace = TRUE, prob = c(rep(.25, 4))), preScore = round(preScore, 0), postScore = round(postScore, 0)); dat

write.csv(dat, "dat.csv", row.names = FALSE)
```
Here we want to load data.  I have provided a csv file called dat.csv.  You can click on session, then set working directory and select the location of where you data is stored.  For example, I have stored by data on google drive so I will use the 

Question: Store your data on your desktop and try to load the data and copy the code into the 
```{r}
library(lavaan)
library(semTools)
library(GPArotation)
HolzingerSwineford1939$ageyr[4:20] = NA
unrotated <- efaUnrotate(HolzingerSwineford1939, nf=3, varList=paste0("x", 1:9), estimator="mlr", missing = "ML")
summary(unrotated, std=TRUE)
inspect(unrotated, "std")
# Rotated by Quartimin
rotated <- oblqRotate(unrotated, method="quartimin")
summary(rotated)

```

