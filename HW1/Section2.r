## Question: What do you mean by Supervised Learning
## Why do we use set.seed()
## Explain conditional expectation

## Some shortcuts
# Running a command - Command + return (Mac) \ Ctrl + enter
# Clearing Console - Ctrl + L (Both Mac and Windows)
# Clearing Environment - rm(list=ls())
# Save active document - Command + S \ Ctrl + S
# Save all documents - Command + Option + S \ Ctrl + Shift + S

##Question 1 a
library(readstata13)
fakenews <- read.dta13("fakenews.dta")
survey <- read.dta13("survey.dta")

## Question 1b.
## How might that skew the sample relative to the population?
## Underrepresented/over represented with logical argument

## Question 2. Usage of na.omit
na.omit(fakenews$fb_share)
sum(fakenews$fb_share)
x <- sum(na.omit(fakenews$fb_share[fakenews$pro=="Trump"])) * 1000
#cat()


## Question 3. Challenging to find percent of articles that dont appear in others
sum(fakenews$buzzfeed)
## Use if and else (Exclusively Tamar)
## 1 1 1 [multiplication of buzzfeed * (1-snopes) * (1-politifact)]
## 1 0 1 [multiplication of buzzfeed * (1-snopes) * (1-politifact)]
## 1 1 0 [multiplication of buzzfeed * (1-snopes) * (1-politifact)]
## 1 0 0 [multiplication of buzzfeed * (1-snopes) * (1-politifact)]

## Question 4. Use average (mean)
##            What if media minutes = 0 
##            What if social media minutes = 0 
##            What if media minutes=0 & social media minutes >0
##            round(mean(survey$SocialMediaMinutesperDay[survey$MediaMinutesPerDay>0]/)

## Question 5. Usage of tapply
## Similar to sales_by_brand_and_ads <- tapply(oj$sales, oj[,c("feat","brand")], sum)

getwd()


