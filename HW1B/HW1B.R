library(readstata13)
library(scales)
library(caret)
library(dplyr)


surveyData <- read.dta13("/Users/kabirkapur/Desktop/SCHOOL/ECON124/HW1A/survey1.dta")
fakeNewsData <- read.dta13("/Users/kabirkapur/Desktop/SCHOOL/ECON124/HW1A/fakenews1.dta")

# question 6
# a)
lR <- glm(formula = UseSocialMedia ~ Party  + Voted_Trump + Educ_Years + Male + Income, data = surveyData, family = "binomial")
summary(lR)

# b)
r_squared = 1 - (lR$deviance / lR$null.deviance)
r_squared

# c)
(exp(coef(lR)["Voted_Trump"]) - 1) * 100

# question 7 
surveyDataOnlySeenArticle <- surveyData[(surveyData$Heard == "Yes"),]
show(surveyDataOnlySeenArticle)

x <- tapply(rep(1, nrow(surveyDataOnlySeenArticle)), surveyDataOnlySeenArticle$BarLabel, sum)
y <- tapply(rep(1, nrow(surveyDataOnlySeenArticle)), surveyDataOnlySeenArticle$ThoughtTrue, sum)


par(mfrow=c(1,2))
barplot(sort(x), las = 2)
barplot(sort(y), las = 2)

# question 8
surveyTruthFakeOrPlacebo <- surveyData[(surveyData$BarLabel == "Fake" | surveyData$BarLabel == "Placebo"),]

#bar <- select(surveyTruthFakeOrPlacebo, BarLabel)

#ummy <- dummyVars(" ~ .", data = bar)
#encodedSurveyData <- data.frame(predict(dummy, newdata = bar))

#allData <- cbind(surveyTruthFakeOrPlacebo, BarLabelFake = encodedSurveyData$BarLabelFake, BarLabelPlacebo = encodedSurveyData$BarLabelPlacebo)

surveyTruthFakeOrPlacebo$placebo <- surveyTruthFakeOrPlacebo$BarLabel == "Placebo"
surveyTruthFakeOrPlacebo$fake <- surveyTruthFakeOrPlacebo$BarLabel == "Fake"
surveyTruthFakeOrPlacebo$heard <- surveyTruthFakeOrPlacebo$Heard == "Yes"
surveyTruthFakeOrPlacebo$notSure <- surveyTruthFakeOrPlacebo$Heard == "Not sure"
surveyTruthFakeOrPlacebo$notHeard <- surveyTruthFakeOrPlacebo$Heard == "No"


lR8 <- glm(formula = heard ~ placebo, data = surveyTruthFakeOrPlacebo, family = "binomial")
summary(lR8)

# 9a
surveyData$notSure <- surveyData$ThoughtTrue == "Not sure"
surveyData$correct <- ((surveyData$ThoughtTrue == "Yes" & (surveyData$BarLabel == "Small True" | surveyData$BarLabel == "Big True")) | (surveyData$ThoughtTrue == "No"  & (surveyData$BarLabel == "Fake")))

surveyData$K <- ifelse((surveyData$correct == TRUE), 1, ifelse(surveyData$notSure == TRUE, .5, 0))

#9b
surveyData$rep <- surveyData$Party == 6 | surveyData$Party == 7
surveyData$dem <- surveyData$Party == 1 | surveyData$Party == 2

# 9c) 

linReg <- glm(formula = K ~ dem + rep, data = surveyData)
linReg

# 9d) 
interestingLinReg <- glm(formula = K ~ dem + rep + Voted_Clinton + Voted_Trump + Educ_Years, data = surveyData)
interestingLinReg

# 10a) 
surveyData$B <- ifelse((surveyData$ThoughtTrue == "Yes"), 1, ifelse(surveyData$ThoughtTrue == "No", .5, 0))
#10b)
linReg10 <- glm(formula = B ~ dem + rep, data = surveyData)
summary(linReg10)
