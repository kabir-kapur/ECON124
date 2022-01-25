library(readstata13)
library(scales)
library(caret)


surveyData <- read.dta13("/Users/kabirkapur/Desktop/SCHOOL/ECON124/HW1/survey1.dta")
fakeNewsData <- read.dta13("/Users/kabirkapur/Desktop/SCHOOL/ECON124/HW1/fakenews1.dta")

# question 6
# a)
lR <- glm(formula = UseSocialMedia ~ Party  + Voted_Trump + Educ_Years + Age + Male + Income, data = surveyData, family = "binomial")
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

dummy <- dummyVars(" ~ .", data=c(surveyTruthFakeOrPlacebo$BarLabel))

#perform one-hot encoding on data frame
encodedSurveyTruthFakeOrPlacebo <- data.frame(predict(dummy, newdata=c("surveyTruthFakeOrPlacebo")))

#view final data frame
head(encodedSurveyTruthFakeOrPlacebo)

lR8 <- glm(formula = Heard ~ BarLabel, data = surveyTruthFakeOrPlacebo, family = "binomial")
