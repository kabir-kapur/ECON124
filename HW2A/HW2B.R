library(scales)
library(caTools)
library(gamlr)
library(dpylr)
# question 1
nba <- read.csv("/Users/kabirkapur/Desktop/SCHOOL/ECON124/HW2A/NBA.csv")

# question 2
is.na(nba)
nba <- na.omit(nba)
is.na(nba)

# question 3
hist(nba$Salary, breaks = 15, main = "NBA Salary Distribution", ylab = "Frequency", xlab = "Salary", col = "Gold", border = "Blue")
abline(v = mean(nba$Salary), col = "Red")

# question 4
fiftyG <- nba[(nba$G > 50),]
perSort <- fiftyG[order(-fiftyG$PER),]
perSort
perSort[1,1]
tsSort <- fiftyG[order(-fiftyG$TS),]
tsSort[1,1]
vorpSort <- fiftyG[order(-fiftyG$VORP),]
vorpSort[1,1]

# question 5
row <- nba[nba$Player == "Stephen Curry",]
stephPER <- row$PER
stephTS <- row$TS
stephVORP<- row$VORP

percentPER <- (sum(nba$PER > stephPER))/nrow(nba)
percentPER
percentTS <- (sum(nba$TS > stephTS))/nrow(nba)
percentTS
percentVORP <- (sum(nba$VORP > stephVORP))/nrow(nba)
percentVORP

# question 6
americanNBA <- nba[nba$NBA_Country == "USA",]
americanNBA <- within(americanNBA, rm(NBA_Country))
americanNBA

testSet_size <- round(.9*nrow(americanNBA)) # abstracting testSet pseudo-object attribute pre-definition
set.seed(777)

picked = sample(seq_len(nrow(americanNBA)),size = testSet_size)
testSet = americanNBA[picked,]
trainingSet = americanNBA[-picked,]

testSet
trainingSet

# question 7
# part a
olsModel <- glm(log(Salary) ~ . - Player, data = trainingSet)
summary(olsModel)
r_squaredIS = 1 - (olsModel$deviance / olsModel$null.deviance)
r_squaredIS
# part c
r_squared_is <- function(df){
  olsModelTest <- glm(log(Salary) ~ . - Player, data = df)

  r_squaredOOS = 1 - (olsModelTest$deviance / olsModelTest$null.deviance)
  return(r_squaredOOS)
}

r_squared_is(testSet)

# question 8
# for (col in 1:ncol(trainingSet)){
#   trainingSet[ , col] <- factor(trainingSet[ , col], NA)
# }

naref(testSet)
naref(trainingSet)

X <- model.matrix(Salary ~  . - Player, data=trainingSet)[,-1]

# question 9
lasso_model <- gamlr(X, log(trainingSet$Salary))
plot(lasso_model)

# question 10
set.seed(0)

cv.lasso_model <- cv.gamlr(X, log(trainingSet$Salary), verb=TRUE, nfold = 10, df = TRUE)
plot(cv.lasso_model)
summary(cv.lasso_model)

coef(lasso_model)[-1,]

# question 11
coefs <- coef(cv.lasso_model, select = "min")[-1,]
coefs[which(coefs!=0)]
# question 12
b <- coef(lasso_model)[-1,]
b[which(b!=0)]

# question 13

# question 14
Xnew <- model.matrix(~ . * Tm - Player, data = trainingSet)[,-1]
lasso2 <-gamlr(X, log(trainingSet$Salary), verb = TRUE)
coefs2 <- coef(lasso2)
coefs2[which(coefs2!=0)]
# question 15
lebron <- nba[192,]
lebron <- lebron[4:28]
lebron
prediction <- predict(lasso2)
exp(lasso2[which(prediction$Player=="LeBron James")])
# R^2 = 1 - (((Y - Yhat)^2)/(Y - Ybar)^2)
# Ybar is means of actuals, Yhat is prediction

# adjusted R^2 penalizes you for adding more data to your model
# any time you add more variables to your regression, your R^2 will always go up
# we want to minimize deviation
# cv.gamlr something about cross validation with respect to gamlr idk bruh

# naref()