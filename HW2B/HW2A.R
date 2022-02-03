library(scales)
library(caTools)
library(gamlr)
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
for (col in colnames(trainingSet)){
  if (col != "Player"){  
    trainingSet$col <- factor(trainingSet$col, NA)
  }
}

X <- model.matrix(Salary ~  . - Player, data=trainingSet)[,-1]
lasso_model <- gamlr(X, trainingSet$Salary)
plot(lasso_model)
