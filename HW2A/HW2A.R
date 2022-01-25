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
