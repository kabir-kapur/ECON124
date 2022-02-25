library(gamlr)
library(Matrix)
# Question 1
snf <- read.csv("/Users/kabirkapur/Desktop/SCHOOL/ECON124/HW3/NY_stop_frisk.csv")

snf$BORO <- factor(snf$BORO, levels=c(1,2,3,4,5), labels=c("Manhattan", 
                                                           "Bronx", 
                                                           "Brooklyn", 
                                                           "Queens", 
                                                           "Staten Island"))
snf$BORO <- factor(snf$BORO)

snf$DAYSTOP2 <- factor(snf$DAYSTOP2, levels=c(-9,1,2,3,4,5,6,7), labels=c(NA,
                                                                          "Sunday",
                                                                          "Monday",
                                                                          "Tuesday",
                                                                          "Wednesday",
                                                                          "Thursday",
                                                                          "Friday",
                                                                          "Saturday"
                                                                          ))
snf$DAYSTOP2 <- factor(snf$DAYSTOP2)

snf$TIMESTP2 <- factor(snf$TIMESTP2, levels=c(1,2,3,4,5,6,7,8,9,10,11,12), labels=c("BETWEEN MIDNIGHT AND 2 AM",
                                                                          "BETWEEN 2 AM AND 4 AM",
                                                                          "BETWEEN 4 AM AND 6 AM",
                                                                          "BETWEEN 6 AM AND 8 AM",
                                                                          "BETWEEN 8 AM AND 10 AM",
                                                                          "BETWEEN 10 AM AND NOON",
                                                                          "BETWEEN NOON AND 2 PM",
                                                                          "BETWEEN 2 PM AND 4 PM",
                                                                          "BETWEEN 4 PM AND 6 PM",
                                                                          "BETWEEN 6 PM AND 8 PM",
                                                                          "BETWEEN 8 PM AND 10 PM",
                                                                          "BETWEEN 10 PM AND MIDNIGHT"))
snf$TIMESTP2 <- factor(snf$TIMESTP2)


snf$INOUT <- factor(snf$INOUT,labels = c("In","Out"))
snf$INOUT <- factor(snf$INOUT)

snf$TYPEOFID <- factor(snf$TYPEOFID, labels = c("Other", 
                                                "Photo", 
                                                "Refused", 
                                                "Verbal"))
snf$TYPEOFID <- factor(snf$TYPEOFID)

snf$RACE <- factor(snf$RACE, labels = c("Asian/Pacific Islander",
                                        "Black",
                                        "American Indian/Alaskan native",
                                        "Black-Hispanic",
                                        "White-Hispanic",
                                        "White",
                                        "Other"))
snf$RACE <- factor(snf$RACE)

snf <- naref(snf)

# Question 2
# part a)
snf_omit <- subset(snf, select = -c(SEARCHED, 
                                    PF_BATON, 
                                    PF_DRWEP, 
                                    PF_GRND, 
                                    PF_HANDS, 
                                    PF_HCUFF, 
                                    PF_OTHER, 
                                    PF_PEPSP, 
                                    PF_PTWEP, 
                                    PF_WALL))

Xmatrix <- model.matrix(FRISKED ~., data = naref(snf_omit))[,-1]
Xmatrix

set.seed(69)
ll_model <- cv.gamlr(Xmatrix, snf_omit$FRISKED, family="binomial", verb=TRUE, nfold=10)
summary(ll_model)

# part b)
plot(ll_model$gamlr)
plot(ll_model)
plot(log(ll_model$gamlr$lambda), AICc(ll_model$gamlr))


# part c)
cat("Number of regressors chosen by CV:", length(which(coef(ll_model, select = "min") != 0)))
cat("Number of non-zero regressors chosen by AICc:", length(which(coef(ll_model$gamlr) !=0 )))

# part e)
coefs <- coef(ll_model, select = "min")
coefs
coefs <- coefs[which(coefs!=0)]
coefs


# Question 3
# part a)
pred <- drop(predict(ll_model$gamlr, Xmatrix, type = "response"))
par(mar = c(2,2,2,2))
hist(pred,)

# part b)
set.seed(0)

roc <- function(p,y, ...){
  y <- factor(y)
  n <- length(p)
  p <- as.vector(p)
  Q <- p > matrix(rep(seq(0,1,length=100),n),ncol=100,byrow=TRUE)
  specificity <- colMeans(!Q[y==levels(y)[1],])
  sensitivity <- colMeans(Q[y==levels(y)[2],])
  plot(1-specificity, sensitivity, type="l", ...)
  abline(a=0,b=1,lty=2,col=8)
}

par(mai=c(.9,.9,.2,.1))
roc(pred, snf_omit$FRISKED, bty="n", main= "ROC")

dict = c("red" = .02, 
         "gold" = .1,
         "purple" = .33,
         "orange" = .8,
         "pink" = .9)

for (val in names(dict)){
  points(x = 1 - mean((pred <= dict[val])[snf_omit$FRISKED == 0]), y = mean((pred > dict[val])[snf_omit$FRISKED ==1]), cex = 1.5, pch = 20, col = val)
}

legend("bottomright", fill=names(dict), legend=c("p = 0.02", 
                                                 "p = 0.1",
                                                 "p = 0.33", 
                                                 "p = 0.8", 
                                                 "p = 0.9"), bty="n",title="IS Legend")


set.seed(0)
testSet <- sample.int(1000,500)
logit_lasso_half <- gamlr(Xmatrix[-testSet,], snf_omit$FRISKED[-testSet], family = "binomial")

dictOOS = c("green" = .02, 
            "blue" = .1, 
            "black" = .33,
            "grey" = .8,
            "turquoise" = .9)

oosPred <- predict(logit_lasso_half, Xmatrix[testSet,], type= "response")
oosY <- snf_omit$FRISKED[test]

for (val in names(dictOOS)){
  points(x = 1 - mean((oosPred <= dictOOS[val])[oosY == 0]), y = mean((pred > dictOOS[val])[oosY ==1]), cex = 1.5, pch = 20, col = val)
}

legend("topright", fill=names(dictOOS), legend=c("p = 0.02", 
                                                 "p = 0.1",
                                                 "p = 0.33", 
                                                 "p = 0.8", 
                                                 "p = 0.9"), bty="n",title="OOS Legend")
