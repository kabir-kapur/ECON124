library("gamlr")
# @question1
data <- read.csv("/Users/kabirkapur/Desktop/SCHOOL/ECON124/HW4/sipp1991.csv")
coef(summary(glm(net_tfa ~ e401 + age + inc , data = data)))

# @question 2(b)
# model <- glm(net_tfa ~ factor(age) + factor(educ) + poly(inc, 8, raw = TRUE) + poly(fsize, 8, raw = TRUE) + ((age + educ + inc + fsize + .)^2) + . , data = data)
# matrix <- (model)[,-1]
# matrix

matrix <- model.matrix(glm(net_tfa ~ factor(age) + factor(educ) + poly(inc, 8, raw = TRUE) + poly(fsize, 8, raw = TRUE) + ((age + educ + inc + fsize + .)^2) + . , data = data))[,-1]
ncol(matrix) # 138 cols
nrow(matrix) # 5000 observations

# @question 2(c) -- use matrix assembled in part b
set.seed(0)
lasso <- cv.gamlr(matrix, data$net_tfa)
coef(lasso)
