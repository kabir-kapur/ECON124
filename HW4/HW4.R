# @question1
data <- read.csv("/Users/kabirkapur/Desktop/SCHOOL/ECON124/HW4/sipp1991.csv")
coef(summary(glm(net_tfa ~ e401 + age + inc , data = data)))

# @question 2(b)
coef(summary(glm(net_tfa ~ 
                   factor(age) + 
                   factor(educ) + 
                   poly(inc, 8, raw = TRUE) + 
                   poly(fsize, 8, raw = TRUE) + 
                   ((factor(age) + factor(educ) + poly(inc, 8, raw = TRUE) + poly(fsize, 8, raw = TRUE) + .)^2) + ., data = data)))

# @question 2(c) -- use matrix assembled in part b
set.seed(0)
glm(net_tfa ~ e401, data = data)