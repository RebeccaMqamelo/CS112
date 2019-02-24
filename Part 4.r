
set.seed(1234)
library(foreign)
nsw2 <- read.dta("Downloads/nsw.dta")
logit_model <- glm(nsw2$treat ~ nsw2$age + nsw2$education + nsw2$black + nsw2$hispanic + nsw2$married + nsw2$nodegree + nsw2$re75, family = binomial())
treat_prob <- predict(logit_model,newdata = nsw2, type = "response")
treatment <- subset(treat_prob, nsw2$treat == '1')
control <- subset(treat_prob, nsw2$treat == '0')


hist(treatment, breaks = 20, col = "red", main = "Histogram of the Treatment Group's Probability", xlab = "Probability" )
legend(x= .425, y= 60, legend = "Treatment Group Probability Frequency", fill = "red")

hist(control, breaks = 20, col = "blue", main = "Histogram of the Control Group's Probability", xlab = "Probability" )
legend(x= 0.425, y= 100, legend = "Control Group Probability Frequency", fill = "blue")
