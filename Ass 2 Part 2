
library(arm)
library(Matching)
data(lalonde)
lalonde2 <- lalonde[which(lalonde$treat == 0),]; rm(lalonde)
summary(lalonde2)

lm1 <- lm(re78 ~ age + educ + re74 + re75 + I(educ*re74) + I(educ*re75) + I(age*re74) + I(age*re75) + I(age*age) + I(re74*re75), data=lalonde2)
lm1$coef

set.seed(123)
sim_results <- sim(lm1, n.sims = 10000)
sim_coefs <- sim_results@coef

med_educ <- median(lalonde2$educ[order(lalonde2$educ)])
med_re74 <- median(lalonde2$re74[order(lalonde2$re74)])
med_re75 <- median(lalonde2$re75[order(lalonde2$re75)])

educ_75 <- quantile(lalonde2$educ)[[4]]
re74_75 <- quantile(lalonde2$re74)[[4]]
re75_75 <- quantile(lalonde2$re75)[[4]]

results1 <- data.frame(matrix("", ncol = 7, nrow = 0))

for (age in min(lalonde2$age):max(lalonde2$age)) {
  storage_1 <- c()
  for (i in 1:10000) {
    vec_of_chars1 <- c(1, age, med_educ, med_re74, med_re75, I(med_educ * med_re74), 
      I(med_educ * med_re75), I(age * med_re74), I(age * med_re75), I(age * 
        age), I(med_re74 * med_re75))
    storage_1[i] <- sum(vec_of_chars1 * sim_coefs[i, ])
  }
  EV_1 <- mean(na.omit(storage_1))  #expected value is the mean simulated prediction for a particular age group
  EVint_1 <- quantile(na.omit(storage_1), probs = c(0.025, 0.975))
  row <- c(age, EV_1, EVint_1[[1]], EVint_1[[2]], med_educ, med_re74, med_re75)
  results1 <- rbind(results1, row)
  colnames(results1) <- c("age", "expected value (mean)", "EV_min", "EV_max", "median educ", 
    "median re74", "median re75")
}

results1

results2 <- data.frame(matrix("", ncol = 7, nrow = 0))

for(age in min(lalonde2$age):max(lalonde2$age)) {
    storage_2 <- c()
    for(i in 1:10000) {
        vec_of_chars1 <- c(1, age, educ_75, re74_75, re75_75, I(educ_75*re74_75), I(educ_75*re75_75), I(age*re74_75), I(age*re75_75), I(age*age), I(re74_75*re75_75))
        storage_2[i] <- sum(vec_of_chars1*sim_coefs[i, ])
        }
    EV_2 <- mean(na.omit(storage_2)) #expected value is the mean simulated prediction for a particular age group
    EVint_2 <- quantile(na.omit(storage_2), probs=c(0.025, 0.975))
    row <- c(age, EV_2, EVint_2[[1]], EVint_2[[2]], educ_75, re74_75, re75_75)
    results2 <- rbind(results2, row)
    colnames(results2) <- c("age_group", "expected value (mean)", "EV_min", "EV_max", "75%_educ_quantile", "75%_re74_quantile", "75%_re75_quantile")
}

results2

results3 <- data.frame(matrix("", ncol = 7, nrow = 0))

for(age in min(lalonde2$age):max(lalonde2$age)) {
    storage_3 <- c()
    for(i in 1:10000) {
        vec_of_chars1 <- c(1, age, med_educ, med_re74, med_re75, I(med_educ*med_re74), I(med_educ*med_re75), I(age*med_re74), I(age*med_re75), I(age*age), I(med_re74*med_re75))
        storage_3[i] <- sum(vec_of_chars1*sim_coefs[i, ] + sim_results@sigma[i])
        }
    PV_3 <- mean(na.omit(storage_3)) #predicted value is the mean simulated prediction for a particular age group
    PVint_3 <- quantile(na.omit(storage_3), probs=c(0.025, 0.975))
    row <- c(age, PV_3, PVint_3[[1]], PVint_3[[2]], med_educ, med_re74, med_re75)
    results3 <- rbind(results3, row)
    colnames(results3) <- c("age", "predicted value (mean)", "PV_min", "PV_max", "median educ", "median re74", "median re75")
}

results3

results4 <- data.frame(matrix("", ncol = 7, nrow = 0))

for(age in min(lalonde2$age):max(lalonde2$age)) {
    storage_4 <- c()
    for(i in 1:10000) {
        vec_of_chars1 <- c(1, age, educ_75, re74_75, re75_75, I(educ_75*re74_75), I(educ_75*re75_75), I(age*re74_75), I(age*re75_75), I(age*age), I(re74_75*re75_75))
        storage_4[i] <- sum(vec_of_chars1*sim_coefs[i, ] + sim_results@sigma[i])
        }
    PV_4 <- mean(na.omit(storage_4)) #predicted value is the mean simulated prediction for a particular age group
    PVint_4 <- quantile(na.omit(storage_4), probs=c(0.025, 0.975))
    row <- c(age, PV_4, PVint_4[[1]], PVint_4[[2]], educ_75, re74_75, re75_75)
    results4 <- rbind(results4, row)
    colnames(results4) <- c("age_group", "predicted value (mean)", "PV_min", "PV_max", "75%_educ_quantile", "75%_re74_quantile", "75%_re75_quantile")
}

results4

# Create empty plot
plot(x = c(1:10000), y = c(1:10000), type = "n", xlim = c(17,55), ylim = c(min(results1$EV_min), max(results2$EV_max)), 
     main = "Expected Intervals for 1978 Income by Age", xlab = "Age", 
     ylab = "Income (USD)")

#Loop over the age range. The goal is that for each age, we will draw a line representing the confidence interval for that age
for (age in 17:55) {
   segments(
   x0 = age,
   y0 = results1$EV_min[age-16],
   x1 = age,
   y1 = results1$EV_max[age-16],
   lwd = 2,
   col="blue")
   segments(
   x0 = age,
   y0 = results2$EV_min[age-16],
   x1 = age,
   y1 = results2$EV_max[age-16],
   lwd = 2,
   col="red")
}

legend(20, 12000, bty="n", c("median values", "75% quantile values"), lty=c(1,1), lwd=c(2.5,2.5),col=c("blue", "red"))

# Create empty plpot
plot(x = c(1:10000), y = c(1:10000), type = "n", xlim = c(17,55), ylim = c(min(results3$PV_min), max(results4$PV_max)),
     main = "Predicted Intervals for 1978 Income by Age", xlab = "Age", 
     ylab = "Income (USD)")

#Loop over the age range. The goal is that for each age, we will draw a line representing the confidence interval for that age
for (age in 17:55) {
   segments(
   x0 = age,
   y0 = results3$PV_min[age-16],
   x1 = age,
   y1 = results3$PV_max[age-16],
   lwd = 2,
   col="green")
   segments(
   x0 = age,
   y0 = results4$PV_min[age-16],
   x1 = age,
   y1 = results4$PV_max[age-16],
   lwd = 2,
   col="purple")
}

legend(20, 75000, bty="n", c("median values", "75% quantile values"), lty=c(1,1), lwd=c(2.5,2.5),col=c("green", "purple"))


