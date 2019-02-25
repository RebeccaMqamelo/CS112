set.seed(123)

#Data without outlier

x <- rnorm(999, 12, 3) #years of education
y <- 300*x + rnorm(999, 350, 500) #monthly income
testdata999 <- data.frame(x, y)
reg1 <- lm(y ~ x, data = testdata999)
summary(reg1)

#Data with outlier added

testdata1000 <- rbind(testdata999, c(0, 700000)) #0 years of education, but earning $700,000 per month!
reg2 <- lm(y ~ x, data = testdata1000)
summary(reg2)

plot(testdata999, main = "Education vs Income", xlab = "Years of Education", ylab = "Monthly income ($)")
abline(reg1, col = "blue",testdata999)
abline(reg2, col = "red", testdata1000)
text(10, 7000, c("original"), col = "blue")
text(0, 5000, c("outlier"), col = "red")

