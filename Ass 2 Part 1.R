set.seed(123)

#Data without outlier

x <- rnorm(999, 12, 3) #years of education
y <- 300*x + rnorm(999, 350, 500) #monthly income
testdata999 <- data.frame(x, y)
lm1 <- lm(y ~ x, data = testdata999)
summary(lm1)

#Data with outlier added
testdata1000 <- rbind(testdata999, c(0, 700000))
lm2 <- lm(y ~ x, data = testdata1000)
summary(lm2)

plot(testdata999, main = "Education vs Income", xlab = "Years of Education", ylab = "Monthly income ($)")
abline(lm1, col = "blue",testdata999)
abline(lm2, col = "red", testdata1000)
text(10, 7000, c("original"), col = "blue")
text(0, 5000, c("outlier"), col = "red")

