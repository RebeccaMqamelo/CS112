
library(Matching)
data(PlantGrowth)
summary(PlantGrowth)
trt2 <-  which(PlantGrowth$group == "trt2")
PG_new <- PlantGrowth[-which(PlantGrowth$group == "trt2"), ]

# Create and indicator variable for treatment1 and control

PG_new[,"indicator"] <- NA
PG_new$indicator[PG_new$group == "trt1"] <- 1
PG_new$indicator[PG_new$group == "ctrl"] <- 0
PG_new
PG_new$indicator

storage_vector <- c()

for(i in 1:10000) {

  boot_data <- PG_new[sample(nrow(PG_new), 20, replace = TRUE),]
  m1 <- lm(weight ~ indicator, data = boot_data)
  coef <- m1$coef[2]
  storage_vector[i] <- coef

}

quantile(na.omit(storage_vector), probs = c(0.975, 0.025))
summary(storage_vector)

hist(storage_vector, col = "lightblue", main = "Histogram of Boostrapped Coefficients for Treatment", xlab = "weight variation (coefficient)" , ylab = "frequency" )

# Analyical calculation of 95% confidence interval

m2 <- lm(weight ~ indicator, data = PG_new)
confint(m2, m2$coef[2], level = 0.95)

# PART 4

rsquared <- function (actual, predicted) {
    Total_SS <- sum((actual - mean(predicted))^2)
    Residual_SS <- sum((actual - predicted)^2)
    rsq <- 1 - (Residual_SS/Total_SS)
    return(print(rsq))
}

test.y <- PG_new$weight
predict.y <- predict(lm(weight ~ indicator, data = PG_new))

rsquared(test.y, predict.y)
