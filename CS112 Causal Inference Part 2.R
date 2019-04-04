# REPLICATION: King, Gary;Zeng, Langche, 2007 
#              "Replication data for: When Can History be Our Guide? The Pitfalls of Counterfactual Inference"

# Load packages
library(MASS)
library(Matching)

# Load dataset and extract relevant columns according to Cohen's codebook: 
# https://www.nyu.edu/gsas/dept/politics/faculty/cohen/codebook.pdf
# King's logdead = Cohen's logcost
# King's UNOP4 = Cohen's untype4

foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
foo <- foo[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10)]

# Remove two rows with missing data
foo <- foo[c(-19, -47), ]

# Check that all missing data is gone...
foo[which(is.na(foo) == TRUE),]

# Identify the columns
names(foo)

#Check that we still have the correct number of treatment and controls (should be 7 & 115, respectively)
length(which(foo$untype4 == 1)) # => 7 treated units
length(which(foo$untype4 == 0)) #=> 115 control units

### Outcome is "pbs2s3": "democracy" and "peace" within 2 years after the end of the war
### codebook is here: http://www.nyu.edu/gsas/dept/politics/faculty/cohen/codebook.pdf

### Treatment indicator is "untype4": "multidimensional peacekeeping/peacebuilding"

### ORIGINAL & MODIFIED LOGIT MODELS ###
# Note covariates have been decoded from King's to Cohen's

# No interaction term:
glm1 <- glm(pbs2s3~wartype+logcost+wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade,data=foo,family=binomial)
summary(glm1)

# Interaction term:
glm2 <- glm(pbs2s3~wartype+logcost+wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade+I(logcost*untype4),data=foo,family=binomial)
summary(glm2) 

# Hold all covariates other than wardur and untype4 at their means
wartype_mean <- mean(foo$wartype) 
logcost_mean <- mean(foo$logcost)
factnum_mean <-mean(foo$factnum)
factnum2_mean <- mean(foo$factnum2)
trnsfcap_mean <- mean(foo$trnsfcap)
treaty_mean <- mean(foo$treaty)
develop_mean <- mean(foo$develop)
exp_mean <-  mean(foo$exp)
decade_mean <- mean(foo$decade)


# Average Treatment Effect (ATE) = average outcome for treatment group - average outcome for control group
# Since we are comparing models, we will use test data predictions based off those models

# Storage vector for covariates of treatment and control groups in glm1:
treatment1 <- c()
control1 <- c()
# Storage vector for ATE of glm1:
ATE1 <- c()

# Loop through each duration amount in wardur:
for(i in 1:315) {
  treatment1 <- data.frame(wartype_mean, logcost_mean, i, factnum_mean, factnum2_mean, trnsfcap_mean, 1, treaty_mean, develop_mean, exp_mean, decade_mean)
  control1 <- data.frame(wartype_mean, logcost_mean, i, factnum_mean, factnum2_mean, trnsfcap_mean, 0, treaty_mean, develop_mean, exp_mean, decade_mean)
  names(treatment1) <- c('wartype','logcost','wardur','factnum','factnum2','trnsfcap','untype4','treaty','develop',
                          'exp','decade')
  names(control1) <- c('wartype','logcost','wardur','factnum','factnum2','trnsfcap','untype4','treaty','develop',
                            'exp','decade')
  ATE1[i] <- mean(predict(glm1, newdata = treatment1, type='response')) - mean(predict(glm1, newdata=control1, type='response'))
  }

# Repeat the process for glm2, which has the interaction term:
treatment2 <- c()
control2 <- c()
ATE2 <- c()

for(i in 1:315) {
  treatment2 <- data.frame(wartype_mean, logcost_mean, i, factnum_mean, factnum2_mean, trnsfcap_mean, 1, treaty_mean, develop_mean, exp_mean, decade_mean)
  control2 <- data.frame(wartype_mean, logcost_mean, i, factnum_mean, factnum2_mean, trnsfcap_mean, 0, treaty_mean, develop_mean, exp_mean, decade_mean)
  names(treatment2) <- c('wartype','logcost','wardur','factnum','factnum2','trnsfcap','untype4','treaty','develop',
                         'exp','decade')
  names(control2) <- c('wartype','logcost','wardur','factnum','factnum2','trnsfcap','untype4','treaty','develop',
                       'exp','decade')
  ATE2[i] <- mean(predict(glm2, newdata = treatment2, type='response')) - mean(predict(glm2, newdata=control2, type='response'))
}

# Plot ATE for both models:
plot(ATE1, type ='l',lty ='dotted', ylim = c(0,0.8),xlim = c(0,315),
     xlab="Duration of wars in months", ylab = "Marginal effects of UN peacekeeping operations", axes = FALSE)
axis(side = 1, at = c(5,20,35,50,65,80,95,115,140,165,190,215,240,265,290,315))
axis(side = 2, at = c(0.0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8))
box()

par(new=TRUE)

plot(ATE2,type ='l', ylim = c(0,0.8),axes = FALSE,
     ylab = "",
     xlab="",cex.lab=1.1)

legend(115, .2, legend=c("Model With Interaction Term", "Dotted: Original Model"),
       col=c("black", "black"), lty=1:2, cex=0.8)
