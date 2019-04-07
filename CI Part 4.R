# Load the data
foo_raw <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
names(foo_raw)

# Add relevant columns (unlike Part 2, we now also need pbs2l, pbs5l and uncint)
foo <- foo_raw[, c(6:8, 11:16, 99, 50, 114, 49, 63, 136, 109, 126, 48, 160, 142, 10, 34, 35, 52, 55)]

# Remove rows with missing data
foo <- foo[c(-4, -16, -19, -47, -84, -93, -98), ]
names(foo)

# Check that all missing data is gone...
foo[which(is.na(foo) == TRUE),]

# Convert pbs2l and pbs5l into numeric values (0=failure, 1=success)
typeof(foo$pbs2l)
typeof(foo$pbs5l)

is.list(foo$pbs2l)           #-> no
is.data.frame(foo$pbs2l)     #->no
is.vector(foo$pbs2l)         #-> yes
str(foo$pbs2l)               #-> "integer"

is.list(foo$pbs5l)           #-> no
is.data.frame(foo$pbs5l)     #-> no
is.vector(foo$pbs5l)         #-> no
is.array(foo$pbs5l)          #-> no
str(foo$pbs5l)              #-> "integer"

foo$pbs2l <- as.numeric(factor(as.character(foo$pbs2l))) #-> 1=F, 2=S
typeof(foo$pbs2l)
foo$pbs2l[which(foo$pbs2l == 1)] <- 0
foo$pbs2l[which(foo$pbs2l == 2)] <- 1
as.factor(foo$pbs2l)

foo$pbs5l <- as.numeric(factor(as.character(foo$pbs5l))) #-> 1=F, 2=S
typeof(foo$pbs5l)
foo$pbs5l[which(foo$pbs5l == 1)] <- 0
foo$pbs5l[which(foo$pbs5l == 2)] <- 1
as.factor(foo$pbs5l)

##############
### PART 3 ###
##############

foo$uncint
foo$uncint <- as.numeric(factor(as.character(foo$uncint)))
# Note: 4 levels instead of 5 (as in Cohen's codebook) 
# Numerize the data so it matches Cohen's codebook:

foo$uncint[which(foo$uncint == 2)] <- 0 #-> Cohen's "0" = None
                                        # Missing from data: Cohen's "1" = Mediation
foo$uncint[which(foo$uncint == 3)] <- 2 #-> Cohen's "2" = Observer 
foo$uncint[which(foo$uncint == 4)] <- 3 #-> Cohen's "3" = Traditional & multidimensional peacekeeping operations
foo$uncint[which(foo$uncint == 1)] <- 4 #-> Cohen's "4" = Enforcement

# Check that it makes sense:
foo$uncint

# Define treatment according to instructions
Tr <- rep(0, length(foo$uncint))
Tr[which(foo$uncint != 0 & foo$uncint != 1)] <- 1
Tr

foo <- cbind(foo, Tr)

##############
### PART 4 ###
##############

### 1. Logistic regression
glm_2yrs <- glm(pbs2l~wartype+logcost+wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade+I(logcost*untype4)+Tr,data=foo,family=binomial)
summary(glm_2yrs)
# ----------
glm_5yrs <- glm(pbs5l~wartype+logcost+wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade+I(logcost*untype4)+Tr,data=foo,family=binomial)
summary(glm_5yrs)


### 2. Propensity Score Matching
glm_2yrs_prop <- glm(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade+I(logcost*untype4),data=foo,family=binomial)
summary(glm_2yrs_prop)
glm_2yrs_prop$fitted

m_2yrs_prop <- Match(Tr=Tr, X=glm_2yrs_prop$fitted, Y=foo$pbs2l, estimand="ATT", M=1, BiasAdjust=TRUE)
m_2yrs_mb  <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                          factnum2 + trnsfcap + untype4 + treaty + develop + exp + decade + I(logcost*untype4), 
                          data=foo, match.out = m1, nboots=500)
# ----------
glm_5yrs_prop <- glm(Tr~wartype+logcost+wardur+factnum+factnum2+trnsfcap+untype4+treaty+develop+exp+decade+I(logcost*untype4),data=foo,family=binomial)
summary(glm_5yrs_prop)
glm_5yrs_prop$fitted

m_5yrs_prop <- Match(Tr=Tr, X=glm_5yrs_prop$fitted, Y=foo$pbs5l, estimand="ATT", M=1, BiasAdjust=TRUE)
m_5yrs_mb  <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                           factnum2 + trnsfcap + untype4 + treaty + develop + exp + decade + I(logcost*untype4), 
                           data=foo, match.out = m1, nboots=500)


### 3. Genetic matching on outcome (Y=pbs2s3)
X = cbind(foo$wartype, foo$logcost, foo$wardur, foo$factnum, foo$factnum2, foo$trnsfcap, foo$untype4, foo$treaty, foo$develop, foo$exp, foo$decade)
X_all <- cbind(foo$wartype, foo$logcost, foo$wardur, foo$factnum, foo$factnum2, foo$trnsfcap, foo$untype4, foo$treaty, foo$develop, foo$exp, foo$decade, foo$logcost*foo$untype4)

genout <- GenMatch(Tr=Tr, X=X, BalanceMatrix=X_all, estimand="ATT", M=1,
                  pop.size=200, max.generations=25, wait.generations=1, caliper=0.01)

m_2yrs_gen <- Match(Tr=Tr, Weight.matrix=genout, X=X, Y=foo$pbs2l, estimand="ATT", M=1, BiasAdjust=TRUE, caliper=0.01)

m_2yrs_gen_mb  <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                              factnum2 + trnsfcap + treaty + develop + exp + decade + I(logcost*untype4), 
                              data=foo, match.out = m_2yrs_gen, nboots=500)
# ----------
m_5yrs_gen <- Match(Tr=Tr, Weight.matrix=genout, X=X, Y=foo$pbs5l, estimand="ATT", M=1, BiasAdjust=TRUE, caliper=0.01)

m_5yrs_gen_mb  <- MatchBalance(Tr ~ wartype + logcost + wardur + factnum + 
                                 factnum2 + trnsfcap + treaty + develop + exp + decade + I(logcost*untype4), 
                                 data=foo, match.out = m_5yrs_gen, nboots=500)
 
