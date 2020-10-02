### Flow Thoughts ###
# * Properly encode factors
# * Remove UIDClient -> Not useful
# * Remove DateJoined -> data encapsulated in DaysSinceJoined
# * Fix "" and "#NULL!" Gender rows... Encode as "Missing"
# * Convert UpgradeRevenue -> hasUpgraded 0,1 -> better business intelligence
# * training validation split
# * Run regression (first order ~ no interaction)
#   * Best subsets regression
#   * Stepwise regression
#   * Which is best solution based on validation? Which is best for using
# * Some nice plots and exploratory analysis of the model
# * How to predict hos much a customer will generate: retailrevenue

if(require(pacman)==FALSE)
  install.packages("pacman")
pacman::p_load(dplyr, DataExplorer, corrplot, leaps)

### Preprocessing ###
tan = read.csv("Data/BeachTan.csv", stringsAsFactors=TRUE)
tan$UIDStoreLocation = as.factor(tan$UIDStoreLocation)
tan$Gender = as.factor(tan$Gender)
tan$MembershipType = as.factor(tan$MembershipType)
tan$MembershipLevel = as.factor(tan$MembershipLevel)
tan = select(tan, -UIDClient, -DateJoined)
tan$Gender = recode_factor(tan$Gender, "0"="0", "1"="1", .default="Missing")
tan$hasUpgraded = with(tan, ifelse(UpgradeRevenue == 0, as.character(0), as.character(1)))
tan$hasUpgraded = as.factor(tan$hasUpgraded)
tan = select(tan, -UpgradeRevenue)
set.seed(1337)
sample_size = floor(nrow(tan)*0.7)
index = sample(1:nrow(tan), size=sample_size)
tan.train = tan[index,]
tan.valid = tan[-index,]

### Regression ###
tan.reg = lm(RetailRevenue~., data=tan.train)
options(scipen=999)
summary(tan.reg)

plot(tan.reg$fitted.values, tan.reg$residuals)
qqnorm(tan.reg$residuals)

nullmod = formula(RetailRevenue~1)
null = lm(nullmod, data=tan.train)
fullmod = formula(RetailRevenue~.)
full = lm(fullmod, data=tan.train)
tan.step = step(null,scope=list(lower=null,upper=full),direction="both", trace=0, k=2)
summary(tan.step)
tan.step.forward = step(null,scope=list(lower=null,upper=full),direction="forward", trace=0, k=2)
summary(tan.step.forward)
tan.step.backward = step(full,scope=list(lower=null,upper=full),direction="backward", trace=0, k=2)
summary(tan.step.backward)

# Best subsets
tan.best.subset = regsubsets(RetailRevenue~., tan.train, nbest=1,nvmax=22)
tan.best.summary = summary(tan.best.subset)
tan.best.summary

df = rowSums(tan.best.summary$which)-1
SSE = tan.best.summary$rss
n = nrow(tan.train)
AIC.best = n*log(SSE/n)+2*df
AIC.best
AIC.min.best = which.min(AIC.best)
AIC.min.best
tan.best.summary$outmat[AIC.min.best,]
# Best: UIDStoreLocation2, UIDStoreLocation3, UIDStoreLocation6, UIDStoreLocation8, Gender1, MembershipType2, MembershipLevel2,
#       MembershipLevel3, MembershipLevel4, Age, UVTans, SunlessTans, hasUpgraded1
# Vars: 13
# missing some levels to categorical variables

tan.best.subset.bic = which.min(tan.best.summary$bic)
tan.best.summary$outmat[tan.best.subset.bic,]
plot(tan.best.summary$bic, xlab="Number of Variables", ylab="BIC", type="l")
points(tan.best.subset.bic, tan.best.summary$bic[tan.best.subset.bic],  col="red" )
# Best: Gender1, MembershipType2, MembershipLevel2, MembershipLevel3, MembershipLevel4, UVTans, SunlessTans, hasUpgraded1
# Vars: 8
# missing some levels to categorical variables

tan.best.subset.ar2<-which.max(tan.best.summary$adjr2)
tan.best.summary$outmat[tan.best.subset.ar2,]
plot(tan.best.summary$adjr2, xlab="Number of Variables", ylab="Adjusted RSq", type="l")
points(tan.best.subset.ar2, tan.best.summary$adjr[tan.best.subset.ar2],  col="red" )
# Best: UIDStoreLocation2, UIDStoreLocation3, UIDStoreLocation6, UIDStoreLocation8, Gender1, GenderMissing, MembershipType1
#       MembershipType2, MembershipLevel1, MembershipLevel2, MembershipLevel3, MembershipLevel4, Age, UVTans, SunlessTans,
#       hasUpgraded1
# Vars: 16
# missing some levels to categorical variables

# consider vars 19-22 to retain all levels of categorical variables
tan.best.summary$outmat[19:22,]
# DaysSinceJoined is only significant in the complete 22 variable model.
# UIDStoreLocation9 is only significant in the 21 and 22 variable models.
# UIDStoreLocation10 is only significant in the 20-22 variable models.
# Thus, only model in which we will have all levels of categorical variables in is the complete model

# However in the complete model Age, DaysSinceJoined, and all but one of the UIDStoreLocation's are not significant

# Model without Age and DaysSinceJoined
tan.reg.candidate = lm(RetailRevenue~.-Age-DaysSinceJoined, data=tan.train)
summary(tan.reg.candidate)
# Adjusted r^2 is better than complete model but simpler and 2 insignificant predictors removed.

# Model without UIDStoreLocation, Age, and DaysSinceJoined
tan.reg.candidate2 = lm(RetailRevenue~.-Age-DaysSinceJoined-UIDStoreLocation, data=tan.train)
summary(tan.reg.candidate2)
# Adjusted R^2 is marginally lower: 33.97% compared to 34.09% in the prior model and 34.1% in the complete model.
# However, 10 less variables are included in the model.

# For exploratory analysis: The final model tan.reg.candidate2 is simplest and comes very close on metrics

## Model Validity

pred.val.full = as.data.frame(predict(tan.reg, newdata=tan.valid, interval="prediction"))
r2valid.full = sum((tan.valid$RetailRevenue-pred.val.full$fit)^2)/sum((tan.valid$RetailRevenue-mean(tan.valid$RetailRevenue))^2)
r2valid.full = 1 - r2valid.full
ase.full = sum((tan.valid$RetailRevenue-pred.val.full$fit)^2)/nrow(tan.valid)
results = c(r2valid.full, ase.full)

pred.val.1 = as.data.frame(predict(tan.reg.candidate, newdata=tan.valid, interval="prediction"))
r2valid.1 = sum((tan.valid$RetailRevenue-pred.val.1$fit)^2)/sum((tan.valid$RetailRevenue-mean(tan.valid$RetailRevenue))^2)
r2valid.1 = 1 - r2valid.1
ase.1 = sum((tan.valid$RetailRevenue-pred.val.1$fit)^2)/nrow(tan.valid)
results = rbind(results, c(r2valid.1, ase.1))

pred.val.2 = as.data.frame(predict(tan.reg.candidate2, newdata=tan.valid, interval="prediction"))
r2valid.2 = sum((tan.valid$RetailRevenue-pred.val.2$fit)^2)/sum((tan.valid$RetailRevenue-mean(tan.valid$RetailRevenue))^2)
r2valid.2 = 1 - r2valid.2
ase.2 = sum((tan.valid$RetailRevenue-pred.val.2$fit)^2)/nrow(tan.valid)
results = rbind(results, c(r2valid.2, ase.2))

rownames(results) = c("Full model", "Candidate 1", "Candidate 2")
colnames(results) = c("R2Validation","ASE")

# If our goal is only prediction we would choose candidate 1 as it gives the highest r2valudation and lowest ase
# That being said there is not a large margin between the models in either metric so may choose candiate 2 for its
# simplicity if location is not important for analysis.

# Gender missing is not significant. Try without.
tan.train.2 = tan.train[!(tan.train$Gender=="Missing"),]
tan.reg.candidate3 = lm(RetailRevenue~.-Age-DaysSinceJoined-UIDStoreLocation, data=tan.train.2)
summary(tan.reg.candidate3)

### End Regression ###