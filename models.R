if(require(pacman)==FALSE)
  install.packages("pacman")
pacman::p_load(dplyr, DataExplorer, corrplot, leaps, ggplot2)

options(scipen=999)

## Data preprocessing - detailed in the analysis summary ##
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

## Data training & validation split ##
set.seed(1337)
sample_size = floor(nrow(tan)*0.7)
index = sample(1:nrow(tan), size=sample_size)
tan.train = tan[index,]
tan.valid = tan[-index,]

## Stepwise Regression ##
nullmod = formula(RetailRevenue~1)
null = lm(nullmod, data=tan.train)
fullmod = formula(RetailRevenue~.)
full = lm(fullmod, data=tan.train)
tan.model.step = step(null,scope=list(lower=null,upper=full),direction="both", trace=0, k=2)
summary(tan.model.step)
tan.model.forward = step(null,scope=list(lower=null,upper=full),direction="forward", trace=0, k=2)
summary(tan.model.forward)
tan.model.backward = step(full,scope=list(lower=null,upper=full),direction="backward", trace=0, k=2)
summary(tan.model.backward)

## Best Subsets
tan.best.subset = regsubsets(RetailRevenue~., tan.train, nbest=1,nvmax=22)
tan.best.summary = summary(tan.best.subset)
tan.best.summary

# Best model from best subsets for AIC
df = rowSums(tan.best.summary$which)-1
SSE = tan.best.summary$rss
n = nrow(tan.train)
AIC.best = n*log(SSE/n)+2*df
tan.best.subset.aic = which.min(AIC.best)
tan.best.summary$outmat[tan.best.subset.aic,]

# Best model from best subsets for BIC
tan.best.subset.bic = which.min(tan.best.summary$bic)
tan.best.summary$outmat[tan.best.subset.bic,]

# Best model from best subsets for adjusted R^2
tan.best.subset.ar2 = which.max(tan.best.summary$adjr2)
tan.best.summary$outmat[tan.best.subset.ar2,]

## Model 1 ##
tan.model.1 = lm(RetailRevenue~., data=tan.train)
summary(tan.model.1)

## Model 2 ##
tan.model.2 = lm(RetailRevenue~.-Age-DaysSinceJoined, data=tan.train)
summary(tan.model.2)

## Model 3 ##
tan.model.3 = lm(RetailRevenue~.-Age-DaysSinceJoined-UIDStoreLocation, data=tan.train)
summary(tan.model.3)

## Model 4 ##
tan.train.no.missing.gender = tan.train[!(tan.train$Gender=="Missing"),]
tan.model.4 = lm(RetailRevenue~.-Age-DaysSinceJoined-UIDStoreLocation, data=tan.train.no.missing.gender)
summary(tan.model.4)

## Model Validity Table ##
pred.val.model.1 = as.data.frame(predict(tan.model.1, newdata=tan.valid, interval="prediction"))
r2valid.model.1 = sum((tan.valid$RetailRevenue-pred.val.model.1$fit)^2)/sum((tan.valid$RetailRevenue-mean(tan.valid$RetailRevenue))^2)
r2valid.model.1 = 1 - r2valid.model.1
ase.model.1 = sum((tan.valid$RetailRevenue-pred.val.model.1$fit)^2)/nrow(tan.valid)
results = c(r2valid.model.1, ase.model.1)

pred.val.model.2 = as.data.frame(predict(tan.model.2, newdata=tan.valid, interval="prediction"))
r2valid.model.2 = sum((tan.valid$RetailRevenue-pred.val.model.2$fit)^2)/sum((tan.valid$RetailRevenue-mean(tan.valid$RetailRevenue))^2)
r2valid.model.2 = 1 - r2valid.model.2
ase.model.2 = sum((tan.valid$RetailRevenue-pred.val.model.2$fit)^2)/nrow(tan.valid)
results = rbind(results, c(r2valid.model.2, ase.model.2))

pred.val.model.3 = as.data.frame(predict(tan.model.3, newdata=tan.valid, interval="prediction"))
r2valid.model.3 = sum((tan.valid$RetailRevenue-pred.val.model.3$fit)^2)/sum((tan.valid$RetailRevenue-mean(tan.valid$RetailRevenue))^2)
r2valid.model.3 = 1 - r2valid.model.3
ase.model.3 = sum((tan.valid$RetailRevenue-pred.val.model.3$fit)^2)/nrow(tan.valid)
results = rbind(results, c(r2valid.model.3, ase.model.3))

tan.valid.no.missing.gender = tan.valid[!(tan.valid$Gender=="Missing"),]
pred.val.model.4 = as.data.frame(predict(tan.model.4, newdata=tan.valid.no.missing.gender, interval="prediction"))
r2valid.model.4 = sum((tan.valid.no.missing.gender$RetailRevenue-pred.val.model.4$fit)^2)/sum((tan.valid.no.missing.gender$RetailRevenue-mean(tan.valid.no.missing.gender$RetailRevenue))^2)
r2valid.model.4 = 1 - r2valid.model.4
ase.model.4 = sum((tan.valid.no.missing.gender$RetailRevenue-pred.val.model.4$fit)^2)/nrow(tan.valid.no.missing.gender)
results = rbind(results, c(r2valid.model.4, ase.model.4))

rownames(results) = c("Model 1", "Model 2", "Model 3", "Model 4")
colnames(results) = c("R2Validation","ASE")

results

## Graph of Predicted Values ##
p=ggplot(data=tan.valid)+
  geom_point(aes(RetailRevenue,pred.val.model.1$fit,col="blue"),size=1.5)
p=p+xlim(0,1600)+ylim(0,1000)
p=p+geom_point(aes(RetailRevenue,pred.val.model.2$fit,col="red"),size=1.5)
p=p+geom_point(aes(RetailRevenue,pred.val.model.3$fit,col="green"),size=1.5)
p=p+labs(x="Actual Retail Revenue", y="Predicted Retail Revenue")+ggtitle("Out-of-Sample Predictive Performance")
p=p+scale_color_manual(labels=c("Model 1","Model 2", "Model 3"),values=c("blue","red", "green"))
p=p+geom_abline(slope=1,intercept=0)
p

p=ggplot(data=tan.valid.no.missing.gender)+
  geom_point(aes(RetailRevenue,pred.val.model.4$fit,col="purple"),size=1.5)
p=p+xlim(0,1600)+ylim(0,1000)
p=p+labs(x="Actual Retail Revenue", y="Predicted Retail Revenue")+ggtitle("Out-of-Sample Predictive Performance")
p=p+scale_color_manual(labels=c("Model 4"),values=c("purple"))
p=p+geom_abline(slope=1,intercept=0)
p
