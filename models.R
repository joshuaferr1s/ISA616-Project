if(require(pacman)==FALSE)
  install.packages("pacman")
pacman::p_load(dplyr, DataExplorer, corrplot, leaps)

options(scipen=999)

## Data preprocessing ##
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







