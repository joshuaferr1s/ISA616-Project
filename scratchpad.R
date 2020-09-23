library(dplyr)
library(DataExplorer)

tan = read.csv("Data/BeachTan.csv", stringsAsFactors=TRUE)
summary(tan)

### PREPROCESSING ###

# Proper Encoding #
tan$UIDStoreLocation = as.factor(tan$UIDStoreLocation)
tan$Gender = as.factor(tan$Gender)
tan$MembershipType = as.factor(tan$MembershipType)
tan$MembershipLevel = as.factor(tan$MembershipLevel)
# End Encoding #

# Remove unnecessary columns #
tan = select(tan, -UIDClient, -DateJoined)
# End Remove unnecessary columns #

# Fix gender column #
# bank$Personal.Loan = recode_factor(bank$Personal.Loan,"1"="Yes", "0"="No")
tan$Gender = recode_factor(tan$Gender, "0"="0", "1"="1", .default="Missing")
# End Fix gender column #

# Convert UpgradeRevenue #
tan$hasUpgraded = with(tan, ifelse(UpgradeRevenue == 0, as.character(0), as.character(1)))
tan$hasUpgraded = as.factor(tan$hasUpgraded)
tan = select(tan, -UpgradeRevenue)
# End Convert UpgradeRevenue #

### END PREPROCESSING ###

# corelation - no highly corelated. PERFECT
nums = unlist(lapply(tan, is.numeric))
tan.num = tan[,nums]
M = cor(tan.num, use = "complete.obs")
library("corrplot")
corrplot(M, method='circle')
# end

DataExplorer::plot_histogram(tan)
# Some outliers - Age: 112, DaysSinceJoin: 41944, retailRevenue: 2539.2, sunlesstans: 278, uvtans: 469
# All right skewed. Maybe log transform to get more normal data
DataExplorer::plot_bar(tan) # as expected.

# scaled data
tan.scaled = mutate_if(tan, is.numeric,scale)
# end scaled data

# Regression#
tan.reg1 = lm(RetailRevenue~., data=tan.scaled)
options(scipen=999)
summary(tan.reg1)

plot(tan.reg1$fitted.values, tan.reg1$residuals)
qqnorm(tan.reg1$residuals)
# End Regression #

# Flow Thoughts
# * Properly encode factors
# * Remove UIDClient -> Not useful
# * Remove DateJoined -> data encapsulated in DaysSinceJoined
# * Fix "" and "#NULL!" Gender rows... Encode as "Missing"
# * Convert UpgradeRevenue -> hasUpgraded 0,1 -> better business intelligence
# * training validation split
# * Run regression
#   * Stepwise regression
#   * Best subsets regression
#   * Which is best solution based on validation? Which is best for using
# * Some nice plots and exploratory analysis of the model
# * How to predict hos much a customer will generate: retailrevenue

