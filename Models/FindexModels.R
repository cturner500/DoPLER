###################################################
#       Gates Foundation: Project DoPLER          #
#       The Data Guild, Summer/Fall 2016          #
###################################################


#Steps:
#0: Import Libraries
#1: Import Kenya, Uganada, Tanzania FINDEX data
#2: Simple Recursive Partition Tree to visualize splits
#3: Random Forest and Logistic Regression to predict mobile financial services usage
#4: Conclusions

#0: Import Libraries
library(rpart)
library(rpart.plot)
library()
library(randomForest)

#1: Import Kenya, Uganada, Tanzania FINDEX data
kenya <- read.csv("/Users/Cameron/Google Drive/DataGuild/Rockefeller/DoPLER/Data/FIndex/KEN_2014_FINDEX_v01_M_EXCEL/KEN_2014_FINDEX_v01_M_EXCEL_micro_ken_varlabel.csv", comment.char="#")
summary(kenya)
names(kenya)
#mobile financial services (response variable) is col 31
#account is column 9
kenya <- data.frame(kenya)
#filter to those w/account
k2 <- kenya[kenya$Has.an.account == 'yes',]

#filter again to those with answer to mobile money question:
k2 <- k2[k2$If.has.account..made.a.transaction.using.a.mobile.phone == 'yes'
            | k2$If.has.account..made.a.transaction.using.a.mobile.phone == 'no',]
#filter out Gallup ID:
names(k2)
#gallup is 3:
k2 <- k2[,-3]

#ignore weight for now:
names(k2)
#weight is now 3:
k2 <- k2[,-3]

#remove has mobile money acct (pre-requisite to using mobile money account)
k2 <- k2[,-9]

#2: Simple Recursive Partition Tree to visualize splits
tree <- rpart(If.has.account..made.a.transaction.using.a.mobile.phone~.,data=k2, cp=.016)
plotcp(tree, sub="Kenya, 2014: 1000 Respondents, 533 mobile account holders. Unweighted.")

#using cp of 0.016 above.
summary(k2$If.has.account..made.a.transaction.using.a.mobile.phone)
rpart.plot(tree, 
           main = "Recursive Partitioning Tree for Mobile Financial Services Usage", 
           sub="Kenya, 2014: 1000 Respondents, 533 mobile account holders. Unweighted.",
           extra = 102, 
           shadow.col="GREY", 
           border.col = "BLACK", 
           box.col = c("WHITE","WHITE","RED", "GREEN")[tree$frame$yval],
           varlen = 15,
           branch = .5,
           cex=.6) 
#summary(tree)
par(mar=c(10,20,10,10))
length(tree$variable.importance)
barplot(sort(tree$variable.importance), col=4, las=2, cex.names = .7, horiz=T, main="RPart Tree Variable Importance Plot: Predicting Use of Mobile Money", sub="Kenya, 2014: 1000 Respondents, 533 mobile account holders. Unweighted.")
names(tree)

#3: Random Forest and Logistic Regression to predict mobile financial services usage
k3 = k2
#k3 <- na.roughfix(k2)
k3$If.has.account..made.a.transaction.using.a.mobile.phone <- factor(k3$If.has.account..made.a.transaction.using.a.mobile.phone)
summary(k3$If.has.account..made.a.transaction.using.a.mobile.phone)
forest <- randomForest(If.has.account..made.a.transaction.using.a.mobile.phone~.,data=k3, ntree=500)
summary(forest)
names(forest)

head(forest$err)
plot(forest$err.rate[,1], type='l',col=4, main = "OOB Error Rate, Random Forest",sub="Kenya, 2014: 1000 Respondents, 533 mobile account holders. Unweighted.")
lines(forest$err.rate[,2], type='l',col=2)
lines(forest$err.rate[,3], type='l',col=3)

plot(forest$err.rate[,1], type='l',col=4, main = "OOB Error Rate, Random Forest", sub="Kenya, 2014: 1000 Respondents, 533 mobile account holders. Unweighted.")
varImpPlot(forest, main = "Random Forest Variable Importance Plot: Predicting Use of Mobile Money", col=4, sub="Kenya, 2014: 1000 Respondents, 533 mobile account holders. Unweighted.")



#4: Conclusions

