###################################################
#       Gates Foundation: Project DoPLER          #
#       The Data Guild, Summer/Fall 2016          #
###################################################


#Steps:
#0: Import Libraries
#1: Import Kenya, Uganada, Tanzania FINDEX data
#2: Simple Recursive Partition Tree to visualize splits
#3: Random Forest and Logistic Regression to predict mobile financial services usage
#4: Frequency Tables
#5: Conclusions

#0: Import Libraries
library(rpart)
library(rpart.plot)
library(randomForest)
library(htmlTable)
library(Gmisc)

#1: Import Kenya, Uganada, Tanzania FINDEX data
kenya <- read.csv("/Users/Cameron/Google Drive/DataGuild/Rockefeller/DoPLER/Data/FIndex/KEN_2014_FINDEX_v01_M_EXCEL/KEN_2014_FINDEX_v01_M_EXCEL_micro_ken_varlabel.csv", comment.char="#", na.strings=c(""," ", "NA"))
uganda <- read.csv("/Users/Cameron/Google Drive/DataGuild/Rockefeller/DoPLER/Data/FIndex/UGA_2014_FINDEX_v01_M_EXCEL/UGA_2014_FINDEX_v01_M_EXCEL_micro_uga_varlabel.csv", comment.char="#",na.strings=c(""," ", "NA"))
tanzania <- read.csv("/Users/Cameron/Google Drive/DataGuild/Rockefeller/DoPLER/Data/FIndex/TZA_2014_FINDEX_v01_M_EXCEL/TZA_2014_FINDEX_v01_M_EXCEL_micro_tza_varlabel.csv", comment.char="#", na.strings=c(""," ", "NA"))

#same names/order?
table(names(kenya)==names(uganda))
table(names(tanzania)==names(uganda))
#yup, nice.

data <- rbind(kenya, uganda, tanzania)

names(data)

#mobile financial services (response variable) is col 31
#account is column 9
data <- as.data.frame(lapply(data, function (x) if (is.factor(x)) factor(x) else x)) 
data <- data.frame(data)

#filter to those w/account
d2 <- data[data$Has.an.account == 'yes',]
#1813 remaining
#filter again to those with answer to mobile money question:
d2 <- d2[d2$If.has.account..made.a.transaction.using.a.mobile.phone == 'yes'
            | d2$If.has.account..made.a.transaction.using.a.mobile.phone == 'no',]
#1076 remaining

#filter out Gallup ID:
names(d2)
#gallup is 3:
d2 <- d2[,-3]

#ignore weight for now:
names(d2)
#weight is now 3:
d2 <- d2[,-3]

#remove has mobile money acct (pre-requisite to using mobile money account)
d2 <- d2[,-9]
d2 <- data.frame(d2)
summary(d2)

#refactor to get rid of empty bins
d2 <- as.data.frame(lapply(d2, function (x) if (is.factor(x)) factor(x) else x)) 
d2$Respondent.age <- as.numeric(as.character(d2$Respondent.age))
#2: Simple Recursive Partition Tree to visualize splits
tree <- rpart(If.has.account..made.a.transaction.using.a.mobile.phone ~.,data=d2, cp=.005)
plotcp(tree, sub="Kenya, Uganda, Tanzania, 2014: 1809 Respondents, 533 mobile account holders. Unweighted.")
#using 0.034 per plot above.
tree <- rpart(If.has.account..made.a.transaction.using.a.mobile.phone ~.,data=d2, cp=0.01)

summary(tree$frame$yval)
summary(d2$If.has.account..made.a.transaction.using.a.mobile.phone)
rpart.plot(tree, 
           main = "Recursive Partitioning Tree for Mobile Financial Services Usage", 
           sub="Kenya, Tanzania, Uganda, 2014: 3008 Respondents, 1076 account holders. Unweighted.",
           extra = 102, 
           shadow.col="GREY", 
           border.col = "BLACK", 
           box.col = c("RED", "GREEN")[tree$frame$yval],
           varlen = 15,
           branch = .5,
           cex=.6) 
#summary(tree)
par(mar=c(10,20,10,10))
length(tree$variable.importance)
barplot(sort(tree$variable.importance), col=4, las=2, cex.names = .7, horiz=T, main="RPart Tree Variable Importance Plot: Predicting Use of Mobile Money", sub="Kenya, 2014: 1000 Respondents, 533 mobile account holders. Unweighted.")
names(tree)

#3: Random Forest and Logistic Regression to predict mobile financial services usage
d3 <- d2
#d3 <- na.roughfix(d3)
d3$If.has.account..made.a.transaction.using.a.mobile.phone <- factor(d3$If.has.account..made.a.transaction.using.a.mobile.phone)
table(d3$If.has.account..made.a.transaction.using.a.mobile.phone, d3$Economy.Code, exclude=NULL)

#get rid of empty cols
d3 <- Filter(function(x)!all(is.na(x)), d3)
#get rid of empty rows
d3 <- d3[rowSums(is.na(d3)) != ncol(d3),]

#refactor
d3 <- as.data.frame(lapply(d3, function (x) if (is.factor(x)) factor(x) else x)) 
summary(d3$If.has.account..made.a.transaction.using.a.mobile.phone)

d3 <- na.roughfix(d3)
forest <- randomForest(If.has.account..made.a.transaction.using.a.mobile.phone~.,data=d3, ntree=500)
summary(forest)
names(forest)

head(forest$err)

plot(forest$err.rate[,1], type='l',col=4, main = "OOB Error Rate, Random Forest", sub="Kenya, Tanzania, Uganda, 2014: 3008 Respondents, 1076 account holders. Unweighted.")
forest <- randomForest(If.has.account..made.a.transaction.using.a.mobile.phone~.,data=d3, ntree=100)
varImpPlot(forest, main = "Random Forest Variable Importance Plot: Predicting Use of Mobile Money", col=4, sub="Kenya, Tanzania, Uganda, 2014: 3008 Respondents, 1076 account holders. Unweighted.")

#age is super important.  Let's look at distributions:
par(mar=c(5,5,5,5))
boxplot(d3$Respondent.age ~ d3$If.has.account..made.a.transaction.using.a.mobile.phone, col=4)
#hmm, does not look to be independently interesting.
t.test(d3$Respondent.age[d3$If.has.account..made.a.transaction.using.a.mobile.phone=='yes'], d3$Respondent.age[d3$If.has.account..made.a.transaction.using.a.mobile.phone=='no'])
#definitely doesn't satisfy a t-test.
plot(density(d3$Respondent.age[d3$If.has.account..made.a.transaction.using.a.mobile.phone=='yes']), col=3)
lines(density(d3$Respondent.age[d3$If.has.account..made.a.transaction.using.a.mobile.phone=='no']), col=2)
#maybe a tiny direct effect (skews younger...)

#4: Frequency Tables

cgroup <- "Made Mobile Transaction"
n.cgroup <- 3 #no, yes, p-value
rgroup <- names(d3)

vars <- NULL
n.rgroup <- NULL
curRows <- NULL
tempData <- d3
#remove has.account(no variance, was a filter)
tempData <- tempData[,-c(7:8)]

j <- 17 #response varaible, mobile acct transaction

for(i in 1:ncol(tempData)){
  print(i)
  curRows <- getDescriptionStatsBy(tempData[,i], tempData[,j], useNA='ifany', html=TRUE, show_all_values=TRUE, statistics=TRUE)
  curRows <- as.matrix(curRows, nrow=length(names(tempData[,i])), ncol=3)
  #curRows <- data.frame(curRows)
  if(i>1){ #use colnames defined by first column
    colnames(curRows) <- colnames(vars)
  }
  vars <- do.call(rbind, list(vars, curRows))
  #create rowcount for htmlTable:
  n.rgroup <- c(n.rgroup, nrow(curRows))
}

htmlTable(vars, 
          rowlabel="", 
          cgroup=cgroup, 
          n.cgroup=n.cgroup, 
          rgroup=rgroup, 
          n.rgroup = n.rgroup, 
          caption="Kenya, Tanzania, Uganda: 3008 Respondents, 1076 account holders. Unweighted. <sup>&dagger;</sup> 2014", 
          tfoot="<sup>&dagger;</sup> n=1076 observations, FINDEX 2014",
          ctable=TRUE)




#5: Conclusions

