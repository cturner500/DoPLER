###################################################
#       Gates Foundation: Project DoPLER          #
#       The Data Guild, Summer/Fall 2016          #
###################################################


#Steps:
#0: Import Libraries
#1: Import Kenya, Uganada, Tanzania FINDEX data
#2: Simple Recursive Partition Tree to visualize splits
#3: Random Forest and Logistic Regression to predict mobile financial services usage
#4: FINDEX: Frequency Tables
#5: Import Merged FII Wave 3 data
#6: FII: Model Longitudinal Usage (Retention Proxy)
#7: FII: Frequency Tables
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

#now let's look just at demographic factors (non-behavioral factors)
names(d2)
d4 <- d2[,c(1:6,28)]
tree <- rpart(If.has.account..made.a.transaction.using.a.mobile.phone ~.,data=d4, cp=.005)
par(mar=c(5,5,5,5))
plot(tree)
text(tree)
barplot(sort(tree$variable.importance), col=4, las=2, cex.names = .7, horiz=T, main="RPart Tree Variable Importance Plot: Predicting Use of Mobile Money", sub="Kenya, 2014: 1000 Respondents, 533 mobile account holders. Unweighted.")

boxplot(d4$Respondent.age~d4$If.has.account..made.a.transaction.using.a.mobile.phone)


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
#rgroup <- names(d3)

vars <- NULL
n.rgroup <- NULL
curRows <- NULL
tempData <- d3
#remove has.account(no variance, was a filter)
tempData <- tempData[,-c(7:8)]
rgroup <- names(tempData)

j <- 17 #response varaible, mobile acct transaction

for(i in 1:ncol(tempData)){
  print(i)
  curRows <- getDescriptionStatsBy(tempData[,i], tempData[,j], useNA='ifany', html=TRUE, show_all_values=TRUE, statistics=TRUE)
  #curRows <- as.matrix(curRows, nrow=length(names(tempData[,i])), ncol=3)
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
          caption="The Data Guild, Project DoPLER: Kenya, Tanzania, Uganda, 3008 Respondents, 1076 account holders. Unweighted. <sup>&dagger;</sup> 2014", 
          tfoot="<sup>&dagger;</sup> n=1076 observations, FINDEX 2014",
          ctable=TRUE)


#5: Import Merged FII Wave 3 data
fii <- read.csv("/Users/Cameron/Google Drive/DataGuild/Rockefeller/DoPLER/Data/merged_fii_data_wave3_20160915.csv")
summary(fii)
barplot(table(fii$country),col=4)
#let's strip nigeria:
fii <- fii[fii$country!='nigeria',]
fii$country<- factor(fii$country)
barplot(table(fii$country),col=4)
table(fii$country)
#about 3k per country.
names(fii)
summary(fii[,82:95])
names(fii)

#create new variable that is the min of these variable:

fii$minFS <- apply(fii[,82:95], 1, min, na.rm=T)
barplot(table(fii$minFS), col=4)

#let's change the Infs to 6's (never)
fii$minFS[fii$minFS=='Inf'] <- 6
#let's create an "ever use" variable
fii$everFS <- ifelse(fii$minFS==6,0,1)
summary(fii$everFS)
barplot(table(fii$minFS), col=4)
plot(density(fii$minFS[fii$country=="uganda"]), col=2,
     main="Time Since Last Mobile Money Tx",
     sub="Ken=Grn,Ugd=Red,Tnz=Blu, 1:1d,2:7d,3:30d,4:90d,5:>90d,6:Never")
lines(density(fii$minFS[fii$country=="kenya"]), col=3)
lines(density(fii$minFS[fii$country=="tanzania"]), col=4)

#looks like a decent response variable, ordinal, where 1 is used fin svcs yesterday and 6 is never  



#6: FII: Model Longitudinal Usage (Retention Proxy)

names(fii)
tree <- rpart(minFS~.,data=fii[,c(2:6,126)], cp=0.001)
rpart.plot(tree)
par(mar=c(5,10,5,5))

barplot(sort(tree$variable.importance), col=4, las=2, cex.names = .7, horiz=T, main="RPart Tree Variable Importance Plot: Predicting Use of Mobile Money", sub="Kenya, 2014: 1000 Respondents, 533 mobile account holders. Unweighted.")

boxplot(fii$year_of_birth~fii$minFS, col=4)
boxplot(fii$education_level~fii$minFS, col=4, main="Time Since Last Mobile Financial Usage (X) by Education Level (Y)")
boxplot(fii$minFS~fii$gender, col=4)

fiisub <- na.roughfix(fii[,c(2:6,126)])
forest <- randomForest(as.factor(minFS)~.,data=fiisub)
summary(forest)

plot(forest$err.rate[,1], type='l',col=4, main = "OOB Error Rate, Random Forest", sub="Kenya, Tanzania, Uganda, 2014: FII 8995 Respondents.")
varImpPlot(forest, main = "Random Forest Variable Importance Plot: Predicting Use of Mobile Money", col=4, sub="Kenya, Tanzania, Uganda, wave3: FII 8995 Respondents. Unweighted.")

#7: Compare to "ever use" FII:
tree <- rpart(everFS~.,data=fii[,c(2:6,127)], cp=0.01)
rpart.plot(tree)
barplot(sort(tree$variable.importance), col=4, las=2, cex.names = .7, horiz=T, main="RPart Tree Variable Importance Plot: Predicting Use of Mobile Money", sub="Kenya, 2014: 1000 Respondents, 533 mobile account holders. Unweighted.")


forest <- randomForest(as.factor(everFS)~.,data=fii[complete.cases(fii[,c(2:6,127)]),c(2:6,127)],cp=0.001)
varImpPlot(forest, main = "Random Forest Variable Importance Plot: Predicting Use of Mobile Money", col=4, sub="Kenya, Tanzania, Uganda, 2014: 3008 Respondents, 1076 account holders. Unweighted.")


#hmm, "ever" looks very similar in variable importance vs. differing the time windows.
#likely the signal for 6 (never used) is dominating the nuance of the smaller time variations.
#let's look at a model that just seeks to differentiate 1-5:
fii2 <- fii[fii$minFS<6,c(2:6,126)]
summary(fii2)
hist(fii2$minFS)
tree <- rpart(as.factor(minFS)~.,data=fii2, cp=0.003)
rpart.plot(tree)
barplot(sort(tree$variable.importance), col=4, las=2, cex.names = .7, horiz=T, main="RPart Tree Variable Importance Plot: Predicting Use of Mobile Money", sub="Kenya, 2014: 1000 Respondents, 533 mobile account holders. Unweighted.")
par(mar=c(5,5,5,5))

summary(complete.cases(fii2))
fii3 <- fii2[complete.cases(fii2),]
summary(fii3)
forest <- randomForest(as.factor(minFS)~.,data=fii3, cp=0.001)
varImpPlot(forest, main = "Random Forest Variable Importance Plot: Predicting Use of Mobile Money", col=4, sub="Kenya, Tanzania, Uganda, 2014: 3008 Respondents, 1076 account holders. Unweighted.")
boxplot(fii3$year_of_birth~fii3$minFS,col=4)

tree <- rpart(as.factor(minFS)~.,data=fii3,cp=0.002)
barplot(sort(tree$variable.importance), col=4, las=2, cex.names = .7, horiz=T, main="RPart Tree Variable Importance Plot: Predicting Use of Mobile Money", sub="Kenya, 2014: 1000 Respondents, 533 mobile account holders. Unweighted.")
rpart.plot(tree)
names(tree)
tree$cptable

#7: FII: Frequency Tables

#Friendly Factor Row Names:

#label gender:
levels(fii$gender) <- c("Male","Female")
summary(fii$gender)

#factor marital status:
marital_status = c("Single","Polygmous","Monogomous","Divorced","Separated","Widowed","Living Together","Other","Refused")
length(marital_status)
summary(as.factor(fii$marital_status))
fii$marital_status <- as.factor(fii$marital_status)
levels(fii$marital_status) <- marital_status
summary(fii$marital_status)

#factor up education level:
education_level <- c("None","Primary","Some Secondary","Secondary Complete","Some Secondary Vocational",
                     "Complete Secondary Vocational", "Some Diploma","Diploma Complete","Some College",
                     "Some University","Complete University","Post Graduate","Koranic","Other","Refused")
fii$education_level <- as.factor(fii$education_level)
levels(fii$education_level) <- education_level
summary(fii$education_level)

#Heard of mobile money:
fii$heard_of_mobile_money <- as.factor(fii$heard_of_mobile_money)
levels(fii$heard_of_mobile_money) <- c("Yes","No")
summary(fii$heard_of_mobile_money)

#Biggest Challenge

fii$biggest_challenge_with_mobile_money <- as.factor(fii$biggest_challenge_with_mobile_money)

# Service system down time/ Mtambo wa huduma kuwa haifanyi kazi
# Agent system down time/ Mtambo wa agenti kuwa haifanyi kazi
# Difficulty operating the phone/using menu/  Ugumu wa kutumia simu ukitumia menu
# Unclear transaction charges/fees/ Malipo yasiyoeleweka
# Agent float/cash availability/ Kutopatikana kwa pesa  kwenye agenti
# Contacting customer care/  Kuwasiliana na wanaojali maslahi ya wateja
# Sending to a wrong number/ Kutuma pesa kwa nambari isiyostahili
# Family/friends stealing money/ Jamaa/marafiki kuiba pesa
# Other fraud (Specify)/ Udanganyifu mwingine
# Other (Specify)/ Nyingine (taja)
# DK/Refused

levels(fii$biggest_challenge_with_mobile_money) <- c("Service down time",
                                                     "Agent down time",
                                                     "Operating phone",
                                                     "Unclear Tx fees",
                                                     "Agent float cash avail",
                                                     "Contacting customer care",
                                                     "Sending to wrong number",
                                                     "Family friends stealing money",
                                                     "Other fraud",
                                                     "Other",
                                                     "DK/Refused")
summary(fii$biggest_challenge_with_mobile_money)

#Reason never used
fii$reason_never_used_mobile_money <- as.factor(fii$reason_never_used_mobile_money)
# I do not know what it is/ Sijui hiyo ni  nini
# I do not know how to open one/ Sijui jinsi ya kufungua moja
# I do not have a state ID or other required documents /Sina kitambulisho cha kitaifa /stikababadhi zinginezo zinazotakikana
# There is no point-of-service/agent close to where I live/ Hakuna muhudumu/au ajenti karibu na mahali ninapoishi
# I do not need one, I do not make any transactions/ Sihitaji moja, sifanyi shughuli zozoteza kuhutaji huduma hio
# Registration paperwork is too complicated /Shughuli ya kujiandikisha ni ngumu sana kwangu
# Registration fee is too high /Ada ya kujisajili iko juu
# Using such account is difficult /Kutumia akaunti kama hiyo ni ngumu
# Fees for using this service are too high Ada  ya kutumia huduma hii ni juu sana
# I never have money to make transactions with this service /Sina pesa ya kufanya shughuli na huduma hii
# No one among my friends or family use this service/Hakuna yeyote kati ya marafiki wangu au jamaa hutumia huduma hii
# I do not understand this service; I do not know what I can use it for/Sielewi huduma hii ,sijui nitaitumia vipi
# I do not have a mobile phone /Sina simu ya mkononi
# I do not trust that my money is safe on a mobile money account/Siamini kuwa pesa yangu iko salama katika akaunti ya pesa kwenye simu ya  mkononi
# My husband, family, in-laws do not approve of me having a mobile money account/Mume wangu,familia, shemeji hawakubali nikuwe na akaunti ya pesa ya simu ya mkononi
# It is against my religion// Ni kinyume na dini yangu
# I don’t use because all agents are men/ Situmii kwa sababu maajenti wote ni wanaume
# Mobile money does not provide anything better/any advantage over the financial services I currently use/ Huduma ya pesa  kwenye simu ya mkononi haitoi kitu chochote bora / faida yoyote kuliko huduma za kifedha mimi hutumia kwa sasa
# Other (Specify) Nyingine (Taja)

summary(fii$reason_never_used_mobile_money)
levels(fii$reason_never_used_mobile_money) <- c("Don't know what it is",
                                                "Don't know how to open",
                                                "Don't have ID",
                                                "No POS",
                                                "Don't need",
                                                "Registration too complicated",
                                                "Registration fee too high",
                                                "Too difficult",
                                                "Tx fee too high",
                                                "No money",
                                                "No friends or family using",
                                                "Don't understand",
                                                "No phone",
                                                "Don't trust",
                                                "Family don't approve",
                                                #No one answered this: "Against religion",
                                                "All agents men",
                                                "No advantage over existing",
                                                "Other")
summary(fii$reason_never_used_mobile_money)

#CONVERT ALL "ever used" to Yes/no

#df[ , grepl( "ABC" , names( df ) ) ]

x <- fii[, grepl( "ever_used_mobile_money_for" , names(fii ) )] 
x <- apply(x,2,as.factor)
x[x==1] <- "Yes"
x[x==2] <- "No"
summary(x)
x <- data.frame(x)
fii[, grepl( "ever_used_mobile_money_for" , names(fii ) )] <- x


#now fix all the "how often" questions:

# 1=Daily/ Kila siku
# 2=Weekly/ Kila wiki
# 3=Once in 15 days/ Mara moja kwa siku 15
# 4=Monthly/Kila mwezi
# 5=Once every 3 months/
#   Mara moja kila miezi 3
# 6=Once every 6 months/
#   Mara moja kila miezi 6
# 7=Once a year/Mara moja kwa mwaka
# 8=Almost never/Karibu na hakuna


x <- fii[, grepl( "how_often_use_mobile_money_for" , names(fii ) )] 
#x <- apply(x,2,as.factor)
for (i in 1:ncol(x)){
  x[,i]<- as.factor(x[,i])
  levels(x[,i]) <-c(
               "Daily",
               "Weekly",
               "15 days",
               "Monthly",
               "3 Months",
               "6 Months",
               "Annually",
               "Almost Never"
               )
}
x <- data.frame(x)
fii[, grepl( "how_often_use_mobile_money_for" , names(fii ) )] <- x

#remove "all NA" cols:
fii <- fii[,colSums(is.na(fii))<nrow(fii)]
#remove cols w/>1% NA values:
x <- fii
x <- x[,colSums(is.na(x))<(nrow(x)*.99)]
fii <- x

#Fix up: ever_used_any_mobile_money_service
x <- fii[, grepl( "ever_used_any_mobile_money_service" , names(fii ) )] 
x <- apply(x,2,as.factor)
x[x==1] <- "Yes"
x[x==2] <- "No"
summary(x)
x <- data.frame(x)
fii[, grepl( "ever_used_any_mobile_money_service" , names(fii ) )] <- x

#Fix up: have_account_with_any_mobile_money_service
x <- fii[, grepl( "have_account_with_any_mobile_money_service" , names(fii ) )] 
x <- apply(x,2,as.factor)
x[x==1] <- "Yes"
x[x==2] <- "No"
summary(x)
x <- data.frame(x)
fii[, grepl( "have_account_with_any_mobile_money_service" , names(fii ) )] <- x

#fix up last time w mobile money

# 1=Yesterday/ Jana
# 2=In the past 7 days/
#   Siku 7 zilizopita
# 3=In the past 30 days/
#   Siku 30 zilizopita
# 4=In the past 90 days/
#   Siku 90 zilizopita
# 5=More than 90 days ago/
#   Zaidi ya siku 90 zilizopita
# 6=Never/ Hakuna

x <- fii[, grepl( "last_time_fin_activity_w_mobile_money" , names(fii ) )] 
for (i in 1:ncol(x)){
x[,i] <- as.factor(x[,i])
levels(x[,i])<-c("Yesterday",
                 "7 days",
                 "30 days",
                 "90 days",
                 ">90 days",
                 "Never")
}
summary(x)
x <- data.frame(x)
fii[, grepl( "last_time_fin_activity_w_mobile_money" , names(fii ) )] <- x

#fix up last time w acccount

# 1=Yesterday/ Jana
# 2=In the past 7 days/
#   Siku 7 zilizopita
# 3=In the past 30 days/
#   Siku 30 zilizopita
# 4=In the past 90 days/
#   Siku 90 zilizopita
# 5=More than 90 days ago/
#   Zaidi ya siku 90 zilizopita
# 6=Never/ Hakuna

x <- fii[, grepl( "last_time_financial_activity_using_account" , names(fii ) )] 
#x <- apply(x,2,as.factor)
for (i in 1:ncol(x)){
  x[,i] <- as.factor(x[,i])
  levels(x[,i])<-c("Yesterday",
                   "7 days",
                   "30 days",
                   "90 days",
                   ">90 days",
                   "Never")
}
summary(x)
x <- data.frame(x)
fii[, grepl( "last_time_financial_activity_using_account" , names(fii ) )] <- x


#fix up how_long_using_mobile_money

# Less than 1 month/ Chini ya mwezi 1
# More than 1 month to 3 months/ Zaidi ya mwezi 1 hadi miezi 3
# More than 3 months to 6 months/ Zaidi ya miezi 3 ha 6
# More than 6 months to 1 year/ Zaidi ya miezi 6 hadi mwaka 1
# More than 1 year/ Zaidi yam waka 1

levels(fii$how_long_using_mobile_money) <- c("<1 month",
                                             "1-3 months",
                                             "3-6 months",
                                             "6-12 months",
                                             ">12 months")


#Now fix up minFS
# 1=Yesterday/Jana
# 2=In the past 7 days/ Katika siku saba zilizopita
# 3=In the past 30 days/ Katika siku thelathini zilizopita
# 4=In the past 90 days/ Katika siku tisini zilizopita
# 5=More than 90 days ago/ Zaidi ya siku tisini zilizopita
# 6=Never/ Sijawahi

fii$minFS <- as.factor(fii$minFS)
levels(fii$minFS) <- c("1 day",
                       "7 days",
                       "30 days",
                       "90 days",
                       ">90 days",
                       "Never")
summary(fii$minFS)

fii$everFS <- as.factor(fii$everFS)
fii$everFS[fii$everFS==0] <- "No"
fii$everFS[fii$everFS==1] <- "Yes"
summary(fii$everFS)


#Now add friendly names for services:
x<- fii
# Safaricom M-Pesa
# Airtel Money
# YU Cash
# Orange Money
# Tangaza
# Mobicash
# Equitel

names(x) <- sub("serv1", "MPesa", names(x) )
names(x) <- sub("serv2", "AirtelMoney", names(x) )
names(x) <- sub("serv3", "YUCash", names(x) )
names(x) <- sub("serv4", "OrangeMoney", names(x) )
names(x) <- sub("serv5", "Tangaza", names(x) )
names(x) <- sub("serv6", "MobiCash", names(x) )
names(x) <- sub("serv7", "Equitel", names(x) )

#Now create table

cgroup <- "Responses by Country"
n.cgroup <- 4 #Countries, p-value
rgroup <- names(fii)

vars <- NULL
n.rgroup <- NULL
curRows <- NULL


tempData <- fii

j <- 2 #response varaible, country
response <- tempData[,j]

for(i in 1:ncol(tempData)){#start @2 to avoid X column
  print(i)
  tempCol <- tempData[,i]
  
  curRows <- getDescriptionStatsBy(tempCol, response, useNA='ifany', html=TRUE, show_all_values=TRUE, statistics=TRUE)
  #curRows <- as.matrix(curRows, nrow=length(names(tempData[,i])), ncol=3)
  #curRows <- data.frame(curRows)
  if(i>1){ #use colnames defined by first column
    colnames(curRows) <- colnames(vars)
  }
  #rownames(curRows[1:length(levels(tempData[,i])),] <- levels(tempdata[i,])
  vars <- do.call(rbind, list(vars, curRows))
  #create rowcount for htmlTable:
  n.rgroup <- c(n.rgroup, nrow(curRows))
}
#remove first row:
vars <- vars[-1,]
n.rgroup <- n.rgroup[-1]
rgroup <- rgroup[-1]

htmlTable(vars, 
          rowlabel="", 
          cgroup=cgroup, 
          n.cgroup=n.cgroup, 
          rgroup=rgroup, 
          n.rgroup = n.rgroup, 
          caption="The Data Guild, Project DoPLER: Kenya, Tanzania, Uganda, 8895 Respondents.<sup>&dagger;</sup> ", 
          tfoot="<sup>&dagger;</sup> n=8955 observations, FII",
          ctable=TRUE)
#summary(fii)
#names(fii)
#5: Conclusions


View(vars)
