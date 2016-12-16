library(rpart)
library(rpart.plot)  			# Enhanced tree plots
library(rattle)           # Fancy tree plot
library(RColorBrewer)     # colors needed for rattle

rm(list=ls())
final1 <- read.csv("C:/Courses/CS-513/project/cart_dataset.csv")

set.seed(37)
trimming_data <- sample(1:nrow(final1), nrow(final1)/50)
final2 <- final1[trimming_data,]
final3 <- final2[,-1]


############### Catergorizing#################

# ******Categorizing VerificationStatus
vrf <- ifelse(final3$verification_status == "Not Verified","Not vrf","Yes vrf")
final4 <- data.frame(final3,vrf)
final5 <- final4[,-5]

# ******categorizing Loan Amount
final5$loan_amnt_cat[final5$loan_amnt >= 500 & final5$loan_amnt <= 10000] <- "Level 1"
final5$loan_amnt_cat[final5$loan_amnt >= 10001 & final5$loan_amnt <= 20000] <- "Level 2"
final5$loan_amnt_cat[final5$loan_amnt >= 20001 & final5$loan_amnt <= 35000] <- "Level 3"

# ******categorizing Home Ownership

final5$home_ownership_cat[final5$home_ownership %in% c("ANY","MORTGAGE","NONE","OTHER")] <- "Low Asset"
final5$home_ownership_cat[final5$home_ownership %in% c("RENT")] <- "Medium Asset"
final5$home_ownership_cat[final5$home_ownership %in% c("OWN")] <- "High Asset"

# ******categorizing INCOME LEVEL

final5$income_level[final5$annual_inc>=100000]<-"HIGH"
final5$income_level[final5$annual_inc>=60000 & final5$annual_inc<100000]<-"MEDIUM"
final5$income_level[final5$annual_inc<60000]<-"LOW"

# ******categorizing DTI
final5$dti_level[final5$dti<=10]<-"LOW"
final5$dti_level[final5$dti>10 & final5$dti<20]<-"MEDIUM"
final5$dti_level[final5$dti>=20]<-"HIGH"



############################
#write.csv(final5, file = "C:/Courses/CS-513/project/final5.csv")

# Removing Duplicate columns who's categorized variables are available
final6<-final5[,-c(1,3,4,5)]
final7<-final6[,-1]

# Final Data set
final8<-na.omit(final7)


set.seed(27)
# creating Training and Test datasets
train= sample(1:nrow(final8), nrow(final8)/2)
test=-train
training_data= final8[train, ]
testing_data= final8[test, ]

# Applying CART for classification using rpart
dtree<-rpart(loan_class~. ,data=training_data, control=rpart.control(minsplit=2, minbucket=1, cp=0.001))
dtree


# much fancier graph
fancyRpartPlot(dtree)

summary(dtree)

# predicting the training model just for nothing
prediction <- predict(dtree, training_data, type="class")
table(prediction,training_data[ ,5])


# predicting the test data set on model
pred <- predict(dtree, testing_data, type = "class")
xtab<-table(pred,testing_data[,5])
accuracy=(sum(diag(xtab))/nrow(testing_data))
accuracy
