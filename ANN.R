actual_data<-read.csv("loan.csv")

library(stringr)
library(caTools)
attach(actual_data)
#search()
##############################################################################################
#                                   ADDING COUNTY COLUMN
##############################################################################################

##FORMATTING THE Zipcode-County Dataset
county_data<-read.csv("county.csv")
#head(county_data)
zipcode<-substr(county_data$ZIP.Code, 1, 3)
county_filter<-word(county_data$county,start = 1,end = 2,sep = " ")
zip_county_dataframe<-data.frame(cbind(zipcode,county_filter))
#head(zip_county_dataframe)
zip_county_dataframe$zipcode<-as.numeric(zip_county_dataframe$zipcode)
zipcode<-substr(actual_data$zip_code,1,3)
#head(zipcode)
new_zipcode<-as.numeric(zipcode)
#head(new_zipcode)
bindData<-cbind(actual_data, new_zipcode)
#head(bindData)

class(zip_county_dataframe$zipcode)
bindData$newZip_code<-as.numeric(bindData$new_zipcode)
class(bindData$new_zipcode)
actual_data <- merge(x=bindData, y=zip_county_dataframe,by.y = "zipcode", by.x = "new_zipcode")
#head(actual_data)

##############################################################################################
#                               SOME IMPORTANT COLUMNS
##############################################################################################

actual_dataset<-data.frame(
    member_id,
    loan_amnt,
    funded_amnt,
    funded_amnt_inv,
    int_rate,
    emp_length,
    term,
    installment,
    grade,
    sub_grade,
    home_ownership,
    annual_inc,
    verification_status,
    loan_status,
    purpose,
    zip_code,
    actual_data$county,
    addr_state,
    dti,
    delinq_2yrs,
    earliest_cr_line,
    inq_last_6mths,
    mths_since_last_delinq,
    issue_d,
    revol_util
)


##############################################################################################
#                                           PREDICTION
#############################################################################################
attach(actual_data)
names(actual_data)
prediction_dataset<-data.frame(

    annual_inc,
    grade,
    loan_amnt,
    sub_grade,
    verification_status,
    home_ownership,
    actual_data$county_filter,
    addr_state,
    dti,
    delinq_2yrs,
    loan_status
)
head(prediction_dataset)





bad<- c("Charged Off ",
                    "Default",
                    "Does not meet the credit policy. Status:Charged Off",
                    "In Grace Period",
                    "Default Receiver",
                    "Late (16-30 days)",
                    "Late (31-120 days)")
good<-c("Current","Issued","Fully Paid","Does not meet the credit policy. Status:Fully Paid")

prediction_dataset$loan_status <- ifelse(prediction_dataset$loan_status %in% bad, 0,
                                         ifelse(prediction_dataset$loan_status=="", NA, 1)
)
levels(prediction_dataset$loan_status)
##################################################################################
#INCOME LEVEL
#
#prediction_dataset$income_level<-NULL
class(prediction_dataset$income_level)
prediction_dataset$income_level[prediction_dataset$annual_inc>=100000]<-"HIGH"
prediction_dataset$income_level[prediction_dataset$annual_inc>=60000 & prediction_dataset$annual_inc<100000]<-"MEDIUM"
prediction_dataset$income_level[prediction_dataset$annual_inc<60000]<-"LOW"


##############################################
#DTI
#summary(prediction_dataset)
prediction_dataset$dti_level<-NULL
prediction_dataset$dti_level[prediction_dataset$dti<=10]<-"LOW"
prediction_dataset$dti_level[prediction_dataset$dti>10 & prediction_dataset$dti<20]<-"MEDIUM"
prediction_dataset$dti_level[prediction_dataset$dti>=20]<-"HIGH"

#########################
# ****categorizing Loan Amount
prediction_dataset$loan_amnt_cat[prediction_dataset$loan_amnt >= 500 & prediction_dataset$loan_amnt <= 10000] <- "Level 1"
prediction_dataset$loan_amnt_cat[prediction_dataset$loan_amnt >= 10001 & prediction_dataset$loan_amnt <= 20000] <- "Level 2"
prediction_dataset$loan_amnt_cat[prediction_dataset$loan_amnt >= 20001 & prediction_dataset$loan_amnt <= 35000] <- "Level 3"

# ****categorizing Home Ownership
#low=non, other, any, morgage
#medium=rent
#high=own
prediction_dataset$home_ownership_cat[prediction_dataset$home_ownership %in% c("ANY","MORTGAGE","NONE","OTHER")] <- "Low Asset"
prediction_dataset$home_ownership_cat[prediction_dataset$home_ownership %in% c("RENT")] <- "Medium Asset"
prediction_dataset$home_ownership_cat[prediction_dataset$home_ownership %in% c("OWN")] <- "High Asset"
#######################################3
#delinq_2yrs
prediction_dataset$delinq_2yrs_cat[prediction_dataset$delinq_2yrs==0]<-"1"
prediction_dataset$delinq_2yrs_cat[prediction_dataset$delinq_2yrs >0 & prediction_dataset$delinq_2yrs<=2]<-"2"
prediction_dataset$delinq_2yrs_cat[prediction_dataset$delinq_2yrs >2 & prediction_dataset$delinq_2yrs<=5]<-"3"
prediction_dataset$delinq_2yrs_cat[prediction_dataset$delinq_2yrs >5 & prediction_dataset$delinq_2yrs<=10]<-"4"
prediction_dataset$delinq_2yrs_cat[prediction_dataset$delinq_2yrs >10 & prediction_dataset$delinq_2yrs<=15]<-"5"
prediction_dataset$delinq_2yrs_cat[prediction_dataset$delinq_2yrs >15 & prediction_dataset$delinq_2yrs<=20]<-"6"
prediction_dataset$delinq_2yrs_cat[prediction_dataset$delinq_2yrs >20]<-"6"



prediction_test<-prediction_dataset

completeFun <- function(data, desiredCols) {
    completeVec <- complete.cases(data[, desiredCols])
    return(data[completeVec, ])
}

summary(prediction_test)
prediction_test<-completeFun(data = prediction_test,desiredCols = "annual_inc")
prediction_test<-completeFun(data = prediction_test,desiredCols = "delinq_2yrs")

head(prediction_test)
a<-which(prediction_test$dti==9999)
prediction_test$dti[1:5]
prediction_test$dti[c(604245,613559)]<-1092
mmnorm <-function(x) {z<-((x-min(x))/(max(x)-min(x)));return(z) }

prediction_test$loan_amnt<-mmnorm(prediction_test$loan_amnt)
prediction_test$dti<-mmnorm(prediction_test$dti)
summary(prediction_test)





prediction_test$income_level<-as.numeric(as.factor(prediction_test$income_level))
prediction_test$dti_level<-as.numeric(as.factor(prediction_test$dti_level))
prediction_test$grade<-as.numeric(prediction_test$grade)
prediction_test$addr_state<-as.numeric(prediction_test$addr_state)
prediction_test$home_ownership<-as.numeric(as.factor(prediction_test$home_ownership))
prediction_test$loan_status<-as.numeric(prediction_test$loan_status)
prediction_test$loan_amnt_cat<-as.numeric(as.factor(prediction_test$loan_amnt_cat))
prediction_test$delinq_2yrs_cat<-as.numeric((as.factor(prediction_test$delinq_2yrs_cat)))
prediction_test$verification_status<-as.numeric((as.factor(prediction_test$verification_status)))
prediction_test$sub_grade<-as.numeric((as.factor(prediction_test$sub_grade)))

?sample.split
split = sample.split(prediction_test$loan_status, SplitRatio = .75) #take a sample of the data
loantrain <- subset(prediction_test, split == TRUE)
summary(loantrain)
loantest <- subset(prediction_test, split == FALSE)
head(loantrain)

#install.packages("neuralnet")
require(neuralnet)
rate<-0
wrong<-0

summary(loantrain$loan_status)


?neuralnet
detach(actual_data)
search()
z<-neuralnet(formula =loantrain$loan_status~
                 loantrain$annual_inc+
                 loantrain$grade+
                 loantrain$loan_amnt_cat+
                 loantrain$dti_level+
                 loantrain$verification_status+
                 loantrain$home_ownership+
                 loantrain$addr_state
             ,
             data=loantrain,
             hidden = 5,threshold = 0.01)

plot(z)

#net.result1 <- compute(net.ArrDelay, subset(test, select=-ArrDelay_cat))
#fit <- round(net.result1$net.result, digits = 0)
#results <- cbind(test$ArrDelay_cat, fit)
#wrong <- results[,1]!=results[,2]
#rate <- sum(wrong)/length(wrong)
attach(loantest)
loantest<-data.frame(
    annual_inc,
    grade,

    loan_amnt_cat,
    dti_level,

    verification_status,
    home_ownership,
    addr_state,
    loan_status
)
summary(loantest)
y<-compute(z,loantest[,1:7])
fit <- round(y$net.result, digits = 0)
#head(fit)
results <- cbind(loantest$loan_status, fit)
#head(results,n = 20)
wrong <- results[,1]!=results[,2]
#head(wrong)

rate<-(length(which(wrong==FALSE))/length(wrong))

#rate <- sum(wrong)/length(wrong)
rate



