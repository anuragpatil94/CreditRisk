
actual_data<-read.csv("loan.csv")
library(stringr)
library(caTools)
attach(actual_data)


##############################################################################################
#                                   ADDING COUNTY COLUMN
##############################################################################################
##FORMATTING THE Zipcode-County Dataset
county_data<-read.csv("county.csv")
zipcode<-substr(county_data$ZIP.Code, 1, 3)
county_filter<-word(county_data$county,start = 1,end = 2,sep = " ")
county_filter<-as.character(county_filter)
zip_county_dataframe<-data.frame(cbind(zipcode,county_filter))
zipcode<-substr(actual_data$zip_code,1,3)
new_zipcode<-as.numeric(zipcode)
bindData<-cbind(actual_data, new_zipcode)
zip_county_dataframe$county_filter<-as.character(zip_county_dataframe$county_filter)
zip_county_dataframe$zipcode<-as.numeric(zip_county_dataframe$zipcode)
class(zip_county_dataframe$zipcode)
bindData$new_zipcode<-as.numeric(bindData$new_zipcode)
class(bindData$new_zipcode)
actual_data <- merge(x=bindData, y=zip_county_dataframe,by.x = "new_zipcode", by.y = "zipcode")

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

detach(actual_data)

attach(actual_dataset)

prediction_dataset<-data.frame(
    annual_inc,
    grade,
    sub_grade,
    loan_amnt,
    verification_status,
    home_ownership,
    addr_state,
    dti,
    delinq_2yrs,
    loan_status
)
#head(prediction_dataset)


#summary(prediction_dataset)
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


prediction_tests<-prediction_dataset





#?sample.split
prediction_tests$loan_status<-as.numeric(as.factor(prediction_tests$loan_status))
prediction_tests$addr_state<-as.numeric(as.factor(prediction_tests$addr_state))
prediction_tests$grade<-as.numeric(as.factor(prediction_tests$grade))
prediction_tests$home_ownership<-as.numeric(as.factor(prediction_tests$home_ownership))
prediction_tests$verification_status<-as.numeric(as.factor(prediction_tests$verification_status))
prediction_tests$dti_level<-as.numeric(as.factor(prediction_tests$dti_level))
prediction_tests$sub_grade<-as.numeric(as.factor(prediction_tests$sub_grade))
prediction_tests$loan_amnt_cat<-as.numeric(as.factor(prediction_tests$loan_amnt_cat))
prediction_tests$income_level<-as.numeric(as.factor(prediction_tests$income_level))


split = sample.split(prediction_tests$loan_status, SplitRatio = .75) #take a sample of the data
loantrain <- subset(prediction_tests, split == TRUE)
#summary(loantrain)
loantest <- subset(prediction_tests, split == FALSE)
#summary(loantest)
head(loantrain)
##########################################
library(C50)
require(C50)
#install.packages("cwhmisc")
#source("R/C5.0.graphviz.R")
#C5.0.graphviz ( m, "c55.png", fontname ='Arial',  col.draw ='black', col.font ='blue',  col.conclusion ='lightpink',  col.question = 'grey78', shape.conclusion ='box3d',  shape.question ='diamond', bool.substitute = c('None',     'yesno','truefalse','TF'), prefix=FALSE, vertical=TRUE )
m<-C5.0(loantrain[,c(1,3,5,6,7,12,13)],factor(loantrain[,10]),trials = 10)
summary(m)

#plot(m)
?predict
#summary(loantest)
result<-predict(m,loantest,type = "class")
library(caret)
confusionMatrix(result,loantest$loan_status)
rTable<-table(predict=result,test=loantest[,10])
#View(rTable)
head(diag(rTable))
accuracy<-(sum(diag(rTable))/nrow(loantest))
accuracy
