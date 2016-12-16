actual_data<-read.csv("loan.csv")

library(stringr)
library(caTools)
attach(actual_data)



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

    addr_state,
    dti,
    delinq_2yrs,
    earliest_cr_line,
    inq_last_6mths,
    mths_since_last_delinq,
    issue_d,
    revol_util
)
detach(actual_data)

attach(actual_dataset)
summary(actual_dataset)
names(actual_data)

prediction_dataset<-data.frame(

    annual_inc,
    grade,
    loan_amnt,
    loan_status,
    verification_status,
    home_ownership,
    addr_state,
    dti
)

prediction_dataset$annual_inc[is.na(prediction_dataset$annual_inc)]<-75028

##################################################################################
#INCOME LEVEL
##################################################################################
prediction_dataset$income_level<-NULL
class(prediction_dataset$income_level)
prediction_dataset$income_level[prediction_dataset$annual_inc>=100000]<-"HIGH"
prediction_dataset$income_level[prediction_dataset$annual_inc>=60000 & prediction_dataset$annual_inc<100000]<-"MEDIUM"
prediction_dataset$income_level[prediction_dataset$annual_inc<60000]<-"LOW"

prediction_dataset$annual_inc<-NULL
##################################################################################
#DTI
##################################################################################
prediction_dataset$dti_level<-NULL
prediction_dataset$dti_level[prediction_dataset$dti<=10]<-"LOW"
prediction_dataset$dti_level[prediction_dataset$dti>10 & prediction_dataset$dti<20]<-"MEDIUM"
prediction_dataset$dti_level[prediction_dataset$dti>=20]<-"HIGH"

prediction_dataset$dti<-NULL
##################################################################################
#categorizing Home Ownership
##################################################################################
prediction_dataset$home_ownership_cat[prediction_dataset$home_ownership %in% c("ANY","MORTGAGE","NONE","OTHER")] <- "Low Asset"
prediction_dataset$home_ownership_cat[prediction_dataset$home_ownership %in% c("RENT")] <- "Medium Asset"
prediction_dataset$home_ownership_cat[prediction_dataset$home_ownership %in% c("OWN")] <- "High Asset"

prediction_dataset$home_ownership<-NULL
prediction_tests<-prediction_dataset
prediction_tests$loan_result<-prediction_tests$loan_status
prediction_tests$loan_status<-NULL
prediction_tests$income_level<-as.numeric(as.factor(prediction_tests$income_level))
prediction_tests$income_level[which(is.na(prediction_tests$income_level))]<-0

##################################################################################
mmnorm <-function(x) {z<-((x-min(x))/(max(x)-min(x)));return(z) }
##################################################################################
data_new<-cbind(grade=mmnorm(as.numeric(factor(prediction_tests$grade))),
                loan_amnt=mmnorm(prediction_tests$loan_amnt),
                verification_status=mmnorm(as.numeric(factor(prediction_tests$verification_status))),
                addr_state=mmnorm(as.numeric(factor(prediction_tests$addr_state))),
                income_level=mmnorm(prediction_tests$income_level),
                dti_level=mmnorm(as.numeric(factor(prediction_tests$dti_level))),
                home_ownership_cat=mmnorm(as.numeric(factor(prediction_tests$home_ownership_cat))),
                loan_result=as.character(prediction_tests$loan_result)

)
head(data_new)
#*****taking 20000 entries*******
idx1<-seq(1:20000)
data_new<-data_new[idx1,]
#****sampling data**************
idx<-sample(nrow(data_new),as.integer(.70*nrow(data_new)))
#****training  & test dataset***********
training<-data_new[idx,]
test<-data_new[-idx,]


library(class)
####to find proper k value::
#running knn 50 time for itterative  k   starting from k=1 to k=20
# here which k's average error rate is minimumm,that k is best.
for (j in 1:40){
    counter<- 0
    total<-0
    for (i in 1:50) {
        newpredict<-knn(training[,-8],test[,-8],training[,8],k <- j)
        newresults<-cbind(test,as.character(newpredict) )
        wrong<-newresults[,8]!=newresults[,9]
        rate<-sum(wrong)/length(wrong)
        rates<-rbind(rate,rate)
        total<-total+rate
        counter<-counter+1
    }
    print(j)
    avg=total/counter
    print(avg)
}
######################
#******applying knn***************
    newpredict<-knn(training[,-8],test[,-8],training[,8],k=10)
    newresults<-cbind(test,as.character(newpredict) )

    head(newresults)
    table(newresults[,8],newresults[,9])
    #Checking accuracy
    results<-cbind(newresults[,8],newresults[,9])
    head(results)
    wrong <- results[,1]!=results[,2]
    rate<-(length(which(wrong==FALSE))/length(wrong))
    #rate <- sum(wrong)/length(wrong)
    rate
