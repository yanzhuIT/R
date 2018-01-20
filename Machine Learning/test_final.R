# what we need: data.table for reading data set
# dplyr for manipulating data frame
# magrittr for pipeline command
# caret/gam/randomForest/randomForestSRC for feature selection
# xgboost for modelin
# mlr for optimizing model tune
# h2o for ensemble
##--------------------------------------------------------------------------##
# memory management for big data
memory.limit()
## current memory utility
memory.size()
## set new memory limitation for R, remember to do it every opening time
memory.limit(102400)
# load required package
library(dplyr)
library(data.table)
library(magrittr)
library(caret)
library(randomForest)
library(xgboost)
## garbage collection, after remove()
gc()
## -------------------------------------------------------------------------##
# load test data and merge

## train_v2 is the refreshed data set, we use it as our original training set
test = fread("sample_submission_v2.csv", stringsAsFactors = TRUE)
#test = as.data.frame(test)

## load and combine member data
members = fread("members_v3.csv", stringsAsFactors = TRUE)
#members = as.data.frame(members)
### clean the data 
members = filter(members, between(members$bd,0,100))
## inner join training and members
test = left_join(test, members, by = "msno")
remove(members)
gc()
test = test %>% mutate_if(is.character, factor)


## load and combine transaction information
transactions = fread("transactions.csv", stringsAsFactors = TRUE) 
# transactions = as.data.frame(transactions)
transactions2 = fread("transactions_v2.csv", stringsAsFactors = TRUE)
# transactions2 = as.data.frame(transactions2)
transactions = bind_rows(transactions, transactions2)
remove(transactions2)
gc()
transactions = transactions %>% mutate_if(is.character, factor)
### clean transactions
transactions = filter(transactions,transactions$membership_expire_date >= transactions$transaction_date)
## combine test and transactions
test = left_join(test,transactions, by = "msno")
remove(transactions)
gc()
test = test %>% mutate_if(is.character, factor)


## load and combine user logs information
user_logs = fread("user_logs.csv", stringsAsFactors = TRUE)
#user_logs = as.data.frame(user_logs)
user_logs2 = fread("user_logs_v2.csv", stringsAsFactors = TRUE)
#user_logs2 = as.data.frame(user_logs2)
## combine user logs information
user_logs = bind_rows(user_logs, user_logs2)
remove(user_logs2)
gc()
### randomly select rows from user_logs
set.seed(100)
user_logs = sample_frac(user_logs,0.01)
## convert msno: character to factor
user_logs <- user_logs %>% mutate_if(is.character, factor)
## left join training and logs
test =left_join(test, user_logs, by = "msno")
remove(user_logs)
gc()
test = test %>% mutate_if(is.character, factor)



##---------------------predict---------------------------##
#-------------------
test_msno = test_mutate[,1]
test_data = test_mutate[,-1]
test_data_final = select(test_data, final_var, is_churn)
test_data_final[which(test_data_final$bd == 0),]$bd = NA
test_data_final = na.roughfix(test_data_final)
test_data_final1 = data.matrix(test_data_final[, -14]) 
p1 = predict(xgb, test_data_final1)
submission1 = data.frame(msno = test_msno, is_churn = p1)
submission1 = group_by(submission1,msno)
submitting1 = summarise(submission1, mean(is_churn))
names(submitting1)[2] = "is_churn"
write.csv(submitting1, file = "submission.csv", row.names = FALSE)


