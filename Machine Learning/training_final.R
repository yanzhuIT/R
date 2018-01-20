# what we need: data.table for reading data set
# dplyr for manipulating data frame
# magrittr for pipeline command
# caret/gam/randomForest/randomForestSRC for feature selection
# xgboost for modelin
# mlr for optimizing model tune
# h2o for ensemble

##-------------------------memory management for big data--------------------------------##
memory.limit()
## current memory utility
memory.size()
## set new memory limitation for R, remember to do it every opening time
memory.limit(102400)
## garbage collection, after remove()
gc()

## --------------------------data handling-----------------------------------------------##
# load training data and merge
library(dplyr)
library(data.table)
library(magrittr)

## train_v2 is the refreshed data set, we use it as our original training set
original = fread("train_v2.csv", stringsAsFactors = TRUE)

## load and combine member data
members = fread("members_v3.csv", stringsAsFactors = TRUE)
### clean the data 
members = filter(members, between(members$bd,0,100))

## inner join training and members
training = inner_join(original, members, by = "msno")
remove(original, members)
training = training %>% mutate_if(is.character, factor)
gc()

## load and combine transaction information
transactions = fread("transactions.csv", stringsAsFactors = TRUE) 
transactions2 = fread("transactions_v2.csv", stringsAsFactors = TRUE)
transactions = bind_rows(transactions, transactions2)
remove(transactions2)
gc()
transactions = transactions %>% mutate_if(is.character, factor)
### clean transactions
transactions = filter(transactions,transactions$membership_expire_date >= transactions$transaction_date)
## combine training and transactions
training = inner_join(training,transactions, by = "msno")
remove(transactions)
gc()
training = training %>% mutate_if(is.character, factor)

## load user logs information
user_logs = fread("user_logs.csv", stringsAsFactors = TRUE)
user_logs2 = fread("user_logs_v2.csv", stringsAsFactors = TRUE)
## combine user logs information
user_logs = bind_rows(user_logs, user_logs2)
remove(user_logs2)
gc()
### randomly select rows from user_logs
set.seed(100)
user_logs_reduce = sample_frac(user_logs,0.05)
remove(user_logs)
gc()
## convert msno: character to factor
user_logs_reduce <- user_logs_reduce %>% mutate_if(is.character, factor)
## inner join training and logs
training = inner_join(training, user_logs_reduce, by = "msno")
remove(user_logs_reduce)
gc()
set.seed(100)
## continte to random selection to reduce data
training = sample_frac(training,0.1)
# get complete records
training_complete_obs <- training[complete.cases(training),] 
gc()
training_final = training_complete_obs %>% mutate_if(is.character, factor)
## clean data again
training_final = unique(training_final)


##--------------------------------feature selection---------------------------------##
##Notes: these two algorithms by using R package are just reference for final feature selection
dim(training)
library(caret)
library(randomForest)
training_for_fs = sample_frac(training_complete_obs, 0.001)
training_for_fs = training_for_fs[, -1]
# feature selection----select by filter
### create control parameter of sbf
sbfControls_fs <- sbfControl(functions = rfSBF, method = 'cv', repeats = 5)
fs_filter <- sbf(x = training_for_fs[,-1], 
                 y = training_for_fs[,1], 
                 sbfControl = sbfControls_fs)
filter_var = fs_filter$optVariables

#########################################
#feature selection----wrapper method
training_for_fs_reduce = sample_frac(training_for_fs, 0.1)
rfeControls_fs <- rfeControl(functions = rfFuncs, method = 'cv', repeats = 5)
fs_wrapper <- rfe(x = training_for_fs[,-1], 
                  y = training_for_fs[,1], 
                  sizes = seq(7,21,2), 
                  rfeControl = rfeControls_fs)
wrapper_var = fs_wrapper$optVariables
print(fs_wrapper)
plot(fs_wrapper, type = c('g','o'))

##-------------------------final feature selection including experience analysis ------#
final_var = c("transaction_date", 
              "membership_expire_date", "is_cancel", 
              "is_auto_renew", "total_secs", "registration_init_time",
              "num_25", "num_75", "num_100","num_unq", "registered_via", "bd", "city"
              )
              
training_fs = select(training_final_mutate,final_var, is_churn)

## for bd, do further optimization
training_fs[which(training_fs$bd == 0),]$bd = NA
training_fs = na.roughfix(training_fs)


##------------------------xgboost: tuning parameters-----------------#
library(mlr)
set.seed(150)
training_final_reduce = sample_frac(training_fs, 0.01)
trainTask <- makeClassifTask(data = training_final_reduce,target = "is_churn")
#getParamSet("classif.xgboost")
xg_set <- makeLearner("classif.xgboost", predict.type = "response")
xg_set$par.vals <- list(
  objective = "binary:logistic",
  eval_metric = "error",
  nrounds = 250
)

#define parameters for tuning
xg_ps <- makeParamSet(
  makeIntegerParam("nrounds",lower=200,upper=600),
  makeIntegerParam("max_depth",lower=3,upper=20),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)
#define search function 
#do 100 iterations
rancontrol <- makeTuneControlRandom(maxit = 50L)
# 3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)
# tune parameters
xg_tune <- tuneParams(learner = xg_set, task = trainTask, 
                      resampling = set_cv,
                      measures = acc,
                      par.set = xg_ps, 
                      control = rancontrol
)
#set parameters
xg_new <- setHyperPars(learner = xg_set, par.vals = xg_tune$x)
print(xg_new)

#----------------------XGBoost modeling----------------------------------

library(xgboost)
#training_final_reduce = sample_frac(training_fs, 0.01)
training_final_reduce_object = data.matrix(training_final_reduce[, 14])
training_final_reduce_variable = data.matrix(training_final_reduce[,-14]) 

xgb = xgboost(data = training_final_reduce_variable, 
              label = training_final_reduce_object,
              objective="binary:logistic",
              nrounds=476,
              max_depth=8,
              lambda=0.564,
              eta=0.0515,
              subsample=0.614,
              min_child_weight=3.93,
              colsample_bytree=0.581,
              seed = 1,
              nfold = 10
)
names <- dimnames(data.matrix(training[,-14]))[[2]]
importance_matrix <- xgb.importance(names, model = xgb)
print(importance_matrix)


