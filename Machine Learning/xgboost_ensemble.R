h2o.init()
training_final_reduce = sample_frac(training_fs, 0.1)
x <- names(training_final_reduce)[which(names(training_final_reduce)!="is_churn")]
y <- "is_churn"
train <- as.h2o(training_final)
nfolds <- 5
eta_opt <- c(0.1,0.01,0.001)
learn_rate_opt <- c(0.01, 0.03)
max_depth_opt <- c(2, 4, 6, 8, 10)
sample_rate_opt <- c(0.5, 0.75, 0.9, 1.0)
col_sample_rate_opt <- c(0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)
hyper_params <- list(eta = eta_opt,
                     learn_rate = learn_rate_opt,
                     max_depth = max_depth_opt,
                     sample_rate = sample_rate_opt,
                     col_sample_rate = col_sample_rate_opt)
search_criteria <- list(strategy = "RandomDiscrete",
                        max_models = 3,
                        seed = 1)
xgb_grid <- h2o.grid(algorithm = "xgboost",
                     grid_id = "xgb_grid_kk",
                     x = x,
                     y = y,
                     training_frame = train,
                     ntrees = 20,                     
                     seed = 1,
                     nfolds = nfolds,
                     fold_assignment = "Modulo",
                     keep_cross_validation_predictions = TRUE,
                     hyper_params = hyper_params,
                     search_criteria = search_criteria)
ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                model_id = "ensemble_xgb_grid_kk",
                                base_models = xgb_grid@model_ids
                                )
test_msno = test[,1]
test_data = test[,-1]
test_data_final = select(test_data, final_var, is_churn)
test_data_final[which(test_data_final$bd == 0),]$bd = NA
test_data_final = na.roughfix(test_data_final)
test4 = as.h2o(test_data_final)
p4 = h2o.predict(ensemble, test4)
p4new = as.vector(p4)
submission = data.frame(msno = test_msno, is_churn = p4new)
submission = group_by(submission,msno)
submitting = summarise(submission, mean(is_churn))
names(submitting)[2] = "is_churn"
submitting[which(submitting$is_churn < 0 | submitting$is_churn >1),]$is_churn = NA
submitting = na.roughfix(submitting)
write.csv(submitting, file = "submission.csv", row.names = FALSE)

