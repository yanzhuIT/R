##----------------ensemble learning------------------------------
## need JDK 64 bit,JDK 7 or JDK 8, not JDK 9
## open needed port
## 
# The following two commands remove any previously installed H2O packages for R.

if ("package:h2o" %in% search()) { detach("package:h2o", unload=TRUE) }
if ("h2o" %in% rownames(installed.packages())) { remove.packages("h2o") }

# Next, we download packages that H2O depends on.
pkgs <- c("RCurl","jsonlite")
for (pkg in pkgs) {
  if (! (pkg %in% rownames(installed.packages()))) { install.packages(pkg) }
}

# Now we download, install and initialize the H2O package for R.
install.packages("h2o", type="source", repos="http://h2o-release.s3.amazonaws.com/h2o/rel-wheeler/2/R")

# Finally, let's load H2O and start up an H2O cluster
library(h2o)
h2o.init()


## prepare train
training_final_reduce = sample_frac(training_fs, 0.01)
x <- names(training_final_reduce)[which(names(training_final_reduce)!="is_churn")]
y <- "is_churn"

##--------------------h20-------------------------------------
## Import Data to h2o
train <- as.h2o(training_final_reduce)
nfolds = 5

#--------gbm
gbm <- h2o.gbm(x = x, y = y,
                training_frame = train,
                seed = 1,
                nfolds = nfolds,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE
)

#--------glm
glm <- h2o.glm(x = x, y = y,
                training_frame = train,
                seed = 1,
                nfolds = nfolds,
                fold_assignment = "Modulo",
                keep_cross_validation_predictions = TRUE
)


#---------randomForest
rf <- h2o.randomForest(x = x, y = y,
                        training_frame = train,
                        seed = 1,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE
)

xgboost <- h2o.xgboost(x = x, y = y,
                        training_frame = train,
                        seed = 1,
                        nfolds = nfolds,
                        fold_assignment = "Modulo",
                        keep_cross_validation_predictions = TRUE
)



ensemble <- h2o.stackedEnsemble(x = x,
                                y = y,
                                training_frame = train,
                                base_models = list(gbm@model_id, rf@model_id,glm@model_id,
                                                   xgboost@model_id)
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

#----------------------------------------------------------------
#ensemble results
p_mean = (submitting$is_churn + submitting1$is_churn)/2
submission_combine = data.frame(msno = submitting$msno, is_churn = p_mean)
write.csv(submission_combine, file = "submission.csv", row.names = FALSE)

