
######################################################################
#   6. Performance evaluation

library(caret)

# generate a confusion matrix for the each predicted model
#   and inspect them: the caret confusionMatrix function
#   returns also Accuracy, Kappa, Sensitivity and Specificity
#     note: the positive class should be explicitely declared
#       with the argument 'positive'
rf_confmat <- confusionMatrix(data = rf_pred1, reference = test_data$FTR, positive = "True")
nn_confmat <- confusionMatrix(data = as.factor(round(pred_nn_1$net.result, 0)), reference = as.factor(nn_test_data$FTR), 
                              positive = "True")
nn_confmat
prob.result <- pred_nn_1$net.result

#install.packages("pROC")
library(pROC)
multi_roc <- multiclass.roc(test_data$FTR, predict(rf1, test_data[, -22], type = 'prob'))
auc(multi_roc)

rf_predicted <-  cbind(
  actual = test_data$FTR,
  predicted = rf_pred1,
  predict(rf1, test_data[, -22], type = 'prob')
)
rf_predicted <- as.data.frame(rf_predicted)

# prepare two data frames to generate a ROC curve:
#   a data frame with the probability scores for the prediction of True
#   a data frame with the actual classes (repeated twice)
models_prob <- data.frame(
  rf1 = rf_predicted$H,
  nn = prob.result
)
levels(test_data$FTR) <- list(H=c("H"), AorD=c("A", "D"))

label <- data.frame(
  rf1 = test_data$FTR,
  nn1 = test_data$FTR
)

# ROCR requires to create a prediction and a performance object
#   note: the performance object can be created for different measures
#     e.g. TPR and FPR in this case
ROC_pred = prediction(models_prob, label)
ROC_perf = performance(ROC_pred, "tpr", "fpr")

# plot the ROC curve for the two methods
opar <- par()
par(pty = 's')
plot(
  ROC_perf,
  col = as.list(c("orange", "blue"))
)
abline(a = 0, b = 1, lty = 2, col = 'red')
legend(
  "bottomright",
  names(models_prob),
  col = c("orange", "blue"),
  lty = 1,
  bty = 'n'
)
par <- opar

