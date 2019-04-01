######################################################################
#   6. Performance evaluation

tchurn_rf_predict <-  cbind(
  actual = test_data$FTR,
  predicted = predict(rf, test_data[,-22], type = 'raw'),
  predict(rf, test_data[,-22], type = 'prob')
)

# generate a confusion matrix for the each predicted model
#   and inspect them: the caret confusionMatrix function
#   returns also Accuracy, Kappa, Sensitivity and Specificity
#     note: the positive class should be explicitely declared
#       with the argument 'positive'
rf_confmat <- confusionMatrix(data = rf_predict$predicted, reference = rf_predict$actual, positive = "True")
rf_confmat <- confusionMatrix(data = tchurn_rf_predict$predicted, reference = tchurn_rf_predict$actua, positive = "True")
tree_confmat
rf_confmat

# prepare two data frames to generate a ROC curve:
#   a data frame with the probability scores for the prediction of True
#   a data frame with the actual classes (repeated twice)
tchurn_models_prob <- data.frame(
  tree = tchurn_tree_predict$True,
  rf = tchurn_rf_predict$True
)
tchurn_label <- data.frame(
  tree = tchurn_tree_predict$actual,
  rf = tchurn_rf_predict$actual
)

# ROCR requires to create a prediction and a performance object
#   note: the performance object can be created for different measures
#     e.g. TPR and FPR in this case
tchurn_ROC_pred = prediction(tchurn_models_prob, tchurn_label)
tchurn_ROC_perf = performance(tchurn_ROC_pred, "tpr", "fpr")

# plot the ROC curve for the two methods
opar <- par()
par(pty = 's')
plot(
  tchurn_ROC_perf,
  col = as.list(c("orange", "blue"))
)
abline(a = 0, b = 1, lty = 2, col = 'red')
legend(
  "bottomright",
  names(tchurn_models_prob),
  col = c("orange", "blue"),
  lty = 1,
  bty = 'n'
)
par <- opar

