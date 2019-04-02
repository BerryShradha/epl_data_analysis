#######################################################################
######################## SUPERVISED LEARNING ########################

# 1. Random Forest
# 2. Neural Networks

#######################################################################
str(PCA_full)

##Convert HomeTeam to factors consistent with AwayTeam
levels <- levels(PCA_full$AwayTeam)
PCA_full$HomeTeam <- factor(PCA_full$HomeTeam, levels)

# transform the data using a min-max function
MinMax <- function(x){
  tx <- (x - min(x)) / (max(x) - min(x))
  return(tx)
}
# then apply the function to each column of the data set
#   note: the apply function returns a matrix
numeric_data <- select_if(PCA_full, is.numeric)
data_minmax <- apply(numeric_data, 2, MinMax)

# the matrix needs to be 'cast' into a data frame
#   note: R has an as.data.frame function for this purpose
data <- data.frame(data_minmax, PCA_full[, !names(PCA_full) %in% c(names(numeric_data))])

# set random seed
set.seed(2018)
# create a 70/30 training/test set split
n_rows <- nrow(data)
# sample 70% (n_rows * 0.7) indices in the ranges 1:nrows
training_idx <- sample(n_rows, n_rows * 0.7)
# filter the data frame with the training indices (and the complement)
training_data <- data[training_idx,]
test_data <- data[-training_idx,]
formula <- reformulate(names(training_data[, -c(19, 22, 23, 24, 25)]), response = 'FTR')

###################################################################################3

## 1. RANDOM FOREST
library(randomForest)
#data <- as.data.frame(unclass(data));
# 5. Random forest training

library(caret)
ctrl_parameters <- trainControl(method = 'CV', number = 10)

# train a model with random forest
#   note: number of trees is set to 500
#     and calculation of attributes importance is requested
rf <- randomForest(formula, ntree = 500, importance = T, data = training_data) ##plot shows 300 trees ideal
rf1 <- randomForest(formula, ntree = 500, mtry = 6, importance = T, data = training_data)
rf2 <- randomForest(formula, ntree = 500, mtry = 8, importance = T, data = training_data)
rf3 <- randomForest(formula, ntree = 500, mtry = 12, importance = T, data = training_data)
rf4 <- randomForest(formula, ntree = 500, mtry = 15, importance = T, data = training_data)
rf5 <- randomForest(formula, ntree = 500, mtry = 20, importance = T, data = training_data)
rf6 <- train(formula, data = training_data, method = "rf", trControl = ctrl_parameters)
# plot the error rates
#   note: the labels for the legend are extracted from the rf object
#     and they include Out-of-bag (OOB) error. OOB is the average error
#     calculate for each point using only trees that were not trained
#     using that point
plot(rf)
plot(rf1)
plot(rf2)
plot(rf3)
plot(rf4)
plot(rf5)
plot(rf6)
legend('topright', colnames(rf$err.rate), bty = 'n', lty = c(1,2,3), col = c(1:3))

# plot the variable importancea according to the
varImpPlot(rf, type = 1)
varImpPlot(rf1, type = 1)
varImpPlot(rf2, type = 1)
varImpPlot(rf3, type = 1)
varImpPlot(rf4, type = 1)
varImpPlot(rf5, type = 1)

# 6. Random forest prediction

# compute the prediction for the random forest model
#   note: the Sales attribute (column 1) is excluded from the test data set
rf_pred <- predict(rf, test_data[,-22], type= "class")
rf_pred1 <- predict(rf1, test_data[,-22], type= "class")
rf_pred2 <- predict(rf2, test_data[,-22], type= "class")
rf_pred3 <- predict(rf3, test_data[,-22], type= "class")
rf_pred4 <- predict(rf4, test_data[,-22], type= "class")
rf_pred5 <- predict(rf5, test_data[,-22], type= "class")
rf_pred6 <- predict(rf6, test_data[,-22], type= "raw")

# create a contingency table for the actual VS predicted for the random forest model
rf_results_table <- table(rf = rf_pred,  actual = test_data$FTR)
rf_results_table
rf_results_table1 <- table(rf = rf_pred1,  actual = test_data$FTR)
rf_results_table1
rf_results_table2 <- table(rf = rf_pred2,  actual = test_data$FTR)
rf_results_table2
rf_results_table3 <- table(rf = rf_pred3,  actual = test_data$FTR)
rf_results_table3
rf_results_table4 <- table(rf = rf_pred4,  actual = test_data$FTR)
rf_results_table4
rf_results_table5 <- table(rf = rf_pred5,  actual = test_data$FTR)
rf_results_table5
rf_results_table6 <- table(rf = rf_pred6,  actual = test_data$FTR)
rf_results_table6

# calculate accuracy from each contigency table
#   as sum of diagonal elements over sum of the matrix values
acc_rf <- sum(diag(rf_results_table)) / sum(rf_results_table) #0.6296296
acc_rf
acc_rf1 <- sum(diag(rf_results_table1)) / sum(rf_results_table1) #0.6393762
acc_rf1
acc_rf2 <- sum(diag(rf_results_table2)) / sum(rf_results_table2) #0.6218324
acc_rf2
acc_rf3 <- sum(diag(rf_results_table3)) / sum(rf_results_table3) #0.6237817
acc_rf3
acc_rf4 <- sum(diag(rf_results_table4)) / sum(rf_results_table4) #0.6345029
acc_rf4
acc_rf5 <- sum(diag(rf_results_table5)) / sum(rf_results_table5) #0.6218324
acc_rf5
acc_rf6 <- sum(diag(rf_results_table6)) / sum(rf_results_table6) #0.6218324
acc_rf6

#########################################################################

## NEURAL NETWORKS
library(neuralnet)
# 3. Neural network training 
#IMPORTANT: Error closer to zero is better. Three architectures are used below to see which is giving lower error.

data_nn <- data[, -c(19, 23, 24, 25)]
idx <- sapply(data_nn, is.factor)
data_nn[idx] <- lapply(data_nn[idx], function(x) as.numeric(x))
#data["FTR"] <- PCA_full$FTR
str(data_nn)

# set random seed
set.seed(2018)
# create a 70/30 training/test set split
n_rows <- nrow(data_nn)
# sample 70% (n_rows * 0.7) indices in the ranges 1:nrows
training_idx <- sample(n_rows, n_rows * 0.7)
# filter the data frame with the training indices (and the complement)
nn_training_data <- data_nn[training_idx,]
nn_test_data <- data_nn[-training_idx,]
nn_formula <- reformulate(names(nn_training_data[, -c(21)]), response = 'FTR')

# train a neural network with 1 hidden node
nn_1 <- neuralnet(nn_formula, data = nn_training_data)
nn_1$result.matrix

# train a neural network with 5 nodes on one hidden layer
#   note: the number of layers is set with the hidden option parameter
nn_5 <- neuralnet(nn_formula, hidden = 5, data = nn_training_data)
nn_5$result.matrix

# train a neural network with 5 nodes on each of two hidden layers
nn_55 <- neuralnet(nn_formula, hidden = c(5,5), data = nn_training_data, stepmax = 2000000)
nn_55$result.matrix

# plot the three neural networks and compare their structure
plot(nn_1)
plot(nn_5)
plot(nn_55)

# 4. Neural network prediction
##IMPORTANT: To get fair results, run ML algos multiple times, and take average of the results.

# compute the prediction for each neural network
#   note: the strength attribute (column 9) is excluded from the test data set
pred_nn_1 <- compute(nn_1, nn_test_data[, -c(21)])
pred_nn_5 <- compute(nn_5, nn_test_data[, -c(21)])
pred_nn_55 <- compute(nn_55, nn_test_data[, -c(21)])

# create a table with actual values and the three predictions
#   note: predicted values are stored as net_result attribute of the prediction object
nn_results <- data.frame(
  actual = test_data$FTR,
  nn_1 = pred_nn_1$net.result,
  nn_5 = pred_nn_5$net.result,
  nn_55 = pred_nn_55$net.result
)

# calculate the correlation between actual and predicted values to identify the best predictor
cor(nn_results[,'actual'], nn_results[,c("nn_1","nn_5", "nn_55")])

# plot actual vs predicted values for the worst (blue) and best predictor (orange)
#   note: points is used to add points on a graph
opar <- par()
#png("sample.png")
plot(
  nn_results$actual,
  nn_results$nn_1,
  col = 'blue',
  xlab = 'actual strength',
  ylab = 'predicted strength',
  xlim = c(0,1),
  ylim = c(0,1)
)
points(
  nn_results$actual,
  nn_results$nn_55,
  col = 'orange'
)
abline(a = 0, b = 1, col = 'red', lty = 'dashed')
legend(
  'topleft',
  c('nn_1', 'nn_55'),
  pch = 1,
  col = c('blue', 'orange'),
  bty = 'n'
)
#dev.off()
par(opar)

comparison_1 <- data.frame(actual = test_data$FTR, predicted_1 = round(pred_nn_1$net.result))
comparison_5 <- data.frame(actual = test_data$FTR, predicted_5 = round(pred_nn_5$net.result))

### 3.3 create a contigency table of the actual VS predicted
tab1 <- table(comparison_1)
tab5 <- table(comparison_5)

### 3.4 calculate accuracy from the contigency table as:
###   sum of diagonal elements over sum of the matrix values
accuracy1 <- sum(diag(tab1)) / sum(tab1)
accuracy1
accuracy5 <- sum(diag(tab5)) / sum(tab5)
accuracy5

