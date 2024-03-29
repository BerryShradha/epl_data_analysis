##############################################################################
######################## UNSUPERVISED LEARNING ###############################

# 1. Correlation -- Why remove correlated variables before clustering or PCA?
# 2. PCA

##############################################################################

## 1. Correlation
#install.packages("tidyverse")
library(tidyverse)
# create label vectors for numeric weather attributes and numeric epl attributes
weather_num <- names(select_if(final_data %>% dplyr:: select(starts_with("daily")), is.numeric))
epl_num <- names(select_if(final_data %>% dplyr:: select(-starts_with("daily")), is.numeric))

# exploring relationships among features: correlation matrix
cor_weather_num <- cor(final_data[weather_num])
cor_epl_num <- cor(final_data[epl_num])
cor_weather_num
cor_epl_num

library("caret")
#Find correlations with cutoff .9 (handles absolute values. So .90 and -.90)
idx <- findCorrelation(cor_epl_num, cutoff = .90, verbose = FALSE) ##No highly correlated fields
idx
idx <- findCorrelation(cor_weather_num, cutoff = .90, verbose = FALSE) #5, 6, 7, 9, 18, 19
idx
corr <- cor_weather_num[, c(idx)]
#Remove correlated attributes
final_data <- select (final_data,-c(colnames(corr)))

# Check again. All correlations should be less than (-)0.9
weather_num <- names(select_if(final_data %>% dplyr:: select(starts_with("daily")), is.numeric))
cor_weather_num <- cor(final_data[weather_num])
cor_weather_num

# 2. PCA
## PCA on weather data only
pc_weather <- prcomp(final_data[weather_num], center = T, scale. = T)
attributes(pc_weather)
summary(pc_weather)
print(pc_weather)
plot(pc_weather)  

# Plot:2
#library(psych)
#pairs.panels(pc_weather$x,gap=0, main="Plot:2 correlation of PC Variables")

# Visual analysis of PCA results

# calculate the proportion of explained variance (PEV) from the std values
# Plot:3
pc_weather_var <- pc_weather$sdev^2
pc_weather_var
pc_weather_PEV <- pc_weather_var / sum(pc_weather_var)
pc_weather_PEV
plot(pc_weather_PEV,main="Plot:3 Proportion of explained Variance")

# # plot the cumulative value of PEV for increasing number of additional PCs
#   note: add an 90% threshold line to inform the feature extraction
#     according to the plot the first 3 PCs should be selected
# Plot:4 To choose the number of PC s
opar <- par()
plot(
  cumsum(pc_weather_PEV),
  ylim = c(0,1),
  xlab = 'PC',
  ylab = 'cumulative PEV',
  pch = 20,
  col = 'orange',
  main="Plot:4 Cummulative PEV values"
)
abline(h = 0.9, col = 'red', lty = 'dashed')
par(opar)

PCA_weather<-data.frame(pc_weather$x[,c(1:8)], final_data[, !names(final_data) %in% c(weather_num)])
str(PCA_weather)
dim(PCA_weather)

## PCA on epl data only
pc_epl <- prcomp(final_data[epl_num], center = T, scale. = T)
attributes(pc_epl)
summary(pc_epl)
print(pc_epl)
plot(pc_epl)  

# Plot:2
#library(psych)
#pairs.panels(pc_epl$x,gap=0, main="Plot:2 correlation of PC Variables")

# Visual analysis of PCA results

# calculate the proportion of explained variance (PEV) from the std values
# Plot:3
pc_epl_var <- pc_epl$sdev^2
pc_epl_var
pc_epl_PEV <- pc_epl_var / sum(pc_epl_var)
pc_epl_PEV
plot(pc_epl_PEV,main="Plot:3 Proportion of explained Variance")

# # plot the cumulative value of PEV for increasing number of additional PCs
#   note: add an 90% threshold line to inform the feature extraction
#     according to the plot the first 3 PCs should be selected
# Plot:4 To choose the number of PC s
opar <- par()
plot(
  cumsum(pc_epl_PEV),
  ylim = c(0,1),
  xlab = 'PC',
  ylab = 'cumulative PEV',
  pch = 20,
  col = 'orange',
  main="Plot:4 Cummulative PEV values"
)
abline(h = 0.9, col = 'red', lty = 'dashed')
par(opar)

PCA_epl<-data.frame(pc_epl$x[,c(1:10)], PCA_weather[, !names(PCA_weather) %in% c(epl_num)])
str(PCA_epl)
dim(PCA_epl)

##PCA of full data
full_num <- names(select_if(final_data, is.numeric))
pc_full <- prcomp(final_data[full_num], center = T, scale. = T)
attributes(pc_full)
summary(pc_full)
print(pc_full)
plot(pc_full)  

# Plot:2
#library(psych)
#pairs.panels(pc_full$x,gap=0, main="Plot:2 correlation of PC Variables")

# Visual analysis of PCA results

# calculate the proportion of explained variance (PEV) from the std values
# Plot:3
pc_full_var <- pc_full$sdev^2
pc_full_var
pc_full_PEV <- pc_full_var / sum(pc_full_var)
pc_full_PEV
plot(pc_full_PEV,main="Plot:3 Proportion of explained Variance")

# # plot the cumulative value of PEV for increasing number of additional PCs
#   note: add an 90% threshold line to inform the feature extraction
#     according to the plot the first 3 PCs should be selected
# Plot:4 To choose the number of PC s
opar <- par()
plot(
  cumsum(pc_full_PEV),
  ylim = c(0,1),
  xlab = 'PC',
  ylab = 'cumulative PEV',
  pch = 20,
  col = 'orange',
  main="Plot:4 Cummulative PEV values"
)
abline(h = 0.9, col = 'red', lty = 'dashed')
par(opar)

PCA_full<-data.frame(pc_full$x[,c(1:18)], final_data[, !names(final_data) %in% c(full_num)])
str(final_data)

dim(PCA_full)
dim(PCA_epl)
