#######################################################################
######################## UNSUPERVISED LEARNING ########################

# 1. Correlation -- Why remove correlated variables before clustering or PCA?
# 2. Cluster analysis
# 3. PCA

#######################################################################

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

# 2. Cluster analysis


# 3. PCA

