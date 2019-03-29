#######################################################################
######################## UNSUPERVISED LEARNING ########################

# 1. Correlation
# 2. Cluster analysis
# 3. PCA

#######################################################################

## 1. Correlation
install.packages("tidyverse")
library(tidyverse)
# create label vectors for numeric weather attributes and numeric epl attributes
weather_num <- names(select_if(final_data %>% dplyr:: select(starts_with("daily")), is.numeric))
epl_num <- names(select_if(final_data %>% dplyr:: select(-starts_with("daily")), is.numeric))

# exploring relationships among features: correlation matrix
cor_weather_num <- cor(final_data[weather_num])
cor_epl_num <- cor(final_data[epl_num])

# visualize the correlation matrix
cor_weather_num
cor_epl_num

# plot the relationships among features - scatterplot matrix
pairs(final_data[weather_num])
pairs(final_data[epl_num])


# plot a more informative scatterplot matrix
psych::pairs.panels(final_data[weather_num])
psych::pairs.panels(final_data[epl_num])


