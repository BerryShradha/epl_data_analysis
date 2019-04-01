#######################################################################
######################## UNSUPERVISED LEARNING ########################

# 1. Correlation -- Why remove correlated variables before clustering or PCA?
# 2. Cluster analysis
# 3. PCA

#######################################################################

# 3. Cluster analysis BEFORE CORRELATION AND PCA
##Hierarchical Clustering
table(final_data$FTR)
str(final_data)
numeric_data <- select_if(final_data, is.numeric)
distance <- dist(numeric_data, method = 'euclidian')

#   then apply complete linkage
hc <- hclust(distance, method = 'complete')
hc

# plot the associated dendrogram
plot(hc, hang = -0.1, labels = final_data$FTR)

# 'cut' the dendrogram to select one partition with 5 groups
#   note: the cutree command requires a distance cutoff h
#      or the number k of desired groups
hc_cluster_id <- cutree(hc, k = 3)
table(hc_cluster_id, final_data$FTR)

##k-means clustering
k_means = kmeans(numeric_data, 3)
k_means

# get the cluster id from the kmeans object
k_cluster_id <- k_means$cluster
table(k_cluster_id, final_data$FTR)

# 3. Evaluation of cluster results

# silhoutte plot
# first install the package cluster
#install.packages('cluster')
# then calculate the silhoutte score for the two cluster solutions
#   note: look at the help for silhoutte to understand the required input
sil_hc <- cluster::silhouette(hc_cluster_id, distance)
sil_k <- cluster::silhouette(k_cluster_id, distance)

# plot the results of the silhoutte analysis for the two cluster solutions
opar <- par()
par(mfrow = c(2,1))
plot(sil_hc)
plot(sil_k)
par(opar)

#install.packages("factoextra")
#install.packages("NbClust")
library("factoextra")
fviz_nbclust(numeric_data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

##CLUSTER ANALYSIS OF EPL AND WEATHER DATA SEPERATELY
##Hierarchical Clustering
table(final_data$FTR)
str(final_data)
weather_num <- select_if(final_data %>% dplyr:: select(starts_with("daily")), is.numeric)
epl_num <- select_if(final_data %>% dplyr:: select(-starts_with("daily")), is.numeric)

distance_weather <- dist(weather_num, method = 'euclidian')
distance_epl <- dist(epl_num, method = 'euclidian')

#   then apply complete linkage
hc_weather <- hclust(distance_weather, method = 'complete')
hc_weather
hc_epl <- hclust(distance_epl, method = 'complete')
hc_epl

# plot the associated dendrogram
plot(hc_weather, hang = -0.1, labels = final_data$daily.icon)
plot(hc_epl, hang = -0.1, labels = final_data$FTR)

# 'cut' the dendrogram to select one partition with 5 groups
#   note: the cutree command requires a distance cutoff h
#      or the number k of desired groups
hc_weather_cluster_id <- cutree(hc_weather, k = 9)
hc_epl_cluster_id <- cutree(hc_epl, k = 3)
table(hc_weather_cluster_id, final_data$daily.icon)
table(hc_epl_cluster_id, final_data$FTR)

##k-means clustering
k_means_weather = kmeans(weather_num, 9)
k_means_weather
k_means_epl = kmeans(epl_num, 9)
k_means_epl

# get the cluster id from the kmeans object
k_weather_cluster_id <- k_means_weather$cluster
table(k_weather_cluster_id, final_data$daily.icon)
k_epl_cluster_id <- k_means_epl$cluster
table(k_epl_cluster_id, final_data$FTR)
final_data$FTR[k_epl_cluster_id == 1]

# 3. Evaluation of cluster results

# silhoutte plot
# first install the package cluster
#install.packages('cluster')
# then calculate the silhoutte score for the two cluster solutions
#   note: look at the help for silhoutte to understand the required input
sil_hc <- cluster::silhouette(hc_cluster_id, distance)
sil_k <- cluster::silhouette(k_cluster_id, distance)

# plot the results of the silhoutte analysis for the two cluster solutions
opar <- par()
par(mfrow = c(2,1))
plot(sil_hc)
plot(sil_k)
par(opar)
fviz_nbclust(weather_num, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
fviz_nbclust(epl_num, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")

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

# 3. Cluster analysis AFTER PCA
##Hierarchical Clustering
table(PCA_full$FTR)
str(PCA_full)
numeric_data <- select_if(PCA_full, is.numeric)
distance <- dist(numeric_data, method = 'euclidian')

#   then apply complete linkage
hc <- hclust(distance, method = 'complete')
hc

# plot the associated dendrogram
plot(hc, hang = -0.1, labels = PCA_full$FTR)

# 'cut' the dendrogram to select one partition with 5 groups
#   note: the cutree command requires a distance cutoff h
#      or the number k of desired groups
hc_cluster_id <- cutree(hc, k = 3)
table(hc_cluster_id, PCA_full$FTR)

##k-means clustering
k_means = kmeans(numeric_data, 3)
k_means

# get the cluster id from the kmeans object
k_cluster_id <- k_means$cluster
table(k_cluster_id, PCA_full$FTR)

# 3. Evaluation of cluster results

# silhoutte plot
# first install the package cluster
#install.packages('cluster')
# then calculate the silhoutte score for the two cluster solutions
#   note: look at the help for silhoutte to understand the required input
sil_hc <- cluster::silhouette(hc_cluster_id, distance)
sil_k <- cluster::silhouette(k_cluster_id, distance)

# plot the results of the silhoutte analysis for the two cluster solutions
opar <- par()
par(mfrow = c(2,1))
plot(sil_hc)
plot(sil_k)
par(opar)
fviz_nbclust(numeric_data, kmeans, method = "wss") +
  geom_vline(xintercept = 4, linetype = 2)+
  labs(subtitle = "Elbow method")
