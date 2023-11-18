#library(MASS)
#data("voting", package = "HSAUR2")

#reading in 2015 cleansed data set. 
data <- read.csv("https://raw.githubusercontent.com/eGamez01/lfs_data/master/2015_diabetes_cleansed.csv")


#example from lectures on Non-Metric MDS
voting[1:5,1:5]
voting.mds <- isoMDS(voting)
plot(voting.mds$points, type = "n",
     main = "Non-Metric MDS on Voting Data",
     xlab = "Component 1", ylab = "Component 2")
text(voting.mds$points, labels = colnames(voting),
     cex = 1.2)



#replicating non-metric method on dataset. Cant get this to work
data2 <- data[1:1000,]
dist <- dist(as.matrix(data2))

data.mds <- isoMDS(dist)

plot(data.mds$points, type = "n",
     main = "Non-Metric MDS on Diabetes Data",
     xlab = "Component 1", ylab = "Component 2")
text(data.mds$points, labels = colnames(data),
     cex = 1.2)




# Calculate the correlation matrix
cor_matrix <- cor(data)

# The correlation distance is defined as (1 - correlation)
cor_diabetes <- as.dist(1 - cor_matrix)
# Perform MDS using the cmdscale() function

mds <- cmdscale(cor_diabetes, eig = TRUE, k = 2)  # k = 2 for two-dimensional MDS
# Create a data frame for plotting
mds_data <- data.frame(
  Var1 = rownames(cor_matrix),
  MDS1 = mds$points[, 1],
  MDS2 = mds$points[, 2]
)
# Plot the MDS
plot(mds_data$MDS1, mds_data$MDS2, type = "n", xlab = "MDS1", ylab = "MDS2", main = "MDS Plot of Health Variables")
text(mds_data$MDS1, mds_data$MDS2, labels = mds_data$Var1, cex = 0.7)



#exploratory pca
data.pca <- princomp(covmat = data)
summary(data.pca, loadings = TRUE)

#pca with subset of symptoms 
var <- c("Diabetes_012", "HighBP", "HighChol", "CholCheck", "Smoker", "Stroke", "HeartDiseaseorAttack", "HvyAlcoholConsump")
symp <- data[var]

head(symp)

cov(symp)

symp.pca <- princomp(symp = covmat)

summary(symp.pca, loadings = TRUE)
(symp.pca$sdev^2) > mean(symp.pca$sdev^2)

abs(symp.pca$loadings[,1:3]) > 0.5