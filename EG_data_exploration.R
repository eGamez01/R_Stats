#library(MASS)
#data("voting", package = "HSAUR2")

#reading in 2015 cleansed data set. 
data <- read.csv("https://raw.githubusercontent.com/eGamez01/lfs_data/master/2015_diabetes_cleansed.csv")


# -----------------Data cleaning & Visulaization----------------------------------------------------------------------------

# check for missing data
RowsWithNA <- which(rowSums(is.na(data)) > 0)
length(RowsWithNA) # No missing values 

plot(data[,1:22])

# rug plot of diabetes & highBP
plot(jitter(data$Diabetes_012) , jitter(data$HighBP))
,
     ylab = "HighBP",
     xlab = "Diabetes Status", type = "n")
rug(data$Diabetes_012, side = 1) # put rug symbols on horizontal
rug(data$HighBP, side = 2) # put rug symbols on vertical
text(data$Diabetes_012,data$HighBP,
     cex = 0.6, labels = abbreviate(row.names(data)))

# bivariate boxplot 
library(MVA)
diab_j <- scale(data$Diabetes_012)
hbp_j <-  scale(data$HighBP)
x <- scale(cbind(diab_j, hbp_j))
bvbox(x)

# We may have a hard time finding any useful information from visulizations before dimensionality reduction 
# due to the data being mostly binary

# -------------Dimensionality Reduction---------------------------------------------------------------------


#example from lectures on Non-Metric MDS
# This example has the same values for rows as they do columns. Maybe this difference is why the replication was unsuccessful.
voting[1:5,1:5]
voting.mds <- isoMDS(voting)
plot(voting.mds$points, type = "n",
     main = "Non-Metric MDS on Voting Data",
     xlab = "Component 1", ylab = "Component 2")
text(voting.mds$points, labels = colnames(voting),
     cex = 1.2)



#replicating non-metric method on dataset. Cant get this to work
library(MASS)
data2 <- data[1:1000,]
dist <- dist(as.matrix(data2))

data.mds <- isoMDS(dist)

plot(data.mds$points, type = "n",
     main = "Non-Metric MDS on Diabetes Data",
     xlab = "Component 1", ylab = "Component 2")
text(data.mds$points, labels = colnames(data),
     cex = 1.2)

# Retrying non-metric MDS with correlation matrix
cor_matrix <- cor(data)
cor_diabetes <- as.dist(1 - cor_matrix)

data.mds <- isoMDS(cor_diabetes)

plot(data.mds$points, type = "n",
     main = "Non-Metric MDS on Diabetes Data",
     xlab = "Component 1", ylab = "Component 2")
text(data.mds$points, labels = colnames(data),
     cex = 1.2)

# Display Stress
data.mds$stress # This is between poor fit (20%) and fair fit (10%)

# Calculate the correlation matrix
cor_matrix <- cor(data)

# The correlation distance is defined as (1 - correlation)
cor_diabetes <- as.dist(1 - cor_matrix)
# Perform MDS using the cmdscale() function

mds <- cmdscale(cor_diabetes, eig = TRUE, k = 2)  # k = 2 for two-dimensional MDS

# Check the ratio of the k eigen values compared to the sum of all eigenvalues
mds.eig <- cmdscale(cor_diabetes, eig = TRUE, k = 21)
cumsum(mds.eig$eig)/sum(mds.eig$eig) # This is not giving a ratio of 0.8 or greater for the first two eigenvalues

# display the loadings for MDS
mds$points

# Create a data frame for plotting
mds_data <- data.frame(
  Var1 = rownames(cor_matrix),
  MDS1 = mds$points[, 1],
  MDS2 = mds$points[, 2]
)
# Plot the MDS
plot(mds_data$MDS1, mds_data$MDS2, type = "n", xlab = "MDS1", ylab = "MDS2", main = "MDS Plot of Health Variables")
text(mds_data$MDS1, mds_data$MDS2, labels = mds_data$Var1, cex = 0.7)


############------------- Exploratory PCA ----------------------#############
#exploratory pca
data.pca <- princomp(covmat = data) # The reason this isn't working is because the object "data" is not a correlation or cov matrix. Either use correlation or Cov matrix, or use princomp(data).
data.pca <- princomp(data)
summary(data.pca, loadings = TRUE)

# Determine number of components
# Components that have a variance greater than the mean variance
data.pca$sdev^2 > mean(data.pca$sdev^2)

# Scree plot of the variance for each component
plot(1:22, data.pca$sdev^2 , type = "b",
     main = "Scree Plot of PCA Data",
     xlab = "Component Index", ylab = "Variance")
# We can get away with 2 components, 3 would be better.

# Analyze first 3 components loadings
data.pca$loadings[,1:3]

abs(data.pca$loadings[,1:3]) > 0.5

#pca with subset of symptoms 
var <- c("Diabetes_012", "HighBP", "HighChol", "CholCheck", "Smoker", "Stroke", "HeartDiseaseorAttack", "HvyAlcoholConsump")
symp <- data[var]

head(symp)


symp <- cor(symp)

symp.pca <- princomp(covmat = symp)

summary(symp.pca, loadings = TRUE)
(symp.pca$sdev^2) > mean(symp.pca$sdev^2)

abs(symp.pca$loadings[,1:3]) > 0.5

####### ----------------- HC Clustering --------------------######

#Perform HC Clustering

#Using the correlation matrix find the distance
dist_hc = dist(cor_matrix)

#Use Complete Linkage for HC Clustering
hc.d = hclust(dist_hc, "complete")


#Plot the resulting Dendrogram
plot( hc.d, main = "Hierarchical Cluster Dendrogram Using Complete Linkage" , cex = 0.5)


#Perform K-means clustering 

#Scale the data
data.s = scale(data)

#Scree Plot to decide how many clusters to use
plot.wgss <- function(mydata, maxc){
  wss <- numeric(maxc)
  for (i in 1:maxc){
    wss[i] <- kmeans(mydata, iter.max = 100,
                     centers = i, nstart = 10)$tot.withinss
  }
  plot(1:maxc, wss, type = "b",
       xlab = "Number of Clusters",
       ylab = "Within Groups Sum of Squares",
       main = "Scree Plot")
}

#Plot the Scree plot function using the Scaled Data
plot.wgss(data.s, 20)

#Based on scree plot 2 clusters should be used
kmd = kmeans(data.s, centers = 2)

table(kmd$cluster)

kmd$tot.withinss

#Look at the cluster #1
round(subset(data.s, kmd$cluster==1), 3)


####### ----------- MARKET BASKET ANALYSES ----------------- #####
#run market basket analysis 

#reading in reduced 2015 dataset. 20% sample of original file.
data <- read.csv("https://raw.githubusercontent.com/eGamez01/lfs_data/master/2015_diabetes_cleansed.csv")

#install library 
install.packages("arules")
library(arules)

#create subset with binary data only (i.e., exclude all the data recorded on a scale)
#includes all binary data
subset1 <- c("Diabetes_012", "HighBP", "HighChol", "CholCheck", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "AnyHealthcare", "NoDocbcCost", "DiffWalk", "Sex")
symptoms1 <- data[subset1]

head(symptoms1)

#recode diabetes_012 variable as binary to indicate the presence of diabetes or no diabetes 
#specification of gestational diabetes is not necessary here

data$Diabetes_012<- ifelse(data$Diabetes_012 > 0, 1, 0)

#restructure as matrix
diab <- as.matrix(symptoms1)
head(diab)

#obtain association rules 
rules <- apriori(diab, list(support = 0.05, confidence = 0.8, minlen = 3))
options(digits = 2)

#view first 5 to check 
inspect(sort(rules, by = "lift")[1:10])


##rerun after removing variables cholesterol check, and access to healthcare variables
subset2 <- c("Diabetes_012", "HighBP", "HighChol", "Smoker", "Stroke", "HeartDiseaseorAttack", "PhysActivity", "Fruits", "Veggies", "HvyAlcoholConsump", "DiffWalk", "Sex")
symptoms2 <- data[subset2]

head(symptoms2)

#new matrix with subset2
diab2 <- as.matrix(symptoms2)
head(diab2)

#obtain association rules 
rules <- apriori(diab2, list(support = 0.05, confidence = 0.8, minlen = 3))
options(digits = 2)

#view first 5 to check 
inspect(sort(rules, by = "lift")[1:10])
