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


#replicating method on dataset
diabetes.mds <- isoMDS(data)
plot(data.mds$points, type = "n",
     main = "Non-Metric MDS on Diabetes Data",
     xlab = "Component 1", ylab = "Component 2")
text(data.mds$points, labels = colnames(data),
     cex = 1.2)


#comment
#comment2


