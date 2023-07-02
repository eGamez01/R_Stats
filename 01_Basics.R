##Basics
#reading data
pgs <- read.csv("https://raw.githubusercontent.com/EricBrownTtu/ISQS5346/main/pgs.csv")

#read first 5 rows
head(pgs)

#read first 5 rows and first 6 columns
head(pgs[,1:6])

#Subsetting Data. c is temp vector creation
pgs_sub1 <- pgs[,c("FacTeaching", "COL")]

#frequency distribution of the "COL" variable using table() function.
table(pgs_sub1$COL)

#There are two options for proportion table creation.
#Option 1 is to divide by the sum of rows in dataset
round(table(pgs_sub1$COL) / sum(table(pgs_sub1$COL)),3)

#Option 2 is to count number of rows using nrow
round(table(pgs_sub1$COL) / nrow(pgs_sub1),3)

#Create a bar chart
barplot(table(pgs_sub1$COL), main = "Bar Chart of Number of Students by College",
        xlab = "College",
        ylab = "Number of Students")

#Create a pie chart
pie(table(x$COL), main = "Pie Chart of Proportion of Students by College")

#Get the median
median(pgs_sub1$FacTeaching, na.rm = TRUE)

#Get mode with function
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
getmode(pgs_sub1$FacTeaching)

#Generating random data
rand_norm <- rnorm(1000)

#Histogram of randomly generated data
hist(rand_norm, main = "Histogram of Random Standard Normal Data")

#Creating boxplot
boxplot(rand_norm, main = "Boxplot of Random Standard Normal Data",
        ylab = "x")

#Median of rnorm
median(rand_norm)

quantile(rand_norm, 0.25)

quantile(rand_norm, 0.75)

#interquartile range
iqr <- quantile(rand_norm,0.75) - quantile(rand_norm,0.25)

#finding outliers
rand_norm[rand_norm < quantile(rand_norm,0.25) - 1.5 * iqr | rand_norm > quantile(rand_norm,0.75) + 1.5 * iqr]

#installing packages
#install.packages("e1071")

#loading packages
library(e1071)

#using skewness function from library previously installed
skewness(rand_norm)

mean(rand_norm)
var(rand_norm)
sd(rand_norm)

#read body measure data
measure_data <- read.csv("https://raw.githubusercontent.com/EricBrownTTU/ISQS5346/main/measure.csv")
head(measure_data)

#scaling data to show sd for each datapoint
scale(measure_data[,1:3])

#install.packages("MASS")
library(MASS)

#using dataset of cars
head(Cars93)

cars <- table(Cars93$Type,Cars93$Origin)

#We obtain the marginal distributions by summing the rows and/or columns. We can achieve this with the rowSums() and colSums() functions, respectively.
rowSums(cars)
colSums(cars)

#To obtain the conditional distributions, we need to obtain the proportions each cell makes up out of the entire row/column. We do so by using the prop.table() function. First, we hold the column variable constant, and get the proportions of each row.
prop.table(cars, margin = 1)

#Now, we can hold the row variable constant and get the proportions of each column.
prop.table(cars, margin = 2)

#To obtain the chi-square statistic, we use the chisq.test() function
chisq_cars <- chisq.test(cars)

#To obtain Cramer’s V, we will need to install and load in a new package
install.packages("rcompanion")
library(rcompanion)
v <- cramerV(cars)

#Covariance and Correlation
measure_data_2 <- read.csv("https://raw.githubusercontent.com/EricBrownTTU/ISQS5346/main/measure.csv")

cov(measure_data_2[,1:3])
var(measure_data_2[,1])
cov(measure_data_2$waist,measure_data_2$chest)
cov(measure_data_2$chest,measure_data_2$waist)
cor(measure_data_2$chest,measure_data_2$waist)
cov(measure_data_2$chest,measure_data_2$waist)/(sd(measure_data_2$chest)*sd(measure_data_2$waist))
cor(measure_data_2[,1:3])
cov2cor(cov(measure_data_2[,1:3]))


