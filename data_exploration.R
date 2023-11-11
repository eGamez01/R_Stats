#reading in reduced 2015 dataset. 20% sample of original file.
data <- read.csv("https://raw.githubusercontent.com/eGamez01/lfs_data/master/reduced_2015.csv")

# copy of data to test cleaning with 
data2 <- data

# remove first character (b) and last character (') from IDATE 
data2$IDATE <- gsub('^.|.$', '', data2$IDATE)
# remove first character
data2$IDATE <- sub('.', '', data2$IDATE)
