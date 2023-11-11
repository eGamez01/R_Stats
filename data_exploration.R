#reading in reduced 2015 dataset. 20% sample of original file.
data <- read.csv("https://raw.githubusercontent.com/eGamez01/lfs_data/master/reduced_2015.csv")

# copy of data to test cleaning with 
data2 <- data

# remove first character (b) and last character (') from IDATE 
data2$IDATE <- gsub('^.|.$', '', data2$IDATE)
# remove remaining first character
data2$IDATE <- sub('.', '', data2$IDATE)

# remove first character (b) and last character (') from IMONTH
data2$IMONTH <- gsub('^.|.$', '', data2$IMONTH)
# remove remaining first character
data2$IMONTH <- sub('.', '', data2$IMONTH)

# remove first character (b) and last character (') from IDAY
data2$IDAY <- gsub('^.|.$', '', data2$IDAY)
# remove remaining first character
data2$IDAY <- sub('.', '', data2$IDAY)

# remove first character (b) and last character (') from IDATE 
data2$IYEAR <- gsub('^.|.$', '', data2$IYEAR)
# remove remaining first character
data2$IYEAR <- sub('.', '', data2$IYEAR)
head(data2$IYEAR)


