#Law of large numbers

# set the possible values the die can show
dice_values <- c(1,2,3,4,5,6)

x <- sample(dice_values, 10000, replace = TRUE)

head(sample_dice)

x2 <- sample_dice == 2
head(x2)

x_cumavg <- cumsum(x2) / seq_along(x2)
head(x_cumavg)

#plot(x_cumavg[1:1000], type = "l",
plot(x_cumavg, type = "l",
     main = "Cumulative Average that the Die Shows 2",
     xlab = "Trial Number",
     ylab = "Probability/Frequency")

sum(x <= 2) / length(x)
sum(x %in% c(1,3,5)) / length(x)

choose(10,5)
factorial(10) / (factorial(5) * factorial(5))

