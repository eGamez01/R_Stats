#We have n = 300 participants, and a probability of success p = 0.7. Therefore, our expected number of “yes” responses out of this group is:
n = 300
p = 0.7
n*p

#What is the variance of this random variable?
n*p*(1-p)

#Now let’s return our attention to the active focus group. Let’s suppose your group consists of 15 people. Then you will have as few as zero yes responses and as many as 15. Let’s plot the probability of each of the 16 possibilities.

x <- 0:15
plot(x, dbinom(x, size = 15, prob = 0.7),
     main = "Probability of Number of Yes Responses",
     xlab = "Number of Yes Responses",
     ylab = "Probability")
1


#simulate 1000 more focus groups
data <- rbinom(1000, size = 15, prob = 0.7)
mean(data)
var(data)

#Let’s look at a similar experiment using the Poisson distribution. Every lecture, on average, 17 students clap
#at the end of my lecture (no not really, the actual number is much much much closer to zero). Pretending
#that the number is actually 17, I would like to predict the number who will clap at the end of my next lecture
#so I can know how good I will feel on the way back to my office.
#Similar to above, let’s plot the probability of the number of clappers from 0 up to 50.

x <- 0:40
plot(x,dpois(x, lambda = 17))

#simulate 1000
data <- rpois(1000, lambda = 17)
mean(data)

var(data)

#plot
x <- c(5:30)
invisible(plot(hist(data), freq = FALSE,
               main = "Simulated Data vs True Probabilities",
               xlab = "Number of Clappers",
               ylab = "Expected Number of Clappers"))

#overlay points
points(x, dpois(x, lambda = 17))


#change1

#change2

#change3

#change4

#changefrombrowser

#changefrombrowser2

#changefrom VSCODE