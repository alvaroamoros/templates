
# DATA SCIENCE: INFERENCE AND MODELING
# 30/08/20
library(tidyverse)
library(dslabs)
take_poll(25)

# Statistical inference aims to estimate a unknown population parameter using data from an obseved sample

# In a sampling model, the colection of elements in the urn is called Population

# A Parameter is a number that summarizes data for an entire population

# A Sample is observed data from a subset of the population

# An Estimate is a summary of observed data about a parameter. A data-driven guess of the population parameter
#(We estimate a parameter)

# The Estimate of Spread of beads in the urn is given by <- 2*p-1 ( The estimated difference between the suport of booth candidates)
#This is the Margin of Error <- 2*SE (2 standard deviations, 2 Z values)  
pnorm(2) - pnorm(-2)

#SE of the spred = 2 * SE  <- 2*sqrt(p*(1-p)/N)

# X? is the Sample Average. The bar on the top denotates that the average is a Rando Variable, it is the average of random Draws
#X? = X1+X2...Xn / N

#p <- The proportion of blue beads in the urn, is a Parameter that We estimate, we do not knoe the true proportion, we can only estimate it


#Properties of Our Estimate

# The expected value of  X?  is the parameter of interest  p, as X? is the sum of independent draws times a constant N
#E(X?) = p

#As N increases the Standard error of X? decreases
#SE(X?)sqrt(p*1-p/N)

#The expecterd value of the sum of draws E(S) and the SE(S)
# E(S) = N*p
# SE(S) = sqrt(N*p(1-p))


#Assessment 1.1: Parameters and Estimates

#Exercise 1
#Suppose you poll a population in which a proportion p of voters are Democrats and 1???p are Republicans. Your sample size is N=25
#Consider the random variable S, which is the total number of Democrats in your sample.
#What is the expected value of this random variable S?
# E(S)=25p

# What is the standard error of S?
# SE(S)=sqrt(25p(1???p))

# What is the expected value of X??
# E(X?)=p


# What is the standard error of the sample average, X??
# SE(X?)=p(1???p)/N

# Exercise 2
# Write a line of code that calculates the standard error se of a sample average when you poll 25 people in the population. 
# Generate a sequence of 100 proportions of Democrats p that vary from 0 (no Democrats) to 1 (all Democrats).
N <- 25
p <- seq(0, 1, 0.01)
se <- sqrt(p*(1-p)/N)
p
se
plot(p, se)

# Exercise 3 Using the same code as in the previous exercise, create a for-loop that generates three plots of p versus se when the sample sizes equal N=25, N=100, and N=1000.
p <- seq(0, 1, 0.01)

sample_sizes <- c(25, 100, 1000, 10000)

for (N in sample_sizes){
  se <- sqrt(p*(1-p)/N)
  plot(p, se, ylim = c(0, 0.10))
  
}

#The Central Limit Theorem in Practice

# Because  X?  is the sum of random draws divided by a constant, the distribution of  X?  is approximately norma

# We can convert  X?  to a standard normal random variable  Z : 
# Z= X? ??? E(X?) / SE(X?)

#The probability that  X?  is within .01 of the actual value of  p  is:
#Pr(Z <= 0.01 / sqrt(p*(1-p)/N) - Pr(Z <=0.01 / sqrt(p*(1-p)/N)

#The Central Limit Theorem (CLT) still works if  X?  is used in place of  p . This is called a plug-in estimate. Hats over values denote estimates. Therefore:
#SE?(X?) = sqrt(X?*(1-X?)*N)

# Using the CLT, the probability that  X?  is within .01 of the actual value of  p  is:
# 0.1/sqrt(X?*(1-X?)*N) - -0.1/sqrt(X?*(1-X?)*N)

#Computing the probabilities of X? being within 0.1 of p
X_hat <- 0.48
se <- sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se) - pnorm(-0.01/se)
X_hat <- 0.48

# A Monte Carlo Simulation for the CLT
p <- 0.45 # In practice we would not know this value as we dont know the population. We use it to see how our aproximations work
N <- 1000
X_hat <- 0.48

#We simulate B polls of size N to determine X_hat
B<-10000
x_hat <- replicate(B, {
  x <- sample(c(0,1), size = N, replace = TRUE, prob = c(1-p, p))
  mean(x)
})
mean(x_hat)
sd(x_hat)

# The Spread

#The spread between two outcomes with probabilities  p  and  1???p  is  2p???1.
#The expected value of the spread is  2*X_hat ???1 
#The standard error of the spread is  2*SE_hat(X_hat) 
#The margin of error of the spread is 2 times the margin of error of  X_hat.

# Assessment 2.1: Introduction to Inference
# Exercise 1 - Write function called take_sample that takes the proportion of Democrats p and the sample size N as arguments and returns the sample average of Democrats (1) and Republicans (0).
# Calculate the sample average if the proportion of Democrats equals 0.45 and the sample size is 100.
p <- 0.45
N <- 100

take_sample <- function(p, N){
  X_hat <- sample(c(0, 1), size = N, replace = TRUE, prob = c((1-p), p))
  mean(X_hat)
}
take_sample(0.45, 100)


# Exercise 2. Distribution of errors 
#Assume the proportion of Democrats in the population p equals 0.45 and that your sample size N is 100 polled voters. 
#The take_sample function you defined previously generates our estimate, X?.
# Replicate the random sampling 10,000 times and calculate p???X? for each random sample. Save these differences as a vector called errors. 
#Find the average of errors and plot a histogram of the distribution.
# Calculate the sample average if the proportion of Democrats equals 0.45 and the sample size is 100.
p <- 0.45
N <- 100

B <- 10000

set.seed(1, sample.kind="Rounding")
errors <- replicate(B, {
  X_hat <- sample(c(0, 1), size = N, replace = TRUE, prob = c((1-p), p))
  y <- p - X_hat
})

mean(errors)

# Exercise 3 - Plot a histogram of the values contained in the vector errors. Which statement best describes the distribution of the errors?
hist(errors)

# Exercise 4 - What is the average size of the error if we define the size by taking the absolute value ???p???X???? ?
p <-0.45
N <- 1000
B <- 10000

set.seed(1, sample.kind="Rounding")
errors <- replicate(B, p - take_sample(p, N))

mean(abs(errors))    

# Exercise 5 - Standard deviation of the spread
# As we have discussed, the standard error is the square root of the average squared distance (X????p)^2
sqrt(mean(errors^2))

# Exercise 6 - Estimate the standard error given an expected value of 0.45 and a sample size of 100
sqrt(p*(1-p)/N)


# Exercise 7. Standard error of the estimate
# In practice, we don't know p, so we construct an estimate of the theoretical prediction based by plugging in X? for p. Calculate the standard error of the estimate:
N <- 100
p <- 0.45

B <- 1000
X <- sample(c(0, 1), size = N, replace = TRUE, prob = c((1-p),p))
X_bar <- mean(X)
sqrt(X_bar*(1-X_bar)/N)


# Exercise 8 Create a plot of the largest standard error for N ranging from 100 to 5,000. Based on this plot, 
# how large does the sample size have to be to have a standard error of about 1%?

N <- seq(100, 5000, len = 100)
p <- 0.5
se <- sqrt(p*(1-p)/N)
plot(se, N)
hist(errors)

# Exercise 9 Make a qq-plot of the errors you generated previously to see if they follow a normal distribution.
qqnorm(errors) 
qqline(errors)

#Exercise 12. Estimating the probability of a specific value of X-bar
# If p=0.45 and N=100, use the central limit theorem to estimate the probability that X?>0.5.
1- pnorm(0.5, p, se)

# Exercise 13. Exercise 13. Estimating the probability of a specific error size
# Assume you are in a practical situation and you don't know p. Take a sample of size N=100 and obtain a sample average of X?=0.51.
#What is the CLT approximation for the probability that your error size is equal or larger than 0.01?

X_hat <- 0.51
N <- 100

se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat

1 - pnorm(.01, 0, se_hat) + pnorm(-0.01, 0, se_hat)



# Section 3: Confidence Intervals and p-Values

#Confidence intervales

#Plot
data("nhtemp")
nhtemp
data.frame(year =as.numeric(time(nhtemp)), temperature = as.numeric(nhtemp))%>%
  ggplot(aes(year, temperature)) +
  geom_point()+
  geom_smooth()

#calculate confidence interval. They have a random component
p <- 0.45
N <- 1000

X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p))
X_hat <- mean(X)
SE_hat <- sqrt(X_hat*(1-X_hat)/N)
c(X_hat - 2*SE_hat, X_hat + 2*SE_hat)

z <- qnorm(0.995)
z
pnorm(qnorm(0.995))
pnorm(qnorm(1-0.995))
pnorm(-z)
pnorm(z)-pnorm(-z)
qnorm(0.975)
#95% confidence nterval means that we have ~95% chances that our variable will be in 2 SDs of the mean (X_hat - 2*SE_hat, X_hat + 2*SE_hat)

# A Monte Carlo Simulation for Confidence Intervals
B <- 10000

inside <- replicate(B,{
  X <- sample(c(0, 1), size = N, replace = TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p, X_hat - 2*SE_hat, X_hat + 2*SE_hat)
  
})
mean(inside)

# p-Values
# Insted of being interesetd in a interval we want to kwno if this interval goeas trough 0
#The questions is if we can  now for sure that there are more bue beads than red ones
#This is the same as asking if the Spread is bigger than 0: 2p-1>0

#The Null Hypothesis: THe spread is 0
#If we have 52 blue
# 2 - X_hat = 0.04 <- The p-values asks ?How likely is it to see a value this large when H0 is true
#Pr(|X_hat-0.5|>0.02)  <- THis is te same as asking if the spread is 4% or more
#H0: Spread=0 <- p=0.5
N=100
z<- sqrt(N)*0.02/0.5
1 - (pnorm(z) -pnorm(-z))


# Assessment 3.1: Confidence Intervals and p-Values      
library(dslabs)
data("polls_us_election_2016")
#Exercise 1 - Assume there are only two candidates and construct a 95% confidence interval for the election night proportion p.    
head(polls_us_election_2016)
polls_us_election_2016

#Use filter to subset the data set for the poll data you want. Include polls that ended on or after October 31, 2016 (enddate). Only include polls that took place in the United States. Call this filtered object polls.
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-31" & state =="U.S.")

# How many rows does `polls` contain? Print this value to the console.
nrow(polls)

# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N  

# For the first poll in `polls`, assign the estimated percentage of Clinton voters to a variable called `X_hat`. Print this value to the console.
X_hat <- polls$rawpoll_clinton[1]/100
X_hat    

# Calculate the standard error of `X_hat` and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(X_hat*(1-X_hat)/N)
se_hat    

# Use `qnorm` to calculate the 95% confidence interval for the proportion of Clinton voters. Save the lower and then the upper confidence interval to a variable called `ci`.
ci<- c(X_hat - qnorm(0.975)*se_hat, X_hat + qnorm(0.975)*se_hat)
ci  

# Exercise 2 Create a new object called pollster_results that contains the pollster's name, the end date of the poll, 
# the proportion of voters who declared a vote for Clinton, the standard error of this estimate, 
# and the lower and upper bounds of the confidence interval for the estimate

pollster_results <- polls %>%
  mutate(name = pollster, 
         end_date = enddate, 
         X_hat = rawpoll_clinton/100, 
         se_hat = sqrt(rawpoll_clinton/100*(1-rawpoll_clinton/100)/samplesize),
         lower_bond = rawpoll_clinton/100 - qnorm(0.975) * sqrt(rawpoll_clinton/100)*(1-rawpoll_clinton/100)/samplesize,
         upper_bond = rawpoll_clinton/100 + qnorm(0.975) * sqrt(rawpoll_clinton/100*(1-rawpoll_clinton/100)/samplesize)) %>%
  select(name, end_date, X_hat, se_hat, lower_bond, upper_bond)

pollster_results

# Exercise 3 Add a column called hit to pollster_results that states if the confidence interval included the true proportion p=0.482 or not. 
# What proportion of confidence intervals included p?
p <- 0.482

avg_hit <- pollster_results %>% mutate(hit = lower_bond <=0.482 & upper_bond >= 0.482) %>% summarize(mean(hit))

#Exercsie 4 Assume that there are only two parties and that d=2p???1. Construct a 95% confidence interval for difference in proportions on election night.

# Add a statement to this line of code that will add a new column named `d_hat` to `polls`. The new column should contain the difference in the proportion of voters.
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")  %>%
  mutate(d_hat = rawpoll_clinton/100 - rawpoll_trump/100)
# Assign the sample size of the first poll in `polls` to a variable called `N`. Print this value to the console.
N <- polls$samplesize[1]
N

# Assign the difference `d_hat` of the first poll in `polls` to a variable called `d_hat`. Print this value to the console.
d_hat <- polls$d_hat[1]
d_hat    

# Assign proportion of votes for Clinton to the variable `X_hat`.
X_hat <- (d_hat+1)/2
X_hat    

# Calculate the standard error of the spread and save it to a variable called `se_hat`. Print this value to the console.
se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
se_hat    

## Use `qnorm` to calculate the 95% confidence interval for the difference in the proportions of voters. 
#Save the lower and then the upper confidence interval to a variable called `ci`
ci <- c(d_hat - qnorm(0.975)*de_hat, d_hat + qnorm(0.975)*d_hat)
ci <- c(d_hat - qnorm(0.975)*se_hat, d_hat + qnorm(0.975)*se_hat)
ci

#Exercise 5 In this case, it is more informative to estimate the spread or the difference between the proportion of two candidates d, or 0.482???0.461=0.021 for this election.
#polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")  %>%
polls <- polls_us_election_2016 %>% filter(enddate >= "2016-10-31" & state == "U.S.")  %>%
  mutate(d_hat = rawpoll_clinton/100 - rawpoll_trump/100)


N <- polls$samplesize[1]
N

d_hat <- polls$d_hat[1]
d_hat  

se_hat <- 2*sqrt(X_hat*(1-X_hat)/N)
se_hat  

ci <- c(d_hat - qnorm(0.975)*se_hat, d_hat + qnorm(0.975)*se_hat)

d_hat

# Exercise 6 - Create a new object called pollster_results that contains the pollster's name, the end date of the poll, 
#the difference in the proportion of voters who declared a vote either, and the lower and upper bounds of the confidence interval for the estimate.
#In the X_hat column, calculate the proportion of voters for Clinton using d_hat

head(polls)

pollster_results <-  polls %>%
  mutate(
    X_hat = (d_hat+1)/2, 
    se_hat = 2*sqrt(X_hat*(1-X_hat)/samplesize), 
    lower = d_hat - qnorm(0.975)*se_hat, 
    upper = d_hat + qnorm(0.975)*se_hat) %>% 
  select(pollster, enddate, X_hat, d_hat, lower, upper)        
pollster_results

# Wehen we calculate the conficence intervals for the spread this can go trough 0, as that menas that 0 is one of the option of our interval,
# and spread 0 means we cand determne who is going to win

# Exercise 7 - Add a logical variable called `hit` that indicates whether the actual value (0.021) exists within the confidence interval of each poll. 
#Summarize the average `hit` result to determine the proportion of polls with confidence intervals include the actual value.
#Save the result as an object called `avg_hit`.

avg_hit <- pollster_results %>% mutate(hit = lower<=0.021 & upper>=0.021) %>% summarise(mean(hit))
avg_hit    

# Exercise 8 - # Add variable called `error` to the object `polls` that contains the difference between d_hat and the actual difference on election day. 
#Then make a plot of the error stratified by pollster.
polls %>% mutate(error = d_hat - 0.021) %>% 
  ggplot(aes(pollster, error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Exercise 9 - make a plot of the error stratified by pollster, but only for pollsters who took 5 or more polls.
polls %>% mutate(error = d_hat - 0.021) %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>%
  ggplot(aes(pollster, error)) +
  geom_point() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))



# Statistical Models  

d <- 0.039
Ns <- c(1298, 533, 1342, 897, 774, 254, 812, 324, 1291, 1056, 2172, 516)
p <- (d+1)/2

#Cofidece itervales for te spread of different polls

confidence_intervals <- sapply(Ns, function(N){
  X <- sample(c(0,1), size=N, replace=TRUE, prob = c(1-p, p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat, X_hat - 2*SE_hat, X_hat + 2*SE_hat) -1
})
confidence_intervals

#Data frame to store the results
polls <- data.frame(poll = 1:ncol(confidence_intervals),
                    t(confidence_intervals),
                    sample_size = Ns)
names(polls) <- c("poll","estimate", "low", "high", "sample_size")
polls     

#Calculate the spread of combined polls

d_hat <- polls %>%
  summarize(avg = sum(estimate*sample_size)/sum(sample_size)) %>%
  .$avg
d_hat

p_hat <- (1+d_hat)/2
p_hat      

moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))   
moe      

round(d_hat*100, 1)      
round(moe*100, 1)      

#Poll Data and Pollster Bias
library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

# keep only national polls from week before election with a grade considered reliable
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade)))

# add spread estimate
polls <- polls %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# keep latest poll
one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%   
  ungroup()

one_poll_per_pollster %>%
  ggplot(aes(spread)) + geom_histogram(binwidth = 0.01)


#New model to treat the data directly.
#Our standard error will be pollster to pollster variability
#We have 2 unknown parametes, expected value(d) and standard deviation(sigma)
# As we are searching for an average of independent rando variables, it is normally distributed

sd(one_poll_per_pollster$spread)
results <- one_poll_per_pollster %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg-1.96*se, end = avg +1.96*se)
round(results*100, 1)      


# Assessment 4.1: Statistical Models
library(dslabs)
data(heights)
x <- heights %>% filter(sex == "Male") %>%
  .$height

# Exercise 1- Take a sample of size 50, with replacement, and construct an estimate for ?? and ??.
N <- 50

X <- sample(x, N, replace = TRUE)

mean(X)
sd(X)  

# Exercise 2- Construct a 95% confidence interval for ??. 
se <- sd(X)/sqrt(N)

ci <- c(lower = mean(X)-qnorm(0.975)*se, upper = mean(X)+qnorm(0.975)*se)
ci  

#Exercise 3 - Now run a Monte Carlo simulation in which you compute 10,000 confidence intervals as you have just done. 
#What proportion of these intervals include ??

mu <- mean(x) # We define mu as the population average

B <- 10000

res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qnorm(0.975)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})
mean(res)

#Exercise 4 - # Make a boxplot with points of the spread for each pollster

polls <- polls_us_election_2016 %>% 
  filter(pollster %in% c("Rasmussen Reports/Pulse Opinion Research","The Times-Picayune/Lucid") &
           enddate >= "2016-10-15" &
           state == "U.S.") %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) 

polls %>% ggplot(aes(pollster, spread)) + 
  geom_boxplot() + 
  geom_point()

# Exercise 5 - Compute the estimates of ??1 and ??2.
head(polls)

sigma <- polls %>% 
  group_by(pollster) %>%
  summarize(s = sd(spread))

sigma


# Exercise - 6 - Calculate the 95% Confidence Interval of the Spreads
#Construct a 95% confidence interval for the difference b2 and b1. Does this interval contain zero?
head(polls)

## Create an object called `res` that summarizes the average, standard deviation, and number of polls for the two pollsters.
res <- polls %>%
  group_by(pollster) %>%
  summarize(avg = mean(spread), s = sd(spread), N = sum(length(pollster)))
res
## Store the difference between the larger average and the smaller in a variable called `estimate`. Print this value to the console.
estimate <- res$avg[2] - res$avg[1]
estimate

## Store the standard error of the estimates as a variable called `se_hat`. Print this value to the console.
se_hat <- sqrt(res$s[2]^2/res$N[2] + res$s[1]^2/res$N[1])
se_hat

## Calculate the 95% confidence interval of the spreads. Save the lower and then the upper confidence interval to a variable called `ci`.
ci <- c(estimate-qnorm(0.975)*se_hat, estimate+qnorm(0.975)*se_hat)
ci  

#Exercise 7 - Use the pnorm function to calculate the probability that a random value is larger than the observed ratio of the estimate to the standard error.
2*(1 - pnorm(estimate/se_hat, 0, 1))


#Exercise 8 -  Comparing Within-Poll and Between-Poll Variability
# Analysis of Variance or ANOVA
#Compute the average and standard deviation for each pollster and examine the variability across the averages and how it compares to the variability within the pollsters, 
#summarized by the standard deviation.
polls <- polls_us_election_2016 %>% 
  filter(enddate >= "2016-10-15" &
           state == "U.S.") %>%
  group_by(pollster) %>%
  filter(n() >= 5) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ungroup()

## Create an object called `var` that contains columns for the pollster, mean spread, and standard deviation. 
var <- polls %>% group_by(pollster) %>%
  summarize(avg = mean(spread), s = sd(spread))
var

#Section 5: Bayesian Statistics 

#Bayes' Theorem
# Monte Carlo Simulation for Bayesian theory
prev <- 0.00025
N <- 10000
outcome <- sample(c("Disease", "Healthy"), N, replace = TRUE, prob = c(prev, 1-prev))

N_D <- sum(outcome == "Disease")
N_D
N_H <- sum(outcome == "Healthy")
N_H

accuracy <- 0.99
test <- vector("character", N)
test[outcome == "Disease"] <- sample(c("+", "-"), N_D, replace=TRUE, prob = c(accuracy, 1-accuracy))
test[outcome == "Healthy"] <- sample(c("-", "+"), N_H, replace=TRUE, prob = c(accuracy, 1-accuracy))
test
table(outcome, test)

# Assessment 5.1: Bayesian Statistics. 

#Exercise 6 - Back to Election Polls. The Prior Distribution
library(dplyr)
library(dslabs)
data(polls_us_election_2016)

polls <- polls_us_election_2016 %>% 
  filter(state == "Florida" & enddate >= "2016-11-04" ) %>% 
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
head(polls)

## Create an object called `results` that has two columns containing the average spread (`avg`) and the standard error (`se`) in the Prior Distribution.

results <- polls %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(n()))

results

#Exercise 7- Exercise 8 - Estimate the Posterior Distribution
# Use the formulas for the posterior distribution to calculate the expected value of the posterior distribution if we set ??=0 and ??=0.01.

##The results` object has already been loaded. Examine the values stored: `avg` and `se` of the spread
results

## Define `mu` and `tau`
mu <- 0
tau <- 0.01

## Define a variable called `sigma` that contains the standard error in the object `results`
sigma <- results$se

## Define a variable called `Y` that contains the average in the object `results`
Y <- results$avg


## Define a variable `B` using `sigma` and `tau`. Print this value to the console.
B <-(sigma^2)/(sigma^2 + tau^2)
B

## Calculate the expected value of the posterior distribution
E <- (B*mu) + (1-B)* Y

# Exercise 9 - Standard Error of the Posterior Distribution
sqrt((1)/((1/sigma^2)+(1/tau^2)))

# Exercise 10 - Construct the 95% credible interval. Save the lower and then the upper confidence interval to a variable called `ci`.
mu <- 0
tau <- 0.01
sigma <- results$se
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
E <- (B*mu) + (1-B)* Y
E
se

ci <- c(E- qnorm(0.975)*se, E + qnorm(0.975)*se)
ci

ci2 <- B*mu + (1-B)*Y + c(-1,1)*qnorm(0.975)*sqrt( 1/ (1/sigma^2 + 1/tau^2))
ci2

# Exercise 11 - Odds of Winning Florida

## Assign the expected value of the posterior distribution to the variable `exp_value`
exp_value <- B*mu + (1-B)*Y 

## Assign the standard error of the posterior distribution to the variable `se`
se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))

# Using the `pnorm` function, calculate the probability that the actual spread was less than 0 (in Trump's favor). Print this value to the console.
pnorm(0, exp_value, se)

# Exercise 12 - Change the prior variance to include values ranging from 0.005 to 0.05 and observe how the probability of Trump winning Florida changes by making a plot.
mu <- 0
sigma <- results$se
Y <- results$avg
taus <- seq(0.005, 0.05, len = 100)

## Create a function called `p_calc` that generates `B` and calculates the probability of the spread being less than 0
p_calc <- function(tau){
  B <- sigma^2 / (sigma^2 + tau^2)
  se <- sqrt( 1/ (1/sigma^2 + 1/tau^2))
  exp_value <- B*mu + (1-B)*Y 
  pnorm(0, exp_value, se)
  
}
ps <- p_calc(taus)
ps

plot( x = taus, y = ps)


  # Section 6 Election Forecasting


# Definition of results object
polls <- polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

one_poll_per_pollster <- polls %>% group_by(pollster) %>%
  filter(enddate == max(enddate)) %>%
  ungroup()

results <- one_poll_per_pollster  %>%
  summarize(avg = mean(spread), se = sd(spread)/sqrt(length(spread))) %>%
  mutate(start = avg -1.96*se, end =avg + 1.96*se)
results

# Computing the posterior mean, standard error, credible interval and probability

mu <- 0
tau <- 0.35
sigma <- results$se
Y <- results$avg
B <- sigma^2/(sigma^2 + tau^2)
posterior_mean  <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))
posterior_mean
posterior_se

  # 95% credible interval
posterior_mean + c(-1.96, 1.96)*posterior_se

  # Probability of d > 0
1- pnorm(0, posterior_mean, posterior_se)

# Mathematical Representations of Models
#  Key points
  #  If we collect several polls with measured spreads  X1,...,Xj  with a sample size of  N , these random variables have expected value  d  and standard error  2p(1???p)/N????????????????????????????????? .
  #    We represent each measurement as  Xi,j=d+b+hi+??i,j  where:
  #  The index  i  represents the different pollsters
  #  The index  j  represents the different polls
  #  Xi,j  is the  j th poll by the  i th pollster 
  #  d  is the actual spread of the election
  #  b  is the general bias affecting all pollsters
  #  hi  represents the house effect for the  i th pollster
  #  ??i,j  represents the random error associated with the  i,j th poll.
  #  The sample average is now  X?=d+b+1N???i=1NXi  with standard deviation  SE(X?)=??2/N+??2b?????????????????????????????? .
  #  The standard error of the general bias  ??b  does not get reduced by averaging multiple polls, which increases the variability of our final estimate.

# Simulated data with  Xj=d+??j
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
X <- d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
X

# Simulated data with  Xi,j=d+??i,j
I <- 5
J <-6
N <- 2000
d<- .021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
X

# Simulated data with  Xi,j=d+hi+??i,j

I <- 5
J <- 6
N <- 2000
d <- .021
p <- (d+1)/2
h <- rnorm(I, 0, 0.025)    # assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + h[i] + rnorm(J, 0, 2*sqrt(p*(1-p)/N))
})
X


mu <- 0
tau <- .035
sigma <- sqrt(results$se^2 + .025^2)
Y <- results$avg
B <- sigma^2 / (sigma^2 + tau^2)

posterior_mean <- B*mu + (1-B)*Y
posterior_se <- sqrt(1 / (1/sigma^2 + 1/tau^2))

1 - pnorm(0, posterior_mean, posterior_se)

#Predicting the Electoral College

library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
head(results_us_election_2016)

results_us_election_2016 %>% arrange(desc(electoral_votes)) %>% top_n(5, electoral_votes)

# Computing the average and standard deviation for each state

results <- polls_us_election_2016 %>%
  filter(state != "U.S." &
           !grepl("CD", "state") &
           enddate >= "2016-10-31" &
           (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  group_by(state) %>%
  summarize(avg = mean(spread), sd = sd(spread), n = n()) %>%
  mutate(state = as.character(state))
results

# 10 closest races = battleground states
results %>% arrange(abs(avg))
         
# joining electoral college votes and results
results <- left_join(results, results_us_election_2016, by = "state")
results

# states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

# assigns sd to states with just one poll as median of other sd values
results <- results %>%
  mutate(sd = ifelse(is.na(sd), median(results$sd, na.rm = TRUE), sd))

# Calculating the posterior mean and posterior standard error
mu <- 0
tau <- 0.02
results %>% mutate(sigma = sd/sqrt(n),
                   B = sigma^2/ (sigma^2 + tau^2),
                   posterior_mean = B*mu + (1-B)*avg,
                   posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2))) %>%
  arrange(abs(posterior_mean))

# Monte Carlo simulation of Election Night results (no general bias)
mu <-0
tau <- .02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
                     B = sigma^2/(sigma^2 +tau^2),
                     posterior_mean = B*mu +(1-B)*avg,
                     posterior_se = sqrt(1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%
    summarize(clinton = sum(clinton)) %>%
    .$clinton + 7
    
})
clinton_EV

mean(clinton_EV > 269)

data.frame(clinton_EV ) %>%
  ggplot(aes(clinton_EV)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

# Monte Carlo simulation including general bias

mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election
clinton_EV_2

data.frame(clinton_EV_2) %>%
  ggplot(aes(clinton_EV_2)) +
  geom_histogram(binwidth = 1) +
  geom_vline(xintercept = 269)

# Forecasting
# Yi,j,t=d+b+hj+bt+f(t)+??i,j,t

# Select all national polls by one pollster
  one_pollster <- polls_us_election_2016 %>%
    filter(pollster == "Ipsos" & state == "U.S.") %>%
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)
  
  one_pollster

# The distribution of the data is not normal
  se <- one_pollster %>%
    summarize(empirical = sd(spread),
              theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
  se
  
  # the distribution of the data is not normal
  one_pollster %>% ggplot(aes(spread)) +
    geom_histogram(binwidth = 0.01, color = "black")
  
 # Trend across time for several pollsters
  polls_us_election_2016 %>%
    filter(state == "U.S." & enddate >= "2016-07-01") %>%
    group_by(pollster) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
    ggplot(aes(enddate, spread)) +
    geom_smooth(method = "loess", span = 0.1) +
    geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)
  
#Plotting raw percentages across time
  polls_us_election_2016 %>%
    filter(state == "U.S." & enddate >= "2016-07-01") %>%
    select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
    rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
    gather(candidate, percentage, -enddate, -pollster) %>%
    mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
    group_by(pollster) %>%
    filter(n() >= 10) %>%
    ungroup() %>%
    ggplot(aes(enddate, percentage, color = candidate)) +
    geom_point(show.legend = FALSE, alpha = 0.4) +
    geom_smooth(method = "loess", span = 0.15) +
    scale_y_continuous(limits = c(30, 50))
  

# Assessment 6.1: Election Forecasting
  
  #Exercise 1 - Confidence Intervals of Polling Data
  #For each poll in the polling data set, use the CLT to create a 95% confidence interval for the spread. 
  #Create a new table called cis that contains columns for the lower and upper limits of the confidence intervals.
  library(dplyr)
  library(dslabs)
  data("polls_us_election_2016")
  
  polls <- polls_us_election_2016 %>% 
    filter(state != "U.S." & enddate >= "2016-10-31") %>% 
    mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

  
  cis <- polls %>% mutate(X_hat = (spread+1)/2, se = 2*sqrt(X_hat*(1-X_hat)/samplesize),
                        lower = spread - qnorm(0.975) * se , upper = spread + qnorm(0.975)*se) %>%
  select(state, startdate, enddate, pollster, grade, spread, lower, upper)

head(cis)


  # Exercise 2 - Now determine how often the 95% confidence interval includes the actual result.
add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)

ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
head(ci_data)

p_hits <- ci_data %>% mutate( hit = lower <= actual_spread & upper >= actual_spread) %>% summarize(mean(hit))
p_hits

  # Exercise 3 Now find the proportion of hits for each pollster. Show only pollsters with at least 5 polls and order them from best to worst. 
  #Show the number of polls conducted by each pollster and the FiveThirtyEight grade of each pollster.

add <- results_us_election_2016 %>% mutate(actual_spread = clinton/100 - trump/100) %>% select(state, actual_spread)
ci_data <- cis %>% mutate(state = as.character(state)) %>% left_join(add, by = "state")
head(ci_data)

p_hits <- ci_data %>% mutate( hit = lower <= actual_spread & upper >= actual_spread) %>% group_by(pollster) %>% filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit), numer_polls = n(), grade = grade[1]) %>% arrange(desc(proportion_hits))
p_hits

  # Exercise 4 - Repeat the previous exercise, but instead of pollster, stratify by state. 
p_hits <- ci_data %>% mutate( hit = lower <= actual_spread & upper >= actual_spread) %>% group_by(state) %>% filter(n() >= 5) %>%
  summarize(proportion_hits = mean(hit), numer_polls = n()) %>% arrange(desc(proportion_hits))
p_hits

  # Exercise 5 - Make a barplot based on the result from the previous exercise.
p_hits %>%
  ggplot(aes(x = state, y = proportion_hits )) +
  geom_bar(stat = "identity") +
  coord_flip()


  # Exercise 6- Compute, for each poll, the difference between the predicted spread and the actual spread, 
head(cis)
head(ci_data)
errors <- ci_data %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
tail(errors)


  # Exercise 7- Create an object called p_hits that contains the proportion of instances when the sign of the actual spread matches the predicted spread for states with 5 or more polls.
errors <- ci_data %>% mutate(error = spread - actual_spread, hit = sign(spread) == sign(actual_spread))
errors

p_hits <- errors %>% group_by(state) %>% filter(n() >=5) %>%
  summarize(proportion_hits = mean(hit), n = n())

p_hits %>% mutate(state = reorder(state, proportion_hits)) %>%
  ggplot(aes(state, proportion_hits)) + 
  geom_bar(stat = "identity") +
  coord_flip()

  # Exercise 8 - Make a histogram of the errors. What is the median of these errors?
hist(errors$error)
median(errors$error)

  # Exercise 9 - Create a boxplot to examine if the bias was general to all states or if it affected some states differently. 
  # Filter the data to include only pollsters with grades B+ or higher
head(errors)

errors %>% filter(grade %in% c("A+", "A", "A-", "B+") | is.na(grade)) %>% 
mutate(state = reorder(state, error)) %>%
  ggplot(aes(x = state, y = error)) + 
  geom_boxplot() + 
  geom_point() +
  coord_flip()

  #Exercise 10 -  Repeat the previous exercise to plot the errors for each state, but only include states with five good polls or more.

errors %>% filter(grade %in% c("A+", "A", "A-", "B+") | is.na(grade), n()>=5) %>% 
  mutate(state = reorder(state, error)) %>%
  ggplot(aes(x = state, y = error)) + 
  geom_boxplot() + 
  geom_point() +
  coord_flip()


# The t-Distribution
# Calculating 95% confidence intervals with the t-distribution
z <- qt(0.975, nrow(one_poll_per_pollster) - 1)
?qt

  #Assessment 6.2: The t-Distribution

# Exercise 1- Calculate the probability of seeing t-distributed random variables being more than 2 in absolute value when 'df = 3'.
1-pt(2, 3) + pt(-2,3)

# Exercise 2 - Now use sapply to compute the same probability for degrees of freedom from 3 to 50.
# Make a plot and notice when this probability converges to the normal distribution's 5%.
df <- seq(3, 50)

pt_func <- function(df) 1-pt(2, df) + pt(-2, df)

probs <- pt_func(df)

plot(x = df, y = probs)

# Exercise 3 - In a previous section, we repeatedly took random samples of 50 heights from a distribution of heights. 
# We noticed that about 95% of the samples had confidence intervals spanning the true population mean.
# Re-do this Monte Carlo simulation, but now instead of N=50, use N=15. Notice what happens to the proportion of hits.
library(dslabs)
library(dplyr)
data(heights)
     
## Use the sample code to generate 'x', a vector of male heights

x <- heights %>% filter(sex == "Male") %>%
  .$height
x
## Create variables for the mean height 'mu', the sample size 'N', and the number of times the simulation should run 'B'

mu <- mean(x)
N <- 15
B <- 10000

set.seed(1, sample.kind="Rounding")

## Generate a logical vector 'res' that contains the results of the simulations
res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qnorm(0.975)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})
mean(res)

# Exercise - 4 Repeat the previous Monte Carlo simulation using the t-distribution instead of using the normal distribution to construct the confidence intervals.
res <- replicate(B, {
  X <- sample(x, N, replace=TRUE)
  interval <- mean(X) + c(-1,1)*qt(0.975, N-1)*sd(X)/sqrt(N)
  between(mu, interval[1], interval[2])
})
mean(res)


interval <- mean(X) + c(-1,1)*qt(0.975, 15)*sd(X)/sqrt(N)
interval

# Association Tests
# Fisher's exact test determines the p-value as the probability of observing an outcome as extreme or more extreme than the observed outcome given the null distribution.

  # Two-by-two table and p-value for the Lady Tasting Tea problem
tab <- matrix(c(3,1,1,3),2,2)

rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab
fisher.test(tab, alternative = "greater")

#Chi-Squared Tests

library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)
totals

  ## Compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + yes_women)) %>%
  .$percent_total
funding_rate

  ## Construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

  ## Compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

  ## Chis -aquared test
two_by_two %>% select(-awarded) %>% chisq.test()

  ## Odds ratio of getting funded for men
    odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
      (two_by_two$men[1] / sum(two_by_two$men))
    
    # odds of getting funding for women
    odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
      (two_by_two$women[1] / sum(two_by_two$women))
    
    # odds ratio - how many times larger odds are for men than women
    odds_men/odds_women
    
  
# Assessment 7.1: Association and Chi-Squared Tests

# Exercise 1 - In this exercise, filter the errors data for just polls with grades A- and C-. 
#Calculate the proportion of times each grade of poll predicted the correct winner.
head(errors)

  # Generate an object called 'totals' that contains the numbers of good and bad predictions for polls rated A- and C-
  totals <- errors %>% filter(grade %in% c("A-", "C-")) %>%
    group_by(grade, hit) %>%
    summarize(num= n()) %>%
    spread(grade, num)
  totals
  
    # Print the proportion of hits for grade A- polls to the console
  totals[[2,3]]/sum(totals[[3]])
  
   # Print the proportion of hits for grade C- polls to the console
  totals[[2,2]]/sum(totals[[2]])
  

  # Exercise 2 - Use a chi-squared test to determine if these proportions are different.
  chisq_test <- totals %>% 
    select(-hit) %>%
    chisq.test()
  chisq_test
  
  # Exercise 3 - Calculate the odds ratio to determine the magnitude of the difference in performance between these two grades of polls.
  
  odds_C <- (totals[[2,2]] / sum(totals[[2]])) / 
    (totals[[1,2]] / sum(totals[[2]]))
  
  odds_A <- (totals[[2,3]] / sum(totals[[3]])) / 
    (totals[[1,3]] / sum(totals[[3]]))
    
  
  odds_A/odds_C
  
  
    ###Brexit poll analysis - Part 1###
  
  library(tidyverse)
  options(digits = 3)
  
  library(dslabs)
  data(brexit_polls)
  
  p <- 0.481    # official proportion voting "Remain"
  d <- 2*p-1    # official spread

#Question 1: Expected value and standard error of a poll
#Consider a poll with a sample of  N=1500  voters.
  
  #What is the expected total number of voters in the sample choosing "Remain"?
  N <- 1500
  N*p

  # What is the standard error of the total number of voters in the sample choosing "Remain"?
   sqrt(N*(p*(1-p)))
 
  # What is the standard error of  X^ , the proportion of "Remain" voters?
   sqrt(p*(1-p)/N)
   
  #What is the standard error of  d , the spread between the proportion of "Remain" voters and "Leave" voters?
   
   2*sqrt(0.481 *(1-0.481 )/N) 
   
#Question 2: Actual Brexit poll estimates
# Calculate x_hat for each poll, the estimate of the proportion of voters choosing "Remain" on the referendum day ( p=0.481 ), 
#given the observed spread and the relationship  d^=2X^???1 . Use mutate() to add a variable x_hat to the brexit_polls object by filling in the skeleton code below:
head(brexit_polls)

brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1) /2)
brexit_polls  

N <- 127

  # What is the average of the observed spreads (spread)?
brexit_polls <- brexit_polls %>%
  mutate(x_hat = (spread + 1)/2)
mean(brexit_polls$spread)

  # What is the standard deviation of the observed spreads?
sd(brexit_polls$spread)

  #What is the average of x_hat, the estimates of the parameter  p ?
mean(brexit_polls$x_hat)
 
  # What is the standard deviation of the observed spreads?
sd(brexit_polls$spread)

  # What is the standard deviation of x_hat?
sd(brexit_polls$spread)

# Question 3: Confidence interval of a Brexit poll
# Consider the first poll in brexit_polls, a YouGov poll run on the same day as the Brexit referendum:
brexit_polls[1,]

  # Use qnorm() to compute the 95% confidence interval for X^.
  x_hat_youGov <- .52  
  se_hat <- sqrt( 0.52*(1- 0.52)/4772)
  
  ci<- c(x_hat_youGov - qnorm(0.975)*se_hat, x_hat_youGov + qnorm(0.975)*se_hat)
  ci
  
  
    ###Brexit poll analysis - Part 2###
 
# Question 4: Confidence intervals for polls in June
# Create the data frame june_polls containing only Brexit polls ending in June 2016 (enddate of "2016-06-01" and later). 
# We will calculate confidence intervals for all polls and determine how many cover the true value of  d .
d <- -0.038 #Correct spread

june_polls <- brexit_polls %>%
  filter(enddate >= "2016-06-01") %>% 
  mutate(se_x_hat = sqrt(x_hat*(1 - x_hat) / samplesize), 
         se_spread = 2*sqrt(x_hat*(1-x_hat)/samplesize ),
         lower = spread - qnorm(0.975) * se_spread, 
         upper = spread + qnorm(0.975) * se_spread,
         hit = (lower <= d & upper>=d))
june_polls


#What proportion of polls have a confidence interval that covers the value 0?
june_polls %>%
  mutate(interval_cero = lower <= 0 & upper >= 0) %>%
  summarize(mean(interval_cero))
  
#What proportion of polls predict "Remain" (confidence interval entirely above 0)?
june_polls %>%
  filter(lower>0)
4/32


# What proportion of polls have a confidence interval covering the true value of  d ?
mean(june_polls$hit)

  # Question 5: Hit rate by pollster
  # Group and summarize the june_polls object by pollster to find the proportion of hits for each pollster and the number of polls per pollster. 
  # Use arrange() to sort by hit rate.
june_polls %>% 
  group_by(pollster) %>%
  summarize(hit_rate = mean(hit),
            number_polls = length(pollster)) %>%
  arrange(desc(hit_rate))

  # Question 6: Boxplot of Brexit polls by poll type
june_polls %>%
  ggplot(aes(spread, poll_type)) +
  geom_boxplot()

  # Question 7: Combined spread across poll type
  # Calculate the confidence intervals of the spread combined across all polls in june_polls, grouping by poll type.
  # Recall that to determine the standard error of the spread, you will need to double the standard error of the estimate.
combined_by_type <- june_polls %>%
  group_by(poll_type) %>%
  summarize(N = sum(samplesize),
            spread = sum(spread*samplesize)/N,
            p_hat = (spread + 1)/2)
combined_by_type
  
    # What is the lower bound of the 95% confidence interval for online voters?
combined_by_type %>%
  filter(poll_type == "Online") %>%
  mutate(se_p_hat = sqrt(p_hat*(1-p_hat)/N),
         se_spread_online = 2*se_p_hat,
         lower = spread - qnorm(0.975) * se_spread_online, 
         upper = spread + qnorm(0.975) * se_spread_online)

combined_by_type %>%
  filter(poll_type == "Telephone") %>%
  mutate(se_p_hat = sqrt(p_hat*(1-p_hat)/N),
         se_spread_telephone = 2*se_p_hat,
         lower = spread - qnorm(0.975) * se_spread_telephone, 
         upper = spread + qnorm(0.975) * se_spread_telephone)

   ## Brexit poll analysis - Part 3 ##

  #Question 9: Chi-squared p-value
  #Define brexit_hit, with the following code, which computes the confidence intervals for all Brexit polls in 2016 and then calculates whether the 
  #confidence interval covers the actual value of the spread  d=???0.038 :
brexit_hit <- brexit_polls %>%
  mutate(p_hat = (spread + 1)/2,
         se_spread = 2*sqrt(p_hat*(1-p_hat)/samplesize),
         spread_lower = spread - qnorm(.975)*se_spread,
         spread_upper = spread + qnorm(.975)*se_spread,
         hit = spread_lower < -0.038 & spread_upper > -0.038) %>%
  select(poll_type, hit)
brexit_hit
  
  #Use brexit_hit to make a two-by-two table of poll type and hit status. 
  #Then use the chisq.test() function to perform a chi-squared test to determine whether the difference in hit rate is significant.

Online_no <- brexit_hit%>%filter(poll_type=="Online"& hit=="FALSE") %>% nrow() 
Online_yes <- brexit_hit%>%filter(poll_type=="Online"& hit=="TRUE") %>% nrow()

phone_no <- brexit_hit%>%filter(poll_type=="Telephone"& hit=="FALSE")%>% nrow() 
phone_yes <- brexit_hit%>%filter(poll_type=="Telephone"& hit=="TRUE")%>% nrow()

two_by_two<- tibble(Hit=c("yes","no"), Online = c(Online_yes,Online_no), Telephone= c(phone_yes,phone_no))

two_by_two

two_by_two %>% select(-Hit) %>% chisq.test()

# Question 10: Odds ratio of online and telephone poll hit rate

  # Calculate the odds that an online poll generates a confidence interval that covers the actual value of the spread.
  odds_online <- two_by_two$Online[1] / two_by_two$Online[2]
  odds_online
  # Calculate the odds that a telephone poll generates a confidence interval that covers the actual value of the spread.
  odds_phone <- two_by_two$Telephone[1] / two_by_two$Telephone[2]
  odds_phone
  #Calculate the odds ratio to determine how many times larger the odds are for online polls to hit versus telephone polls.
  odds_online / odds_phone
  
# Question 11: Plotting spread over time
  brexit_polls %>%
    ggplot(aes(enddate, spread, color = poll_type)) +
    geom_smooth(method = "loess", span = 0.4) +
    geom_point() +
    geom_hline(aes(yintercept = -.038))
  
  head(brexit_polls)

#Question 12: Plotting raw percentages over time
  brexit_long <- brexit_polls %>%
    gather(vote, proportion, "remain":"undecided") %>%
    mutate(vote = factor(vote))
  
 # Make a graph of proportion over time colored by vote. Add a smooth trendline with geom_smooth() and method = "loess" with a span of 0.3.
  brexit_long %>%
    ggplot(aes(enddate, proportion, color = vote)) +
    geom_smooth(method = "loess", span = 0.3)
  
 
  