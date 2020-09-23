#Data Science: Probability
#17/08/20
library(dslabs)
library(dplyr)
options(digits = 22)

#DISCRETE PROBABILITY

#The probability of an event is the proportion of times the event occurs when we repeat the 
#experiment independently under the same conditions.

#Pr(A)=probability of event A

#Monte Carlo Simulations. 

#sample() <- Random Number Generaton
beads <- rep(c("red", "blue"), times = c(2, 3))
beads
sample(beads, 1)

#Monte Carlo Simulation <- Repit the experimente a large number of times to aproximate the results to the ones
# optained if repeted infinetly (the limit). <- replicate()

#Repete and event 10.000 times
B <-100000
events <- replicate(B, sample(beads, 1))

tab <- table(events)
tab
prop.table(tab)

#If we use sample without replicate(), the experiment is done without replacement.
sample(beads, 5)
sample(beads, 5)
sample(beads, 5)
sample(beads, 6)

#We can solve this with replace argument too
events <- sample(beads, B, replace = TRUE)
prop.table(table(events))

#Setting the Random Seed
#Throughout this book, we use random number generators. This implies that many of the results presented can 
#actually change by chance, which then suggests that a frozen version of the book may show a different result
#than what you obtain when you try to code as shown in the book.

#You can set R's random number generation seed to a specific number, to ensuere the numbers are always the same
set.seed(1986) # R 3.6 Notation

set.seed(1, sample.kind="Rounding") #for R 3.5 Notation (like in the book )


#In R, applying the mean() function to a logical vector returns the proportion of elements that are TRUE. 
#It is very common to use the mean() function in this way to calculate probabilities and we will do so throughout the course.
mean(beads == "blue")
mean(beads =="red")

#Probability distribution <- for categorical data <- the proportion of each group

# Conditional probabilities compute the probability that an event occurs given information about dependent events.
# Pr(Card 2 is a king ??? Card 1 is a king)=3/51

#If two events  A  and  B  are independent,  Pr(A???B)=Pr(A) .

#Multiplication rule <- the probability of multiple events happening
#For independent events: Pr(A and B and C)=Pr(A)×Pr(B)×Pr(C)
#For dependetn evets (we need conditional probabilities): Pr(A and B)=Pr(A)×Pr(B???A)
#For dependent and more than 2 events: Pr(A and B and C)=Pr(A)×Pr(B???A)×Pr(C???A and B)

#Assessment: Introduction to Discrete Probability

#Exercise 1 One ball will be drawn at random from a box containing: 3 cyan balls, 5 magenta balls, and 7 yellow balls.

#What is the probability that the ball will be cyan?
ball <- rep(c("cyan", "magenta", "yellow"), times = c (3,5,7))
mean(ball == "cyan")

#What is the probability that the ball will be  not cyan?
1-0.2

# Without replacement, What is the probability that the first draw is cyan and that the second draw is not cyan?
ball <- rep(c("cyan", "magenta", "yellow"), times = c (3,5,7))
p <- mean(ball == "cyan")

ball2 <-  rep(c("cyan", "magenta", "yellow"), times = c (2,5,7))
p2 <- 1 -mean(ball2 == "cyan")

p1*p2

#Alternative
cyan <- 3
magenta <- 5
yellow <- 7

p_1 <- cyan / (cyan + magenta + yellow)
p_2 <- 1 - (cyan - 1) / (cyan + magenta + yellow - 1)
p_1*p_2



#With replacement, What is the probability that the first draw is cyan and that the second draw is not cyan?
p *(1-p)

#Combinations and Permutations

#paste() <- creates strings by joining smaller strings
number <- "Three"
suit <- "Hearts"
paste(number, suit)

paste(letters[1:5], as.character(1:5))

#Expand.grid <- all the combinantions of 2 lists
expand.grid(pants = c("blue", "black"), shirt = c("white", "grey", "plaid"))

#Creating a card deck
suits <- c( "Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck <- expand.grid (number = numbers, suit = suits)  
deck <- paste(deck$number, deck$suit)
deck

#The probability of a king in the first card
kings <- paste("King", suits)
mean(deck %in% kings)

#The conditional probability of the second card is a King givven the first was a king 
install.packages("gtools")
library(gtools)

#permutation() <- for any list of size N, the different way we can select R items
permutations(5, 2)
permutations(5, 3)
permutations(6, 2)

#All possible phone numbers
all_phone_numbers <- permutations(10, 7, v = 0:9)
all_phone_numbers

#All combination of poker hand
hands <- permutations(52, 2, v = deck)
hands
#We define the first and the second card
first_card <- hands[,1]
second_card <- hands[,2]

#How many cases have a king as a first card
sum(first_card %in% kings)

#Conditional probability. What fraction of these 204 have also a king in the second hand
sum(first_card %in% kings & second_card %in% kings) /
  sum(first_card %in% kings)

#We can do the same by calculating the proportions instead of the totals
mean(first_card %in% kings & second_card %in% kings) /
  mean(first_card %in% kings) #The matematical formula is Pr(B|A=Pr(A and B)/Pr(A))

#combination() <- order does not matter
combinations(3,2)
permutations(3,2)  

#Probability of a natural 21 in Back Jack
#Vector that includes all the aces
aces <- paste("Ace", suits)
#Vector that includes all the facecards
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard <- paste(facecard$number, facecard$suit)
#All combinations
hands <- combinations(52, 2, v=deck) 
hands
# probability of a natural 21 given that the ace is listed first in `combinations`
mean(hands[,1] %in% aces & hands[,2] %in% facecard)

#Montecarlo Simulation to estimate the probability
hand <- sample(deck, 2)
hand

B <- 100000
results <- replicate (B, {
  hand <- sample(deck, 2)
  hand[1] %in% aces & hand[2] %in% facecard |
    hand[2] %in% aces & hand[1] %in% facecard
})
mean(results)

#The Birthday Problem. With Monte Carlo

#duplicated() <- returns true when an element of a vector has already apeared in that vector

n <-50
B <- 10000
results <- replicate(B, {    # returns vector of B logical values
  bdays <- sample(1:365, n, replace = TRUE)
  any(duplicated(bdays))
})
mean(results)
mean(results)

#compute probabilities of two people having the same brithday
#We create a function to canculate duplicated Bdays on different values of n
compute_prob <- function(n, B=10000){
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
n <- seq(0,65)
# Operations are done in the vectors individualy, elementwise
#you cant just send any function to a venctor 
# sapply () <- permits element whise operation on any function
prob <- sapply(n, compute_prob)
plot(n, prob)

#Compute the exact value without Monte Carlo
exact_prob <- function(n){
  prob_unique <- seq(365, 365-n+1)/365
  1- prod (prob_unique)
}
eprob <- sapply(n, exact_prob)

plot(n, prob) 
lines(n, eprob, col= "red")

#How many Monte Carlo experiments are enough?
#Easy aproach, check for the stability of the estimate
#Example bday proble n=22

B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}

prob <- sapply(B, compute_prob)    # apply compute_prob to many values of B
plot(log10(B), prob, type = "l")    # plot a line graph of estimates 

#DataCamp Assessment: Combinations and Permutations
#Exercise 1. Celtics vs Cavs
#Cavs have 60% chance of wining
# Create a Monte Carlo simulation estimating how frequently the Celtics win at least 1 of 4 games. B, 10.000
set.seed(1, sample.kind="Rounding")
B <-10000

simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))


celtic_wins <- replicate(B, {
  simulated_games <- sample(c("lose","win"), 4, replace = TRUE, prob = c(0.6, 0.4))
  any(simulated_games=="win")
})

mean(celtic_wins)

# The Addition Rule  
# The addition rule states that the probability of event  A  or event  B  happening is the probability of event  A  plus the probability 
# of event  B  minus the probability of both events  A  and  B  happening together.
# Pr(A or B)=Pr(A)+Pr(B)???Pr(A and B)

# The Monty Hall Problem

# Monte Carlo simulation of stick strategy
B <- 10000
stick <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))
  prize_door <- doors[prize == "car"]
  my_pick <- sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  stick <- my_pick
  stick == prize_door
})
mean(stick)

# Monte Carlo simulation of switch strategy
switch <- replicate(B, {
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat"))
  prize_door <- doors[prize == "car"]
  my_pick <-sample(doors, 1)
  show <- sample(doors[!doors %in% c(my_pick, prize_door)], 1)
  switch <- doors[!doors %in% c(show, my_pick)]
  switch == prize_door
  
})
mean(switch)

#Exercises

medals <- c("Gold", "Silver", "Bronze")
runers <- as.character(1:8)
paste(medals, runers)

winers <- expand.grid(medals = c("Gold", "Silver", "Bronze"), runers = as.character(1:8))
winers <- expand.grid(runers = as.character(1:8), medals = c("Gold", "Silver", "Bronze") )

# Question 1. How many different ways can the 3 medals be distributed across 8 runners?
permutations(8, 3)

# Question 2 Probability of all 3 medals won by jamaicans
(3/8)*(2/7)*(1/6)

# Question 3 Run a Monte Carlo simulation on this vector representing the countries of the 8 runners in this race.
#select 3 runners representing the 3 medalists and check whether they are all from Jamaica. Repeat this simulation 10,000 times.
#Set the seed to 1 before running the loop.
#Calculate the probability that all the runners are from Jamaica.
set.seed(1)
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")

B <- 10000
all_jamaica <- replicate(B, {
  results <- sample(runners, 3)
  all(results == "Jamaica")
})
mean(all_jamaica)


# Question 2: Restaurant management
#A meal at the restaurant includes 1 entree, 2 sides, and 1 drink. He currently offers a choice of 1 entree from a list of 6 options,
#a choice of 2 different sides from a list of 6 options, and a choice of 1 drink from a list of 2 options.

# Question 2a  How many meal combinations are possible with the current menu?
enter <- as.character(1:6)

side <- combinations(6, 2, v = 1:6)
side
side <- as.character(1:15)

drink <- as.character(1:2)

expand.grid(enter, side, drink)

# Question 2b How many combinations are possible if he expands his original special to 3 drink options?
drink <- as.character(1:3)
expand.grid(enter, side, drink)

# Question 2c How many meal combinations are there if customers can choose from 6 entrees, 3 drinks, and select 3 sides from the current 6 options?
drink <- as.character(1:3)
side <- combinations(6, 3, v = 1:6)
side
side <- as.character(1:20)
side
expand.grid(enter, side, drink)
333+27  

#Question 2d Write a function that takes a number of entree choices and returns the number of meal combinations possible 
#given that number of entree options, 3 drink choices, and a selection of 2 sides from 6 options.
side <- as.character(1:15)
drink <- as.character(1:3)
enter <- combinations(6, n)

nrow(combinations(9,1)) * nrow(combinations(6,2)) * 3

choices <- function(n){
  nrow(combinations(n,1)) * nrow(combinations(6,2)) * 3
}

n <- 1:12
combos <-sapply(n, choices)
ggplo(entrees = 1:12, combos=combos)


#Question 2e. Write a function that takes a number of side choices and returns the number of meal combinations possible given 6 entree choices,
#3 drink choices, and a selection of 2 sides from the specified number of side choices.
6 * nrow(combinations(n,2)) * 3

choices <- function(n) {
  6 * nrow(combinations(n,2)) * 3
  
}

n <-2:12
combos <- sapply(n, choices)
ggplo(sides = 2:12, combos=combos)

#Esophageal cancer and alcohol/tobacco use, part 1
library(tidyverse)
head(esoph)
esoph

#How many cases are there?
all_cases <- sum(esoph$ncases)
all_cases

#How many controls are there?
all_controls <- sum(esoph$ncontrols)
all_controls

#What is the probability that a subject in the highest alcohol consumption group is a cancer case?
drunks <- esoph %>%
  filter(alcgp == "120+")

drunks_cancer <- sum(drunks$ncases)
drunks_control <- sum(drunks$ncontrols)

drunks_cancer/(drunks_cancer+drunks_control)

# What is the probability that a subject in the lowest alcohol consumption group is a cancer case?
sobers <- esoph %>%
  filter(alcgp == "0-39g/day")
sobers

sobers_cancer <-sum(sobers$ncases)
sobers_control <- sum(sobers$ncontrols)

sobers_cancer/(sobers_cancer+sobers_control)

# Given that a person is a case, what is the probability that they smoke 10g or more a day?
smokers <- esoph %>%
  filter(tobgp %in% c("10-19", "20-29", "30+"))
smokers <- sum(smokers$ncases)
smokers/all_cases

# Given that a person is a control, what is the probability that they smoke 10g or more a day?
smokers <- esoph %>%
  filter(tobgp %in% c("10-19", "20-29", "30+")) 
smokers
smokers <- sum(smokers$ncontrols)
smokers
smokers/all_controls

# Esophageal cancer and alcohol/tobacco use, part 2

#For cases, what is the probability of being in the highest alcohol group?
head(esoph)
esoph

drinkers_cases <- esoph %>%
  filter(alcgp == "120+")
drinkers_cases <- sum(drinkers_cases$ncases)
drinkers_cases/all_cases

#For cases, what is the probability of being in the highest tobacco group?
smokers_cases <- esoph %>%
  filter(tobgp  == "30+")
smokers_cases <- sum(smokers_cases$ncases)
smokers_cases/all_cases

#For cases, what is the probability of being in the highest alcohol group and the highest tobacco group?
smokes_drinks <- esoph %>%
  filter(alcgp =="120+" & tobgp == "30+")
smokes_drinks
smokes_drinks <- sum(smokes_drinks$ncases)
smokes_drinks/all_cases

# For cases, what is the probability of being in the highest alcohol group or the highest tobacco group?
smokes_or_drinks <- esoph %>%
  filter(alcgp =="120+"| tobgp == "30+")
smokes_or_drinks <- sum(smokes_or_drinks$ncases)
smokes_or_drinks/all_cases

#How many times more likely are cases than controls to be in the highest alcohol group?

#the amount of cases in the high drinking group divided by the total number of cases
drinkers_cases <- esoph %>%
  filter(alcgp == "120+")
drinkers_cases <- sum(drinkers_cases$ncases)
r1 <- drinkers_cases/all_cases
r1
#the amount of controls in the high drinking group divided by the total number of controls
drinkers_control <- esoph %>%
  filter(alcgp == "120+")
drinkers_control <- sum(drinkers_control$ncontrols)
r2 <-drinkers_control/all_controls
r2

r1/r2

# For controls, what is the probability of being in the highest tobacco group?
smokers_controls <- esoph %>%
  filter(tobgp  == "30+")
smokers_controls <- sum(smokers_controls$ncontrols)
smokers_controls/all_controls

# For controls, what is the probability of being in the highest alcohol group and the highest tobacco group?
smokes_drinks <- esoph %>%
  filter(alcgp =="120+" & tobgp == "30+")
smokes_drinks
smokes_drinks <- sum(smokes_drinks$ncontrols)
smokes_drinks/all_controls


# DataCamp Assessment: The Addition Rule and Monty Hall

# Exercise 1. Two teams, say the Cavs and the Warriors, are playing a seven game championship series. 
#The first to win four games wins the series. The teams are equally good, so they each have a 50-50 chance of winning each game.
#If the Cavs lose the first game, what is the probability that they win the series?

n <- 6 #Remaining games
outcomes <- c(0, 1) # 0 = Cavs loose, 1 = Cavs win

l <- rep(list(outcomes), n) # list of all possible outcomes in all remaining games

possibilities <- expand.grid(l)

results <- rowSums(possibilities)>=4
results  

mean(results)  

# Exercise 2. Confirm the results of the previous question with a Monte Carlo simulation to estimate the
#probability of the Cavs winning the series after losing the first game.

B <-10000
results <- replicate(B, {
  possibilities <- c(0, 1)
  results <- sample(possibilities, 6, replace = TRUE)
  all(sum(results) >= 4)
})
mean(results)

possibilities <- c(0, 1)
results <- sample(possibilities, 6, replace = TRUE)
all(sum(results) >= 4)

# A and B play a series - part 1
# Two teams, A and B, are playing a seven series game series. Team A is better than team B and has a p>0.5 chance of winning each game.
# Use the function sapply to compute the probability, call it Pr of winning for p <- seq(0.5, 0.95, 0.025).
# Then plot the result plot(p, Pr).

#We first genrate the sequece of values for our function
p <- seq(0.5, 0.95, 0.025)

#Now we generate of function
prob_win <- function(p){
  B <- 10000
  result <- replicate(B,{
    b_wins <- sample(c(1, 0), 7, replace = TRUE, prob = c(1-p, p))
    sum(b_wins) >= 4
  })
  mean(result)
}

pr <- sapply(p, prob_win)

plot(p, pr)

# Repeat the previous exercise, but now keep the probability that team A wins fixed at p <- 0.75 and compute the probability 
#for different series lengths. For example, wins in best of 1 game, 3 games, 5 games, and so on through a series that lasts 25 games.
p <- 0.75
n <- seq(1, 25, 2)

prob_win <- function(n){
  B <- 10000
  result <- replicate(B,{
    b_wins <- sample(c(1, 0), n, replace = TRUE, prob = c(1-p, p))
    sum(b_wins)>=(n+1)/2
  })
  mean(result)
}

pr <- sapply(n, prob_win)


plot(n, pr)

#CONTINUOUS PROPABILITY

#Empirical distribution function
library(tidyverse)
library(dslabs)
data("heights")

x <- heights %>% filter(sex=="Male") %>%
  .$height

F <- function(a) mean(x<=a) # Foer every value of a. this gives a number of values in the list x, smaller or ecual to X

# What is the cahnce that if a pick one student randomply he is taller thatn 70.5""
1-F(70)

# The probability of a student been between A and B is: F(A) - F(B)

#Theoretical Distribution

#pnorm(a, avg, s) <- gives the value of the cumulative distribution function  F(a)  for the normal distribution defined 
#by average avg and standard deviation s.

#We say that a random quantity is normally distributed with average avg and standard deviation s 
#if the approximation pnorm(a, avg, s) holds for all values of a.

# If we are willing to use the normal approximation for height, we can estimate the distribution 
#simply from the mean and standard deviation of our values.

#  Discretization. <- when continouds data is reporten in rouded values. Ej. Heights

#With pnorm() we only need mean and sd to know a distribution (if it is normal)
1 - pnorm(70.5, mean(x), sd(x)) # This is derived mathematicly, it does not requeire data

#Using data
mean(x <= 68.5) - mean(x <=67.5)

mean(x <= 69.5) - mean(x <= 68.5)

#Using aproximation
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))

pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))

# If the itnervals dont include reouded normals they are not usfull

mean(x <= 70.9) - mean(x<= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))
# We call this discretization

#Probability Density

#The probability of a single value is not defined for a continuous distribution.

#The quantity with the most similar interpretation to the probability of a single value is the probability density function  f(x) .

# The probability density  f(x)  is defined such that the integral of  f(x)  over a range gives the CDF of that range.
# F(a)=Pr(X???a)=???a??????f(x)dx

# dnorm() <-  the probability density function for the normal distribution.

# Note that dnorm() gives the density function and pnorm() gives the distribution function, which is the integral of the density function.

# dnorm(z) gives the probability density  f(z)  of a certain z-score, so we can draw a curve by calculating the density over a range of possible values of z.
# 99.7% of observations will be within  ???3???z???3 , we can use a value of  z  slightly larger than 3

x <- seq(-4, 4, length = 100)

ggplo(x, f = dnorm(x)) %>%
  ggplot(aes(x, f)) +
  geom_line()

# Dnorm gives by default the density for normal distribution, probabilities for alternative dsiributions can be created by dnorm(z, mu, sigma)

x <- seq(-4, 4, length = 100)

ggplo(x, f = dnorm(x, 0, 3)) %>%
  ggplot(aes(x, f)) +
  geom_line()

ggplo(x, f = dnorm(x, 2, 1)) %>%
  ggplot(aes(x, f)) +
  geom_line()

# Monte Carlo Simulations With normally distributed variables <. rnorm

x <- heights %>% filter(sex=="Male") %>% .$height
n <- length(x)
avg <- mean(x)
s <- sd(x) 
data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

data.frame(simulated_heights = simulated_heights) %>%
  ggplot(aes(simulated_heights)) +
  geom_histogram(color="black", binwidth = 2)

#We can use this with montecarlo simulations to see the chanes of certain fenomena happening
#For examole, which is the chance of peaking a 7 foot tall male randomply.

B <-1000 #1000 simulations
tallest <- replicate (B, {
  simulated_data <- rnorm(8000, avg, s) #800 simulated values
  max(simulated_data) #Pick up the biggest
})
mean(tallest >= 7*12) #Wich percetage of the simulation returned a max value bigger than 7 foot

# Other Continuous Distributions
#You may encounter other continuous distributions (Student t, chi-squared, exponential, gamma, beta, etc.).
#R provides functions for density (d), quantile (q), probability distribution (p) and random number generation (r) for many of these
#Each distribution has a matching abbreviation (for example, norm() or t()) that is paired with the related function abbreviations (d, p, q, r) to create appropriate functions.
#For example, use rt() to generate random numbers for a Monte Carlo simulation using the Student t distribution.

#Use d to plot the density function of a continuous distribution.
x <- seq(-4, 4, length.out = 100)
data.frame(x, f = dnorm(x))  %>%
  ggplot(aes(x,f)) +
  geom_line()

x <- seq(-4, 4, length.out = 100)
data.frame(x, f = qnorm(x))  %>%
  ggplot(aes(x,f)) +
  geom_line()


x <- seq(-4, 4, length.out = 100)
data.frame(x, f = pnorm(x))  %>%
  ggplot(aes(x,f)) +
  geom_line()


x <- seq(-4, 4, length.out = 100)
data.frame(x, f = rnorm(x))  %>%
  ggplot(aes(x,f)) +
  geom_line()

#DataCamp Assessment: Continuous Probability

#Exercise 1
#Assume the distribution of female heights is approximated by a normal distribution with a mean of 64 inches and a standard deviation of 3 inches. 
#If we pick a female at random, what is the probability that she is 5 feet or shorter?

female_mean <- 64
female_sd <- 3

pnorm(5*12, female_mean, female_sd)


# If we pick a female at random, what is the probability that she is 6 feet or taller?
1 - pnorm(6*12, female_mean, female_sd)

#If we pick a female at random, what is the probability that she is between 61 and 67 inches?
pnorm(67 , female_mean, female_sd) -  pnorm(61 , female_mean, female_sd) 

#Repeat the previous calculation using pnorm to define the probability that a randomly chosen woman will have a height between 61 and 67 inches, 
#converted to centimeters by multiplying each value by 2.54
female_mean_cm <- 64*2.54
female_sd_cm <- 3*2.54

pnorm(67*2.54 , female_mean_cm, female_sd_cm) -  pnorm(61*2.54 , female_mean_cm, female_sd_cm) 

#To a variable named 'taller', assign the value of a height that is one SD taller than average.
taller <- pnorm(67 , female_mean, female_sd) -  pnorm(64 , female_mean, female_sd) 
taller 

# To a variable named 'shorter', assign the value of a height that is one SD shorter than average.
shorter <- pnorm(64 , female_mean, female_sd) -  pnorm(61 , female_mean, female_sd) 
shorter
shorter + taller

#Determine the height of a man in the 99th percentile, given an average height of 69 inches and a standard deviation of 3 inches.
male_avg <- 69
male_sd <- 3

qnorm(0.99, male_avg, male_sd)


# Generate 10,000 IQ scores 1,000 times using a Monte Carlo simulation. Make a histogram of the highest IQ scores.
set.seed(1)
avg_IQ <- 100
sd_IQ <- 15

B <-1000
highestIQ <- replicate (B, {
  IQ <- rnorm(10000, avg_IQ, sd_IQ)
  max(IQ)
})
mean(highestIQ)

data.frame(highestIQ == highestIQ) %>%
  ggplot(aes(highestIQ)) +
  geom_histogram()

# Questions 1 and 2: ACT scores, part 1

#Set the seed to 16, then use rnorm() to generate a normal distribution of 10000 tests with a mean of 20.9 and standard deviation of 5.7.
#Save these values as act_scores. You'll be using this dataset throughout these four multi-part questions.

set.seed(16, sample.kind = "Rounding") 

act_scores <- rnorm(10000, 20.9, 5.7)

#Question 1. Mean and SD
mean(act_scores)
sd(act_scores)

#Question 2. A perfect score is 36 or greater (the maximum reported score is 36).
#In act_scores, how many perfect scores are there out of 10,000 simulated tests?

sum(act_scores>=36)


#Question 3. In act_scores, what is the probability of an ACT score greater than 30?
sum(act_scores >= 30)
527/10000

#Question 4. In act_scores, what is the probability of an ACT score less than or equal to 10?
sum(act_scores<=10)
282/10000

# Set x equal to the sequence of integers 1 to 36. Use dnorm to determine the value of the probability density function 
# over x given a mean of 20.9 and standard deviation of 5.7; save the result as f_x. Plot x against f_x.
x <- seq(1, 36, 1)
f_x <-dnorm(x, 20.9, 5.7)

data.frame(x, f_x) %>%
  ggplot(aes(x, f_x)) +
  geom_line()

#Question 5 - What is the probability of a Z-score greater than 2 ?
#Use the mean and standard deviation of act_scores, not the original values used to generate random test scores.
z <- scale(act_scores)
mean(z>2)

#Question 5 - What ACT score value corresponds to 2 standard deviations above the mean (Z = 2)?
frame <- data.frame(act_scores, z)

above <- frame %>%
  filter(z > 2)
above
min(above)

#Alternative answer, from the exercise 
2*sd(act_scores) + mean(act_scores)

#Question 6 - What is the 97.5th percentile of act_scores?
avg<- mean(act_scores)
sd <- sd(act_scores)
avg

qnorm(0.975, avg, sd)

# Question 7- What is the minimum integer score such that the probability of that score or lower is at least .95?
qnorm(0.95, avg, sd)

#Alternative answer 
cdf <- sapply(1:36, function (x){
  mean(act_scores <= x)
})
cdf
min(which(cdf >= .95))

# Question 8 - Use qnorm() to determine the expected 95th percentile, the value for which the probability of receiving that score or lower is 0.95,
#given a mean score of 20.9 and standard deviation of 5.7.
qnorm(0.95, 20.9, 5.7)


# Question 9 Make a vector containing the quantiles for p <- seq(0.01, 0.99, 0.01), 
#the 1st through 99th percentiles of the act_scores data. Save these as sample_quantiles
p <- seq(0.01, 0.99, 0.01)

sample_quantiles <- quantile(act_scores, p)

#Question 10 - Make a QQ-plot graphing sample_quantiles on the y-axis versus theoretical_quantiles on the x-axis.
theoretica_quantiles <- qnorm(p, 20.9, 5.7)

frame <- data.frame(theoretica_quantiles, sample_quantiles)
frame

frame %>%
  ggplot(aes(sample = theoretica_quantiles)) +
  geom_qq(dparams =sample_quantiles ) +
  geom_qq_line()



# Section 3: Random Variables, Sampling Models, and the Central Limit Theorem

# Sampling Models

# Casino game example. 1000 peopble plays rulete. Red or black. chance of loosing money-
# 18 red, 18 black, 2 green
color <- rep(c("Black", "Red", "Green"), c(18, 18, 2)) #If red comes pout the casino looses 1$

n <-1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X[1:10]

#Now model with the proportions. WE genrate the probabilities inside the earn without defininf color
X <- sample(c(-1, 1), n , replace= TRUE, prob = c(9/19, 10/19)) # THis is a samplic model
s <- sum(X) # TOtal winings
s

# The probability distribution of a random variable is the probability of the observed value falling in any given interval.

# WE think about the porbability of loosing money as an interval in a CUmulative density Function
#CDF  F(a)=Pr(S???a)  <- F is the random variables distribution function.
#We can estimate the distribution funtion of the variable S witha a Monte Carlo simulation
n <- 1000
B <- 10000
S <- replicate(B, {
  X <- sample(c(-1, 1), n , replace= TRUE, prob = c(9/19, 10/19))
  sum(X)
})
mean(S<0) #Probability of casino loosing money

#Distributions versus Probability Distributions
# The first is an actual distribution , the other is a thoretical aproximation of the probabilities of a number coming put of an urn.
#Distribution functios are for RANDOM VARIABLES 

# Notation for Random Variables
#Capital letters denote random variables ( X ) and lowercase letters denote observed values ( x )

#The Central Limit Theorem (CLT) says that the distribution of the sum of a random variable is approximated by a normal distribution.

# Wen speeking about radom variables

#AVERAGE == EXPECTED VALUES

#STANDARD DEVIATION == STANDARD ERROR

# The expected value of a random variable,  E[X]=?? ,
#?? <- means average

#Equations
#These equations apply to the case where there are only two outcomes,  a  and  b  with proportions  p  and  1???p  respectively. The general principles above also apply to random variables with more than two outcomes.

# Instead uf using the montecarlo simulatuion we can use probability thoery and the Central Limit Theorem
n <- 1000
EX <- n*(20-18)/38
EX
SE <- sqrt(n)*2*sqrt(90)/19
SE

#Probability of a casino loosing money useing these aproximations
pnorm(0, EX, SE) # Same result as the Monte Carlo Simulation

#DataCamp Assessment: Random Variables and Sampling Models
green <- 2
black <- 18
red <- 18

#Exercise 1 - What are the chances that the ball lands in a green pocket?
p_green <- 2/(18+18+2)
p_green

#Exercise 2- In American roulette, the payout for winning on green is $17. This means that if you bet $1 and it lands on green, you get $17 as a prize.
#Create a model to predict your winnings from betting on green one time.
set.seed(1, sample.kind="Rounding")

p_not_green <- (18+18)/(18+18+2)
p_not_green

n <-1
X <- sample(c(17,-1), n, prob = c(p_green, p_not_green))
X

#Exercise 3 - Now, compute the expected value of X, the random variable you generated previously.
ap + b(1-p)
17*p_green + -1*p_not_green # EXPECTED VALUE of a random variable 

#Exercise 4 - compute the standard error of that random variable, which represents a single outcome after one spin of the roulette wheel.
abs((17 - -1))*sqrt(p_green*p_not_green)

#Exercise - 5 Now create a random variable S that sums your winnings after betting on green 1,000 times.
n <- 1000
X <- sample(c(17,-1), size = n, replace = TRUE, prob = c(p_green, p_not_green))
X
S <- sum(X)
S  

#Exercise 6 - Calculate the expected outcome of 1,000 spins if you win $17 when the ball lands on green and you lose $1 when the ball doesn't land on green
n <- 1000
n * (17*p_green + -1*p_not_green)

# Exercise 7 - # Compute the standard error of the sum of 1,000 outcomes
sqrt(n) * abs((17 - -1))*sqrt(p_green*p_not_green)


# DataCamp Assessment: The Central Limit Theorem
# Exercise 1. American Roulette probability of winning money

# What is the probability that you end up winning money if you bet on green 100 times?

p_green <- 2/38
p_not_green <- 1-p_green
n <- 100

avg <- n* (17*p_green + (-1)*p_not_green)
avg

se <- sqrt(n)*abs(17 - -1)* sqrt(p_green*p_not_green)
se

1- pnorm(0, avg,se)

# Exercise 2 - Create a Monte Carlo simulation that generates 10,000 outcomes of S, the sum of 100 bets.
# Compute the average and standard deviation of the resulting list and compare them to the expected value (-5.263158) and standard error (40.19344) for S that you calculated previously.
B <-10000
S <- replicate(B, {
  X <- sample(c(17, -1), size = n, replace = TRUE, prob = c(p_green, p_not_green))
  sum(X)
})
S
mean(S)
sd(S)

#Whit replicate, we obtain a vecto S, is this vector normally disctibuted
library(ggplot2)
winings <- data.frame(S)

winings %>%
  ggplot(aes(S))+
  geom_histogram()

#Exercise 3 - Calculate the proportion of outcomes in the vector `S` that exceed $0
sum(S>0)
mean(S>0)


#Exercise 4 - Now create a random variable Y that contains your average winnings per bet after betting on green 10,000 times.
X <- sample(c(17, -1), size = 10000, replace = TRUE, prob = c(p_green, p_not_green))
Y <- mean(X)
Y  

# Exercise 5 - Calculate the expected outcome of `Y`, the mean outcome per bet in 10,000 bets
17*p_green + -1*p_not_green

# Exercise 6- Compute the standard error of 'Y', the mean outcome per bet from 10,000 bets.
abs((17 - -1))*sqrt(p_green*p_not_green) / sqrt(n)

# Exercise 6- Create a Monte Carlo simulation that generates 10,000 outcomes of S, the average outcome from 10,000 bets on green.
# Compute the average and standard deviation of the resulting list to confirm the results from previous exercises using the Central Limit Theorem.
B <- 10000
S <- replicate(B, {
  X <- sample(c(17, -1), size = 10000, replace = TRUE, prob = c(p_green, p_not_green))
  mean(X)  
})
mean(S)
sd(S)  


# Questions 1 and 2: SAT testing
#An old version of the SAT college entrance exam had a -0.25 point penalty for every incorrect answer and awarded 1 point for a correct answer. 
#The quantitative test consisted of 44 multiple-choice questions each with 5 answer choices. Suppose a student chooses answers by guessing for all questions on the test.

#Question 1 - What is the expected value of points for guessing on one question?
p_correct <- 1/5
p_not_correct <- 4/5

p_correct*1 + -0.25*p_not_correct  

#Question 2 - What is the expected score of guessing on all 44 questions?
n <- 44
ev <- n *(  p_correct*1 + -0.25*p_not_correct)  

#Question 3 - What is the standard error of guessing on all 44 questions?
se <- sqrt(n)*abs(-0.25 -1)*sqrt(p_correct*p_not_correct)

# Question 4 - Use the Central Limit Theorem to determine the probability that a guessing student scores 8 points or higher on the test.
1-pnorm(8, ev, se)

# Question 5 - Set the seed to 21, then run a Monte Carlo simulation of 10,000 students guessing on the test.
# What is the probability that a guessing student scores 8 points or higher?
set.seed(21, sample.kind = "Rounding")

B <-10000
S <- replicate(B, {
  X <- sample(c(1, -0.25), size = 44, replace = TRUE, prob = c(p_correct, p_not_correct))
  sum(X)
})
mean(S>=8)

#Question 6 - Suppose that the number of multiple choice options is 4 and that there is no penalty for guessing - that is, an incorrect question gives a score of 0.
# - What is the expected value of the score when guessing on this new test?
n <- 44
p_correct <- 1/4
p_not_correct <- 3/4
n*(p_correct*1 + 0*p_not_correct)

# Question 7 - Consider a range of correct answer probabilities p <- seq(0.25, 0.95, 0.05) representing a range of student skills.
# What is the lowest p such that the probability of scoring over 35 exceeds 80%?
a <- 1
b <- 0

n <- 44

p <- seq(0.25, 0.95, 0.05)

results <- seq(0, 44, 1)

EV <- n*(a*p + b*(1-p))

SE <- sqrt(n) * abs((a - b) * sqrt(p*(1-p)))

distribution <- function(p){
  EV <- n*(a*p + b*(1-p))
  SE <- sqrt(n) * abs((a - b) * sqrt(p*(1-p)))
  1-pnorm(35, EV, SE)
}
distribution(0.85)


#A casino offers a House Special bet on roulette, which is a bet on five pockets (00, 0, 1, 2, 3) out of 38 total pockets. 
#The bet pays out 6 to 1. In other words, a losing bet yields -$1 and a successful bet yields $6. A gambler wants to know the chance 
#of losing money if he places 500 bets on the roulette House Special.
p_win <- 5/38
p_loose <- 1-p_win
p_win
p_loose

# Question 1 - What is the expected value of the payout for one bet?
p_win*6+p_loose*-1

#Question 2 - What is the standard error of the payout for one bet?
abs((6 - -1)*sqrt(p_win*p_loose))

# Question 3 - What is the expected value of the average payout over 500 bets?
p_win*6+p_loose*-1

#Question 4 - What is the expected value of the sum of 500 bets?

EV <- 500*(p_win*6+p_loose*-1)

#Question 5 - What is the standard error of the average payout over 500 bets?
abs((6 - -1)*sqrt(p_win*p_loose))/sqrt(n)

# Question 6 - What is the standard error of the sum of 500 bets?
SE <- sqrt(n)*abs((6 - -1)*sqrt((p_win*p_loose)))

# Question 7 - Use pnorm() with the expected value of the sum and standard error of the sum to calculate the probability of losing money over 500 bets.
pnorm(0, EV, SE)


#The Big Short: Interest Rates Explained

n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample( c(0,1), n, prob=c(1-p, p), replace = TRUE)
sum(defaults * loss_per_foreclosure)

#Monte Carlo Simulation to see the distribution of random outcomes
B <- 10000
losses <- replicate(B,{
  defaults <- sample(c(0, 1), n, replace = TRUE, prob = c(1-p, p))
  sum(defaults*loss_per_foreclosure)
})
losses

data.frame(losses_in_millions = losses/10^6) %>%
  ggplot(aes(losses_in_millions)) +
  geom_histogram()

#The Expected Values and Standard Deviation
EV <- n*(p*loss_per_foreclosure + (1-p)*0)
EV
SE <- sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p))
SE

#Our EV is negative, -4e+06, to ensure we do not have loses we need to turn it in to 0
l = loss_per_foreclosure
# l*p + x*(1-p) = 0 %>%  x = -(l*p)/(1-p)
x = -(l*p)/(1-p)
x
interest <- x/180000
interest # This interest garantees that on average we do not loose money, the Expected Value is 0
# Even so there is still a 50% chance we will losse money
#We want to have only 0.01 chances of lossing money, we need a bigger X
# Pr(S<0)=0.01
#Ev S <- n*(l*p + x*(1-p))
#ER S <-  |x-l|sqrt(n*p*(1-p))

# Pr(S<0)=0.01 <- S < 0 <- this is our ecuation <-
#we will substract the EV of S and divide by the ER of X in booth sites
#Pr((S-EV)/SE > -EV/SE)  # The term on the left has become a STANDARIZED NORMAL RANDOM VARIABLE <- Z
# When we standarize a variable like we did, the EV is always 0 and the SE 1
# Z <- -EV/SE  #Now we fill the blancks with the formulas of EV and SE
#Pr( Z > n*-(l*p + x*(1-p)) / |x-l|sqrt(n*p*(1-p)) ) = 0.01
#Because Z is NORMALLY DISTRIBUTED WITH EV = 0 AND SE = 1, the left side of the ecuation
#needs to be == to qnorm 0.01 (we will call this side little z)
# n*(l*p + x*(1-p)) / |x-l|sqrt(n*p*(1-p)) = qnorm(0.01)
z <- qnorm(0.01)
#n*(l*p + x*(1-p)) / |x-l|sqrt(n*p*(1-p)) = -2.326  This gives a formula for which Pr(Z <= z) = 0.01
# z = n*-(l*p + x*(1-p)) / |x-l|sqrt(n*p*(1-p)) <- Now we only need to solve for x
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))    
x  
interest <- x/180000
profit_per_lone <- loss_per_foreclosure*p + x*(1-p)
profit_per_lone  
profit_per_lone*1000

# We run a Monte Carlo Simulation to check our theoretica aproximations 
B <- 1000
profits <-replicate(B, {
  simulation <- sample(c(x, l), n, replace = TRUE, prob = c(1-p, p))
  sum(simulation)
})
mean(profits)  
mean(profit<0)

# The Big Short
loss_per_foreclosure <- -200000
p <- .04
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)

#Because 
# Pr(S>0)=Pr(Z < -EV[s]/SE[S]), as SE increases our chances of loosing money decrease
# EV[S] = n . u
# SE[S] = sqrt(n)*standard deviation <- WE transform in to:
#  z <= sqrt(n)*u / SD <- n >= (z^2 * SD^2 / u^2) # This gives us the n for which z is less or eacual to our desired value
#The idea is that as long as u is positive we can find a number of loans taht minimices the probability of loses
#Wen n is large our average earning per lone converges on u.
z <- qnorm(0.01)
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n # Probability of lossing of 0.01
n*(l*p+x*(1-p))

B <- 10000
profits <- replicate(B, {
  draws <- sample(c(x, l), replace = TRUE, n, prob = c(1-p, p))
  sum(draws)
})
mean(profits<0)
#This model would hold only under the condition that X´s are independent,
#the fact of one person defaulting should be independent of other defaults

# We asume a global event wich afect all mortages, with a 50% chance all probabilities of default
# go up or down one point. now p isbetween 0.03 and 0.05. This hapens to everybody at ones
#The draws are not onger independent.
p <- 0.04
x <- 0.05*180000
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)

#DataCamp Assessment: The Big Short
#Say you manage a bank that gives out 10,000 loans. The default rate is 0.03 and you lose $200,000 in each foreclosure.
set.seed(1, sample.kind="Rounding")
#Exercise 1. Create a random variable S that contains the earnings of your bank. Calculate the total amount of money lost in this scenario.
n <-10000
p <- 0.03
l <- -200000
x <- 0
S <- sample(c(x, l), n, replace = TRUE, prob = c((1-p), p))
sum(S)

#Exercise 2. Make a historiogram with 10.000 S
B <- 10000
benefits <- replicate(B, {
  S <- sample(c(x, l), n, replace = TRUE, prob = c((1-p), p))
  sum(S)
  
})

data.frame(benefits = benefits) %>%
  ggplot(aes(benefits)) +
  geom_histogram()

#Exercise 3 - Calculate the expected loss due to default out of 10,000 loans
n*(l*p)

#Exercise 4- Compute the standard error of the random variable S
sqrt(n)*abs((l - x)*sqrt(p*(1-p)))

#Exercise 5 - . Assume we give out loans for $180,000. How much money do we need to make when people pay their loans so that our net loss is $0?
(l*p)+x*(1-p)
x <- -(l*p)/(1-p)
r <- x/180000  
r  

# Exercise 6- What should the interest rate be so that the chance of losing money is 1 in 20  
n <- 10000
l <- -200000
p_default <- 0.03
z <- qnorm(0.05)
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))    
x  
r <- x/180000
r  

B <- 10000
benefits <- replicate(B,{
  draws <- sample(c(x,l), n, replace = TRUE, prob = c((1-p), p))
  sum(draws)
})
mean(benefits<0)  

#The Big Short Final assesment
library(tidyverse)
library(dslabs)
data(death_prob)
head(death_prob)
death_prob

#Questions 1 and 2: Insurance rates, part 1
#An insurance company offers a one-year term life insurance policy that pays $150,000 in the event of death within one year. 
#The premium (annual cost) for this policy for a 50 year old female is $1,150. Suppose that in the event of a claim, 
#the company forfeits the premium and loses a total of $150,000, and if there is no claim the company gains the premium amount of $1,150. 
#The company plans to sell 1,000 policies to this demographic.
l <- -150000
x <- 1150
p <- 0.003193
n <- 1000
death_prob %>%
  filter(sex == "Female", age == 50)

#Question 1 - What is p
0.003193

#Question 2 - What is the expected value of the company's net profit on one policy for a 50 year old female?
l*p+x*(1-p)

# Question 3 -Calculate the standard error of the profit on one policy for a 50 year old female.
abs((l - x)* sqrt(p*(1-p)))  

# Question 4 - What is the expected value of the company's profit over all 1,000 policies for 50 year old females?
EV <- n*(l*p+x*(1-p))

# Question 5 - What is the standard error of the sum of the expected value over all 1,000 policies for 50 year old females?
SE <-sqrt(n) * abs((l - x)* sqrt(p*(1-p)))  

# Question 6 - Use the Central Limit Theorem to calculate the probability that the insurance company loses money on this set of 1,000 policies
pr(S<0)

pnorm(0, EV, SE )

# Question 7 - Use death_prob to determine the probability of death within one year for a 50 year old male.
death_prob %>%
  filter(sex == "Male", age == 50)

# Question 8 - Suppose the company wants its expected profits from 1,000 50 year old males with $150,000 life insurance policies to be $700,000. 
#Use the formula for expected value of the sum of draws with the following values and solve for the premium  x :
# How much does x needs to be i order to E[S]=??S=700000
p <- 0.005013
l <- -150000
n <-1000
EV <- 700000

EV <- n*(l*p+x*(1-p))
x <- (EV / n) - (l*p) / (1-p)

# Question 9 - Using the new 50 year old male premium rate, calculate the standard error of the sum of 1,000 premiums.
SE <-sqrt(n) * abs((l - x)* sqrt(p*(1-p)))  

# Question 9  What is the probability of losing money on a series of 1,000 policies to 50 year old males?
pnorm(0, EV, SE)  


# Questions 3 and 4: insurance rates, part 2
# In this 6-part question, we'll look at a scenario in which a lethal pandemic disease increases the probability of death within 1 year for a 50 year old to .015.
# Unable to predict the outbreak, the company has sold 1,000 $150,000 life insurance policies for $1,150.
death_prob %>%
  filter(age == 50)
p <-.015
l <- -150000
x <- 1150
n <-1000
# Question 1 - What is the expected value of the company's profits over 1,000 policies?
EV <- n*(l*p+x*(1-p))
EV  

# Question 2 - What is the standard error of the expected value of the company's profits over 1,000 policies?
SE <-sqrt(n) * abs((l - x)* sqrt(p*(1-p)))  
SE

# Question 3 - What is the probability of the company losing money?
pnorm(0, EV, SE)

# Question 4 - What is the probability of losing more than $1 million?
pnorm(-1000000, EV, SE)

# Question 5 - Investigate death probabilities p <- seq(.01, .03, .001)
# What is the lowest death probability for which the chance of losing money exceeds 90%?
p <- seq(.01, .03, .001)
p  
lossing <- function(p){
  EV <- n*(l*p+x*(1-p))
  SE <-sqrt(n) * abs((l - x)* sqrt(p*(1-p)))  
  pnorm(0, EV, SE)
  
}
lossing(0.013)

# Question 6 - What is the lowest death probability for which the chance of losing over $1 million exceeds 90%? p <- seq(.01, .03, .0025)
p <- seq(.01, .03, .0025)
p

lossing <- function(p){
  EV <- n*(l*p+x*(1-p))
  SE <-sqrt(n) * abs((l - x)* sqrt(p*(1-p)))  
  pnorm(-1000000, EV, SE)
  
}
lossing(0.0200)

# Question 6 - Define a sampling model for simulating the total profit over 1,000 loans with probability of claim p_loss = .015, 
#loss of -$150,000 on a claim, and profit of $1,150 when there is no claim. Set the seed to 25.
# What is the reported profit (or loss) in millions (that is, divided by  10^6 )?
n <-1000
p <- 0.015
l <- -150000
x <- 1150

set.seed(25, sample.kind = "Rounding")
S <- sample(c(x, l), n, replace = TRUE, prob = c((1-p), p))
profits <- sum(S)  
profits/10^6

# Set the seed to 27, then run a Monte Carlo simulation of your sampling model with 10,000 replicates to simulate the range of profits/losses over 1,000 loans.
# What is the observed probability of losing $1 million or more?
set.seed(27, sample.kind = "Rounding")
B <- 10000
S <- replicate(B, {
  draws <- sample(c(x, l), n, replace = TRUE, prob = c((1-p), p))
  sum(draws)
})
mean(S<=-1000000)

# Questions 5 and 6: Insurance rates, part 3
#Suppose that there is a massive demand for life insurance due to the pandemic, 
#and the company wants to find a premium cost for which the probability of losing money is under 5%, assuming the death rate stays stable at  p=0.015 .

# Question 1 - Calculate the premium required for a 5% chance of losing money given  n=1000  loans, probability of death  p=0.015 , and loss per claim  l=???150000 . 
#Save this premium as x for use in further questions.
n <-1000
p <- 0.015
l <- - 150000
z <- qnorm(0.05)
z

x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))    
x  

# Question 2 - What is the expected profit per policy at this rate?
EV <- (l*p+x*(1-p))
EV  

#Question 6 - What is the expected profit over 1,000 policies?
EV <- n*(l*p+x*(1-p))
EV  

#Question 7 - Run a Monte Carlo simulation with B=10000 to determine the probability of losing money on 1,000. Set the seed to 28 
set.seed(28, sample.kind = "Rounding")

EV <- n*(l*p+x*(1-p))
SE <-sqrt(n) * abs((l - x)* sqrt(p*(1-p)))  

B <- 10000
S <- replicate(B, {
  draws <- sample(c(x, l), n, replace = TRUE, prob = c(1-p, p))
  sum(draws)
})
mean(S<0)

# Question - 8 
#The company cannot predict whether the pandemic death rate will stay stable. Set the seed to 29, then write a Monte Carlo simulation that for each of  B=10000  iterations
# randomly changes  p  by adding a value between -0.01 and 0.01 with sample(seq(-0.01, 0.01, length = 100), 1)
# uses the new random  p  to generate a sample of  n=1,000  policies with premium x and loss per claim  l=???150000
# returns the profit over  n  policies (sum of random variable)
#The outcome should be a vector of  B  total profits.  
set.seed(29, sample.kind = "Rounding")
l <- -150000
x
n <- 1000
B <- 10000

set.seed(29, sample.kind = "Rounding")
S <- replicate(B,{
  new_p <- 0.015 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample(c(x, l), n, replace = TRUE, prob = c(1-new_p, new_p))
  sum(draws)
})

# 8.1 What is the expected value over 1,000 policies? 
mean(S)

# 8.2 What is the probability of losing money?
mean(S<0)

# 8.3 What is the probability of losing more than $1 million?
mean(S < -1000000)

