#DATA SCIENCE R BASICS
#31/07/20

#install.packages("tidyverse", "dslabs")

#INTRODUCTION
#1.2 R BASICS

library(tidyverse)
library(dslabs)
data(murders)

murders %>%
  ggplot(aes(population, total, label=abb, color=region)) +
  geom_label()

#Objects

#x^2+x-1=0
a <- 1
b <- 1
c <- -1

print(a)

#ls() <- Name of the objects stored
ls()

#Cuadratic Formula
(-b + sqrt(b^2-4*a*c))/(2*a)
(-b - sqrt(b^2-4*a*c))/(2*a)

#Functions

log(a)
log(8)
exp(1)
log(2.718282)
log(exp(1))
help(log)
?log

#args <- Arguments of a function
args(log)
log(8, base=2)
log(8,2)
2^3  

#data() <- All data sets
data()

#1.3 DATA TYPES

class(a)
class(ls)  

#Data Frames <- They are like tables

library(dslabs)
data("murders")
class(murders)
str(murders)
head(murders)

#Acces to specific colum
names(murders)
murders$population  

#Define object as a column of the data frame
pop <- murders$population
length(pop)

#catacter strains
class(murders$state)

#Logical Vectos <- can be true or false
z <- 3==2
z
class(z)

#factors <- Categorical data
class(murders$region)
levels(murders$region)

#Section 1 Assessment

# Find the solutions to an equation of the format  ax^2+bx+c 
#What are the two solutions to  2x^2???x???4=0
a <- 2
b <- -1
c <- -4

(-b + sqrt(b^2-4*a*c))/(2*a)
(-b - sqrt(b^2-4*a*c))/(2*a)

# Compute log base 4 of 1024. 
log(1024, 4)

#Explore movielens data
library(dslabs)
data("movielens")

# How many rows and vars
str(movielens)

#Variable type title
class(movielens$title)

#levels
nlevels(movielens$genres)


#2
###############################
#VECTOS AND SORTING
###############################

#2.1 VECTORS

#c <- concadenate, creates vectos
codes <- c(380,124,818)
country <- c("italy", "canada", "egypt")

#Asign names
codes <- c(italy=380, canada=124, egypt=818)
codes
class(codes)

# Asign names after values
names(codes)<- country
codes

#seq <- generate sequences 
seq(1,10)
seq(1,10,2)
1:10

#subsetion [] <- acces specific part of a vector
codes[2]
#multi enty vector
codes[c(1,3)]
#secuence
codes[1:2]
#by names
codes["canada"]

#2.2 VECTOR COERCION

#R coerces to character
x <- c(1, "canada", 3)
class(x)  

#Force coercion
x <-1:5
y <- as.character(x)  
class(y)  
y <- as.numeric(y)
class(y)  

#Missing data. Can be a result of coercion
x <- c("1", "b", "3")
as.numeric(x)  
class(x)  

#SORTING

#SORT <- Sorts a vector in increasing order
sort(murders$total)

#ORDER <- Returns the indices that sort a vector
x <- c(31, 4, 15, 92, 65)
sort(x)
index <- order(x)
index
x[index]

x
order(x)

index <- order(murders$total)
index
murders$abb

murders$abb[index]

#Max <- Biggest value
max(murders$total)
#Which Max <- The index in which the largers value resides
which.max(murders$total)

i_max <- which.max(murders$total)
i_max
murders$state[i_max]

i_min <- which.min(murders$total)
murders$state[i_min]

#RANK
x <- c(31, 4, 15, 92, 65)
rank(x)

##DataCamp Assessment: 2.2 Sorting (External resource)##

#Exercise 1

# Access the `state` variable and store it in an object 
states <- murders$state 

# Sort the object alphabetically and redefine the object 
states <- sort(states) 

# Report the first alphabetical value  
states[1]

# Access population values from the dataset and store it in pop
pop <- murders$population

# Sort the object and save it in the same object 
pop <- sort(pop)

# Report the smallest population size 
min(murders$pop)

#Exercise 2

# Access population from the dataset and store it in pop
pop <- murders$population

# Use the command order to find the vector of indexes that orders pop and store in object ord
ord <- order(pop)

# Find the index number of the entry with the smallest population size
ord[1]

#Exercise 3

# Define the variable i to be the index of the smallest state
i <- which.min(murders$population)

# Define variable states to hold the states
states <- murders$state

# Use the index you just defined to find the state with the smallest population
states[1]

#Exercise 4

# Store temperatures in an object 
temp <- c(35, 88, 42, 84, 81, 30)

# Store city names in an object 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Create data frame with city names and temperature 
city_temps <- data.frame(name = city, temperature = temp)

# Define a variable states to be the state names 
states <- murders$state

# Define a variable ranks to determine the population size ranks 
ranks <- rank(murders$population)

# Create a data frame my_df with the state name and its rank
my_df <- data.frame(name = states, population = ranks)

#Exercise 5

# Define a variable states to be the state names from the murders data frame
states <- murders$state

# Define a variable ranks to determine the population size ranks 
ranks <- rank(murders$population)

# Define a variable ind to store the indexes needed to order the population values
ind <- order(murders$population)

# Create a data frame my_df with the state name and its rank and ordered from least populous to most 
my_df <- data.frame(name = states[ind], population = ranks[ind])
my_df

#Exercise 6

# Using new dataset 
library(dslabs)
data(na_example)

# Checking the structure 
str(na_example)

# Find out the mean of the entire dataset 
mean(na_example)

# Use is.na to create a logical index ind that tells which entries are NA
ind <- is.na(na_example)
ind

# Determine how many NA ind has using the sum function
sum(ind)

#Exercise 7

# Note what we can do with the ! operator
x <- c(1, 2, 3)
ind <- c(FALSE, TRUE, FALSE)
x[!ind]

# Create the ind vector
library(dslabs)
data(na_example)
ind <- is.na(na_example)

# We saw that this gives an NA
mean(na_example)


# Compute the average, for entries of na_example that are not NA 
mean(na_example[!ind])

#2.3 VECTOR ARITHMETIC

murders$state[which.max(murders$population)]

#In R, arithmetic operations happen entry wise
heights <- c(69,62,66,70,70,73,67,73,67,70)
heights*2.54

#Substract the average (average is 69)
heights-69

#If we add two vectos they get added entry by entr
#To compute the murder rate per 100.000 habitants
murder_rate <- murders$total/murders$population*100000
murder_rate  

#Order states by murder rate, in decreasing order
murders$state[order(murder_rate, decreasing = TRUE)]

##DataCamp Assessment: 2.3 Vector Arithmetic (External resource)##

#EXERCISE 1

# Assign city names to `city` 
city <- c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")

# Store temperature values in `temp`
temp <- c(35, 88, 42, 84, 81, 30)

# Convert temperature into Celsius and overwrite the original values of 'temp' with these Celsius values
temp <- (5/9)*(temp - 32)
temp
# Create a data frame `city_temps` 
city_temps <- data.frame(name=city, temperature=temp)
city_temps  

#EXERCISE 2

# Define an object `x` with the numbers 1 through 100
x <- c(1:100)

# Compute the sum 
sum(1/x^2)  

#EXERCISE 3

# Load the data
library(dslabs)
data(murders)

# Store the per 100,000 murder rate for each state in murder_rate
murder_rate <- murders$total/murders$population*100000
murder_rate

# Calculate the average murder rate in the US 
mean(murder_rate)  

##Section 2 Assessment##
x <- c(2, 43, 27, 96, 18)

#Question 1
# Match the following outputs to the function which produces that output. Options include sort(x), order(x), rank(x) and none of these.
# 1, 2, 3, 4, 5
order(x)

#Question 2
#Match the following functions to their output. Options include integers 1 through 5 and none of these.
min(x)
which.min(x)

max(x)
which.max(x)

#Question 3
#Mandi, Amy, Nicole, and Olivia all ran different distances in different time intervals. Their distances (in
#miles) and times (in minutes) are as follows:
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)

#How many hours did Olivia run?
time_hours <- time/60
df_time <- data.frame(name=name, time=time_hours)
df_time

#What was Mandi's speed in miles per hour?
speed <- distance/time_hours
df_time <- data.frame(name=name, speed=speed)
df_time

#Which runner had the fastest speed?
max(speed)
which.max(speed)

#3
###############################
#Indexing, Data Wrangling, Plots
###############################

#3.1 Indexing
# We want to move to a Us state with a lower murder rate than Italy 0.71/100.000
#We can subset a vector based on the information of another vector

index <- murder_rate <=0.71
index

murders$state[index]

sum(index)    

#We want a safe state in the West region of the US

west <- murders$region == "West"
safe <- murder_rate <= 1

index <- safe & west
index
murders$state[index]

#3.1.2 Indexing Functions <- which match %in%

#Which <- the entries of a logical vector that are true
x <- c(FALSE, TRUE, FALSE, TRUE, TRUE, FALSE)
which(x)

index <- which(murders$state == "Massachusetts")
index  
murder_rate[index]

# match <- Looks for entries in a vector and returns the index to acces them

index <- match(c("New York","Florida","Texas"),murders$state)
index
murders$state[index]
murder_rate[index]

#%in% In a element of a first vector is in a second vector

x <- c("a", "b", "d", "e")
y <- c("a", "d", "f")    

y %in% x

c("Boston", "Dakota", "Washington") %in% murders$state

#DATA CAMP ASESMENT 3.1
#Exercise 1 
# Store the murder rate per 100,000 for each state, in `murder_rate`
murder_rate <- (murders$total/murders$population)* 100000
murder_rate 

# Store the `murder_rate < 1` in `low` 

low <-  murder_rate < 1
low      
murders$state[low]      

# Get the indices of entries that are below 1
which(murder_rate<1)

# Create a vector ind for states in the Northeast and with murder rates lower than 1.
ind <- low & murders$region == "Northeast"
ind      
murders$state[ind]

#Exercise 2
# Store the murder rate per 100,000 for each state, in murder_rate
murder_rate <- murders$total/murders$population*100000


# Compute the average murder rate using `mean` and store it in object named `avg`
avg <- mean(murder_rate)

# How many states have murder rates below avg ? Check using sum 
sum(murder_rate<avg)

#Exercise 3
# Store the 3 abbreviations AK, MI and IA in a vector called `abbs` (remember that they are character vectors and need quotes)
abbs <- c("AK", "MI", "IA")

# Match the abbs to the murders$abb and store in ind
ind <- match(abbs, murders$abb)

#Print state names from ind
murders$state[ind]

#Exercise 4
#Which of the following are actual abbreviations: MA, ME, MI, MO, MU?

# Store the 5 abbreviations in `abbs`. (remember that they are character vectors)
abbs <- c("MA","ME","MI","MO","MU")

# Use the %in% command to check if the entries of abbs are abbreviations in the the murders data frame
abbs %in% murders$abb

#Exercise 5
# Store the 5 abbreviations in abbs. (remember that they are character vectors)
abbs <- c("MA", "ME", "MI", "MO", "MU") 

# Use the `which` command and `!` operator to find out which index abbreviations are not actually part of the dataset and store in `ind`
ind <- which(!abbs%in%murders$abb)

# Names of abbreviations in `ind`
abbs[ind]

#3.2 BASIC DATA WRANGLING
#depyr pacage

install.packages("dplyr")
library(dplyr)

#mutate <- adding or changing new column
#filter <-subseting rows
#select <- selecting specific column
#%>% <- pipe operatuin, sending the results of a function to another function

#mutate <- takes data frame as the first argument, adn the name and value of +
#the variable as the second argument name = value
murders <- mutate(murders, rate=total/population*100000)
head(murders)

#filter <- takes data table as first argument and conditional as next
#Entries with murder rate lower than 0.71 
filter(murders,rate <=0.71)

#select
new_table <- select(murders, state, region, rate)
filter(new_table, rate<=0.71)

#Put them all toguether usig the pipe %>%

murders %>% select(state,region,rate) %>% filter(rate<=0.71)

#Creating Data Frames

grades <- data.frame(names=c("John","Juan","Jean","Yao"))
exam_1=c(95,80,90,85)
exam_2=c(90,85,85,90)
class(grades$names)

grades <- data.frame(names=c("John","Juan","Jean","Yao"))
exam_1=c(95,80,90,85)
exam_2=c(90,85,85,90)
stringsAsFactors = FALSE
class(grades$names)

#DataCamp Assessment: 3.2 Basic Data Wrangling

#Exercise 1
#Loading data
library(dslabs)
data(murders)

#Loading dplyr
library(dplyr)

# Redefine murders so that it includes a column named rate with the per 100,000 murder rates
murders <-mutate(murders, rate=total/population*100000)
head(murders)

#Exercise 2
#Note that if you want ranks from highest to lowest you can take the negative and then compute the ranks 
x <- c(88, 100, 83, 92, 94)
rank(-x)

# Defining rate
rate <-  murders$total/ murders$population * 100000

# Redefine murders to include a column named rank
# with the ranks of rate from highest to lowest
murders <- mutate(murders, rank=rank(-rate))
head(murders)    

#Exercise 3
# Load dplyr
library(dplyr)

# Use select to only show state names and abbreviations from murders
select(murders, state, abb)

#Exercise 4
# Add the necessary columns
murders <- mutate(murders, rate = total/population * 100000, rank = rank(-rate))
# Filter to show the top 5 states with the highest murder rates
filter(murders, rank < 6)

#Exercise 5
# Use filter to create a new data frame no_south
no_south <- filter(murders, region !="South")

# Use nrow() to calculate the number of rows
nrow(no_south)

#Exercise 6
# Create a new data frame called murders_nw with only the states from the northeast and the west
murders_nw <- filter(murders, region %in% c("Northeast","West"))

# Number of states (rows) in this category 
nrow(murders_nw)

#Exercise 7
# add the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# Create a table, call it my_states, that satisfies both the conditions. 
#Live in the Northeast or West and want the murder rate to be less than 1. 
my_states <- filter(murders, region %in% c("West","Northeast") & rate < 1)
my_states
# Use select to show only the state name, the murder rate and the rank
select(my_states, state, rate, rank)

#Exercise 8
# Load library
library(dplyr)

## Define the rate column
murders <- mutate(murders, rate =  total / population * 100000, rank = rank(-rate))

# show the result and only include the state, rate, and rank columns, all in one line, in that order
filter(murders, region %in% c("Northeast", "West") & rate < 1) %>% 
  select(state, rate, rank)    

#Exercise 9
#Use one line of code to create a new data frame, called my_states,
#that has murder rate and rank columns (with the rank ordered from highest to lowest), 
#considers only states in the Northeast or West which have a murder rate lower than 1, 
#and contain only the state, rate, and rank columns. The line should have four components 
#separated by three %>% operators.

# Loading the libraries
library(dplyr)
data(murders)

# Create new data frame called my_states (with specifications in the instructions)
my_states <- murders %>% mutate(rate = total / population * 100000, rank = rank(-rate)) %>% filter(region %in% c("Northeast", "West") & rate <1) %>% select(state, rate, rank)


#Basic Plots 3.3

#Scatter Plot
population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total
plot(population_in_millions, total_gun_murders)

#Histogram
hist(murders$rate)
murders$state[which.max(murders$rate)]

#Boxplots
boxplot(rate~region, data = murders)

#DataCamp Assessment: 3.3 Basic Plots

#Exercise 1
# Load the datasets and define some variables
library(dslabs)
data(murders)

population_in_millions <- murders$population/10^6
total_gun_murders <- murders$total

plot(population_in_millions, total_gun_murders)

# Transform population using the log10 transformation and save to object log10_population
# Transform total gun murders using log10 transformation and save to object log10_total_gun_murders
# Create a scatterplot with the log scale transformed population and murders
log10_population <- log10(murders$population)
log10_total_gun_murder <- log10(murders$total)  
plot(log10_population, log10_total_gun_murder)  

#Exercise 2 
# Create a boxplot of state populations by region for the murders dataset
boxplot(population~region, data=murders)

#Section 3 Assessment
library(dslabs)
data(heights)
options(digits = 3)
head(heights)

#Question 1
#First, determine the average height in this dataset. Then create a logical vector ind with the indices for those individuals who are above average height.
#How many individuals in the dataset are above average height?
average_height <- mean(heights$height)
average_height    
ind <- sum(heights$height>average_height)
ind  

#Question 2
#How many individuals in the dataset are above average height and are female?
ind <- filter(heights, height> average_height & sex == "Female")  

#Question 3
#If you use mean() on a logical (TRUE/FALSE) vector, it returns the proportion of observations that are TRUE.
#What proportion of individuals in the dataset are female?
mean(heights$sex == "Female")

#Question 4a
#Determine the minimum height in the heights dataset.
x <- which.min(heights$height)
heights$height[x]

#Question 4b
x <- which.min(heights$height)

#Question 4b
#Subset the sex column of the dataset by the index in 4b to determine the individual's sex.
heights$sex[x]

#Question 5 
#This question takes you through three steps to determine how many of the integer height 
#values between the minimum and maximum heights are not actual heights of individuals in 
#the heights dataset.

#Question 5a. Maximum height
x <-which.max(heights$height)
heights$height[x]

#Question 5b. Which integer values are between the maximum and minimum heights?
x <- 50:82

#Question 5c. How many of the integers in x are NOT heights in the dataset?
y <- x %in% heights$height
y    
sum(!y)    

#Question 6
#Using the heights dataset, create a new column of heights in centimeters named ht_cm. 
#Recall that 1 inch = 2.54 centimeters. Save the resulting dataset as heights2.
library(dplyr)
data("heights")
heights2 <- mutate(heights, ht_cm = height * 2.54)
heights2

#Question 6a. What is the height in centimeters of the 18th individual (index 18)?
index <-  order(heights2$ht_cm)
heights2$ht_cm[index]
rank (heights2$ht_cm)
#Wrong answer

#Question 6c. What is the mean height in centimeters?
mean(heights2$ht_cm)

#Question 7a.How many females are in the heights2 dataset?
females <- filter(heights2, sex =="Female")
females    

#Question 7b. What is the mean height of the females in centimeters?
mean(females$ht_cm)

#Question 8
library(dslabs)
data(olive)
head(olive)

#Question 8.1 Plot the percent palmitic acid versus palmitoleic acid in a scatterplot. What relationship do you see?
plot(olive$palmitic, olive$palmitoleic)

#Question 9 Create a histogram of the percentage of eicosenoic acid in olive.
hist(olive$eicosenoic)

#Question 10 Make a boxplot of palmitic acid percentage in olive with separate distributions for each region.
boxplot(palmitic ~ region, data = olive)

#4
###############################
#Programing Basics
###############################

#4.2 BASIC CONDITIONAL

#if else statemetn
a <-2
if(a!=0){print(1/a)} else{print("No reciprocal for 0")}

a <-0
if(a!=0){print(1/a)} else{print("No reciprocal for 0")}

#General form
if(boolean conditions){expressions}else{alternative expressions}

library(dslabs)
data(murders)
murder_rate <- murders$total / murders$population*100000

ind <- which.min(murder_rate)
if(murder_rate[ind] < 0.5){print(murders$state[ind])}else{print("No state has murder rate than low")}    

#ifelse function <- Takes one logical argument and two possible answers
ifelse(a > 0, 1/a, NA)

#ifelse <- works at vectos, analizes each element acordingly
a <- c(0,1,2,-4,5)
result <- ifelse(a> 0, 1/a, NA)  
result  

#ifelse <- its usefull to substitute Na by 0s
data("na_example")
sum(is.na(na_example))  

no_nas <- ifelse(is.na(na_example), 0, na_example)
sum(is.na(no_nas))  

#any <- Returns true if of the etnrie of a vector is true
z <- c(TRUE, TRUE, FALSE)
any(z)

z <-c(FALSE, FALSE, FALSE)
any(z)  

#all <- returns true if alll the entreis of a vector are true
z <- c(TRUE, TRUE, FALSE)
all(z)

z <-c(TRUE, TRUE, TRUE)
all (z) 

#4.3 BASIC FUNCTION

avg <- function(x){
  s <- sum(x)
  n <- length(x)
  s/n
}
x <- 1:100
avg(x)
identical(mean(x), avg(x))

#function <- General from
my_function <- function(x, y, z){
  operations that operate on x, y ,z
}

#Arithmetic or Geometric mean function
avg <- function(x, arithmetic=TURUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}

#4.4 FOR LOOPS
# sum all numbers from 1 to n
compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
compute_s_n(3)
#If we would like to perform this sum for all numbers from 1 to 25, we could use a loop

#loop <- General form
for (i in range of values){
  operations that use i,
  which changes across the range of values
}

for(i in 1:5){
  print(i)
}
i

#Compute sums off all values from 1 to 25
m <-25
#I create an empty vector to stor the results as I compute them
s_n <- vector(length = m)

for (n in 1:m) {
  s_n[n] <- compute_s_n(n)
}
n <- 1:m
head(data.frame(s_n = s_n, formula = n*(n+1)/2))

plot(n, s_n)
lines (n, n*(n+1)/2)

#DataCamp Assessment 4.0 Programming Basics
#Exercise 1
x <- c(1,2,-3,4)
if(all(x>0)){
  print("All Positives")
} else{
  print("Not All Positives")
}

#Exercise 2. Assign the state abbreviation when the state name is longer than 8 characters 
#Use the ifelse function to write one line of code that assigns
#to the object new_names the state abbreviation when the state
#name is longer than 8 characters and assigns the state name when 
#the name is not longer than 8 characters.
new_names <- ifelse(nchar(murders$state)>8, murders$abb, murders$state)
new_names    

#Exercise 3.
#Create function called `sum_n`
sum_n <- function(n){
  x <- 1:n
  sum(x)
}

# Use the function to determine the sum of integers from 1 to 5000
sum_n(5000)

#Exercise 4
#Create a function altman_plot that takes two arguments x and y 
#and plots y-x (on the y-axis) against x+y (on the x-axis).

altman_plot <- function (x, y){
  plot(x + y, y - x)
}
altman_plot(2222, 33)

#Exercise 5 Write a function compute_s_n that for any given n computes the sum Sn=12+22+32+???+n2.
# Here is an example of a function that adds numbers from 1 to n
example_func <- function(n){
  x <- 1:n
  sum(x)
}

# Here is the sum of the first 100 numbers
example_func(100)

# Write a function compute_s_n with argument n that for any given n computes the sum of 1 + 2^2 + ...+ n^2
compute_s_n <- function(n){
  x <- (1:n)^2
  sum(x)
}
# Report the value of the sum when n=10
compute_s_n(10)

#Exercise 6 Now we are going to compute the sum of the squares for 1:25
# Define a function and store it in `compute_s_n`
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}

# Create a vector for storing results
s_n <- vector("numeric", 25)

# write a for-loop to store the results in s_n
for(i in 1:25){
  s_n[i] <- compute_s_n(i)
}

#Exercise 7
# Define the function
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}

# Define the vector of n
n <- 1:25

# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}

#  Create the plot 
altman_plot <- function (s_n, n){
  plot(n, s_n)
}

#Exercise 8
# Define the function
compute_s_n <- function(n){
  x <- 1:n
  sum(x^2)
}

# Define the vector of n
n <- 1:25

# Define the vector to store data
s_n <- vector("numeric", 25)
for(i in n){
  s_n[i] <- compute_s_n(i)
}

# Check that s_n is identical to the formula given in the instructions.
identical(s_n, n*(n+1)*(2*n+1)/6)

# Section 4 Assessment
library(dslabs)
data(heights)

#Question 1
#Write an ifelse() statement that returns 1 if the sex is Female and 2 if the sex is Male.
#What is the sum of the resulting vector?
head(heights)

x <- ifelse(heights$sex == "Female", 1, 2)    
sum(x)  

#Question 2
#Write an ifelse() statement that takes the height column and returns the height
#if it is greater than 72 inches and returns 0 otherwise.
x <- ifelse(heights$height>72, heights$height, 0)
mean(x)  

#Question 3
#Write a function inches_to_ft that takes a number of inches x and returns the 
#number of feet. One foot equals 12 inches.
#What is inches_to_ft(144)?
inches_to_ft <- function(x){
  y <- x /12
  y
}
inches_to_ft(144)

#How many individuals in the heights dataset have a height less than 5 feet?
ft_to_inches <- function(x){
  y <- x*12
  y
}
ft_to_inches(5)

dwarfs <- ifelse(heights$height<60, 1, 0)
sum(dwarfs)  
#Alternative code
sum(inches_to_ft(heights$height) < 5)

#Question 5
# define a vector of length m
m <- 10
f_n <- vector(length = m)

# make a vector of factorials
for (n in 1:m) {
  
}{
  f_n[n] <- factorial(n)
}

# inspect f_n
f_n
