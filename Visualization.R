#Data Science: Visualization
#09/08/20

#1.1 Introduction to Data Visualization 
library(dslabs)
data(murders)
head(murders)

#Data tipe
#Categorical <- Small numebr of groups. Ej. Sex, State
#Ordinal <- when we can order the groups. Ej, spicines, Cigarret boxes. Few groups with a lot of people

#Continuous <- Any number. Heights
#Discrete <- Many grops with small number of people. Ej. Number of Cigarrets.

#Exercise 1. Extract the variable names from a dataset using the names function.
names(heights)

#Exercise 2. Use the unique and length functions to determine how many unique heights were reported.
x <- heights$height
length(unique(x))

#Exercise 3. Use the table function to compute the frequencies of each unique height value. 
tab <- table(x)
tab

#Exercise 4. For values reported only once tab will be 1. Use logicals and the function sum to count the number of times this happens.
sum(tab==1)

#1.2 Introduction to Distributions
library(dslabs)
data(heights)
head(heights)

prop.table(table(heights$sex))

#CDF <- Cumulative Distribution Function. 
#Reports the proportion of data below a value  a  for all values of  a
#F(a)=Pr(x???a)

#Manually compute CDF
a <- seq(min(heights$height), max(heights$height), length = 100) # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(heights$height <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)

#The CDF defines that proportion of data below a cutoff  a
#To define the proportion of values above  a , we compute:

# 1???F(a)

#To define the proportion of values between  a  and  b , we compute:

#F(b)???F(a)

#Note that the CDF can help compute probabilities. 
#The probability of observing a randomly chosen value between  a  and  b  
#is equal to the proportion of values between  a  and  b , which we compute 
#with the CDF.

#Smoth Density Plots <- basically the curve that gous trough the top of the 
#histogram bars when the bis are really small.We asume that the data from our Hist,
#is a samplo from a greater population, which distribution would be the Smoth
#Density Curve. We can select the degree of smothness

#Hist is based on data Smoth Curve is based on asumptions
#THe area of the Smoth Density curve is always 1


#Normal Distribution, aslo Bell Cure or Gaussian Distribution
#For any interval, a-b, the propotion of balues that fall in that interval
#Can by calcualted vy knowing the SD and the AVG
#95% of the values are in to 2 SDs of the AVG
#If the data set is normal it can be desscribed with only these 2 values
average <- sum(x)/length(x)
sd <- sqrt(sum(x-average)^2)/length(x) #(The average distance between the values adn they average)

# avg and sd for male heights
index <- heights$sex == "Male"
x <- heights$height[index]
average <- mean(x)
SD <- sd(x)
c(average=average, SD=SD)

#Z <- standard unit of a value <- how many SD for the man a value is.
# z = (x-average)/SD 
# scale <- we obpatin standar units
z<- scale(x)
z  

#How many men are in to 2 SDs from the average
mean(abs(z)<2)
help(abs)

#pnorm() <- The CDF of the normal distribution
# What is the probability that a randome selected student is taller than 50.5
1- pnorm(70.5, mean(x),sd(x))

#Plot the proportion of studets that reportet each height
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

#Instead of that aobce, we define probabilties in intervals
#Aproximate students reporting between 69.5 and 70.5
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))#Real <- 0.1145
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))# Real <-0.1031
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))#Real <- 0.1081

#As people thend to report Integers as hight, if our interval does not include
#one, the aproximatios get wors
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x)) #Real 0.02216
#This is called Discretization.

# Assessment: Normal distribution
library(dslabs)
data(heights)
x <- heights$height[heights$sex == "Male"]

#Question 1 
#What proportion of the data is between 69 and 72 inches.
mean(x > 69 & x <= 72)

#Question 2
#Suppose you only have avg and stdev below, but no access to x, can you approximate the proportion of the data that is between 69 and 72 inches?
avg <- mean(x)
stdev <- sd(x)
pnorm(72, avg, stdev) -  pnorm(69, avg, stdev)

#Question 3
x <- heights$height[heights$sex == "Male"]
exact <- mean(x > 79 & x <= 81)
exact
# Use normal approximation to estimate the proportion of heights between 79 and 81 inches and save it in an object called approx.
approx <- pnorm(81,mean(x), sd(x)) -  pnorm(79, mean(x), sd(x))
approx
exact / approx

#Question 4
#Using the normal approximation, estimate the proportion of adult men that are taller than 7 feet.

1-pnorm(7*12, mean(x), sd(x))

#Question 5. In a population of 1Billion how many people are 7 foters.
p <- 1-pnorm(7*12, 69, 3)
N <-round(p*(10^9))

#Question 6. There are 10 7 footers in the NBA.
#Then calculate the proportion of the world's 18 to 40 year old seven footers that are in the NBA
10/287

#Question 7. Repeat the calculations performed in the previous question for Lebron James' height: 6 feet 8 inches. 
#There are about 150 players, instead of 10, that are at least that tall in the NBA.
p <- 1-pnorm(6.8*12, 69, 3)
N <- round(p * 10^9)
150/N

#1.3  Definiton of quantiles

#Quantiles are cutoff points that divide a dataset into intervals with set
#probabilities. The  q th quantile is the value at which  q % of the observations are equal to or less than that value.
#Given a dataset data and desired quantile q, you can find the qth quantile of data with:
quantile(data,q)

#Percentiles are the quantiles that divide a dataset into 100 intervals each with 1% probability. 
#You can determine all percentiles of a dataset data like this:
p <- seq(0.01, 0.99, 0.01)
quantile(data, p)

#Quartiles, divide a dataset into 4 parts each with 25% probability. 
summary(heights$height)

#Example
library(dslabs)
data(heights)

#Find the Quartiles of heights$height
summary(heights$height)

#Find the percentiles of heights$height:
p <- seq(0.01, 0.99, 0.01)
percentiles <- quantile(heights$height, p)   
percentiles

#Confirm that the 25th and 75th percentiles match the 1st and 3rd quartiles
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

#The qnorm() function gives the theoretical value of a quantile with probability p
#of observing a value equal to or less than that quantile value given a normal 
#distribution with mean mu and standard deviation sigma:
qnorm(p)

#The pnorm() function gives the probability that a value from a standard normal distribution
#will be less than or equal to a z-score value z. Consider:
#qnorm() and pnorm() are inverse functions:

pnorm(-1.96)  ??? 0.025

qnorm(0.025)  ??? ???1.96

#Theoretical quantiles
#You can use qnorm() to determine the theoretical quantiles of a dataset: 
#that is, the theoretical value of quantiles assuming that a dataset follows
#a normal distribution. Run the qnorm() function with the desired probabilities p,
#mean mu and standard deviation sigma. 

#Suppose male heights follow a normal distribution with a mean of 69 inches
#and standard deviation of 3 inches. The theoretical quantiles are:
p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)

#Quantile-Quantile Plots <- to check if a distributionn aproximates normallity.
#Example
p <- seq(0.05, 0.95, 0.05)

observer_quantiles <- quantile(x,p)
observer_quantiles

theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd (x))

plot(theoretical_quantiles, observer_quantiles)
abline(0,1)

#same code with standar units
observed_quantiles <- quantiles(z, p)
theoretical_quantiles <- qnorm(p)
plot(theoretical_quantiles, observer_quantiles)

#Exercise 1
#Define a variable male that contains the male heights.
male <- heights$height[heights$sex=="Male"]
#Define a variable female that contains the female heights.
female <- heights$height[heights$sex=="Female"]
#Report the length of each variable.
length(male)
length(female)

#Exercise 2
#Create two five row vectors showing the 10th, 30th, 50th, 70th, 
#and 90th percentiles for the heights of each sex called these vectors 
#female_percentiles and male_percentiles.
female_percentiles <- quantile(female, seq(0.1, 0.9, 0.2))
female_percentiles

male_percentiles <- quantile(male, seq(0.1,0.9,0.2))
male_percentiles

#Then create a data frame called df with these two vectors as columns. 
#The column names should be female and male and should appear in that order
df <- data.frame(female=female_percentiles, male=male_percentiles)
df

#Exercise 3. Heights of children
library(HistData)
data(Galton)
x <- Galton$child
#Compute the average and median of these data. 
mean(x)
median(x)

#Compute the standard deviation and the median absolute deviation of these data.
sd(x)
mad(x)

#Exercise 4. Mesurement error
x_with_error <- x
x_with_error[1] <- x_with_error[1]*10

#Report how many inches the average grows after this mistake.
mean (x_with_error) - mean(x)

#Report how many inches the SD grows after this mistake.
sd(x_with_error)- sd(x)

#Report how many inches the median grows after the mistake.
median(x_with_error)- median(x)

#Report how many inches the MAD grows after the mistake.
mad(x_with_error)- mad(x)

#Write a function called error_avg that takes a value k and returns the average of
#the vector x after the first entry changed to k. Show the results for k=10000 and k=-10000.

error_avg <- function(k){
  x[1] <- k
  mean(x)
}

error_avg(10000)
error_avg(-10000)

###########################
################# 2-ggplot2
###########################
library(tidyverse)
library(dslabs)
data("murders")

ggplot(data = murders)
murders %>% ggplot()

p <- ggplot(data = murders)
class(p)
p

#A + >- Adding layes 
# DATA %>% ggplot() + LAYER_1 + LAYER_2 + ... + LAYER_N
#aes <- asthetic maping, conects data with the graph
murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y =total))

p + geom_point(aes(population/10^6, total))

p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb))

p + geom_point(aes(population/10^6, total), size = 3) +
  geom_text(aes(population/10^6, total, label = abb), nudge_x = 2)

#Global asthetic maping <- if used you dont need to repeat the geometries every layer

p <- murders %>% ggplot(aes(population/10^6, total, label = abb))

p + geom_point(size = 3) +
  geom_text(nudge_x = 2)

#The local mapins override the global ones
p +geom_point(size = 3) + 
  geom_text(aes(x=10,y=800, label = "Hello there!"))

#2.2 Scales, Labels, and Colors
p + geom_point(size=2) +
  geom_text(nudge_x = 0.3) +
  scale_x_continuous(trans = "log10")+
  scale_y_continuous(trans = "log10")

#The same with specific function
p + geom_point(size = 3, color = "blue") +
  geom_text(nudge_x = 0.075) +
  scale_x_log10() +
  scale_y_log10()+ 
  xlab("Population in millions (log scale")+
  ylab("Total number of murders (log scale")+
  ggtitle("US Gun Murders in US 2010")

#Asignig different colors to regions
p + geom_point(aes(col = region), size = 2) +
  geom_text(nudge_x = 0.2) +
  scale_x_log10() +
  scale_y_log10()+ 
  xlab("Population in millions (log scale")+
  ylab("Total number of murders (log scale")+
  ggtitle("US Gun Murders in US 2010")

#Add a line with the national average
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  pull(rate)

p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r))+
  geom_text(nudge_x = 0.2) +
  scale_x_log10() +
  scale_y_log10()+ 
  xlab("Population in millions (log scale")+
  ylab("Total number of murders (log scale")+
  ggtitle("US Gun Murders in US 2010")

p <- p + geom_point(aes(col = region), size = 3) +
  geom_abline(intercept = log10(r), lty = 2, color= "darkgrey")+
  geom_text(nudge_x = 0.2) +
  scale_x_log10() +
  scale_y_log10()+ 
  xlab("Population in millions (log scale")+
  ylab("Total number of murders (log scale")+
  ggtitle("US Gun Murders in US 2010")+
  scale_color_discrete(name = "Region")

library(dslabs)
ds_theme_set()

library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)

# define the intercept
r <- murders %>%
  summarize(rate = sum(total) / sum(population) * 10^6) %>%
  .$rate

# make the plot, combining all elements
murders %>%
  ggplot(aes(population/10^6, total, label = abb)) +
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3) +
  geom_text_repel() +
  scale_x_log10() +
  scale_y_log10() +
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)") +
  ggtitle("US Gun Murders in 2010") +
  scale_color_discrete(name = "Region") +
  theme_economist()

#Other examples
library(tidyverse)
library(dslabs)
data(heights)

p <- heights %>%
  filter(sex == "Male") %>%
  ggplot(aes(x = height))

p + geom_histogram(binwidth = 1, fill= "blue", col ="black")+
  xlab("Male Heights")+
  ggtitle("Historiogram")

p + geom_density(fill = "blue") 

p <- heights %>% 
  filter(sex == "Male") %>%
  ggplot(aes(sample = height))

p + geom_qq()

params <- heights %>%
  filter(sex == "Male")%>%
  summarize(mean=mean(height), sd = sd(height))

p + geom_qq(dparams = params)+
  geom_abline()

##Plot with standarized z units so we dont have to calculate mean and SD
heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

#Grid plots
p <- heights %>%
  filter(ssex=="Male") %>%
  ggplot(aes(x = height))
p1<- p + geom_histogram(binwidth = 1, fill = "blue", col="black")
p1<- p + geom_histogram(binwidth = 2, fill = "blue", col="black")
p1<- p + geom_histogram(binwidth = 3, fill = "blue", col="black")

library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)

#Exercise 1
library(dplyr)
library(ggplot2)
library(dslabs)
data(heights)
data(murders)

p <- murders %>% ggplot()

#Use labels instead of dots
murders %>% ggplot(aes(population, total)) +
  geom_label(aes(label= abb))

murders %>% ggplot(aes(population, total, label=abb))+
  geom_label

#BLue lables
murders %>% ggplot(aes(population, total,label= abb)) +
  geom_label(color="blue")

#Different colors for each region
murders %>% ggplot(aes(population, total,label= abb, color = region)) +
  geom_label

#Change both axes to be in the log scale on a single graph. 
p <- murders %>% ggplot(aes(population, total,label= abb, color = region)) +
  geom_label() 

p + scale_x_log10() + scale_y_log10()

#Exercise 2
#Create a ggplot object called p using the pipe to assign the heights data to a ggplot object.
#Assign height to the x values through the aes function.
p <- heights %>% ggplot(aes(height))
#Add a layer to the object p  using the geom_histogram function to make the histogram.
p + geom_histogram()
#  bins of size 1 inch.
p + geom_histogram(binwidth = 1)

#Exercise 3
#Create separate smooth density plots for males and females
heights %>% 
  ggplot(aes(height, group = sex)) + 
  geom_density()

#Add colors 
heights %>% 
  ggplot(aes(height, group = sex, color= sex)) + 
  geom_density()

#Fill with different colors
heights %>% 
  ggplot(aes(height, fill = sex)) + 
  geom_density(alpha = 0.3)

#Case Study: Trends in World Health and Economics
library(dslabs)l
data(gapminder)
head(gapminder)

#Hiher child mortality turkey cs Sri Lanka
gapminder %>% 
  filter(year == 2015 & country %in% c("Turkey", "Sri Lanka")) %>%
  select(country, infant_mortality)

#Fertiliy life expenctacy plot, in 1962
filter(gapminder, year %in% c(1962)) %>%
  ggplot(aes(life_expectancy, fertility, color = continent))+
  geom_point()

#Faceting Varibles
filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(life_expectancy, fertility, color = continent))+
  geom_point()+
  facet_grid(continent~year)

filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(life_expectancy, fertility, color = continent))+
  geom_point()+
  facet_grid(.~year)

years <- c(1962, 1980, 1990, 2000, 2012)
continents <- c("Europe", "Asia")
gapminder %>%
  filter(year %in% years & continent %in% continents) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_wrap(~year)

#Time Series Plots
gapminder %>%
  filter(country== "United States") %>%
  ggplot(aes(year, fertility))+ 
  geom_line()


gapminder %>%
  filter (country %in% c("South Korea", "Germany")) %>%
  ggplot(aes(year, fertility, group = country ))+
  geom_line()


gapminder %>%
  filter (country %in% c("South Korea", "Germany")) %>%
  ggplot(aes(year, fertility, color =country ))+
  geom_line()

countries <- c("South Korea", "Germany")
labels <- data.frame(country = countries, x = c(1980, 1965), y = c(60, 72))
gapminder %>% filter(country %in% countries) %>%
  ggplot(aes(year, life_expectancy, col = country)) +
  geom_line() +
  geom_text(data = labels, aes(x, y, label = country), size = 5) +
  theme(legend.position = "none")

#Transformation
gapminder <- gapminder %>%
  mutate(dollars_per_day = gdp/population/365)

past_year <- 1970
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black")

gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")

#to log without transforming
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")

#Box plots with differet regions
p <-  gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(region, dollars_per_day))
p +geom_boxplot()+
  theme(axis.text = element_text(angle = 90, hjust = 1))

#re-order the boxplots
p <- gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%    # reorder
  ggplot(aes(region, dollars_per_day, fill = continent)) +    # color by continent
  geom_boxplot() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
p + scale_y_continuous(trans = "log2")+
  geom_point(show.legend = FALSE)

#Define a vector for regions in the west
west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  mutate(group =ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(.~group)


#Dividing west ands developing in 2010 and 1970
new_year <- 2010

gapminder %>%
  filter(year %in% c(new_year, past_year) & !is.na(dollars_per_day)) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

#intersec comand <- to have only contries wich are in both 1970 and 2010
country_list_1 <- gapminder %>%
  filter(year == past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>%
  filter(year == new_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1, country_list_2)


gapminder %>%
  filter(year %in% c(past_year, new_year) & country %in% country_list) %>%    
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ group)

#Boxplots dividing 2010 and 1970

p <- gapminder %>%
  filter(year %in% c(new_year, past_year) & country %in% country_list) %>%
  mutate(region = reorder(region, dollars_per_day, FUN = median)) %>%
  ggplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") +
  scale_y_continuous(trans = "log2")
p + geom_boxplot(aes(region, dollars_per_day, fill = continent))+
  facet_grid(.~year)

#Put everhing together and fill by year
p + geom_boxplot(aes(region, dollars_per_day, fill = factor(year)))

#DENSITY PLOTNS
p <- 
  gapminder %>%
  filter(year %in% c(past_year, new_year) & country %in% country_list) %>%    
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day))+
  scale_x_continuous(trans = "log2")
p + geom_density(fill = "grey")+
  facet_grid(.~ year)

#Overly both grops with wighted counties. (areas of densitis proportiona to grope size)
#We optine this by multiplyin Y values by size opf the group
#The geo:density has te function "count" that does so.
#with .. .. we can put something in the Y axis
p <- 
  gapminder %>%
  filter(year %in% c(past_year, new_year) & country %in% country_list) %>%    
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day, y = ..count.., fill= group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.7)+
  facet_grid(.~year )

#CASE_WHEN <- Show key regions sepaetly. Defines groups

#We star by asigning groups
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))
#We turn the new griup variable in to a factor, to control the order of the levels

gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))

#Now we use the staking argument
#WE neet to redefine P with the new gapminder object
p <- 
  gapminder %>%
  filter(year %in% c(past_year, new_year) & country %in% country_list) %>%    
  ggplot(aes(dollars_per_day, fill = group))+
  scale_x_continuous(trans = "log2")

p +geom_density(alpha = 0.4, bw = 0.7, position = "stack") +
  facet_grid(year ~ .)

#Finaly we weight the countries by population
gapminder %>%
  filter(year %in% c(past_year, new_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

#Comparations of child mortality and dollar per day. Ecological falacy
# We star by addig more regions iwht the case_when Function
gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))

#We create a data fram with average income and average survival rate
surv_income <- gapminder %>%
  filter(year %in% new_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)

#Plot by group 
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 

#Plot by country 
surv_income <- gapminder %>%
  filter(year %in% new_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group, country) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income %>% arrange(income)



surv_income %>% ggplot(aes(income, infant_survival_rate, label = country, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 2.5) 

# Exercises
data(gapminder)
head(gapminder)
#Assessment: Exploring the Gapminder Dataset
#1.Create a scatter plot of life expectancy versus fertility for the African continent in 2012.
gapminder %>% filter(year %in% 2012 & continent %in% "Africa") %>%
  ggplot(aes(life_expectancy, fertility)) +
  geom_point()

#2.Use color to distinguish the different regions of Africa.
gapminder %>% filter(year %in% 2012 & continent %in% "Africa") %>%
  ggplot(aes(life_expectancy, fertility, color = region)) +
  geom_point()

#3. Create a table showing the country and region for the African countries (use select)
#that in 2012 had fertility rates of 3 or less and life expectancies of at least 70.
df <- gapminder %>% 
  filter(continent=="Africa" & year == 2012 & fertility <=3 & life_expectancy>=70) %>%
  select(country, region)
df

#4. Use filter to create a table with data for the years from 1960 to 2010 in Vietnam and the United States.
years <- 1960:2010
countries <- c("United States", "Vietnam")
tab <- gapminder %>% filter(year %in% years & country %in% countries)

#5. Use geom_line to plot life expectancy vs year for Vietnam and the United States and save the plot as p
p <- tab %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()
p

#6. Use a single line of code to create a time series plot from 1960 to 2010 of life expectancy vs year for Cambodia.
gapminder %>% filter(country %in% "Cambodia" & year %in% years) %>%
  ggplot(aes(x = year, y = life_expectancy)) +
  geom_line()

#7. Use mutate to create a dollars_per_day variable, which is defined as gdp/population/365.
daydollars <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter(continent == "Africa" & year == 2010 & !is.na(dollars_per_day)) 
daydollars

#8.Create a smooth density plot of dollars per day from daydollars
daydollars %>%
  ggplot(aes(dollars_per_day)) +
  geom_density() +
  scale_x_continuous(trans = "log2")

#9. Use facet_grid to show a different density plot for 1970 and 2010.
#Use facet_grid to show a different density plot for 1970 and 2010.
year_1970 <- 1970
year_2010 <- 2010

daydollars <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter(continent == "Africa" & year %in% c(year_2010, year_1970) & !is.na(dollars_per_day)) 
daydollars

daydollars %>%
  ggplot(aes(dollars_per_day))+
  scale_x_continuous(trans = "log2")+
  geom_density() +
  facet_grid(. ~ year)

#10. stacked density plot of each region in Africa.
daydollars %>%
  ggplot(aes(dollars_per_day, fill = region))+
  scale_x_continuous(trans = "log2")+
  geom_density(alpha = 0.4, bw = 0.75, position = "stack") +
  facet_grid(. ~ year)

#11 Scatterplot for dollars per day 2010, regions by colors
gapminder_Africa_2010 <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter(continent == "Africa" & year %in% c(2010) & !is.na(dollars_per_day) & !is.na(dollars_per_day)) 
head(gapminder_Africa_2010)
gapminder_Africa_2010 %>%
  ggplot(aes(infant_mortality, dollars_per_day, color = region)) +
  geom_point()

#X axis base 2 log
gapminder_Africa_2010 %>%
  ggplot(aes(infant_mortality, dollars_per_day, color = region)) +
  geom_point() +
  scale_x_continuous(trans = "log2")

# Add a geom_text layer to display country names in addition to of points.
gapminder_Africa_2010 %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_text() + 
  scale_x_continuous(trans = "log2")

#Use facet_grid to show different plots for 1970 and 2010. Align the plots vertically.
daydollars <- gapminder %>% 
  mutate(dollars_per_day = gdp/population/365) %>% 
  filter(continent == "Africa" & year %in% c(year_2010, year_1970) & !is.na(dollars_per_day)) 


daydollars %>% 
  ggplot(aes(dollars_per_day, infant_mortality, color = region, label = country)) +
  geom_text() + 
  scale_x_continuous(trans = "log2") +
  facet_grid(year ~ .)

#Assessment: Data Visualization Principles, Part 2
#Exercsie 1
library(dplyr)
library(ggplot2)
library(dslabs)
dat <- us_contagious_diseases %>%
  filter(year == 1967 & disease=="Measles" & !is.na(population)) %>% mutate(rate = count / population * 10000 * 52 / weeks_reporting)
state <- dat$state 
rate <- dat$count/(dat$population/10000)*(52/dat$weeks_reporting)

#Redefine the state object so that the levels are re-ordered by rate.
state <- reorder(state, rate)
print(state)
levels(state)

#Exercise 2
dat <- us_contagious_diseases %>% filter(year == 1967 & disease=="Measles" & count>0 & !is.na(population)) %>%
  mutate(rate = count / population * 10000 * 52 / weeks_reporting) %>%
  mutate(state = reorder(state, rate))
dat %>% ggplot(aes(state, rate)) +
  geom_bar(stat="identity") +
  coord_flip()

#Exercise 3
library(dplyr)
library(ggplot2)
library(dslabs)
data("murders")
#Order the regions by their median murder rate by using mutate and reorder.
#Make a box plot of the murder rates by region.
murders %>% mutate(rate = total/population*100000) %>%
  mutate(region=reorder(region, rate, FUN=median)) %>%
  ggplot(aes(region, rate)) +
  geom_boxplot() +
  geom_point()

#Slope charts
library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy")

#Bland-Altman plot <. difference against the mean
library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

#Case Study: Vaccines
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# Assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

# tile plot of disease rate by state and year
dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")

#Line plot of measles rate by year and state.
#compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)

# make line plot of measles rate by year by state
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")

#Exercise 1
# time series plot showing the rate of measles cases per population by state
library(dplyr)
library(ggplot2)
library(dslabs)
library(RColorBrewer)
data(us_contagious_diseases)
the_disease = "Smallpox"

dat <- us_contagious_diseases %>%
  filter(!state%in%c("Hawaii","Alaska") & disease == the_disease & weeks_reporting >= 10) %>%
  mutate(rate = count / population * 10000) %>%
  mutate(state = reorder(state, rate))

avg <- us_contagious_diseases %>%
  filter(disease==the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm=TRUE)/sum(population, na.rm=TRUE)*10000)

dat %>% ggplot() +
  geom_line(aes(year, rate, group = state),  color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate),  data = avg, size = 1, color = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5,25,125,300)) + 
  ggtitle("Cases per 10,000 by state") + 
  xlab("") + 
  ylab("") +
  geom_text(data = data.frame(x=1955, y=50), mapping = aes(x, y, label="US average"), color="black") + 
  geom_vline(xintercept=1963, col = "blue")

#Exercise 2
#For the state of California, make a time series plot showing rates for all diseases.
us_contagious_diseases %>% filter(state=="California" & weeks_reporting>=10) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) + 
  geom_line()
#Exercise 3
us_contagious_diseases %>% filter(!is.na(population)) %>% 
  group_by(year, disease) %>%
  summarize(rate = sum(count)/sum(population)*10000) %>%
  ggplot(aes(year, rate, color = disease)) + 
  geom_line()

# Titanic Survival Exercises
options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)

titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

#Question 1: Variable Types
?titanic_train
head (titanic_train)
#Question 2: Demographics of Titanic Passengers
#Make density plots of age grouped by sex.

#1.1 Females and males had the same general shape of age distribution..
titanic_train %>% 
  ggplot(aes(x = Age)) +
  geom_freqpoly() +
  facet_grid(. ~ Sex)

#1.2 There were more females than males.
titanic_train %>% 
  ggplot(aes(Age, color = Sex)) +
  geom_freqpoly(alpha = 1, bw = 0.75,) 

#1.3 Question 3: QQ-plot of Age Distribution
#Use geom_qq() to make a QQ-plot of passenger age and add an identity line with geom_abline(). Filter out any individuals with an age of NA first. 
#Use the following object 
params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
params

titanic_train %>%
  ggplot(aes(sample = Age))+
  geom_qq(dparams = params) +
  geom_abline()

#1.4 Question 4: Survival by Sex. True or False
#To answer the following questions, make barplots of the Survived and Sex variables using geom_bar(). 
#Try plotting one variable and filling by the other variable. You may want to try the default plot, 
#then try adding position = position_dodge() to geom_bar() to make separate bars for each group.

#Less than half of passengers survived.
titanic %>%
  ggplot(aes(Survived)) +
  geom_bar()

#Most of the survivors were female.
titanic %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar()

titanic_train %>%
  ggplot(aes(Survived, fill = Sex)) +
  geom_bar(position = position_dodge(width = 1))

#Question 5: Survival by Age
#Make a density plot of age filled by survival status. Change the y-axis to count and set alpha = 0.2.
#Which age group is the only group more likely to survive than die?
titanic %>% 
  ggplot(aes(Age, fill = Survived)) +
  geom_freqpoly(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(Survived ~.)

#Question 6: Survival by Fare
#Filter the data to remove individuals who paid a fare of 0. Make a boxplot of fare grouped by survival status. 
#Try a log2 transformation of fares. Add the data points with jitter and alpha blending.
titanic %>%
  filter(Fare > 0) %>%
  ggplot(aes(y = Fare, x = Survived)) +
  geom_boxplot()+
  geom_jitter()
facet_grid(. ~Survived)

#Question 7: Survival by Passenger Class
#The Pclass variable corresponds to the passenger class. 
#Make three barplots. For the first, make a basic barplot of passenger class filled by survival. 
#For the second, make the same barplot but use the argument
#position = position_fill() to show relative proportions in each group instead of counts.
#For the third, make a barplot of survival filled by passenger class using position = position_fill().

titanic %>%
  ggplot(aes(Pclass, fill = Survived)) +
  geom_bar()

titanic %>%
  ggplot(aes(Pclass,fill = Survived)) +
  geom_bar( position = position_fill())

titanic %>%
  filter(Survived == 1) %>%
  ggplot(aes(Survived, fill = Pclass)) +
  geom_bar(position = position_fill())

# Question 8: Survival by Age, Sex and Passenger Class
#Create a grid of density plots for age, filled by survival status,
#with count on the y-axis, faceted by sex and passenger class.
titanic %>%
  ggplot(aes(Age, col = Survived)) +
  geom_freqpoly(alpha = 1, bw = 0.75) +
  facet_grid(Sex ~Pclass)

#Properties of Stars Exercises
library(tidyverse)
library(dslabs)
data(stars)
options(digits = 3)

#Question 1
#Mean and sd luminosity
head(stars)
mean(stars$magnitude)
sd(stars$magnitude)

#Question 2
#density plot of magnitud
stars %>%
  ggplot(aes(magnitude)) +
  geom_density()

#QUestion 3
#Distribution of star tempeatura
stars %>%
  ggplot(aes(sample = temp)) +
  geom_qq()+
  geom_abline()

#Question 4
#Make a scatter plot of the data with temperature on the x-axis and magnitude on the y-axi
stars %>%
  ggplot(aes(x = temp, y = magnitude)) +
  geom_point()

#Question 5 
#Flip the y-axis so that lower values of magnitude are at the top of the axis (recall that more luminous stars have lower magnitude) using scale_y_reverse(). 
#Take the log base 10 of temperature and then also flip the x-axis.
log_temp <- log10(stars$temp)

stars %>%
  ggplot(aes(y = log_temp, x = magnitude)) +
  scale_y_reverse() +
  scale_x_reverse()+
  geom_point()

stars %>%
  ggplot(aes(y = temp, x = magnitude)) +
  scale_y_reverse() +
  scale_x_reverse()+
  geom_point()

#Question 8
#The least lumninous star in the sample with a surface temperature over 5000K.
stars %>%
  filter(temp > 5000) %>%
  ggplot(aes(y = temp, x = magnitude)) +
  scale_y_reverse() +
  scale_x_reverse()+
  geom_point()+   
  geom_text(aes(label = star), nudge_x = -2)

stars %>%
  ggplot(aes(y = temp, x = magnitude)) +
  scale_y_reverse() +
  scale_x_reverse()+
  geom_point()+   
  geom_text(aes(label = star), nudge_x = -1)

#Question 9
#Remove the text labels and color the points by star type

stars %>%
  ggplot(aes(y = temp, x = magnitude)) +
  scale_y_reverse() +
  scale_x_reverse()+
  geom_point()+   
  geom_text(aes(label = type), nudge_x = -1)

# Practice Exercise. National Center for Health Statistics
library(NHANES)
data(NHANES)

#There are many NA in this data
mean(na_example)
sd(na_example)

#na.rm <- to remove NA
mean(na_example, na.rm = TRUE)
sd(na_example, na.rm = TRUE)

#Exercise 1
#Filter the NHANES dataset so that only 20-29 year old females are included and assign this new data frame to the object tab.
head(NHANES)

tab <- NHANES %>% 
  filter(AgeDecade == " 20-29" & Gender == "female")

# Exercise 2
# Complete the line of code to save the average and standard deviation of systolic blood pressure as average and standard_deviation to a variable called ref.
ref <- NHANES %>% 
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm = TRUE))

#Exercise 3 
#Modify the line of sample code to assign the average to a numeric variable called ref_avg using the . or pull
ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>%
  .$average

#Exercise 4
#Report the min and max values for the same group as in the previous exercises.
NHANES %>%
  filter(AgeDecade == " 20-29"  & Gender == "female") %>%
  summarize(mxbp = max(BPSysAve, na.rm = TRUE), minbp = min(BPSysAve, na.rm = TRUE))

#Exercie 5
#Compute the average and standard deviation of systolic blood pressure for females for each age group separately.
#Save the average and standard deviation of systolic blood pressure (BPSysAve) as average and standard_deviation
NHANES %>%
  filter(Gender == "female") %>%
  group_by(AgeDecade) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE))

#Exercise 6
#We can use group_by(AgeDecade, Gender) to group by both age decades and gender.
NHANES %>%
  group_by(AgeDecade, Gender) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE))

#Exercise 7
#Compute the average and standard deviation for each value of Race1 for males in the age decade 40-49.
#Order the resulting table from lowest to highest average systolic blood pressure
NHANES %>%
  filter(AgeDecade == " 40-49" & Gender == "male") %>%
  group_by(Race1) %>%
  summarize(average = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE)) %>%
  arrange(average)

#Climate Change Exercises: Questions 1-7

library(tidyverse)
library(dslabs)
data(temp_carbon)
data(greenhouse_gases)
data(historic_co2)

#Question 1
#What is the first year for which carbon emissions (carbon_emissions) data are available?
temp_carbon %>%
  filter(!is.na(carbon_emissions)) %>%
  .$year %>%
  min()

#How many times larger were carbon emissions in the last year relative to the first year?
head(temp_carbon)

carbon_2014 <-temp_carbon %>%
  filter(year == 2014) %>%
  summarize(carbon_emissions)
carbon_2014

carbon_1751 <- temp_carbon %>%
  filter(year == 1751) %>%
  summarize(carbon_emissions)
carbon_1751

carbon_2014/carbon_1751

#Question 3
#Inspect the difference in temperature in temp_carbon from the first available year to the last available year.

#Wat is the first year for which global temperature anomaly (temp_anomaly) data are available?
temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  .$year %>%
  min()

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  .$year %>%
  max()

#How many degrees Celsius has temperature increased over the date range? Compare the temperatures in the most recent year versus the oldest year.
temp_2018 <-temp_carbon %>%
  filter(year == 2018) %>%
  summarize(temp_anomaly)
temp_2018

temp_1880 <- temp_carbon %>%
  filter(year == 1880) %>%
  summarize(temp_anomaly)
temp_1880

temp_2018 - temp_1880

#Question 4
#Create a time series line plot of the temperature anomaly. Only include years where temperatures are reported. Save this plot to the object p.
#Which command adds a blue horizontal line indicating the 20th century mean temperature?
p <- temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(y = temp_anomaly, x = year)) +
  geom_line()
p + geom_hline(aes(yintercept = 0), col = "blue")

#Question 5 Change the y-axis label to be "Temperature anomaly (degrees C)". Add a title, "Temperature anomaly relative to 20th century mean,
#1880-2018". Also add a text layer to the plot: the x-coordinate should be 2000, the y-coordinate should be 0.05, the text should be 
#"20th century mean", and the text color should be blue.  
p + ylab("Temperature anomaly (degrees C)") +
  ggtitle("Temperature anomaly relative to 20th century mean, 1880-2018") +
  geom_text(aes(x = 2000, y = 0.05, label = "20th century mean"), col = "blue")

#Question 6
#Add layers to the previous plot to include line graphs of the temperature anomaly in the ocean (ocean_anomaly) and on land (land_anomaly).
#Assign different colors to the lines
head(temp_carbon)

temp_carbon %>%
  filter(!is.na(temp_anomaly)) %>%
  ggplot(aes(year)) +
  geom_line(aes(year, ocean_anomaly, colour = "ocean")) +
  geom_line(aes(year, land_anomaly, colour = "land")) +
  geom_line(aes(year, temp_anomaly, colour = "temp")) +
  geom_hline(aes(yintercept = 0), col = "blue")

#Question 7
greenhouse_gases %>%
  ggplot(aes(y = concentration, x = year)) +
  geom_line() +
  facet_grid(gas~., scales = "free") +
  geom_vline(aes(xintercept = 1850)) +
  ylab("Concentration (ch4/n2o ppb, co2 ppm)") +
  ggtitle("Atmospheric greenhouse gas concentration by year, 0-2000")

#Question 9
#Make a time series line plot of carbon emissions (carbon_emissions) from the temp_carbon dataset. 
#The y-axis is metric tons of carbon emitted per year.
head(temp_carbon)

temp_carbon %>%
  ggplot(aes(y = carbon_emissions, x = year)) +
  geom_vline(aes(xintercept = 1970)) +
  geom_line()

#Question 11
head(historic_co2)
historic_co2 

co2_time <- historic_co2 %>%
  ggplot(aes(y = co2, x = year, colour = "source"))+
  geom_line()
co2_time

#Question 12
#Change the x-axis limits

co2_time +
  xlim(-800000, -775000)

co2_time +
  xlim(-375000, -330000)

co2_time +
  xlim(-140000, -120000)

co2_time +
  xlim(-3000, 2018)

