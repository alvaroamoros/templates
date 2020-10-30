# Data science: Linear Regression
# 10/22/20
rm(list = ls())

# Section 1: Introduction to Regression / 1.1: Baseball as a Motivating Example
library(Lahman)
library(tidyverse)
library(dslabs)
ds_theme_set()


Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR / G, R_per_game = R / G) %>%
  ggplot(aes(HR_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB / G, R_per_game = R / G) %>%
  ggplot(aes(SB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)


Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB / G, R_per_game = R / G) %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)

data("Teams")
head(Teams)


# Filter the Teams data frame to include years from 1961 to 2001. Make a scatterplot of runs per game versus at bats (AB) per game.

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game = AB / G, R_per_game = R / G) %>%
  ggplot(aes(AB_per_game, R_per_game)) +
  geom_point(alpha = 0.5)


# Make a scatterplot of win rate (number of wins per game) versus number of fielding errors (E) per game.
head(Teams)

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(wins_per_game = W / G,
         fielding_erros = E / G) %>%
  ggplot(aes(wins_per_game, fielding_erros)) +
  geom_point(alpha = 0.5)

#  Make a scatterplot of triples (X3B) per game versus doubles (X2B) per game.

Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(triples = X3B / G,
         doubles = X2B / G) %>%
  ggplot(aes(triples, doubles)) +
  geom_point(alpha = 0.5)

# Correlation
library(tidyverse)
library(HistData)
data("GaltonFamilies")
head(GaltonFamilies)

galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

# means and standard deviations
galton_heights %>%
  summarise(mean(father), sd(father), mean(son),sd(son))

#Scatter plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(aplpha = 0.5)

# Correlation Coefficient
rho <- mean(scale(x) * scale(y))

galton_heights %>% summarise(r = cor(father, son)) %>% pull(r)


# Sample Correlation is a Random Variable

# compute sample correlation
R <- sample_n(galton_heights, 25, replace = TRUE) %>%
  summarise(r = cor(father, son))
R

B <- 1000
N <-50
R <- replicate(B, {
  sample_n(galton_heights, 25, replace = TRUE) %>%
    summarise(r = cor(son, father)) %>%
    pull(r)
})
mean(R)
sd(R)

qplot(R, geom = "histogram", binwidth = 0.05, color = I("black"))

data.frame(R) %>%
  ggplot(aes(sample = R)) +
  stat_qq() +
  geom_abline(intercept = mean(R), slope = sqrt((1-mean(R)^2)/(N-2)))

# Load the Lahman library. Filter the Teams data frame to include years from 1961 to 2001.
# What is the correlation coefficient between number of runs per game and number of at bats per game?
head(Teams)
x <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(runs_per_game = R / G ,
         bats_per_game = AB / G)
cor(x$runs_per_game , x$bats_per_game)

  # What is the correlation coefficient between win rate (number of wins per game) and number of errors per game?
  x <- x %>%
    mutate(win_rate = W / G,
           error_rate = E / G)
cor(x$win_rate, x$error_rate)

  # What is the correlation coefficient between doubles (X2B) per game and triples (X3B) per game?
  x <- x %>%
    mutate(double_per_game = X2B / G,
           triple_per_game = X3B / G)
  cor(x$double_per_game, x$triple_per_game)

# Anscombe's Quartet/Stratification

# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father == 72)  
sum(galton_heights$father == 72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father) == 72) %>%
  summarise(avg = mean(son)) %>%
  pull
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>%
  mutate(strat_father = factor(round(father))) %>%
  ggplot(aes(strat_father, son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>%
  mutate(father = round(father)) %>%
  group_by(father) %>%
  summarise(son_conditiona_avg = mean(son)) %>%
  ggplot(aes(father, son_conditiona_avg)) +
  geom_point()

# calculate values to plot regression line on original data
mu_x <- mean(galton_heights$father)
mu_y <- mean(galton_heights$son)
s_x <- sd(galton_heights$father)
s_y <- sd(galton_heights$son)
r <- cor(galton_heights$father, galton_heights$son)
m <- r * s_y / s_x
b <- mu_y - m*mu_x

# add regression line to plot
galton_heights %>%
  ggplot(aes(father, son)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = b, slope = m)

# Bivariate Normal Distribution

  # When a pair of random variables are approximated by the bivariate normal distribution, scatterplots look like ovals. They can be thin (high correlation) or circle-shaped (no correlation).
  # When two variables follow a bivariate normal distribution, computing the regression line is equivalent to computing conditional expectations.
  # We can obtain a much more stable estimate of the conditional expectation by finding the regression line and using it to make predictions.

galton_heights %>%
  mutate(z_father = round((father - mean(father)) / sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +  
  stat_qq(aes(sample = son)) +
  facet_wrap( ~ z_father)

# Variance Explained

  # Conditioning on a random variable X can help to reduce variance of response variable Y.
  # The standard deviation of the conditional distribution is  SD(Y∣X=x)=σy1−ρ2−−−−−√ , which is smaller than the standard deviation without conditioning  σy .
  # Because variance is the standard deviation squared, the variance of the conditional distribution is  σ2y(1−ρ2) .
  # In the statement "X explains such and such percent of the variability," the percent value refers to the variance. The variance decreases by  ρ2  percent.
  # The “variance explained” statement only makes sense when the data is approximated by a bivariate normal distribution.

# Question 1 
# Suppose the correlation between father and son’s height is 0.5, the standard deviation of fathers’ heights is 2 inches, and the standard deviation of sons’ heights is 3 inches.
# Given a one inch increase in a father’s height, what is the predicted change in the son’s height?
r <- 0.5
s_f <- 2
s_s <- 3
r * s_s/s_f

# Assessment: Stratification and Variance Explained, Part 2
set.seed(1989, sample.kind="Rounding") #if you are using R 3.6 or later
library(HistData)
data("GaltonFamilies")

female_heights <- GaltonFamilies%>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
female_heights

# Question 8
# Calculate the mean and standard deviation of mothers' heights, the mean and standard deviation of daughters' heights, 
# and the correlaton coefficient between mother and daughter heights.
mu_x <- female_heights$mother %>%
  mean()
mu_x

mu_y <- female_heights$daughter %>% 
  mean()
mu_y

s_x <- female_heights$mother %>%
  sd()
s_x

s_y <- female_heights$daughter %>% 
  sd()
s_y

p <- cor(female_heights$mother, female_heights$daughter)

# Question 9
# Slope of regression line predicting daughters' height from mothers' heights
m <- p * (s_y / s_x)
m

# Intercept of regression line predicting daughters' height from mothers' heights
intercept_d_m <-  mu_y - m*mu_x
intercept_d_m

# Change in daughter's height in inches given a 1 inch increase in the mother's height
m*1

# What percent of the variability in daughter heights is explained by the mother's height?
p^2 *100

# QUestion 11
# A mother has a height of 60 inches.
# What is the conditional expected value of her daughter's height given the mother's height?
intercept_d_m + m*60
