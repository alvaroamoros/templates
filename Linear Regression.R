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


#### Section 2: Linear Models ####
# Confounding
library(tidyverse)
library(Lahman)

# find regression line for predicting runs from BBs
bb_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(BB_per_game = BB/G, R_per_game = R/G) %>% 
  lm(R_per_game ~ BB_per_game, data = .) %>%
  .$coef %>%.[2]

bb_slope

# compute regression line for predicting runs from singles
singles_slope <- Teams %>% 
  filter(yearID %in% 1961:2001 ) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G, R_per_game = R/G) %>%
  lm(R_per_game ~ Singles_per_game, data = .) %>%
  .$coef  %>%
  .[2]
singles_slope

Teams %>% 
  filter(yearID %in% 1961:2001 ) %>% 
  mutate(Singles = (H-HR-X2B-X3B)/G, BB = BB/G, HR = HR/G) %>%  
  summarize(cor(BB, HR), cor(Singles, HR), cor(BB,Singles))

# Stratification and Multivariate Regression

# stratify HR per game to nearest 10, filter out strata with few points
dat <- Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata = round(HR / G, 1),
         BB_per_game = BB / G,
         R_per_game = R / G) %>%
  filter(HR_strata > 0.4 & HR_strata <= 1.2)
head(dat)


 # scatterplot for each HR stratum
dat %>%
  ggplot(aes(BB_per_game, R_per_game)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat %>%
  group_by(HR_strata) %>%
  summarize(slope = cor(BB_per_game, R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(BB_strata = round(BB/G, 1), 
         HR_per_game = HR / G,
         R_per_game = R / G) %>%
  filter(BB_strata >= 2.8 & BB_strata <=3.9) 

# scatterplot for each BB stratum
dat %>% ggplot(aes(HR_per_game, R_per_game)) +  
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap( ~ BB_strata)

# slope of regression line after stratifying by BB
dat %>%  
  group_by(BB_strata) %>%
  summarize(slope = cor(HR_per_game, R_per_game)*sd(R_per_game)/sd(HR_per_game)) 


# Least Squares Estimates (LSE)
# For regression, we aim to find the coefficient values that minimize the distance of the fitted model to the data.
# Residual sum of squares (RSS) measures the distance between the true value and the predicted value given by the regression line. The values that minimize the RSS are called the least squares estimates (LSE).
# We can use partial derivatives to get the values for  β0  and  β1  in Galton's data.

  # Compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender == "male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father, childHeight) %>%
  rename(son = childHeight)
rss <- function(beta0, beta1, data){
  resid <- galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# plot RSS as a function of beta1 when beta0=25
beta1 = seq(0, 1, len=nrow(galton_heights))

results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 25))

results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss))

# The lm Function
  # fit regression line to predict son's height from father's height
  # fit regression line to predict son's height from father's height
  fit <- lm(son ~ father, data = galton_heights)
  fit
  
  # summary statistics
  summary(fit)
  
# LSE are Random Variables
  
  # Monte Carlo simulation
  B <- 1000
  N <- 50
  lse <- replicate(N, {
    sample_n(galton_heights, N, replace = TRUE) %>%
      lm(son ~ father, data = .) %>%
      .$coef
  })
  
    lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 
lse  

# Plot the distribution of beta_0 and beta_1

library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# summary statistics
sample_n(galton_heights, N, replace = TRUE) %>%
  lm(son ~ father, data = .) %>%
  summary %>%
  .$coef
coef


lse %>% summarize(se_0 = sd(beta_0), se_1 = sd(beta_1))

# Advanced Note on LSE
  # Although interpretation is not straight-forward, it is also useful to know that the LSE can be strongly correlated, which can be seen using this code:
  lse %>% summarise(cor(beta_0, beta_1))

  # However, the correlation depends on how the predictors are defined or transformed.
  # Here we standardize the father heights, which changes  xi  to  xi−x¯ .
  B <- 1000
  N <- 50
  lse <- replicate(B, {
    sample_n(galton_heights, N, replace = TRUE) %>%
      mutate(father = father - mean(father)) %>%
      lm(son ~ father, data = .) %>% .$coef 
  })
  cor(lse[1,], lse[2,])  

#Predicted Variables are Random Variables  

    # Predicted Variables are Random Variables
    galton_heights %>% ggplot(aes(son, father)) +
    geom_point() +
    geom_smooth(method = "lm")
    
    # predict Y directly
    fit <- galton_heights %>% lm(son ~ father, data = .) 
    Y_hat <- predict(fit, se.fit = TRUE)
    names(Y_hat)  


# Assessment: Least Squares Estimates, part 1
    
  beta1 = seq(0, 1, len=nrow(galton_heights))
    results <- data.frame(beta1 = beta1,
                            rss = sapply(beta1, rss, beta0 = 25))
    results %>% ggplot(aes(beta1, rss)) + geom_line() + 
      geom_line(aes(beta1, rss), col=2)

  
# In a model for sons’ heights vs fathers’ heights, what is the least squares estimate (LSE) for  β1  if we assume  β^0  is 36?  
  beta1 = seq(0, 1, len=nrow(galton_heights))
  results <- data.frame(beta1 = beta1,
                        rss = sapply(beta1, rss, beta0 = 36))
  results %>% ggplot(aes(beta1, rss)) + geom_line() + 
    geom_line(aes(beta1, rss), col=2)
  
  
# QUestion 3 
# Filter the Teams data frame to the years 1961-2001. Run a linear model in R predicting the number of runs per game 
# based on both the number of bases on balls per game and the number of home runs per game.
x <-
  Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game = R/G,
         BB_per_game = BB/G,
         HR_per_game = HR/G)
x <- data.frame(x)



y <- lm(R_per_game ~ BB_per_game + HR_per_game, data = x ) 
summary(y)


# Question 4
# What does the central limit theorem tell us about the variables beta_0 and beta_1?

B <- 1000
N <- 100
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% .$coef 
})

lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 


Beta_0 <-lse %>%
  select(beta_0) %>%
  ggplot(aes(beta_0)) +
  geom_histogram()

Beta_1 <-lse %>%
  select(beta_1) %>%
  ggplot(aes(beta_1)) +
  geom_histogram()

grid.arrange(Beta_0, Beta_1, ncol = 2)


# Question 5
# predictions and confidence intervals for our linear model of sons’ heights
galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")


model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))

# Assessment: Least Squares Estimates, part 2
set.seed(1989, sample.kind="Rounding")
library(HistData)
data("GaltonFamilies")
options(digits = 3)    

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)

# Question 7
# Fit a linear regression model predicting the mothers' heights using daughters' heights.
head(female_heights)

model <- lm(mother ~ daughter, data = female_heights )
model
# Question 8
# What is the predicted height of the first mother in the dataset?
predictions <- predict(model)
data <- data.frame(predictions)
data
female_heights$mother

# Question 9
library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
head(bat_02)

# Now compute a similar table but with rates computed over 1999-2001
# Then calculate the average single rate (mean_singles) and average BB rate (mean_bb) per player over those three seasons.
bat_99_01 <- Batting %>%
  group_by(playerID) %>% 
  mutate(pa = AB + BB, singles = sum(H - X2B - X3B - HR)/sum(pa), bb = sum(BB)/sum(pa)) %>% 
  select(playerID, singles, bb)
head(bat_99_01)


bat_99_01 <- bat_99_01  %>%
  group_by(playerID) %>%
  mutate(mean_singles = mean(singles),
         mean_bb = mean(bb)) %>%
  ungroup()
  

  # How many players had a single rate mean_singles of greater than 0.2
  
  bat_99_01 %>%
    filter(mean_singles > 0.2) %>%
    group_by(playerID) %>%
    tally() %>%
    nrow()

  # How many players had a BB rate mean_bb of greater than 0.2 per plate appearance?
  
  bat_99_01 %>%
    filter(mean_bb > 0.2) %>%
    group_by(playerID) %>%
    tally() %>%
    nrow()

# QUestion 10
# Use inner_join() to combine the bat_02 table with the table of 1999-2001 
  head(bat_02)
  head(bat_99_01)
  
J_table <- inner_join(bat_02, bat_99_01, by = "playerID")
J_table  

  # What is the correlation between 2002 singles rates and 1999-2001 average singles rates?
  cor(J_table$singles.x, J_table$mean_singles)

  # What is the correlation between 2002 BB rates and 1999-2001 average BB rates?
  cor(J_table$bb.x, J_table$mean_bb)
  
# Question 11
# Make scatterplots of mean_singles versus singles and mean_bb versus bb
head(J_table)

J_table %>% 
  ggplot(aes(mean_singles, singles.x)) +
  geom_point()

J_table %>% 
  ggplot(aes(mean_bb, bb.x)) +
  geom_point()

# Question 12
# Fit a linear model to predict 2002 singles given 1999-2001 mean_singles.


lm(singles.x ~ mean_singles, data = J_table)

lm(bb.x ~ mean_bb, data = J_table)

J_table %>%
group_by(playerID) %>% mutate(pa = AB + BB, singles = sum(H - X2B - X3B - HR)/sum(pa), bb = sum(BB)/sum(pa)) 


# 2.3: Tibbles, do, and broom
# Advanced dplyr: Tibbles
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .) %>%
  .$coef

# inspect a grouped tibble
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

# subsetting a data frame sometimes generates vectors
class(Teams[,20])

# subsetting a tibble always generates tibbles
class(as_tibble(Teams[,20]))

# pulling a vector out of a tibble
class(as_tibble(Teams)$HR)

# create a tibble with complex objects
tibble(id = c(1, 2, 3), func = c(mean, median, sd))


# do 
  # The do() function serves as a bridge between R functions, such as lm(), and the tidyverse.
  # We have to specify a column when using the do() function, otherwise we will get an error.
  # If the data frame being returned has more than one row, the rows will be concatenated appropriately.

# use do to fit a regression line to each HR stratum
dat %>%  
  group_by(HR) %>%
  do(fit = lm(R ~ BB, data = .))

# using do without a column name gives an error
dat %>%
  group_by(HR) %>%
  do(lm(R ~ BB, data = .))


# define a function to extract slope from lm
get_slope <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(slope = fit$coefficients[2],
             se = summary(fit)$coefficient[2,2]
             )
}


# return the desired data frame
dat %>%
group_by(HR) %>%
  do(get_slope(.))

# not the desired output: a column containing data frames
dat %>%
  group_by(HR) %>%
  do(slope = get_slope(.))

# data frames with multiple rows will be concatenated appropriately
get_lse <- function(data){
  fit <- lm(R ~ BB, data = data)
  data.frame(term = names(fit$coefficients),
             estimate = fit$coefficients, 
             se = summary(fit)$coefficient[,2])
}
dat %>%  
  group_by(HR) %>%
  do(get_lse(.))

# broom

library(broom)
fit <- lm(R ~ BB, data = dat)
tidy(fit)
