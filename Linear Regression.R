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
