##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

library(ggplot2)
library(scales)
library(ggthemes)
library(data.table)
library(reshape2)
library(tidyverse)
library(caret)
library(readr)
library(lubridate)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 4.0 or later:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
                                           title = as.character(title),
                                           genres = as.character(genres))


movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)


##########################################################
##########################################################

      ### DATA MANIPULATION ###

# Add date and year columns
edx <- edx %>%
  mutate(date = as_datetime(timestamp))  %>%
  mutate(review_year = year(date))


    ### DATA EXPLORATION ###

edx %>%
  summarise(n_users = n_distinct(userId),
            n_movies = n_distinct(movieId),
            n_ratings = nrow(movielens),
            avg_rating = mean(as.numeric(rating)),
            sd_rating = sd(as.numeric(rating)),
            max_raing = max(rating),
            min_rating = min(rating))


# RATINGS
# N of votes per rating
edx %>% group_by(rating) %>% 
  summarise(count=n()) %>%
  ggplot(aes(x=rating, y=count)) + 
  geom_line() +
  geom_point() +
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  ggtitle("Rating Distribution", subtitle = "Higher ratings are prevalent.") + 
  xlab("Rating") +
  ylab("Count") # Higher ratings are more comon as well as integers

# USERS 
# N of votes per user
edx %>% 
  group_by(userId) %>%
  summarise(n=n()) %>%
  ggplot(aes(x = n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of Users", 
          subtitle="The distribution is right skewed.") +
  xlab("Number of Ratings") +
  ylab("Number of Users") + 
  scale_y_continuous(labels = comma) # Skeewed distribution

# MOVIES
#N of vots per movie
edx %>% group_by(movieId) %>%
  summarise(n=n()) %>%
  ggplot(aes(n)) +
  geom_histogram(color = "white") +
  scale_x_log10() + 
  ggtitle("Distribution of Movies", 
          subtitle = "The distribution is almost symetric.") +
  xlab("Number of Ratings") +
  ylab("Number of Movies") # Close to normal

# REVIEW_YEAR
# N of votes per year
edx %>% 
  ggplot(aes(x=review_year)) +
  geom_histogram(color = "white") + 
  ggtitle("Rating Distribution Per Year") +
  xlab("Year") +
  ylab("Number of Ratings") +
  scale_y_continuous(labels = comma) 

# User x Movie matrix
users <- sample(unique(edx$userId), 100)
edx %>% filter(userId %in% users) %>%
  select(userId, movieId, rating) %>%
  mutate(rating = 1) %>%
  spread(movieId, rating) %>% 
  select(sample(ncol(.), 100)) %>% 
  as.matrix() %>% t(.) %>%
  image(1:100, 1:100,. , xlab="Movies", ylab="Users")
abline(h=0:100+0.5, v=0:100+0.5, col = "grey")
title("User x Movie Matrix")


    ### FIRST MODELS ###

# Create a smaller test set for time optimization

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2, 
                                  list = FALSE)

train_set <- edx[-test_index,]
test_set <- edx[test_index,]


test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

# the function for the RMSE
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# NAIVE MODEL
mu_hat <- mean(train_set$rating)

naive_rmse <- RMSE(test_set$rating, mu_hat)
naive_rmse

rmse_results <- tibble(method = "Just the average", RMSE = naive_rmse)
rmse_results

# MOVIE EFFECT
#Calculate the mean vote for each movie
movie_agvs <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i = mean(rating - mu_hat))

qplot(b_i, data = movie_agvs, bins = 30, color = I("black")) # squewed to the left

predicted_ratings <- mu_hat + test_set %>%
  left_join(movie_agvs, by = "movieId") %>%
  pull(b_i)

movie_effect <- RMSE(predicted_ratings, test_set$rating)
movie_effect
# USER EFFECT 
train_set %>% 
  group_by(userId) %>%
  filter(n() >= 10) %>%
  summarise(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) +
  geom_histogram(bins = 30, color = "black") # Almost perfectly normal distribution
           
  
avg_users <- train_set %>%
  left_join(movie_agvs, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = mean(rating - mu_hat - b_i))

predicted_ratings <- test_set %>%
  left_join(movie_agvs, by = "movieId") %>%
  left_join(avg_users, by = "userId") %>%
  mutate(prediction = mu_hat + b_i + b_u) %>%
  pull(prediction)

user_movie_effect <- RMSE(predicted_ratings, test_set$rating)  
user_movie_effect

# REGULARIZTION (We wanto to obtain an optimal penalization for users and movies with few rating as they have high standard errors)
lambdas <- seq(0, 10, 0.5)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_set$rating)
  
  b_i <- train_set %>%
    group_by(movieId) %>%
    summarise(b_i = sum(rating - mu) / (n() + l))
  
  b_u <- train_set %>%
    left_join(b_i, by = "movieId") %>%
    group_by(userId) %>%
    summarise(b_u = sum(rating - mu - b_i) / (n() + l))
  
  predicted_ratings <- test_set %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(predictions = mu + b_i + b_u) %>%
    .$predictions
  
  return(RMSE(test_set$rating, predicted_ratings))
  
  })

qplot(lambdas, rmses)  

lambdas[which.min(rmses)]


# MATRIX FACTORIZATION

# recon system library
if(!require(recosystem)) 
  install.packages("recosystem", repos = "http://cran.us.r-project.org")
library(recosystem)

train_data <-  with(train_set, data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))
test_data  <-  with(test_set,  data_memory(user_index = userId, 
                                           item_index = movieId, 
                                           rating     = rating))

r <- recosystem::Reco()

# optimized parameters
opts <- r$tune(train_data, opts = list(dim = c(10, 20, 30), 
                                       lrate = c(0.1, 0.2),
                                       costp_l2 = c(0.01, 0.1), 
                                       costq_l2 = c(0.01, 0.1),
                                       nthread  = 4, niter = 10))

# train the algorithm 
r$train(train_data, opts = c(opts$min, nthread = 4, niter = 20))

# calculate predicted values
y_hat_reco <- r$predict(test_data, out_memory())
RMSE(test_set$rating, y_hat_reco)


write.table(movielens)