# Machine Learning 

# Assessment: Programming Skills


# Caret package, training and test sets, and overall accuracy
library(tidyverse)
library(caret)
library(dslabs)
data(heights)

# Predict Sex on Height

# define the outcome and predictors
y <- heights$sex
x <- heights$height


# generate training and test sets
set.seed(2, sample.kind = "Rounding")

test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
test_set <- heights[test_index, ]
train_set <- heights[-test_index, ]

# guess the outcome
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE)
y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE) %>% 
  factor(levels = levels(test_set$sex))

# compute accuracy
mean(y_hat == test_set$sex)
  
  # We search menas and SDs to improve our prediction
  heights %>% group_by(sex) %>% summarise(mean(height), sd(height))
  69.3 - (3.61*2)
  
  y_hat <- ifelse(x > 62, "Male", "Female") %>% factor(levels = levels(test_set$sex)) 
  mean(y == y_hat)

# examine the accuracy of 10 cutoffs
  cutoff <- seq(61, 70)
  accuracy <- map_dbl(cutoff, function(x){
    y_hat <- ifelse(train_set$height > x, "Male", "Female") %>% 
      factor(levels = levels(test_set$sex))
    mean(y_hat == train_set$sex)
  })
accuracy
max(accuracy)

data.frame(cutoff, accuracy) %>%
  ggplot(aes(cutoff, accuracy)) +
  geom_point() +
  geom_line()

best_cutoff <- cutoff[which.max(accuracy)]
best_cutoff

y_hat <- ifelse(best_cutoff > x, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == test_set$sex)


y_hat <- ifelse(test_set$height > best_cutoff, "Male", "Female") %>% 
  factor(levels = levels(test_set$sex))
y_hat <- factor(y_hat)
mean(y_hat == test_set$sex)


# Machine Learning Basics
mnist <- read_mnist()
ncol(mnist$train$images)

# Confusion matrix
# A general improvement to using overall accuracy is to study sensitivity and specificity separately. Sensitivity, 
#also known as the true positive rate or recall, is the proportion of actual positive outcomes correctly identified as such. 
#Specificity, also known as the true negative rate, is the proportion of actual negative outcomes that are correctly identified as such.
test_set %>%
  mutate(y_hat = y_hat) %>%
  group_by(sex) %>%
  summarise(accuracy = mean( y_hat == sex))



# tabulate each combination of prediction and actual value
table(predicted = y_hat, actual = test_set$sex)
mean(y == "Male")

confusionMatrix(data = y_hat, reference = test_set$sex)

# Balanced accuracy and F1 score
cutoff <- seq(61, 70)

F_1 <- map_dbl(cutoff, function(x){
  y_hat <- ifelse(train_set$height > x, "Male", "Female") %>%
    factor(levels = levels(test_set$sex))
  F_meas(data = y_hat, reference = factor(train_set$sex))
})

data.frame(F_1, cutoff) %>%
  ggplot(aes(cutoff, F_1)) +
  geom_point() +
  geom_line()

best_cutoff <- cutoff[which.max(F_1)]

y_hat <- ifelse(train_set$height > best_cutoff, "Male", "Female") %>%
  factor(levels = levels(test_set$sex))

F_meas(data = y_hat, reference = train_set$sex)

sensitivity(data = y_hat, reference = train_set$sex)
specificity(data = y_hat, reference =  train_set$sex)
mean(y == y_hat)


# ROC and precision-recall curves
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
p <- 0.9
n = length(test_index)

y_hat <- sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1-p)) %>%
  factor(levels = levels(test_set$sex))
mean(y_hat == y)


# ROC curve. Sensitivity (TPR) versus 1 - specificity or the false positive rate (FPR).
probs <- seq(0, 1, length.out = 10)

guessing <- map_df(probs, function(p){
  y_hat <- 
    sample(c("Male", "Female"), n, replace = TRUE, prob = c(p, 1-p)) %>%
    factor(levels = c("Female", "Male"))
  list(method = "Guessing",
       FPR = 1 - specificity(y_hat, test_set$sex),
       TPR = specificity(y_hat, test_set$sex))
})
guessing

guessing %>% qplot(FPR, TPR, data =., xlab = "False Positive Rate", ylab = "True Positive Rate")


cutoffs <- c(50, seq(60, 75), 80)

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
})
height_cutoff

height_cutoff %>% qplot(FPR, TPR, data = ., xlab = "FPS", ylab = "TPR")

# plot both curves together
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(FPR, TPR, color = method)) +
  geom_line() +
  geom_point() +
  xlab("FPR") +
  ylab("TRP")

library(ggrepel)
map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       cutoff = x, 
       FPR = 1-specificity(y_hat, test_set$sex),
       TPR = sensitivity(y_hat, test_set$sex))
}) %>%
  ggplot(aes(FPR, TPR, label = cutoff)) +
  geom_line() +
  geom_point() +
  geom_text_repel(nudge_x = 0.01, nudge_y = -0.01)

# plot precision against recall(TPR)

cutoffs <- c(50, seq(60, 75), 80)

guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), 
                  replace = TRUE, prob=c(p, 1-p)) %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Guess",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Female", "Male"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, test_set$sex),
       precision = precision(y_hat, test_set$sex))
})

bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

# If we change male to positive and female to negative 
guessing <- map_df(probs, function(p){
  y_hat <- sample(c("Male", "Female"), length(test_index), replace = TRUE, 
                  prob=c(p, 1-p)) %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Guess",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})

height_cutoff <- map_df(cutoffs, function(x){
  y_hat <- ifelse(test_set$height > x, "Male", "Female") %>% 
    factor(levels = c("Male", "Female"))
  list(method = "Height cutoff",
       recall = sensitivity(y_hat, relevel(test_set$sex, "Male", "Female")),
       precision = precision(y_hat, relevel(test_set$sex, "Male", "Female")))
})
bind_rows(guessing, height_cutoff) %>%
  ggplot(aes(recall, precision, color = method)) +
  geom_line() +
  geom_point()

# Comprehension Check: Practice with Machine Learning, Part 1
library(dslabs)
library(dplyr)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# Q1 - What proportion of the inclass group is female? What proportion of the online group is female?
head(dat)

dat %>% group_by(type) %>% summarize(prop_female = mean(sex == "Female"))

# Q2 - Assume that for each class type the students are either all male or all female, based on the most prevalent sex in each class type you calculated in Q1.
# Report the accuracy of your prediction of sex based on type
(0.667 + (1-0.378)) / 2

y_hat <- ifelse(x == "online", "Male", "Female") %>% 
  factor(levels = levels(y))
mean(y_hat==y)
y_hat

# Q3 - Write a line of code using the table() function to show the confusion matrix between y_hat and y. 
# Use the exact format function(a, b) for your answer and do not name the columns and rows.
table(y_hat, y)

# Q4 - What is the sensitivity of this prediction
sensitivity(data = y_hat, reference = y)

#Q5 - What is the specificity of this prediction?
specificity(y_hat, y)

#Q 6 - What is the prevalence
cm <- confusionMatrix(data = y_hat, reference = y)
cm$byClass[c("Sensitivity","Specificity", "Prevalence")]

# Comprehension Check: Practice with Machine Learning, Part 2
library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species


#Q7 - Create an even split of the data into train and test partitions using createDataPartition()
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)

#Q9 - Figure out the singular feature in the dataset that yields the greatest overall accuracy when predicting species.
# Using only the train iris dataset, for each feature, perform a simple search to find the cutoff that produces the highest accuracy, 
# predicting virginica if greater than the cutoff and versicolor otherwise. 
# Use the seq function over the range of each feature by intervals of 0.1 for this search.
head(iris)
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
train_set <- iris[test_index,]
test_set <- iris[-test_index,]

# Sepal.Length
set.seed(2, sample.kind="Rounding")

cutoffs <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), by = 0.1)
predict_S.P <- map_dbl(cutoffs, function(x){
  y_hat <- ifelse(train_set$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test_set$Species))
  mean(y_hat == train_set$Species)
})
data.frame(cutoffs, predict_S.P) %>%
  ggplot(aes(cutoffs, predict_S.P)) +
  geom_line()
max(predict_S.P)


# Sepal.Width 
set.seed(2, sample.kind="Rounding")

cutoffs <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), by = 0.1)
predict_S.W <- map_dbl(cutoffs, function(x){
  y_hat <- ifelse(train_set$Sepal.Width >  x ,"virginica", "versicolor") 
    factor(levels = levels(test_set$Species))
  mean(y_hat == train_set$Species)
  })
data.frame(cutoffs, predict_S.W) %>%
  ggplot(aes(cutoffs, predict_S.W)) +
  geom_line()
max(predict_S.W)

# Petal.Length
cutoffs <- seq(min(iris$Petal.Length), max(iris$Petal.Length), by = 0.1)
cutoffs
predict_P.L <- map_dbl(cutoffs, function(x){
  y_hat <- ifelse(train_set$Petal.Length >  x ,"virginica", "versicolor") 
  factor(levels = levels(test_set$Species))
  mean(y_hat == train_set$Species)
})
data.frame(cutoffs, predict_P.L) %>%
  ggplot(aes(cutoffs, predict_P.L)) +
  geom_line()
max(predict_P.L)

# Petal.Width
set.seed(2, sample.kind="Rounding")

cutoffs <- seq(min(train_set$Petal.Width), max(train_set$Petal.Width), by = 0.1)
predict_P.W <- map_dbl(cutoffs, function(x){
  y_hat <- ifelse(train_set$Petal.Width >  x ,"virginica", "versicolor") 
  factor(levels = levels(train_set$Species))
  mean(y_hat == test_set$Species)
})
data.frame(cutoffs, predict_P.W) %>%
  ggplot(aes(cutoffs, predict_P.W)) +
  geom_line()
max(predict_P.W)

# Q10 - Given that we know the test data, we can treat it like we did our training data to see if the same feature with a different cutoff will optimize our predictions.
# Which feature best optimizes our overall accuracy?
# Sepal.Length

set.seed(2, sample.kind="Rounding")

cutoffs <- seq(min(iris$Sepal.Length), max(iris$Sepal.Length), by = 0.1)
predict_S.P <- map_dbl(cutoffs, function(x){
  y_hat <- ifelse(test_set$Sepal.Length > x, "virginica", "versicolor") %>%
    factor(levels = levels(test_set$Species))
  mean(y_hat == test_set$Species)
})
data.frame(cutoffs, predict_S.P) %>%
  ggplot(aes(cutoffs, predict_S.P)) +
  geom_line()
max(predict_S.P)


# Sepal.Width 
set.seed(2, sample.kind="Rounding")

cutoffs <- seq(min(iris$Sepal.Width), max(iris$Sepal.Width), by = 0.1)
predict_S.W <- map_dbl(cutoffs, function(x){
  y_hat <- ifelse(test_set$Sepal.Width >  x ,"virginica", "versicolor") 
  factor(levels = levels(test_set$Species))
  mean(y_hat == test_set$Species)
})
data.frame(cutoffs, predict_S.W) %>%
  ggplot(aes(cutoffs, predict_S.W)) +
  geom_line()
max(predict_S.W)

# Petal.Length
cutoffs <- seq(min(iris$Petal.Length), max(iris$Petal.Length), by = 0.1)
cutoffs
predict_P.L <- map_dbl(cutoffs, function(x){
  y_hat <- ifelse(test_set$Petal.Length >  x ,"virginica", "versicolor") 
  factor(levels = levels(test_set$Species))
  mean(y_hat == test_set$Species)
})
data.frame(cutoffs, predict_P.L) %>%
  ggplot(aes(cutoffs, predict_P.L)) +
  geom_line()
max(predict_P.L)

# Petal.Width
set.seed(2, sample.kind="Rounding")

cutoffs <- seq(min(iris$Petal.Width), max(iris$Petal.Width), by = 0.1)
predict_P.W <- map_dbl(cutoffs, function(x){
  y_hat <- ifelse(test_set$Petal.Width >  x ,"virginica", "versicolor") 
  factor(levels = levels(test_set$Species))
  mean(y_hat == test_set$Species)
})
data.frame(cutoffs, predict_P.W) %>%
  ggplot(aes(cutoffs, predict_P.W)) +
  geom_line()
max(predict_P.W)

# Q 11 - Predict virginica if Petal.Length is greater than the length cutoff OR Petal.Width is greater than the width cutoff, and versicolor otherwise.
set.seed(2, sample.kind="Rounding")
test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
train_set <- iris[test_index,]
test_set <- iris[-test_index,]


cutoffs <- seq(min(train_set$Petal.Length), max(iris$Petal.Length), by = 0.1)
P.L_cutoff <- cutoffs[which.max(predict_P.L)]
P.L_cutoff
cutoffs <- seq(min(train_set$Petal.Width), max(iris$Petal.Width), by = 0.1)
P.W_cutoff <- cutoffs[which.max(predict_P.W)]
P.W_cutoff

y_hat <- ifelse(train_set$Petal.Length > P.L_cutoff | train_set$Petal.Width > P.W_cutoff, "virginica", "versicolor")
mean(y_hat == train_set$Species)

  ##Show anser code 
  library(caret)
  data(iris)
  iris <- iris[-which(iris$Species=='setosa'),]
  y <- iris$Species
  
  plot(iris,pch=21,bg=iris$Species)
  
  # set.seed(2) # if using R 3.5 or earlier
  set.seed(2, sample.kind="Rounding") # if using R 3.6 or later
  test_index <- createDataPartition(y,times=1,p=0.5,list=FALSE)
  test <- iris[test_index,]
  train <- iris[-test_index,]
  
  petalLengthRange <- seq(range(train$Petal.Length)[1],range(train$Petal.Length)[2],by=0.1)
  petalWidthRange <- seq(range(train$Petal.Width)[1],range(train$Petal.Width)[2],by=0.1)
  
  length_predictions <- sapply(petalLengthRange,function(i){
    y_hat <- ifelse(train$Petal.Length>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
  length_cutoff <- petalLengthRange[which.max(length_predictions)] # 4.7
  
  width_predictions <- sapply(petalWidthRange,function(i){
    y_hat <- ifelse(train$Petal.Width>i,'virginica','versicolor')
    mean(y_hat==train$Species)
  })
  width_cutoff <- petalWidthRange[which.max(width_predictions)] # 1.5
  
  y_hat <- ifelse(test$Petal.Length>length_cutoff | test$Petal.Width>width_cutoff,'virginica','versicolor')
  mean(y_hat==test$Species)
  
  
# Comprehension Check: Conditional Probabilities Part 1
# Q1 - 
  # The test is positive 85% of the time when tested on a patient with the disease (high sensitivity):  P(test+|disease)=0.85
  # The test is negative 90% of the time when tested on a healthy patient (high specificity):  P(test−|heathy)=0.90
  # The disease is prevalent in about 2% of the community:  P(disease)=0.02
  # Calculate the probability that you have the disease if the test is positive.
  prevalence <- 0.02
  true_positive <- 0.85
  true_negative <- 0.9
  false_positive <- 0.15
  false_negative <- 0.1

  true_positive * prevalence / (true_positive * prevalence + (1-true_negative) * (1-prevalence))
  
# Q 2,3,4 
set.seed(1, sample.kind = "Rounding") 
disease <- sample(c(0,1), size=1e6, replace=TRUE, prob=c(0.98,0.02))
test <- rep(NA, 1e6)
test[disease==0] <- sample(c(0,1), size=sum(disease==0), replace=TRUE, prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), size=sum(disease==1), replace=TRUE, prob=c(0.15, 0.85))
  
# What is the probability that a test is positive?
mean(test)

# What is the probability that an individual has the disease if the test is negative?
mean(disease[test==0])

# What is the probability that you have the disease if the test is positive?
mean(disease[test==1])

# If a patient's test is positive, by how many times does that increase their risk of having the disease?
mean(disease[test==1])/prevalence

# Comprehension Check: Conditional Probabilities Part 2

# Q6 - Conditional probabilities for being male in the heights dataset. 
library(dslabs)
data("heights")
  
heights %>% 
  mutate(height = round(height)) %>%
  group_by(height) %>%
  summarize(p = mean(sex == "Male")) %>%
  qplot(height, p, data =.)

# Q7 - This time use the quantile  0.1,0.2,…,0.9  and the cut() function to assure each group has the same number of points.
ps <- seq(0, 1, 0.1)

heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

# Q8 - 
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

plot(dat)

# Estimate the conditional expectations and make a plot.
ps <- seq(0, 1, 0.1)

dat %>% mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)


# Linear Regression for Prediction
library(tidyverse)
library(HistData)
library(caret)

galton_heights <- GaltonFamilies %>%
  filter(childNum == 1 & gender == "male") %>%
  select(father, childHeight) %>%
  rename(son = childHeight)

y <- galton_heights$son
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)

train_set <- galton_heights %>% slice(-test_index)
test_set <- galton_heights %>% slice(test_index)

avg <- mean(train_set$son)
avg

fit <- lm(son ~ father, data = train_set)
coef(fit)
y_hat <- fit$coef[1] + fit$coef[2] * test_set$father
mean((y_hat - test_set$son)^2)  

# Predict Function
y_hat <- predict(fit, test_set)
mean((y_hat - test_set$son)^2)


# Comprehension Check: Linear Regression

set.seed(1, sample.kind="Rounding") 
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))
dat

# Question 1

# Build 100 linear models using the data above
# Within a replicate() loop, 
  # (1) partition the dataset into test and training sets with p = 0.5 and using dat$y to generate your indices
  # (2) train a linear model predicting y from x
  # (3) generate predictions on the test set
  # (4) calculate the RMSE of that model
# Report the mean and standard deviation (SD) of the RMSEs from all 100 models.
head(dat)

B <- 100
set.seed(1, sample.kind="Rounding") 
model <- replicate(B, {
    y <- dat$y
    test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
    train_set <- dat %>% slice(-test_index)
    test_set <- dat %>% slice(test_index)
    fit <- lm(y ~ x , data = train_set)
    y_hat <- predict(fit, test_set)
    sqrt(mean((y_hat - test_set$y)^2))
  })
mean(model)
sd(model)

# Question 2
# Write a function that takes a size n, then 
  #(1) builds a dataset using the code provided at the top of Q1 but with n observations instead of 100 and without the set.seed(1)
  #(2) runs the replicate() loop that you wrote to answer Q1, which builds 100 linear models and returns a vector of RMSEs.
  #(3) calculates the mean and standard deviation of the 100 RMSEs.

# Set the seed to 1 (if using R 3.6 or later, use the argument sample.kind="Rounding") 
# and then use sapply() or map() to apply your new function to n <- c(100, 500, 1000, 5000, 10000)

long_model <- function(n){
 
   Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
   dat <- MASS::mvrnorm(n, c(69, 69), Sigma) %>%
     data.frame() %>% setNames(c("x", "y"))
  
    model <- replicate(100, {
     y <- dat$y
     test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
     train_set <- dat %>% slice(-test_index)
     test_set <- dat %>% slice(test_index)
     fit <- lm(y ~ x , data = train_set)
     y_hat <- predict(fit, test_set)
     sqrt(mean((y_hat - test_set$y)^2))
   })
   
    list(c(mean(model), sd(model)))
   
}

set.seed(1, sample.kind = "Rounding")
n <- c(100,500,1000,5000,10000)
predctions <- sapply(n, long_model)
predctions

# Question 4 
# Now repeat the exercise from Q1, this time making the correlation between x and y larger
set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

B <- 100
set.seed(1, sample.kind="Rounding") 
model <- replicate(B, {
  y <- dat$y
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  train_set <- dat %>% slice(-test_index)
  test_set <- dat %>% slice(test_index)
  fit <- lm(y ~ x , data = train_set)
  y_hat <- predict(fit, test_set)
  sqrt(mean((y_hat - test_set$y)^2))
})
mean(model)
sd(model)

# QUestion 6
set.seed(1, sample.kind="Rounding") 
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
dat
cor(dat)

#(1) partition into test and training sets with p = 0.5
#(2) Compare the RMSE when using just x_1, just x_2 and both x_1 and x_2
#(3)Train a single linear model for each (not 100)
set.seed(1, sample.kind="Rounding") 
y <- dat$y
test_index <- createDataPartition(y, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

model1 <- lm(y ~ x_1, data = train_set)
y_hat <- predict(model1, test_set)
RMSE_model1 <- sqrt(mean((y_hat - test_set$y)^2))

model2 <- lm(y ~ x_2, data = train_set)
y_hat <- predict(model2, test_set)
RMSE_model2 <- sqrt(mean((y_hat - test_set$y)^2))

model3 <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(model3, newdata = test_set)
RMSE_model3 <- sqrt(mean((y_hat - test_set$y)^2))

RMSE_model1
RMSE_model2
RMSE_model3


# Question 8 - Repeat the exercise from Q6 but now create an example in which x_1 and x_2 are highly correlated.
set.seed(1, sample.kind="Rounding") 
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
cor(dat)

# (1) Set the seed to 1, then use the caret package to partition into a test and training set of equal size. 
# (2) Compare the RMSE when using just x_1, just x_2, and both x_1 and x_2.
set.seed(1, sample.kind="Rounding") 

y <- dat$y
test_index <- createDataPartition(y, p = 0.5, list = FALSE)
train_set <- dat %>% slice(-test_index)
test_set <- dat %>% slice(test_index)

model1 <- lm(y ~ x_1, data = train_set)
y_hat <- predict(model1, test_set)
RMSE_model1 <- sqrt(mean((y_hat - test_set$y)^2))

model2 <- lm(y ~ x_2, data = train_set)
y_hat <- predict(model2, test_set)
RMSE_model2 <- sqrt(mean((y_hat - test_set$y)^2))

model3 <- lm(y ~ x_1 + x_2, data = train_set)
y_hat <- predict(model3, newdata = test_set)
RMSE_model3 <- sqrt(mean((y_hat - test_set$y)^2))

RMSE_model1
RMSE_model2
RMSE_model3


# Regression for a Categorical Outcome
library(dslabs)
data("heights")
y <- heights$height
set.seed(2, sample.kind = "Rounding")

test_index <- createDataPartition(y, p = 0.1, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)

train_set %>% 
  filter(round(height)==66) %>%
  summarise(y_hat = mean(sex == "Female"))

heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarise(promp = mean(sex == "Female")) %>%
  ggplot(aes(x, promp)) +
  geom_point()

lm_fit <- mutate(train_set, y = as.numeric(sex == "Female")) %>% lm(y ~ height, data = .)
lm_fit

p_hat <- predict(lm_fit, test_set)
p_hat

y_hat <- ifelse(p_hat > 0.5, "Female", "Male") %>%factor()
y_hat

confusionMatrix(y_hat, test_set$sex)

# Logistic Regression
head(heights)

glm_fit <- train_set %>%
  mutate(y = as.numeric(sex == "Female")) %>%
  glm(y ~ height, data = ., family = "binomial")

p_hat_logit <- predict(glm_fit, newdata = test_set, type = "response")

tmp <- heights %>%
  mutate(x = round(height)) %>%
  group_by(x) %>%
  filter(n() >= 10) %>%
  summarize(prop = mean( sex == "Female"))

logistic_curve <- data.frame(x = seq(min(tmp$x), max(tmp$x))) %>%
  mutate(p_hat = plogis(glm_fit$coef[1] + glm_fit$coef[2]*x))
tmp %>% 
  ggplot(aes(x, prop)) +
  geom_point() +
  geom_line(data = logistic_curve, mapping = aes(x, p_hat), lty = 2)

y_hat_logit <- ifelse(p_hat > 0.5, "Female", "Male") %>% factor

confusionMatrix(y_hat_logit, test_set$sex)

# Case Study: 2 or 7
data("mnist_27")

head(mnist_27$train)

mnist_27$train %>%
  ggplot(aes(x_1, x_2, color = y)) +
  geom_point()


fit_glm <- glm(y ~ x_1 + x_2, data=mnist_27$train, family = "binomial")
p_hat_glm <- predict(fit_glm, mnist_27$test)
y_hat_glm <- factor(ifelse(p_hat_glm > 0.5, 7, 2))
confusionMatrix(data = y_hat_glm, reference = mnist_27$test$y)


# acces the real conditional probabilities to compare
head(mnist_27$true_p)

mnist_27$true_p %>% ggplot(aes(x_1, x_2, fill=p)) +
  geom_raster()

mnist_27$true_p %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5), color="black") 

p_hat <- predict(fit_glm, newdata = mnist_27$true_p)
mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot(aes(x_1, x_2,  z=p_hat, fill=p_hat)) +
  geom_raster() +
  scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
  stat_contour(breaks=c(0.5),color="black") 


mnist_27$true_p %>%
  mutate(p_hat = p_hat) %>%
  ggplot() +
  stat_contour(aes(x_1, x_2, z=p_hat), breaks=c(0.5), color="black") +
  geom_point(mapping = aes(x_1, x_2, color=y), data = mnist_27$test)

# Comprehension Check: Logistic Regression
# Question 1
  # Prepare dataset 
set.seed(2, sample.kind="Rounding") 
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
head(dat$train)

dat$train %>% ggplot(aes(x, color = y)) + geom_density()


# Set the seed to 1, then use the make_data() function defined above to generate 25 different datasets with mu_1 <- seq(0, 3, len=25)
# Perform logistic regression on each of the 25 different datasets (predict 1 if p>0.5) 
# plot accuracy (res in the figures) vs mu_1 (delta in the figures).
delta <- seq(0, 3, len=25)

set.seed(1, sample.kind="Rounding") 
make_data <- function(n = 1000, p = 0.5, 
                      mu_0 = 0, mu_1 = 2, 
                      sigma_0 = 1,  sigma_1 = 1){
  
  y <- rbinom(n, 1, p)
  f_0 <- rnorm(n, mu_0, sigma_0)
  f_1 <- rnorm(n, mu_1, sigma_1)
  x <- ifelse(y == 1, f_1, f_0)
  
  test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
  
  list(train = data.frame(x = x, y = as.factor(y)) %>% slice(-test_index),
       test = data.frame(x = x, y = as.factor(y)) %>% slice(test_index))
}
dat <- make_data()
head(dat)
head(dat$train)

res <- sapply(delta, function(d){
  dat <- make_data(mu_1 = d)
  fit <- dat$train %>% glm(y ~ x, family = "binomial", data = .)
  p_hat <- predict(fit, newdata = dat$test)
  y_hat <- ifelse(p_hat >= 0.5, 1, 0) %>% factor(levels = c(0, 1))
  mean(y_hat == dat$test$y)
})
qplot(delta, res)
res

# Introduction to Smoothing
data("polls_2008")
head(polls_2008)

qplot(day, margin, data = polls_2008)


# Bin Smoothing and Kernels
span <- 7
fit <- with(polls_2008, ksmooth(day, margin, x.points = day, kernel = "box", bandwidth = span))

polls_2008 %>% 
  mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point() +
  geom_line(aes(day, smooth), color = "red")

span <- 7
fit <- with(polls_2008, ksmooth(day, margin, x.points = day, kernel = "normal", bandwidth = span))

polls_2008 %>% 
  mutate(smooth = fit$y) %>%
  ggplot(aes(day, margin)) +
  geom_point() +
  geom_line(aes(day, smooth), color = "red")

# Local Weighted Regression (loess)
total_days <- diff(range(polls_2008$day))
total_days
span <- 21/total_days
span

fit <- loess(margin ~ day, degree = 1, span = span, data = polls_2008)
fit$fitted

head(polls_2008)
polls_2008 %>% 
  mutate(fit = fit$fitted) %>%
  ggplot(aes(x= day, y = margin)) +
  geom_point() +
  geom_line(aes(day, fit), color = "red")




# A limitation of the bin smoothing approach is that we need small windows for the approximately constant assumptions to hold 
# which may lead to imprecise estimates of  f(x) . Local weighted regression (loess) permits us to consider larger window sizes.
polls_2008 %>% ggplot(aes(day, margin)) +
  geom_point() + 
  geom_smooth(color="red", span = 0.25, method = "loess", method.args = list(degree=1))

# Comprehension Check: Smoothing
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

  # Prepare data
  fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
  dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
    s <- str_trim(s)
    header_index <- str_which(s, "2015")[1]
    tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
    month <- tmp[1]
    header <- tmp[-1]
    tail_index  <- str_which(s, "Total")
    n <- str_count(s, "\\d+")
    out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
    s[-out] %>%
      str_remove_all("[^\\d\\s]") %>%
      str_trim() %>%
      str_split_fixed("\\s+", n = 6) %>%
      .[,1:5] %>%
      as_data_frame() %>% 
      setNames(c("day", header)) %>%
      mutate(month = month,
             day = as.numeric(day)) %>%
      gather(year, deaths, -c(day, month)) %>%
      mutate(deaths = as.numeric(deaths))
  }) %>%
    mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                          "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
    mutate(date = make_date(year, month, day)) %>%
    dplyr::filter(date <= "2018-05-01")
head(dat)

# Question 1 - Use the loess() function to obtain a smooth estimate of the expected number of deaths as a function of date.
# Make the span about two months long

span <- 60 / as.numeric(diff(range(dat$date)))
fit <- dat %>% mutate(x = as.numeric(date)) %>% loess(deaths ~ x, data = ., span = span, degree = 1)

dat %>% mutate(smooth = predict(fit, as.numeric(date))) %>%
  ggplot() +
  geom_point(aes(date, deaths)) +
  geom_line(aes(date, smooth), lwd = 2, col = "red")
