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


# Question 2 - Plot smooth estimates against day of the year, all on the same plot, but with different colors for each year.
dat %>% 
  mutate(smooth = predict(fit, as.numeric(date)), day = yday(date), year = as.character(year(date))) %>%
  ggplot(aes(day, smooth, col = year)) +
  geom_line(lwd = 2)

# Q3
library(broom)
mnist_27$train %>% glm(y ~ x_2, family = "binomial", data = .) %>% tidy()

qplot(x_2, y, data = mnist_27$train)

# Fit a loess line to the data above and plot the results. What do you observe?

mnist_27$train %>% 
  mutate(y = ifelse(y=="7", 1, 0)) %>%
  ggplot(aes(x_2, y)) + 
  geom_smooth(method = "loess")


# Matrices
library(tidyverse)
library(dslabs)
if(!exists("mnist")) mnist <- read_mnist()

class(mnist$train$images)

x <- mnist$train$images[1:1000,] 
y <- mnist$train$labels[1:1000]
class(x)

# In R, we can extract the dimension of a matrix with the function dim(). 
# We can convert a vector into a matrix using the function as.matrix()
length(x)
head(x)
length(x[,1])

x_1 <- 1:5
x_2 <- 6:10
x_1
x_2
cbind(x_1,x_2)
dim(x)
dim(x_1)
dim(as.matrix(x_1))
dim(x)

# Converting a Vector to a Matrix
my_vector <- 1:15

mat <- matrix(my_vector, 5, 3)
mat

mat_t <- matrix(my_vector, 3, 5, byrow = TRUE)
mat_t

identical(t(mat), mat_t)

matrix(my_vector, 5, 5)

grid <- matrix(x[3,], 28, 28)
grid

image(1:28, 1:28, grid)
image(1:28, 1:28, grid[,28:1])


# Row and Column Summaries and Apply
sums <- rowSums(x)
sums
avg <- rowMeans(x)
avg

data_frame(labels = as.factor(y), row_averages = avg) %>%
  qplot(labels, row_averages, data = ., geom = "boxplot")

  # The apply() function lets you apply any function to a matrix. 
  # The first argument is the matrix, the second is the dimension (1 for rows, 2 for columns), and the third is the function. 
avgs <- apply(x, 1, mean)
avgs

sds <- apply(x, 2, sd)
sds

# Filtering Columns Based on Summaries
  #The operations used to extract columns: x[,c(351,352)].
  #The operations used to extract rows: x[c(2,3),].
library(matrixStats)

sds <- colSds(x)
qplot(sds, bins = "30", color = I("black"))
image(1:28, 1:28, matrix(sds, 28, 28)[, 28:1])

new_x <- x[, colSds(x) > 60]
dim(new_x)

class(x[,1])
dim(x[1,])
class(x[ , 1, drop=FALSE])
dim(x[, 1, drop=FALSE])

# Indexing with Matrices and Binarizing the Data
mat <- matrix(1:15, 5, 3)
mat
as.vector(mat)

qplot(as.vector(x), bins = 30, color = I("black"))

new_x <- x
new_x[new_x < 50] <- 0 
qplot(as.vector(new_x), bins = 30, color = I("black"))

mat <- matrix(1:15, 5 ,3 )
mat[mat < 3] <- 0
mat

mat <- matrix(1:15, 5, 3)
mat[mat > 6 & mat < 12] <- 0
mat

# binarize data
bin_x <- x
bin_x
bin_x[bin_x < 255/2] <- 0
bin_x[bin_x > 255/2] <- 1
bin_x
qplot(as.vector(bin_x), bins = 30, color = I("black"))

bin_X <- (x > 255/2)*1
qplot(as.vector(bin_X), bins = 30, color = I("black"))

# Vectorization for Matrices and Matrix Algebra Operations

#scale each row of a matrix
 (x - rowMeans(x)) / rowSds(x)

#scale each column
 t(t(x) - colMeans(x))

#take each entry of a vector and subtracts it from the corresponding row or column
x_mean_0 <- sweep(x, 2, colMeans(x))
x_mean_0
qplot(as.vector(x_mean_0), bins = 30, color = I("black"))

#divide by the standard deviation
x_mean_0 <- sweep(x, 2, colMeans(x))
x_standardized <- sweep(x_mean_0, 2, colSds(x), FUN = "/")
qplot(as.vector(x_standardized), bins = 100, color = I("black"))


#Q2 - Comprehension Check: Working with Matrices
# Dimension of x
dim(x)
  
# Number of rows of x
nrow(x)
dim(x)[1]  
  
# Number of columns of x
ncol(x)
dim(x)[2]

# Question 6 
#For each observation in the mnist training data, compute the proportion of pixels that are in the grey area, 
#defined as values between 50 and 205 (but not including 50 and 205)
# (To visualize this, you can make a boxplot by digit class.)
x <- mnist$train$images
y <- mnist$train$labels
bin_data <- x
bin_data

bin_data[bin_data > 50 & bin_data < 205] <- 1
bin_data[bin_data > 1] <- 0
bin_data

# What proportion of the 60000*784 pixels in the mnist training data are in the grey area overall, defined as values between 50 and 205
mean(bin_data)



# Section 4: Distance, Knn, Cross-validation, and Generative Models /
# Distance
library(tidyverse)
library(dslabs)


if(!exists("mnist")) mnist <- read_mnist()
set.seed(0, sample.kind = "Rounding")

ind <- which(mnist$train$labels %in% c(2,7)) %>% sample(500)

x <- mnist$train$images[ind,]
y <- mnist$train$labels[ind]
y[1:3]

x_1 <- x[1,]
x_2 <- x[2,]
x_3 <- x[3,]

sqrt(sum((x_1 - x_2)^2))
sqrt(sum((x_2 - x_3)^2))
sqrt(sum((x_1 - x_3)^2))

#compute distance using matrix algebra
sqrt(crossprod(x_1 - x_2))
sqrt(crossprod(x_1 - x_3))

#compute distance between each row
d <- dist(x)
class(d)
as.matrix(d)[1:3,1:3]

#visualize these distances
image(as.matrix(d))

#order the distance by labels
image(as.matrix(d)[order(y), order(y)])

#compute distance between predictors
d <- dist(t(x))
dim(as.matrix(d))

d_492 <- as.matrix(d)[492,]

image(1:28, 1:28, matrix(d_492, 28, 28))



# Comprehension Check: Distance
library(dslabs)
data(tissue_gene_expression)

dim(tissue_gene_expression$x)
table(tissue_gene_expression$y)

# Question 1 - Comptute the Euclidean distance between each observation
d <- dist(tissue_gene_expression$x)
d

# Using the dataset from Q1, compare the distances between observations 1 and 2 (both cerebellum), 
# observations 39 and 40 (both colon), and observations 73 and 74 (both endometrium).
tissue_gene_expression$y

x_1 <- tissue_gene_expression$x[1,]
x_2 <- tissue_gene_expression$x[2,]
sqrt(crossprod(x_1 - x_2))

x_39 <- tissue_gene_expression$x[39,]
x_40 <- tissue_gene_expression$x[40,]
sqrt(crossprod(x_39 - x_40))

x_73 <- tissue_gene_expression$x[73,]
x_74 <- tissue_gene_expression$x[74,]
sqrt(crossprod(x_73 - x_74))

# Cerebrum
sqrt(crossprod(x_1 - x_2))
#colon
sqrt(crossprod(x_39 - x_40))
#endometrium
sqrt(crossprod(x_73 - x_74))

sqrt(crossprod(x_1 - x_39))
sqrt(crossprod(x_39 - x_74))
sqrt(crossprod(x_73 - x_2))

ind <- c(1, 2, 39, 40, 73, 74)
as.matrix(d)[ind,ind]

# Question 3 - Make a plot of all the distances using the image() function to see if the pattern you observed in Q2 is general.
image(as.matrix(d))


# Knn
library(tidyverse)
library(dslabs)
library(caret)
data("mnist_27")
mnist_27$test %>% ggplot(aes(x_1, x_2, color = y)) + geom_point()


#logistic regression
fit_glm <- glm(y ~ x_1 + x_2, data = mnist_27$train, family = "binomial")
p_hat_logit <- predict(fit_glm, mnist_27$test)
y_hat_logit <- factor(ifelse(p_hat_logit > 0.5, 7 , 2))
confusionMatrix(data = y_hat_logit, reference = mnist_27$test$y)

#fit knn model
knn_fit <- knn3(y ~ ., data = mnist_27$train)

x <- as.matrix(mnist_27$train[,2:3])
x
y <- mnist_27$train$y
y
knn_fit <- knn3(x, y)
knn_fit


knn_fit <- knn3(y ~ ., data = mnist_27$train, k=5)
y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)


# Overtraining and Oversmoothing
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")  
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=1
knn_fit <- knn3(y ~ ., data = mnist_27$train, k = 1)
y_hat_knn <- predict(knn_fit, mnist_27$train, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$train$y)$overall["Accuracy"]

y_hat_knn <- predict(knn_fit, mnist_27$test, type = "class")
confusionMatrix(data = y_hat_knn, reference = mnist_27$test$y)$overall["Accuracy"]

#fit knn with k=401
knn_fit_401 <- knn3(y ~ ., data = mnist_27$train, k = 401)
y_hat_knn_401 <- predict(knn_fit_401, mnist_27$test, type = "class")
confusionMatrix(data=y_hat_knn_401, reference=mnist_27$test$y)$overall["Accuracy"]


#pick the k in knn
ks <- seq(3, 251, 2)
library(purrr)

accuracy <- map_df(ks, function(k){
  fit <- knn3(y ~ ., data = mnist_27$train, k = k)
  y_hat <- predict(fit, mnist_27$train, type = "class")
  cm_train <- confusionMatrix(data = y_hat, reference = mnist_27$train$y)
  train_error <- cm_train$overall["Accuracy"]
  y_hat <- predict(fit, mnist_27$test, type = "class")
  cm_test <- confusionMatrix(data = y_hat, reference = mnist_27$test$y)
  test_error <- cm_test$overall["Accuracy"]
  
  tibble(train = train_error, test = test_error)
})
accuracy
ks[which.max(accuracy$test)]


# Comprehension Check: Nearest Neighbors
# Question 1 -
# Set the seed to 1, then use the caret package to partition the dslabs heights data into a training and test set of equal size. 
#Use the sapply() function to perform knn with k values of seq(1, 101, 3) 
# and calculate F1 scores with the F_meas() function using the default value of the relevant argument.
data("heights")
set.seed(1, sample.kind = "Rounding")
head(heights)

heights <- heights %>%
  mutate(sex = factor(sex))



y <- heights$sex


index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-index)
test_set <- heights %>% slice(index)

kn <- seq(1, 101, 3)

accuracy <- map_df(kn, function(k){
  fit <- knn3(sex ~ height, data = train_set, k = k)
  y_hat <- predict(fit, test_set, type = "class")
  F_1 <- F_meas(data = y_hat, reference = test_set$sex)
  tibble(F_1 = F_1)
})

max(accuracy$F_1)
kn[which.max(accuracy$F_1)]

# Question 2
library(dslabs)
library(caret)
data("tissue_gene_expression")

# Set the seed to 1 and split the data into training and test sets with p = 0.5
set.seed(1, sample.kind = "Rounding")

test_index <- createDataPartition(tissue_gene_expression$y, times = 1, p = 0.5, list = FALSE)

y_train <- tissue_gene_expression$y[-test_index]
x_train <- tissue_gene_expression$x[-test_index,] 

y_test <- tissue_gene_expression$y[test_index]
x_test <- tissue_gene_expression$x[test_index,]

class(y_test)
class(x_test)
dim(x_test)

class(y_train)
class(x_train)
dim(x_train)


fit <- knn3(x_train, y_train, k = 11)

y_hat <- predict(fit, x_test, type = "class")

mean(y_hat == y_test)

ks = seq(1, 11, 2)

levels <- sapply(ks, function(k){
  fit <- knn3(x_train, y_train, k = k)
  y_hat <- predict(fit, x_test, type = "class")
  accuracy <- mean(y_hat == y_test)
  tibble(acuraccy = accuracy)
})
levels

## EDX enswer 
  set.seed(1, sample.kind = "Rounding") # if using R 3.6 or later
  y <- tissue_gene_expression$y
  x <- tissue_gene_expression$x
  test_index <- createDataPartition(y, list = FALSE)
  sapply(seq(1, 11, 2), function(k){
    fit <- knn3(x[-test_index,], y[-test_index], k = k)
    y_hat <- predict(fit, newdata = data.frame(x=x[test_index,]),
                     type = "class")
    mean(y_hat == y[test_index])
  })

  

# Comprehension Check: Cross-validation

# Q1- Generate a set of random predictors and outcomes using the following code:
library(tidyverse)
library(caret)
  
set.seed(1996, sample.kind="Rounding") #if you are using R 3.6 or later
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

class(x)
class(y)

#Because x and y are completely independent, you should not be able to predict y using x with accuracy greater than 0.5. 
# Run cross-validation using logistic regression to fit the model.
# Use the subset when training the model.
fit <- train(x_subset, y, method = "glm")
fit$results
fit


# QUestion 2 - Now, instead of using a random selection of predictors, we are going to search for those that are most predictive of the outcome.
# We can do this by comparing the values for the  y=1  group to those in the  y=0  group, for each predictor, using a t-test. 
# You can do perform this step like this:
library(genefilter)
tt <- colttests(x, y)
pvals <- tt$p.value
head(pvals)

# Question 3 - Create an index ind with the column numbers of the predictors that were "statistically significantly" associated with y. 
# Use a p-value cutoff of 0.01 to define "statistically significantly."
ind <- which(pvals <= 0.01)
ind

# Question 4 - Now re-run the cross-validation after redefinining x_subset to be the subset of x defined by the columns 
# showing "statistically significant" association with y.
x_subset <- x[,ind]
x_subset

fit <- train(x_subset, y, method = "glm")
fit$results

# Question 4 - Re-run the cross-validation again, but this time using kNN. 
# Try out the following grid k = seq(101, 301, 25) of tuning parameters. Make a plot of the resulting accuracies.
fit <- train(x_subset, y, method = "knn", tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

# Question 7
# Use the train() function with kNN to select the best k for predicting tissue from gene expression on the tissue_gene_expression
# Try k = seq(1,7,2)
# For this question, do not split the data into test and train sets
data(tissue_gene_expression )
fit <- train(tissue_gene_expression$x, tissue_gene_expression$y, method = "knn", tuneGrid = data.frame(k = seq(1,7,2)))
fit$results

# Bootstrap
# The bootstrap permits us to approximate a Monte Carlo simulation without access to the entire distribution. 
# The general idea is relatively simple. We act as if the observed sample is the population.
# We then sample datasets (with replacement) of the same sample size as the original dataset. 
# Then we compute the summary statistic, in this case the median, on this bootstrap sample.

# Note that we can use ideas similar to those used in the bootstrap in cross validation: 
# instead of dividing the data into equal partitions, we simply bootstrap many times.
n <- 10^6
income <- 10^(rnorm(n, log10(45000), log10(3)))
qplot(log10(income), bins = 30, color = I("black"))

m <- median(income)
m

set.seed(1, sample.kind = "Rounding")
N <- 250
X <- sample(income, N)
M <- median(X)
M

library(gridExtra)

B <- 10^4
M <- replicate(B, {
  X <- sample(income, N)
  median(X)
})

p1 <- qplot(M, bins = 30, color = I("black"))
p2 <- qplot(sample = scale(M)) + geom_abline()
grid.arrange(p1, p2, ncol = 2)

mean(M)
sd(M)

M_star <- replicate(B,{
  X <- sample(income, N, replace = TRUE)
  median(X)
})

tibble(monte_carlo = sort(M), bootstrap = sort(M_star)) %>%
  qplot(monte_carlo, bootstrap, data = .) + 
  geom_abline()

quantile(M, c(0.05, 0.95))
quantile(M_star, c(0.05, 0.95))

median(X) + 1.96 * sd(X) / sqrt(N) * c(-1, 1)

mean(M) + 1.96 * sd(M) * c(-1,1)

mean(M_star) + 1.96 * sd(M_star) * c(-1, 1)

# Comprehension Check: Bootstrap

# The createResample() function can be used to create bootstrap samples. 
# For example, we can create the indexes for 10 bootstrap samples for the mnist_27 dataset like this:
library(dslabs)
library(caret)
data(mnist_27)

set.seed(1995, sample.kind="Rounding")
indexes <- createResample(mnist_27$train$y, 10)

# Question 1 - How many times do 3, 4, and 7 appear in the first resampled index?
sum(indexes[[1]] == 3)
sum(indexes[[1]] == 4)
sum(indexes[[1]] == 7)

# Question 2 - Repeat the exercise for all the resampled indexes.
x <- sapply(indexes, function(ind) {
  sum(ind == 3)
})
sum(x)


# Question 3 - Generate a random dataset using the following code:
y <- rnorm(100, 0, 1)
quantile(y, 0.75)

# Now, set the seed to 1 and perform a Monte Carlo simulation with 10,000 repetitions, 
# generating the random dataset and estimating the 75th quantile each time. What is the expected value and standard error of the 75th quantile?
set.seed(1, sample.kind="Rounding")

B <- 10000
x <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
  
})
mean(x)
sd(x)

# Question 4 - Set the seed to 1 again after generating y and use 10 bootstrap samples to estimate the expected value and standard error of the 75th quantile.
set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)

indexes <- createResample(y, 10)

quantile(y[indexes$Resample01], 0.75)

x <- sapply(indexes, function(ind){
  quantile(y[ind], 0.75)
})
mean(x)
sd(x)

# Repeat the exercise from Q4 but with 10,000 bootstrap samples instead of 10. Set the seed to 1 first.

set.seed(1, sample.kind="Rounding")
y <- rnorm(100, 0, 1)

indexes <- createResample(y, 10000)

quantile(y[indexes$Resample01], 0.75)

x <- sapply(indexes, function(ind){
  quantile(y[ind], 0.75)
})
mean(x)
sd(x)

# Generative Models

  # Discriminative approaches estimate the conditional probability directly and do not consider the distribution of the predictors. 
  
  # Generative models are methods that model the joint distribution and  X  (we model how the entire data,  X  and  Y , are generated).

# Naive Bayes

# Generating train and test set
library("caret")
data("heights")
head(heights)
y <- heights$height
set.seed(2)
test_index <- createDataPartition(y, times = 1, p = 0.5, list = FALSE)
train_set <- heights %>% slice(-test_index)
test_set <- heights %>% slice(test_index)


# Estimating averages and standard deviations
params <- train_set %>%
  group_by(sex) %>%
  summarize(avg = mean(height), sd = sd(height))
params

# Estimate prevalence
pi <- train_set %>%
  summarise(pi = mean(sex == "Female")) %>% pull(pi)
pi

# Getting an actual rule
x <- test_set$height
f0 <- dnorm(x, params$avg[2], params$sd[2])
f1 <- dnorm(x, params$avg[1], params$sd[1])
p_hat_bayes <- f1*pi / (f1*pi + f0*(1 - pi))


# Controlling Prevalence
  #The Naive Bayes approach gives us a direct way to correct the imbalance between
  #sensitivity and specificity by simply forcing  π^  to be whatever value 
  #we want it to be in order to better balance specificity and sensitivity. 

# Computing sensitivity
y_hat_bayes <- ifelse(p_hat_bayes > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes), reference = factor(test_set$sex))


# Changing the cutoff of the decision rule
p_hat_bayes_unbiased <- f1 * 0.5 / (f1 * 0.5 + f0 * (1 - 0.5))
y_hat_bayes_unbiased <- ifelse(p_hat_bayes_unbiased > 0.5, "Female", "Male")
sensitivity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))
specificity(data = factor(y_hat_bayes_unbiased), reference = factor(test_set$sex))


# qda and lda
  # Quadratic discriminant analysis (QDA) is a version of Naive Bayes in which we assume that the distributions
  # pX|Y=1(x)  and  pX|Y=0(x)  are multivariate normal. 

  # Forcing the assumption that all predictors share the same standard deviations and correlations,
  # the boundary will be a line, just as with logistic regression. For this reason, we call the method linear discriminant analysis (LDA).

# QDA
data("mnist_27")

# Estimate parameters from the data
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
# Contour plots
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm", lwd = 1.5)

# Fit model
library(caret)

train_qda <- train(y  , method = "qda", data = mnist_27$train)
train_qda

y_hat <- predict(train_qda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)


# Draw separate plots for 2s and 7s
mnist_27$train %>% mutate(y = factor(y)) %>%
  ggplot(aes(x_1, x_2, fill = y, color = y)) +
  geom_point(show.legend = FALSE) +
  stat_ellipse(type="norm") +
  facet_wrap(~y)

# LDA
params <- mnist_27$train %>%
  group_by(y) %>%
  summarize(avg_1 = mean(x_1), avg_2 = mean(x_2),
            sd_1 = sd(x_1), sd_2 = sd(x_2),
            r = cor(x_1, x_2))
params <- params %>% mutate(sd_1 = mean(sd_1), sd_2 = mean(sd_2), r = mean(r))
train_lda <- train(y ~., method = "lda", data = mnist_27$train)
y_hat <- predict(train_lda, mnist_27$test)
confusionMatrix(data = y_hat, reference = mnist_27$test$y)

# Case Study: More than Three Classes

if(!exists("mnist"))mnist <- read_mnist()

set.seed(3456, sample.kind="Rounding") 
index_127 <- sample(which(mnist$train$labels %in% c(1,2,7)), 2000)
y <- mnist$train$labels[index_127] 
x <- mnist$train$images[index_127,]
index_train <- createDataPartition(y, p=0.8, list = FALSE)

# get the quadrants
# temporary object to help figure out the quadrants
row_column <- expand.grid(row=1:28, col=1:28)
upper_left_ind <- which(row_column$col <= 14 & row_column$row <= 14)
lower_right_ind <- which(row_column$col > 14 & row_column$row > 14)

# binarize the values. Above 200 is ink, below is no ink
x <- x > 200 

# cbind proportion of pixels in upper right quadrant and proportion of pixels in lower right quadrant
x <- cbind(rowSums(x[ ,upper_left_ind])/rowSums(x),
           rowSums(x[ ,lower_right_ind])/rowSums(x)) 

train_set <- data.frame(y = factor(y[index_train]),
                        x_1 = x[index_train,1],
                        x_2 = x[index_train,2])

test_set <- data.frame(y = factor(y[-index_train]),
                       x_1 = x[-index_train,1],
                       x_2 = x[-index_train,2])

train_set %>%  ggplot(aes(x_1, x_2, color=y)) + geom_point()

train_qda <- train(y ~ ., method = "qda", data = train_set)
predict(train_qda, test_set, type = "prob") %>% head()
predict(train_qda, test_set) %>% head()
confusionMatrix(predict(train_qda, test_set), test_set$y)$table
confusionMatrix(predict(train_qda, test_set), test_set$y)$overall["Accuracy"]
train_lda <- train(y ~ ., method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$y)$overall["Accuracy"]

train_knn <- train(y ~ ., method = "knn", tuneGrid = data.frame(k = seq(15, 51, 2)),
                   data = train_set)
confusionMatrix(predict(train_knn, test_set), test_set$y)$overall["Accuracy"]
train_set %>% mutate(y = factor(y)) %>% ggplot(aes(x_1, x_2, fill = y, color=y)) + geom_point(show.legend = FALSE) + stat_ellipse(type="norm")



#Comprehension Check: Generative Models

library(dslabs)
library(caret)
library(tidyverse)
data("tissue_gene_expression")

# set.seed(1993) #if using R 3.5 or earlier
set.seed(1993, sample.kind="Rounding") # if using R 3.6 or later
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

#QUESTION 1
# Use the train() function to estimate the accuracy of LDA. For this question, use the version of x and y created with the code above: 
# do not split them or tissue_gene_expression into training and test sets (understand this can lead to overfitting).
# Report the accuracy from the train() results (do not make predictions).
train_lda <- train(x, y, method = "lda")
train_lda

#QUESTION 2
#In this case, LDA fits two 10-dimensional normal distributions. Look at the fitted model by looking at the finalModel component 
# of the result of train(). Notice there is a component called means that includes the estimated means of both distributions. 
# Plot the mean vectors against each other and determine which predictors (genes) appear to be driving the algorithm.
means_lda <- as.data.frame(t(train_lda$finalModel$means))
means_lda
means_lda <- mutate(means_lda, predictor_name=rownames(means_lda))
means_lda

means_lda %>%
  ggplot(aes(cerebellum, hippocampus, label=predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()


#QUESTION 3
# Repeat the exercise in Q1 with QDA.
# Create a dataset of samples from just cerebellum and hippocampus, 
# two parts of the brain, and a predictor matrix with 10 randomly selected columns using the following code:
library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") 
ind <- which(tissue_gene_expression$y %in% c("cerebellum", "hippocampus"))
y <- droplevels(tissue_gene_expression$y[ind])
x <- tissue_gene_expression$x[ind, ]
x <- x[, sample(ncol(x), 10)]

#Use the train() function to estimate the accuracy of QDA. For this question, use the version of x and y created above instead of the default from tissue_gene_expression. 
# Do not split them into training and test sets
train_qda <- train(x, y, method = "qda")
train_qda

#QUESTION 4 
# Which TWO genes drive the algorithm when using QDA instead of LDA (i.e. the two genes with the highest means)?
means_qda <- as.data.frame(t(train_qda$finalModel$means))
means_qda
means_qda <- mutate(means_qda, predictor_name=rownames(means_qda))
means_qda

means_qda %>%
  ggplot(aes(cerebellum, hippocampus, label=predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# QUESTION 5 
# Re-run LDA with preProcess = "center"
# Which TWO genes drive the algorithm after performing the scaling?

train_lda <- train(x, y, method = "lda", preProcess = "center")
train_lda

means_lda <- as.data.frame(t(train_lda$finalModel$means))
means_lda
means_lda <- mutate(means_lda, predictor_name=rownames(means_lda))
means_lda

means_lda %>%
  ggplot(aes(cerebellum, hippocampus, label=predictor_name)) +
  geom_point() +
  geom_text() +
  geom_abline()

# QUESTION 6 
# Repeat the LDA analysis from Q5 but using all tissue types. Use the following code to create your dataset
library(dslabs)      
library(caret)
data("tissue_gene_expression")

set.seed(1993, sample.kind="Rounding") 
y <- tissue_gene_expression$y
x <- tissue_gene_expression$x
x <- x[, sample(ncol(x), 10)]

train_lda <- train(x, y, method = "lda", preProcess = "center")
train_lda


# Section 5: Classification with More than Two Classes and the Caret Pack

# Classification and Regression Trees (CART)
library(tidyverse)
library(dslabs)
data("olive")
olive %>% as_tibble()
table(olive$region)
olive <- select(olive, -area)

# Predict region using KNN

fit <- train(region ~ . , method = "knn",
             tuneGrid = data.frame(k = seq(1, 15, 2)),
             data = olive) 

ggplot(fit)

# Plot distribution of each predictor stratified by region
olive %>% gather(fatty_acid, percentage, -region) %>%
  ggplot(aes(region, percentage, fill = region)) +
  geom_boxplot() +
  facet_wrap(~fatty_acid, scales = "free") +
  theme(axis.text.x = element_blank())

# plot values for eicosenoic and linoleic
p <- olive %>% 
  ggplot(aes(eicosenoic, linoleic, color = region)) + 
  geom_point()
p + geom_vline(xintercept = 0.065, lty = 2) + 
  geom_segment(x = -0.2, y = 10.54, xend = 0.065, yend = 10.54, color = "black", lty = 2)


# load data for regression tree
data("polls_2008")
qplot(day, margin, data = polls_2008)
head(polls_2008)

library(rpart)
fit <- rpart(margin ~ . , data = polls_2008)
fit
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

polls_2008 %>%
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
predict(fit)

# change parameters

fit <- rpart(margin ~ . , data = polls_2008, control = rpart.control(cp = 0.01, minsplit = 5))
polls_2008 %>%
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")
  

# use cross validation to choose cp
train_rpart <- train(margin ~ ., method = "rpart", tuneGrid = data.frame(cp = seq(0, 0.05, len = 25)), data = polls_2008)
ggplot(train_rpart)



# access the final model and plot it
plot(train_rpart$finalModel, margin = 0.1)
text(train_rpart$finalModel, cex = 0.75)

polls_2008 %>% 
  mutate(y_hat = predict(train_rpart)) %>% 
  ggplot() +
  geom_point(aes(day, margin)) +
  geom_step(aes(day, y_hat), col="red")


pruned_fit <- prune(fit, cp = 0.01)
pruned_fit


# Classification (Decision) Trees
# fit a classification tree and plot it
train_rpart <- train(y ~ .,
                     method = "rpart",
                     tuneGrid = data.frame(cp = seq(0.0, 0.1, len = 25)),
                     data = mnist_27$train)
plot(train_rpart)

# compute accuracy
confusionMatrix(predict(train_rpart, mnist_27$test), mnist_27$test$y)$overall["Accuracy"]


# Random Forests
library(randomForest)

fit <- randomForest(margin ~ . , data = polls_2008)
plot(fit)
fit

polls_2008 %>%
  mutate(y_hat = predict(fit, newdata = polls_2008)) %>%
  ggplot(aes(day, margin)) +
  geom_point() +
  geom_line(aes(day, y_hat))

train_rf <- randomForest(y ~ . , data = mnist_27$train)
confusionMatrix(predict(train_rf, mnist_27$test), mnist_27$test$y)

# use cross validation to choose parameter
train_rf2 <- train(y ~ . , 
                   method = "Rborist",
                   tuneGrid = data.frame(predFixed = 2, minNode = c(3,50)),
                   data = mnist_27$train)
confusionMatrix(predict(train_rf2, mnist_27$test), mnist_27$test$y)


# Comprehension Check: Trees and Random Forests

# Question 1 - Create a simple dataset where the outcome grows 0.75 units on average for every increase in a predictor, using this code:
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1, sample.kind = "Rounding") 
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

# Which code correctly uses rpart() to fit a regression tree and saves the result to fit?
fit <- rpart(y ~ ., data = dat)  
plot(fit)
text(fit)

#QUESTION 3 
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col=2)

# QUESTION 4 - Now run Random Forests instead of a regression tree using randomForest()
library(randomForest)

fit <- randomForest(y ~ x , data = dat)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

# Question 5 - See if the Random Forest from Q4 has converged or if we need more trees.
plot(fit)

# It seems that the default values for the Random Forest result in an estimate that is too flexible (unsmooth).
# Re-run the Random Forest but this time with a node size of 50 and a maximum of 25 nodes. Remake the plot.
fit <- randomForest(y ~ x, data = dat, nodesize = 30, maxnodes = 5)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")


# Caret Package
library(tidyverse)
library(dslabs)
library(caret)
data("mnist_27")

train_glm <- train(y ~ . , method = "glm", data = mnist_27$train)
train_knn <- train(y ~ . , method = "knn", data = mnist_27$train)


y_hat_glm <- predict(train_glm, mnist_27$test, type = "raw")
y_hay_knn <- predict(train_knn, mnist_27$test, type = "raw")

confusionMatrix(y_hat_glm, mnist_27$test$y)$overall[["Accuracy"]]
confusionMatrix(y_hay_knn, mnist_27$test$y)$overall[["Accuracy"]]


# Tuning Parameters with Caret
getModelInfo("knn")
modelLookup("knn")

train_knn <- train(y ~ ., method = "knn", data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)

train_knn <- train(y ~ . , method = "knn",
                   tuneGrid = data.frame(k = seq(1, 80, 2)),
                   data = mnist_27$train)
ggplot(train_knn, highlight = TRUE)

train_knn$bestTune
train_knn$finalModel

confusionMatrix(predict(train_knn, mnist_27$test, type = "raw"), mnist_27$test$y)

control <- trainControl(method = "cv", number = 10, p = 0.9)
train_knn_cv <- train(y ~ . , method = "knn",
                      data = mnist_27$train,
                      tuneGrid = data.frame(k = seq(1, 80, 2)),
                      trControl = control)
ggplot(train_knn_cv, highlight = TRUE)

train_knn$results %>% 
  ggplot(aes(x = k, y = Accuracy)) +
  geom_line() +
  geom_point() +
  geom_errorbar(aes(x = k, 
                    ymin = Accuracy - AccuracySD,
                    ymax = Accuracy + AccuracySD))

plot_cond_prob <- function(p_hat=NULL){
  tmp <- mnist_27$true_p
  if(!is.null(p_hat)){
    tmp <- mutate(tmp, p=p_hat)
  }
  tmp %>% ggplot(aes(x_1, x_2, z=p, fill=p)) +
    geom_raster(show.legend = FALSE) +
    scale_fill_gradientn(colors=c("#F8766D","white","#00BFC4")) +
    stat_contour(breaks=c(0.5),color="black")
}
plot_cond_prob(predict(train_knn, mnist_27$true_p, type = "prob")[,2])


modelLookup("gamLoess")
grid <- expand.grid(span = seq(0.15, 0.65, len = 10), degree = 1)

train_loess <- train(y ~ . , method = "gamLoess",
                     tuneGrid = grid,
                     data = mnist_27$train)
ggplot(train_loess, highlight = TRUE)

confusionMatrix(predict(train_loess, mnist_27$test), mnist_27$test$y)

p1 <- plot_cond_prob(predict(train_loess, mnist_27$true_p, type = "prob")[,2])
p1


# Comprehension Check: Caret Package

# QUESTION 1 
# Load the rpart package and then use the caret::train() function with method = "rpart" to fit a classification tree to 
#the tissue_gene_expression dataset. Try out cp values of seq(0, 0.1, 0.01). Plot the accuracies to report the results of the best model. 
# Set the seed to 1991.
set.seed(1991., sample.kind = "Rounding")
library(rpart)
data("tissue_gene_expression")
dat <- data.frame(y = tissue_gene_expression$y , x = tissue_gene_expression$x)


set.seed(1991., sample.kind = "Rounding")
train_rpart <-  caret::train(y ~ . , method = "rpart",
                     tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                     data = dat)

ggplot(train_rpart, highlight = TRUE)
train_rpart$bestTune
train_rpart$finalModel

# QUESTION 2 
# Note that there are only 6 placentas in the dataset. By default, rpart requires 20 observations before splitting a node. T
# hat means that it is difficult to have a node in which placentas are the majority. Rerun the analysis you did in Q1 with caret::train(), 
# but this time with method = "rpart" and allow it to split any node by using the argument control = rpart.control(minsplit = 0). 
# Look at the confusion matrix again to determine whether the accuracy increases.
set.seed(1991., sample.kind = "Rounding")
train_rpart <- caret::train(y ~ . , method = "rpart",
                            tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
                            control = rpart.control(minsplit = 0),
                            data = dat)

confusionMatrix(train_rpart)
confusionMatrix(predict(train_rpart, dat), dat$y)$overall[["Accuracy"]]


# QUESTION 3
# Plot the tree from the best fitting model of the analysis you ran in Q2.
# Which gene is at the first split?
fit <- rpart(y ~ ., data = dat)  
plot(fit)
text(fit)

# QUESTION 4
# We can see that with just seven genes, we are able to predict the tissue type. 
# Now let's see if we can predict the tissue type with even fewer genes using a Random Forest. 
# Use the train() function and the rf method to train a Random Forest model and save it to an object called fit.
#Try out values of mtry ranging from seq(50, 200, 25) (you can also explore other values on your own). 
#What mtry value maximizes accuracy? 
#To permit small nodesize to grow as we did with the classification trees, use the following argument: nodesize = 1.
#Note: This exercise will take some time to run. If you want to test out your code first, try using smaller values with ntree. 
#Set the seed to 1991 again.

set.seed(1991, sample.kind = "Rounding") # if using R 3.6 or later
fit <- with(tissue_gene_expression, 
            train(x, y, method = "rf", 
                  nodesize = 1,
                  tuneGrid = data.frame(mtry = seq(50, 200, 25))))

ggplot(fit)


set.seed(1991., sample.kind = "Rounding")
train_rf <- train(y ~ . , method = "rf",
                  tuneGrid = data.frame(mtry = seq(50, 200, 25)),
                  control = rpart.control(nodesize = 1),
                  data = dat)

plot(train_rf)

# QUESTION 5
# Use the function varImp() on the output of train() and save it to an object called imp:
imp <- varImp(train_rf)
imp

# QUESTION 6
tree_terms <- as.character(unique(train_rf$finalModel$frame$var[!(train_rf$finalModel$frame$var == "<leaf>")]))
tree_terms




###### Titanic Exercises Part 1 #######
library(titanic)
library(caret)
library(tidyverse)
library(rpart)
data("titanic_train")

# 3 significant digits
options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)

# Question 1: Training and test sets
# Split titanic_clean into test and training sets
# Set the seed to 42, then use the caret package to create a 20% data partition based on the Survived column. 
# Assign the 20% partition to test_set and the remaining 80% partition to train_set.
head(titanic_clean)
set.seed(42, sample.kind = "Rounding")
test_index <- createDataPartition(titanic_clean$Survived, times = 1, p = 0.2, list = FALSE)
train_set <- titanic_clean %>% slice(-test_index)
test_set <- titanic_clean %>% slice(test_index)
  
# What proportion of individuals in the training set survived?
p_survived <- mean(train_set$Survived == 1)
 
# Question 2: Baseline prediction by guessing the outcome
# Set the seed to 3. For each individual in the test set, randomly guess whether that person survived or not by sampling from the vector c(0,1) 
# (Note: use the default argument setting of prob from the sample function).

set.seed(3, sample.kind = "Rounding")
guess <- sample(c(0,1), nrow(test_set), replace = TRUE)
mean(guess == test_set$Survived)

#Question 3 
# What proportion of training set females survived?
train_set %>%
  filter(Sex == "female") %>%
  summarise(mean(Survived == 1))

train_set %>%
  filter(Sex == "male") %>%
  summarise(mean(Survived == 1))

# Question 3b: Predicting survival by sex
# Predict survival using sex on the test set: 
# if the survival rate for a sex is over 0.5, predict survival for all individuals of that sex, and predict death if the survival rate for a sex is under 0.5.
# What is the accuracy of this sex-based prediction method on the test set?

simple_model <- test_set %>% 
  mutate(y_hat = (ifelse(Sex == "female", 1, 0)))
head(simple_model)

mean(simple_model$y_hat == test_set$Survived)

# Question 4a- In the training set, which class(es) (Pclass) were passengers more likely to survive than die?
train_set %>%
  group_by(Pclass) %>%
  summarise(mean(Survived == 1))

#Quesstion 4b- Predict survival using passenger class on the test set: 
# predict survival if the survival rate for a class is over 0.5, otherwise predict death.
# What is the accuracy of this class-based prediction method on the test set?
simple_model2 <- test_set %>%
  mutate(y_hat = ifelse(Pclass == 1, 1, 0))
head(simple_model2)

mean(simple_model2$y_hat == test_set$Survived)

# Question 4c: Predicting survival by passenger class
# Use the training set to group passengers by both sex and passenger class.
# Which sex and class combinations were more likely to survive than die?
train_set %>%
  group_by(Sex, Pclass) %>%
  summarise(mean(Survived == 1))

# Question 4d: Predicting survival by passenger class
#Predict survival using both sex and passenger class on the test set. 
#Predict survival if the survival rate for a sex/class combination is over 0.5, otherwise predict death.
simple_model3 <- test_set %>%
  mutate(y_hat = ifelse(Sex == "female" & Pclass %in% c(1,2), 1, 0))
simple_model3

mean(simple_model3$y_hat == test_set$Survived)

head(simple_model)

# Question 5
# Use the confusionMatrix() function to create confusion matrices for the sex model, class model, and combined sex and class model. 
#You will need to convert predictions and survival status to factors to use this function.
head(simple_model)
class(simple_model$y_hat)
class(simple_model$Survived)

simple_model <- simple_model %>%
  mutate(y_hat = as.factor(y_hat))

simple_model2 <- simple_model2 %>%
  mutate(y_hat = as.factor(y_hat))

simple_model3 <- simple_model3 %>%
  mutate(y_hat = as.factor(y_hat))

confusionMatrix(simple_model$y_hat, reference = test_set$Survived)
confusionMatrix(simple_model2$y_hat, reference = test_set$Survived)
confusionMatrix(simple_model3$y_hat, reference = test_set$Survived)

# Question 6 - Compare the F_means of the models
F_meas(simple_model$y_hat, reference = test_set$Survived)
F_meas(simple_model2$y_hat, reference = test_set$Survived)
F_meas(simple_model3$y_hat, reference = test_set$Survived)

# QUestion 7 - Train a model using linear discriminant analysis (LDA) with the caret lda method using fare as the only predictor.
# Set the seed to 1. 
head(train_set)
set.seed(1, sample.kind = "Rounding")
train_lda <- train(Survived ~ Fare, method = "lda", data = train_set)
confusionMatrix(predict(train_lda, test_set), test_set$Survived)

#What is the accuracy on the test set for the QDA model?
set.seed(1, sample.kind = "Rounding")
train_qda <- train(Survived ~ Fare, method = "qda", data = train_set)
confusionMatrix(predict(train_qda, test_set), test_set$Survived)

# Question 8
# Train a logistic regression model with the caret glm method using age as the only predictor.
# Set the seed to 1. 
set.seed(1, sample.kind = "Rounding")
train_glm <- train(Survived ~ Age, method = "glm", data = train_set)
confusionMatrix(predict(train_glm, test_set), test_set$Survived)

# Train a logistic regression model with the caret glm method using four predictors: sex, class, fare, and age.
set.seed(1, sample.kind = "Rounding")
train_glm2 <- train(Survived ~ Age + Sex + Pclass  + Fare, method = "glm", data = train_set)
confusionMatrix(predict(train_glm2, test_set), test_set$Survived)

#  Train a logistic regression model with the caret glm method using all predictors. Ignore warnings about rank-deficient fit.
set.seed(1, sample.kind = "Rounding")
train_glm3 <- train(Survived ~ . , method = "glm", data = train_set)
confusionMatrix(predict(train_glm3, test_set), test_set$Survived)

# Question 9
#  Train a kNN model on the training set using the caret train function.
# Set the seed to 6.
# Try tuning with k = seq(3, 51, 2).
set.seed(6, sample.kind = "Rounding")
train_knn <- train(Survived ~ . , method = "knn", data = train_set,
                   tuneGrid = data.frame(k = seq(3, 51 , 2)))
plot(train_knn)
train_knn$bestTune

confusionMatrix(predict(train_knn, test_set), test_set$Survived)

# Question 10 
# Instead of the default training control, use 10-fold cross-validation where each partition consists of 10% of the total.
# Try tuning with k = seq(3, 51, 2).
# Set the seed to 8
set.seed(8, sample.kind = "Rounding")
train_knn2 <- train(Survived ~ . , method = "knn", data = train_set,
                    trControl = trainControl(method = "cv", number = 10),
                    tuneGrid = data.frame(k = seq(3, 51, 2)))

confusionMatrix(predict(train_knn2, test_set), test_set$Survived)
plot(train_knn2)
train_knn2$bestTune

# Question 11a: Classification tree model
# Use caret to train a decision tree with the rpart method. Tune the complexity parameter with cp = seq(0, 0.05, 0.002).
# Set the seed to 10.
set.seed(10, sample.kind = "Rounding")
train_tree <- train(Survived ~ . , method = "rpart", data = train_set,
                   tuneGrid = data.frame(cp = seq(0, 0.05, 0.002)))
plot(train_tree)
train_tree$bestTune

confusionMatrix(predict(train_tree, test_set), test_set$Survived)

# Question 11 b - Inspect the final model and plot the decision tree.
plot(train_tree$finalModel)
text(train_tree$finalModel)
head(train_set)
library(rattle)
fancyRpartPlot(train_tree$finalModel)

# Question 12 
# Use the caret train() function with the rf method to train a random forest. Test values of mtry = seq(1:7). Set ntree to 100.
# Set the seed to 14.
set.seed(14, sample.kind = "Rounding")
train_rtree <- train(Survived ~ . ,  method = "rf", data = train_set,
                     tuneGrid = data.frame(mtry = seq(1:7)),
                     control = rpart.control(ntree = 100))
                     
ggplot(train_rtree)
train_rtree$bestTune
confusionMatrix(predict(train_rtree, test_set), test_set$Survived)
varImp(train_rtree)

# Section 6: Model Fitting and Recommendation Systems
library(dslabs)
mnist <- read_mnist()

names(mnist)
dim(mnist$train$images)

class(mnist$train$labels)
table(mnist$train$labels)

# sample 10k rows from training set, 1k rows from test set
set.seed(123, sample.kind = "Rounding")
index <- sample(nrow(mnist$train$images), 10000)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])

index <- sample(nrow(mnist$test$images), 1000)
#note that the line above is the corrected code - code in video at 0:52 is incorrect
x_test <- mnist$test$images[index,]
y_test <- factor(mnist$test$labels[index])


# Preprocessing MNIST Data
library(caret)
library(matrixStats)

sds <- colSds(x)
qplot(sds, bins = 256, color = I("black"))

nzv <- nearZeroVar(x)
image(matrix(1:784 %in% nzv, 28, 28))

col_index <- setdiff(1:ncol(x), nzv)
length(col_index)

# Model Fitting for MNIST Data 
#(Final Model, takes very long to run)
colnames(x) <- 1:ncol(mnist$train$images)
colnames(x_test) <- colnames(x)

control <- trainControl(method = "cv", number = 10, p = .9)

train_knn <- train(x[,col_index], y,
                   method = "knn",
                   tuneGrid = data.frame(k = c(2,3,5)),
                   trControl = control)

# Rationalized model for tests before the final
n <- 3000 # number of rows
b <- 4 # number of cros validations
index <- sample(nrow(x), n)
control <- trainControl(method = "cv", number = b, p = .9)

train_knn <- train(x[index, col_index], y[index],
                   method = "knn",
                   tuneGrid = data.frame(k = c(1,3,5,7)),
                   trControl = control)
ggplot(train_knn)

fit_knn <- knn3(x[, col_index], y, k = 3)
 
y_hat_knn <- predict(fit_knn,
                     x_test[, col_index],
                     type = "class")

cm_knn <- confusionMatrix(y_hat_knn, factor(y_test))
cm_knn$overall["Accuracy"]
cm_knn$byClass[,1:2]

# library(Rborist)
control <- trainControl(method = "cv", number = 5, p = .8)

grid <- expand.grid(minNode = c(1,5) , predFixed = c(10, 15, 25, 35, 50))

train_rf <- train(x[, col_index], y,
                  method = "Rborist",
                  nTree = 50,
                  trControl = control,
                  tuneGrid = grid,
                  nSamp = 5000)
ggplot(train_rf)
train_rf$bestTune

fit_rf <- Rborist(x[,col_index], y,
                  nTree = 1000,
                  minNode = train_rf$bestTune$minNode,
                  predFiced = train_rf$bestTune$predFixed)

y_hat_rf <- predict(fit_rf, x_test[,col_index],
                    type = "class")

y_hat_rf <- factor(levels(y)[predict(fit_rf, x_test[, col_index])$yPred])


cm_rf <- confusionMatrix(y_hat_rf, y_test)

rafalib::mypar(3,4)
for(i in 1:12){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste("Our prediction:", y_hat_rf[i]),
        xaxt="n", yaxt="n")
}


# Variable Importance
library(randomForest)
x <- mnist$train$images[index,]
y <- factor(mnist$train$labels[index])
rf <- randomForest(x, y,  ntree = 50)
imp <- importance(rf)

image(matrix(imp, 28, 28))

p_max <- predict(fit_knn, x_test[,col_index])
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_knn != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]

rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1],
        main = paste0("Pr(",y_hat_knn[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

p_max <- predict(fit_rf, x_test[,col_index])$census  
p_max <- p_max / rowSums(p_max)
p_max <- apply(p_max, 1, max)
ind  <- which(y_hat_rf != y_test)
ind <- ind[order(p_max[ind], decreasing = TRUE)]
rafalib::mypar(3,4)
for(i in ind[1:12]){
  image(matrix(x_test[i,], 28, 28)[, 28:1], 
        main = paste0("Pr(",y_hat_rf[i],")=",round(p_max[i], 2),
                      " but is a ",y_test[i]),
        xaxt="n", yaxt="n")
}

# Ensembles

p_rf <- predict(fit_rf, x_test[, col_index])$census
p_rf <- p_rf /rowSums(p_rf)
p_rf

p_knn <- predict(fit_knn, x_test[, col_index])

p <- (p_rf + p_knn) / 2
p

y_pred <- factor(apply(p, 1, which.max)- 1)
confusionMatrix(y_pred, y_test)

# Comprehension Check: Ensembles
# QUESTION 1
# Use the training set to build a model with several of the models available from the caret package. 
# We will test out 10 of the most common machine learning models in this exercise:

models <- c("glm", "lda", "naive_bayes", "svmLinear", "knn", "gamLoess", "multinom", "qda", "rf", "adaboost")

library(caret)
library(dslabs)
library(tidyverse)

set.seed(1, sample.kind = "Rounding") 
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

# QUESTION 2 
# Now that you have all the trained models in a list, use sapply() or map() to create a matrix of predictions for the test set. 
# You should end up with a matrix with length(mnist_27$test$y) rows and length(models) columns.
length(mnist_27$test$y)
length(models)

pred <- sapply(fits, function(object) 
  predict(object, newdata = mnist_27$test))
dim(pred)
