# Data Science: Wrangling
# 09/24/20


     #Data Import Overview

# see working directory
getwd()

# change your working directory
setwd()

# Find the directory of specific data
system.file("extdata", package = "dslabs")

# set path to the location for raw data files in the dslabs package and list files
path <- system.file("extdata", package = "dslabs")
list.files(path)
  
  #We can copy this data in our working directory
  filename <- "murders.csv"
  fullpath <- file.path(path, filename)
  fullpath
  
  file.copy(fullpath, getwd())  
  
      
      # The readr and readxl Packages
  
readLines("murders.csv", n = 3)

dat <- read.csv("murders.csv")
head(dat)
class(dat)
class(dat$abb)

dat2 <- read.csv("murders.csv", stringsAsFactors = FALSE)
class(dat2$abb)

 
      # Downloading Files from the Internet
url <- "https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
data <- read.csv(url)
data

download.file(url, "murders.csv")

tempfile()

tmp_filename <- tempfile()
download.file(url, tmp_filename)
file.remove(tmp_filename)


# Assessment Part 2: Data Import
library(tidyverse)

# Question 1
# Inspect the file at the following URL:
# http://mlr.cs.umass.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data
# Which readr function should be used to import this file?
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
wd <- getwd()
wd
download.file(url, "C:/Users/Petazetas/projects/hardvar-x/templates/wdbc.data")
data <- read.csv("wdbc.data")
head(data)

    # Tidy Data
library(tidyverse)
library(dslabs)
data(gapminder)

# Tidy data fomat
tidy_data <- gapminder %>% 
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

#Wide data format
path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "fertility-two-countries-example.csv")
wide_data <- read_csv(filename)
select(wide_data, country, "1960":"1967")

  # gather argument <- transforms wide data in to tidy one
    # 3rd argument selects the colums that are gathers (default takes alll columns)
    # 1st argument, sets the name of the column that taht will hold the variable that is kept in the wide data column name

new_tidy_data <- wide_data %>%
  gather(year, fertility, `1960`:`2015`)
head(new_tidy_data)

# The same but selecting the colums NOT to gather
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)
new_tidy_data

# The gather function asumes that column names are characters
class(tidy_data$year)
class(new_tidy_data$year)

new_tidy_data <- wide_data %>%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

# To converti tidy data back in to wide format
new_wide_data <- new_tidy_data %>% spread(year, fertility)
select(new_wide_data, country, `1960`:`1967`)
new_wide_data


path <- system.file("extdata", package = "dslabs")
filename <- file.path(path, "life-expectancy-and-fertility-two-countries-example.csv")

raw_data <- read_csv(filename)
select(raw_data, 1:5)

dat <- raw_data %>% gather(key, value, -country)
dat

  # Separate <- separates different elements of a name
    # Arguments (apart from the data):
      # 1. name of the column to be separated
      # 2. names to be used in the new columns
      # 3. the character that separates the variable

      dat %>% separate(key, c("year", "variable_name"), "_") 
      # We optain to many values
   
      dat %>% separate(key, c("year", "first_variable_name", "second_variable_name"), 
                       fill = "right")
      
      # Argument extra, merges two variables if there is an extra separation
      dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") 
      
      dat %>% separate(key, c("year", "variable_name"), sep = "_", extra = "merge") %>%
        spread(variable_name, value)
      
      # We can obtain the same result using the unite function
      dat %>% 
        separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
        unite(variable_name, first_variable_name, second_variable_name, sep="_")
      
      dat %>% 
        separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
        unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
        spread(variable_name, value) %>%
        rename(fertility = fertility_NA)