# Data Science Wrangling
# 092420


#Data Import Overview

# see working directory
getwd()

# change your working directory
setwd()

# Find the directory of specific data
system.file(extdata, package = dslabs)

# set path to the location for raw data files in the dslabs package and list files
path - system.file(extdata, package = dslabs)
list.files(path)

#We can copy this data in our working directory
filename - murders.csv
fullpath - file.path(path, filename)
fullpath

file.copy(fullpath, getwd())  


# The readr and readxl Packages

readLines(murders.csv, n = 3)

dat - read.csv(murders.csv)
head(dat)
class(dat)
class(dat$abb)

dat2 - read.csv(murders.csv, stringsAsFactors = FALSE)
class(dat2$abb)


# Downloading Files from the Internet
url - httpsraw.githubusercontent.comrafalabdslabsmasterinstextdatamurders.csv
data - read.csv(url)
data

download.file(url, murders.csv)

tempfile()

tmp_filename - tempfile()
download.file(url, tmp_filename)
file.remove(tmp_filename)


# Assessment Part 2 Data Import
library(tidyverse)

# Question 1
# Inspect the file at the following URL
# httpmlr.cs.umass.edumlmachine-learning-databasesbreast-cancer-wisconsinwdbc.data
# Which readr function should be used to import this file
url - httpsarchive.ics.uci.edumlmachine-learning-databasesbreast-cancer-wisconsinwdbc.data
wd - getwd()
wd
download.file(url, CUsersPetazetasprojectshardvar-xtemplateswdbc.data)
data - read.csv(wdbc.data)
head(data)

# Tidy Data
library(tidyverse)
library(dslabs)
data(gapminder)

# Tidy data fomat
tidy_data - gapminder %% 
  filter(country %in% c(South Korea, Germany)) %%
  select(country, year, fertility)
head(tidy_data)

#Wide data format
path - system.file(extdata, package = dslabs)
filename - file.path(path, fertility-two-countries-example.csv)
wide_data - read_csv(filename)
select(wide_data, country, 19601967)

# gather argument - transforms wide data in to tidy one
# 3rd argument selects the colums that are gathers (default takes alll columns)
# 1st argument, sets the name of the column that taht will hold the variable that is kept in the wide data column name

new_tidy_data - wide_data %%
  gather(year, fertility, `1960``2015`)
head(new_tidy_data)

# The same but selecting the colums NOT to gather
new_tidy_data - wide_data %%
  gather(year, fertility, -country)
new_tidy_data

# The gather function asumes that column names are characters
class(tidy_data$year)
class(new_tidy_data$year)

new_tidy_data - wide_data %%
  gather(year, fertility, -country, convert = TRUE)
class(new_tidy_data$year)

# To converti tidy data back in to wide format
new_wide_data - new_tidy_data %% spread(year, fertility)
select(new_wide_data, country, `1960``1967`)
new_wide_data


path - system.file(extdata, package = dslabs)
filename - file.path(path, life-expectancy-and-fertility-two-countries-example.csv)

raw_data - read_csv(filename)
select(raw_data, 15)

dat - raw_data %% gather(key, value, -country)
dat

# Separate - separates different elements of a name
# Arguments (apart from the data)
# 1. name of the column to be separated
# 2. names to be used in the new columns
# 3. the character that separates the variable

dat %% separate(key, c(year, variable_name), _) 
# We optain to many values

dat %% separate(key, c(year, first_variable_name, second_variable_name), 
                fill = right)

# Argument extra, merges two variables if there is an extra separation
dat %% separate(key, c(year, variable_name), sep = _, extra = merge) 

dat %% separate(key, c(year, variable_name), sep = _, extra = merge) %%
  spread(variable_name, value)

# We can obtain the same result using the unite function
dat %% 
  separate(key, c(year, first_variable_name, second_variable_name), fill = right) %%
  unite(variable_name, first_variable_name, second_variable_name, sep=_)

dat %% 
  separate(key, c(year, first_variable_name, second_variable_name), fill = right) %%
  unite(variable_name, first_variable_name, second_variable_name, sep=_) %%
  spread(variable_name, value) %%
  rename(fertility = fertility_NA)


#Assessment Part 2 Reshaping Data  
library(tidyverse)
library(dslabs)
co2
head(co2)    

#Question 10 - Run the following command to define the co2_wide object
co2_wide - data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %% 
  setNames(112) %%
  mutate(year = as.character(19591997))
co2_wide
head(co2_wide)

co2_tidy - gather(co2_wide, month, co2, -year)
head(co2_tidy)

# Questio 11 - Use co2_tidy to plot CO2 versus month with a different curve for each year
co2_tidy %% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()

# Question 12 
library(dslabs)
data(admissions)
dat - admissions %% select(-applicants)

dat_tidy - spread(dat, gender, admitted)
dat_tidy

# Question 13 Now use the admissions dataset to create the object tmp, which has columns major, gender, key and value
tmp - gather(admissions, key, value, admittedapplicants)
tmp
# Combine the key and gender and create a new column called column_name to get a variable with the following values admitted_men, admitted_women, applicants_men and applicants_women. Save the new data as tmp2.
tmp2 - unite(tmp, column_name, c(key,gender))
tmp2

# Question 14 - Which function can reshape tmp2 to a table with six rows and five columns named major, admitted_men, admitted_women, applicants_men and applicants_women
spread(tmp2, column_name, value)