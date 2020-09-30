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


    # Combining Tables

      # left_join() only keeps rows that have information in the first table.
      # right_join() only keeps rows that have information in the second table.
      # inner_join() only keeps rows that have information in both tables.
      # full_join() keeps all rows from both tables.
      # semi_join() keeps the part of first table for which we have information in the second.
      # anti_join() keeps the elements of the first table for which there is no information in the second

# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

# import US election results data
data(polls_us_election_2016)
head(results_us_election_2016)
identical(results_us_election_2016$state, murders$state)

# join the murders table and US election results table
tab <- left_join(murders, results_us_election_2016, by = "state")
tab

tab %>% ggplot(aes(population/10^6, electoral_votes, label = abb)) +
  geom_point() +
  geom_text_repel() +
  scale_x_continuous(trans = "log2") +
  scale_y_continuous(trans = "log2") +
  geom_smooth(method = "ln", se =FALSE)

tab1 <- slice(murders, 1:6) %>% select(state, population)
tab1

tab2 <- slice(results_us_election_2016, c(1:3, 5,  7:8)) %>% select(state, electoral_votes)
tab2

left_join(tab1, tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)

inner_join(tab1 , tab2)

full_join(tab1, tab2)

semi_join(tab1, tab2)
anti_join(tab1, tab2)

bind_cols(a = 1:3, b = 4:6)

tab1 <- tab[, 1:3]
tab2 <- tab[, 4:6]
tab3 <- tab[, 7:9]
tab1
tab2
tab3
new_tab <- bind_cols(tab1,  tab2, tab3)
new_tab


tab1 <- tab[1:5,]
tab2 <- tab[6:12,]
tab1
tab2
bind_rows(tab1, tab2)


#  intersect vectors or data frames
intersect(1:10, 5:15)
intersect(c("a", "b", "c"), c("a", "d", "s"))

tab1 <- tab[1:5,]
tab2 <- tab[3:6,]
intersect(tab1, tab2)

# perform a union of vectors or data frames
union(1:10, 6:15)
union(c("a", "b", "c"),c("a","d","s"))
union(tab1, tab2)

# set difference of vectors or data frames
setdiff(1:10, 5:15)
setdiff(c("a", "b", "c"),c("a","d","s"))
setdiff(tab1, tab2)

# setequal determines whether sets have the same elements, regardless of order
setequal(1:10, 5:15)
setequal(c("a", "b", "c"),c("a","d","s"))
setequal(tab1, tab2)


    # Assessment: Combining Tables
tab1 <- slice(murders, 1:6) %>% select(state, population)
tab2 <- slice(results_us_election_2016, c(1:3,  7:8)) %>% select(state, electoral_votes)
tab1
tab2
dat <- left_join(tab1, tab2, by = "state")
dat



install.packages("Lahman")
library(Lahman)

# Question 5 - Use the correct join or bind function to create a combined table of the names and statistics of the top 10 home run (HR) hitters for 2016. 
# This table should have the player ID, first name, last name, and number of HR for the top 10 players. Name this data frame top_names.

top <- Batting %>% 
  filter(yearID == 2016) %>%
  arrange(desc(HR)) %>%    # arrange by descending HR count
  slice(1:10)    # take entries 1-10
top %>% as_tibble()
top

Master %>% as_tibble()

top_names <- top %>%  left_join(Master)%>%
  select(playerID, nameFirst, nameLast, HR)
top_names

# Question 6 Inspect the Salaries data frame. Filter this data frame to the 2016 salaries, 
#then use the correct bind join function to add a salary column to the top_names data frame from the previous question. 
#Name the new data frame top_salary. Use this code framework:

top_salary <- Salaries %>% filter(yearID == 2016) %>%
  right_join(top_names) %>%
  select(nameFirst, nameLast, teamID, HR, salary)
top_salary

# QUestion 7 - 
# Inspect the AwardsPlayers table. Filter awards to include only the year 2016.
head(AwardsPlayers)
awards_2016 <- AwardsPlayers %>% filter(yearID == 2016)
awards_2016

# How many players from the top 10 home run hitters won at least one award in 2016?
inner_join(top, awards_2016)

# How many players won an award in 2016 but were not one of the top 10 home run hitters in 2016?

awards_2016 <- awards_2016 %>% select(playerID)

top <- top %>% select(playerID)

setdiff(awards_2016, top)


    # Web Scraping
install.packages("rvest")
library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h <- read_html(url)
class(h)
h

tab <- h %>% html_nodes("table")
tab 

tab <- tab %>% html_table
class(tab)
tab

tab <- tab %>% setNames(c("state", "population", "total", "murders", "gun_murders", "gun_ownership", "total_rate", "murder_rate", "gun_murder_rate"))
head(tab)

