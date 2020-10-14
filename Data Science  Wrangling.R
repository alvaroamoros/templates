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

<<<<<<< HEAD
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


#Assessment Part 2: Reshaping Data
=======
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
>>>>>>> 0689f598b7962923a708508ee9f30d29efd667f4
library(tidyverse)
library(dslabs)
co2
head(co2)

<<<<<<< HEAD
#Question 10 - Run the following command to define the co2_wide object:
co2_wide <- data.frame(matrix(co2, ncol = 12, byrow = TRUE)) %>%
  setNames(1:12) %>%
  mutate(year = as.character(1959:1997))
co2_wide
head(co2_wide)

co2_tidy <- gather(co2_wide, month, co2, -year)
head(co2_tidy)

# Questio 11 - Use co2_tidy to plot CO2 versus month with a different curve for each year:
co2_tidy %>% ggplot(aes(as.numeric(month), co2, color = year)) + geom_line()
=======
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
>>>>>>> 0689f598b7962923a708508ee9f30d29efd667f4

# Question 12
library(dslabs)
data(admissions)
<<<<<<< HEAD
dat <- admissions %>% select(-applicants)

dat_tidy <- spread(dat, gender, admitted)
dat_tidy

# Question 13 Now use the admissions dataset to create the object tmp, which has columns major, gender, key and value:
tmp <- gather(admissions, key, value, admitted:applicants)
tmp
 # Combine the key and gender and create a new column called column_name to get a variable with the following values: admitted_men, admitted_women, applicants_men and applicants_women. Save the new data as tmp2.
tmp2 <- unite(tmp, column_name, c(key,gender))
tmp2

# Question 14 - Which function can reshape tmp2 to a table with six rows and five columns named major, admitted_men, admitted_women, applicants_men and applicants_women?
spread(tmp2, column_name, value)
=======
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

# CSS Selectors
h <- read_html("https://foodnetwork.co.uk/recipes/guacamole/")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

guacamole <- list(recipe, prep_time, ingredients)
guacamole

get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node("//h1") %>% html_text()
  prep_time <- h %>% html_node("//li[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]//strong") %>% html_text()
  ingredients <- h %>% html_nodes("//*[contains(concat( " ", @class, " " ), concat( " ", "ingredient", " " ))]") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
}
get_recipe("https://foodnetwork.co.uk/recipes/pancakes-4926/")


# Assessment: Web Scraping
library(rvest)
url <- "https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h <- read_html(url)

# Extract first table. In html nodes for tables are "table"
nodes <- html_nodes(h, "table")

html_text(nodes[[8]])

html_table(nodes[[8]])

  # Section 3: String Processing
url <- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw <- read_html(url) %>%
  html_nodes("table") %>%
  html_table() %>%
  .[[1]] %>%
  setNames(c("state", "population", "total", "murder_rate"))
murders_raw

class(murders_raw$population)
class(murders_raw$total)

# to include both single and double quotes in string, escape with \
s <- '5\'10"'
s
cat(s)

s <- "5'10\""
s
cat(s)

murders_raw$population[1:3]
as.numeric(murders_raw$population[1:3])

# detect whether there are commas
commas <- function(x) any(str_detect(x, ","))
murders_raw %>% summarize_all(funs(commas))

# replace commas with the empty string and convert to numeric
test_1 <- str_replace_all(murders_raw$population, ",", "")
test_1 <- as.numeric(test_1)

# parse_number also removes commas and converts to numeric
test_2 <- parse_number(murders_raw$population)
test_2

murders_new <- murders_raw %>% mutate_at(2:3, parse_number)
murders_new

# Case Study 2: Reported Heights
library(dslabs)
data(reported_heights)
class(reported_heights$height)

x <- as.numeric(reported_heights$height)
head(x)
sum(is.na(x))

# keep only entries that result in NAs
reported_heights %>% mutate(new_height = as.numeric(height)) %>%
  filter(is.na(new_height)) %>%
  head()

# calculate cutoffs that cover 99.999% of human population
alpha <- 1/10^6
qnorm(1-alpha/2, 69.1, 2.9)
qnorm(alpha/2, 63.7, 2.7)

# keep only entries that either result in NAs or are outside the plausible range of heights
not_inches <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest
  ind
}

# number of problematic entries
problems <- reported_heights %>%
  filter(not_inches(height)) %>%
  .$height
length(problems)

# 10 examples of x'y or x'y" or x'y\"
pattern <- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems, pattern) %>%
  head %>%
  cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems, pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind <- which(between(suppressWarnings(as.numeric(problems))/2.54, 54, 81) )
ind <- ind[!is.na(ind)]
problems[ind] %>% head(n=10) %>% cat


# Regex
pattern <- ","
str_detect(murders_raw$total, pattern)

str_detect(reported_heights$height, "cm")

yes <- c("180 cm", "70 inches")
no <- c("180", "70''")
s <- c(yes, no)

str_detect(s, "cm") | str_detect(s, "inches")
str_detect(s, "cm|inches")

str_view(s, pattern)
str_view_all(s, pattern)

# Character Classes, Anchors and Quantifiers
# s was defined in the previous video
yes <- c("5", "6", "5'10", "5 feet", "4'11")
no <- c("", ".", "Five", "six")
s <- c(yes, no)
pattern <- "\\d"

str_view(s, "[56]")

# [4-7] means 4, 5, 6 or 7
yes <- as.character(4:7)
no <- as.character(1:3)
s <- c(yes, no)
str_detect(s, "[4-7]")

# ^ means start of string, $ means end of string
pattern <- "^\\d$"
yes <- c("1", "5", "9")
no <- c("12", "123", " 1", "a4", "b")
s <- c(yes, no)
str_view(s, pattern)

# curly braces define quantifiers: 1 or 2 digits
pattern <- "^\\d{1,2}$"
yes <- c("1", "5", "9", "12")
no <- c("123", "a4", "b")
str_view(c(yes, no), pattern)

# combining character class, anchors and quantifier
pattern <- "^[4-7]'\\d{1,2}\"$"
yes <- c("5'7\"", "6'2\"",  "5'12\"")
no <- c("6,2\"", "6.2\"","I am 5'11\"", "3'2\"", "64")
str_detect(yes, pattern)
str_detect(no, pattern)

# Search and Replace with Regex
# * means 0 or more instances of the previous character. ? means 0 or 1 instances. + means 1 or more instances.

# number of entries matching our desired pattern
pattern <- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems, pattern))

# inspect examples of entries with problems
problems[c(2, 10, 11, 12, 15)] %>% str_view(pattern)
str_subset(problems, "inches")
str_subset(problems, "''")

# replace or remove feet/inches words before matching
pattern <- "^[4-7]'\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>%
  sum()

# R does not ignore whitespace
identical("Hi", "Hi ")

# \\s represents whitespace
pattern_2 <- "^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems, pattern_2)

# * means 0 or more instances of a character
yes <- c("AB", "A1B", "A11B", "A111B", "A1111B")
no <- c("A2B", "A21B")
str_detect(yes, "A1*B")
str_detect(no, "A1*B")

# test how *, ? and + differ
data.frame(string = c("AB", "A1B", "A11B", "A111B", "A1111B"),
           none_or_more = str_detect(yes, "A1*B"),
           nore_or_once = str_detect(yes, "A1?B"),
           once_or_more = str_detect(yes, "A1+B"))

# update pattern by adding optional spaces before and after feet symbol
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot", "'") %>% # replace feet, ft, foot with '
  str_replace("inches|in|''|\"", "") %>% # remove all inches symbols
  str_detect(pattern) %>%
  sum()

# Groups with Regex
# define regex with and without groups
pattern_without_groups <- "^[4-7],\\d*$"
pattern_with_groups <-  "^([4-7]),(\\d*)$"

# create examples
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)

# demonstrate the effect of groups
str_detect(s, pattern_without_groups)
str_detect(s, pattern_with_groups)

# demonstrate difference between str_match and str_extract
str_match(s, pattern_with_groups)
str_extract(s, pattern_with_groups)

# improve the pattern to recognize more events
pattern_with_groups <-  "^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s <- c(yes, no)
str_replace(s, pattern_with_groups, "\\1'\\2")

# Final pattern
pattern_with_groups <-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# combine stringr commands with the pipe
str_subset(problems, pattern_with_groups) %>% head
str_subset(problems, pattern_with_groups) %>%
  str_replace(pattern_with_groups, "\\1'\\2") %>% head

#Testing and Improving

# function to detect entries with problems
not_inches_or_cm <- function(x, smallest = 50, tallest = 84){
  inches <- suppressWarnings(as.numeric(x))
  ind <- !is.na(inches) &
    ((inches >= smallest & inches <= tallest) |
       (inches/2.54 >= smallest & inches/2.54 <= tallest))
  !ind
}

# identify entries with problems
problems <- reported_heights %>%
  filter(not_inches_or_cm(height)) %>%
  .$height
problems
length(problems)

converted <- problems %>%
  str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"", "") %>%  #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") ##change format

# find proportion of entries that fit the pattern after reformatting
pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
index <- str_detect(converted, pattern)
mean(index)

converted[!index]



problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")


# Separate with Regex
#The extract() function behaves similarly to the separate() function but allows extraction of groups from regular expressions.

# First example - normally formatted heights
s <- c("5'10","6'1")
tab <- data.frame(x = s)
tab

# the separate and extract functions behave similarly
tab %>% separate(x, c("feet", "inches"), sep = "'")

tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")
tab

# second example - some heights with unusual formats
s <- c("5'10", "6'1\"", "5'8inches")
s
tab <- data.frame(x = s)
tab

# separate fails because it leaves in extra characters, but extract keeps only the digits because of regex groups
tab %>% separate(x, c("feet", "inches"), sep = "'", fill = "right")

tab %>% extract(x, c("feet", "inches"), regex = "(\\d)'(\\d{1,2})")

#Using Groups and Quantifiers. Examples

  #1 Many students measuring exactly 5 or 6 feet did not enter any inches. For example, 6' - our pattern requires that inches be included.
  #2 Some students measuring exactly 5 or 6 feet entered just that number.
  #3 Some of the inches were entered with decimal points. For example 5'7.5''. Our pattern only looks for two digits.
  #4 Some entires have spaces at the end, for example 5 ' 9.
  #5 Some entries are in meters and some of these use European decimals: 1.6, 1,7.
  #6 Two students added cm.
  #7 One student spelled out the numbers: Five foot eight inches.



  # Case 1
  yes <- c("5","6", "5")
  no <- c("5'", "5''", "5'4")
  s <- c(yes, no)

  # Case 2
  str_replace(s, "^([56])'?$", "\\1'0")

  # case 2.2
  str_replace(s, "^([56])'*$", "\\1'0")

  #Case 3
  pattern <- "^[4-7]\\s*'\\s*(\\d+\\.?\\d*)$"

  # case 5
  yes <- c("1,7", "1, 8", "2, ")
  no <- c("5,8", "5,3,2", "1.7")
  s <-c(yes, no)
  s
  str_replace(s, "^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2")

  # Trimming <- remove spaces from start and end of strings
  str_trim("5 ' 9 ")

  # str_to_lower <- Removes ower case letters
  s <- c("Five feet eight inches")
  str_to_lower(s)

# Putting it into a function
  convert_format <- function(s){
    s %>%
      str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
      str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
      str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
      str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
      str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
      str_trim() #remove extra space
  }

  words_to_numbers <- function(s){
    str_to_lower(s) %>%
      str_replace_all("zero", "0") %>%
      str_replace_all("one", "1") %>%
      str_replace_all("two", "2") %>%
      str_replace_all("three", "3") %>%
      str_replace_all("four", "4") %>%
      str_replace_all("five", "5") %>%
      str_replace_all("six", "6") %>%
      str_replace_all("seven", "7") %>%
      str_replace_all("eight", "8") %>%
      str_replace_all("nine", "9") %>%
      str_replace_all("ten", "10") %>%
      str_replace_all("eleven", "11")
  }

  # Now we can see which problematic entries remain:
  converted <- problems %>% words_to_numbers %>% convert_format
  remaining_problems <- converted[not_inches_or_cm(converted)]
  pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
  index <- str_detect(remaining_problems, pattern)
  remaining_problems[!index]

# Putting it All Together
<<<<<<< HEAD
pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"
smallest <- 50
tallest <- 84

new_heights <-reported_heights %>%
  mutate(
    original = height,
    height = words_to_numbers(height) %>% convert_format %>%
    str_
  )

=======
  pattern <- "^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

  smallest <- 50
  tallest <- 84
  new_heights <- reported_heights %>%
    mutate(original = height,
           height = words_to_numbers(height) %>% convert_format()) %>%
    extract(height, c("feet", "inches"), regex = pattern, remove = FALSE) %>%
    mutate_at(c("height", "feet", "inches"), as.numeric) %>%
    mutate(guess = 12*feet + inches) %>%
    mutate(height = case_when(
      !is.na(height) & between(height, smallest, tallest) ~ height, #inches
      !is.na(height) & between(height/2.54, smallest, tallest) ~ height/2.54, #centimeters
      !is.na(height) & between(height*100/2.54, smallest, tallest) ~ height*100/2.54, #meters
      !is.na(guess) & inches < 12 & between(guess, smallest, tallest) ~ guess, #feet'inches
      TRUE ~ as.numeric(NA))) %>%
    select(-guess)
  new_heights


# Check all the entries we converted
  new_heights %>%
    filter(not_inches(original)) %>%
    select(original, height) %>%
    arrange(height) %>%
    View

# String Splitting
  # read raw murders data line by line
  filename <- system.file("extdata/murders.csv", package = "dslabs")
lines <- readLines(filename)
lines %>% head
lines

# split at commas with str_split function, remove row of column names
x <- str_split(lines, ",")
x %>% head
x

# extract first element of each list entry
library(purrr)
map(x, function(y) y[1]) %>% head()
map(x, 1) %>% head

# Extract columns 1-5 as characters, then convert to proper format

dat <- data.frame(parse_guess(map_chr(x, 1)),
                  parse_guess(map_chr(x, 2)),
                  parse_guess(map_chr(x, 3)),
                  parse_guess(map_chr(x, 4)),
                  parse_guess(map_chr(x, 5))) %>%
  setNames(col_names)

dat %>% head

# more efficient code for the same thing
dat <- x %>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>%
  as.data.frame()

# the simplify argument makes str_split return a matrix instead of a list
x <- str_split(lines, ",", simplify = TRUE)
col_names <- x[1,]
x <- x[-1,]
x %>% as_data_frame() %>%
  setNames(col_names) %>%
  mutate_all(parse_guess)

# Case Study: Extracting a Table from a PDF
library(dslabs)
data("research_funding_rates")
research_funding_rates

# We start by downloading the PDF document then importing it into R using the following code:
library("pdftools")
temp_file <- tempfile()
url <- "http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url, temp_file)
txt <- pdf_text(temp_file)
file.remove(temp_file)
txt
raw_data_research_funding_rates <- txt[2]
raw_data_research_funding_rates

raw_data_research_funding_rates %>% head

#we see that it is a long string. Each line on the page, including the table rows, is separated by the symbol for newline: \n.
#We can therefore can create a list with the lines of the text as elements:

tab <- str_split(raw_data_research_funding_rates, "\n")
tab

tab <-tab[[1]]
tab %>% head

# we see that the information for the column names is the third and fourth entires:
the_names_1 <- tab[3]
the_names_2 <- tab[4]
the_names_1
the_names_2

#In the table, the column information is spread across two lines. We want to create one vector with one name for each column.
the_names_1
the_names_1 <- the_names_1 %>%
  str_trim() %>%
  str_replace_all(",\\s.", "") %>%
  str_split("\\s{2,}", simplify = TRUE)
the_names_1

the_names_2 <- the_names_2 %>%
  str_trim() %>%
  str_split("\\s+", simplify = TRUE)
the_names_2

# Now we can join these to generate one name for each column:
tmp_names <- str_c(rep(the_names_1, each = 3), the_names_2[-1], sep = "_")
tmp_names
the_names <- c(the_names_2[1], tmp_names) %>%
  str_to_lower() %>%
  str_replace_all("\\s", "_")
the_names

#Now we are ready to get the actual data. By examining the tab object, we notice that the information is in lines 6 through 14.
#We can use str_split() again to achieve our goal:
new_research_funding_rates <- tab[6:14] %>%
  str_trim %>%
  str_split("\\s{2,}", simplify = TRUE) %>%
  data.frame(stringsAsFactors = FALSE) %>%
  setNames(the_names) %>%
  mutate_at(-1, parse_number)
new_research_funding_rates %>% head()

# Recoding
library(dslabs)
data("gapminder")
gapminder %>%
  filter(region=="Caribbean") %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# display long country names
gapminder %>%
  filter(region=="Caribbean") %>%
  filter(str_length(country) >= 12) %>%
  distinct(country)

# recode long country names and remake plot
gapminder %>% filter(region=="Caribbean") %>%
  mutate(country = recode(country,
                          'Antigua and Barbuda'="Barbuda",
                          'Dominican Republic' = "DR",
                          'St. Vincent and the Grenadines' = "St. Vincent",
                          'Trinidad and Tobago' = "Trinidad")) %>%
  ggplot(aes(year, life_expectancy, color = country)) +
  geom_line()

# Assessment Part 2: String Processing Part 3
library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
polls
head(polls)

  #Question 5

#Update polls by changing the column names to c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
#and only keeping rows that have a percent sign (%) in the remain column.

#How many rows remain in the polls data frame?
names(polls) <- c("dates", "remain", "leave", "undecided", "lead", "samplesize", "pollster", "poll_type", "notes")
polls <- polls[str_detect(polls$remain, "%"), ]
nrow(polls)

  #Question 6
as.numeric(str_replace(polls$remain, "%", ""))/100

parse_number(polls$remain)/100

# Question 8
temp <- str_extract_all(polls$dates, "\\d{1,2}\\s[a-zA-Z]+")
temp
>>>>>>> 1d45fac514f143430dd504ebf5748c199490c616
