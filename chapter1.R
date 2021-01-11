#################################################
ipak <- function(pkg){
   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
   if (length(new.pkg))
       install.packages(new.pkg, dependencies = TRUE)
   try(sapply(pkg, require, character.only = TRUE), silent = TRUE)
}

packages <- c( "ggplot2", "sparklyr","foreach",
             "doParallel", "dplyr",
              "xtable", "data.table",  "DescTools", "ggplot2", "devtools", "stringr", "stringi")
ipak(packages)
###############################################################
### tidyr on datacamp: reshape almost all data into tidy format
### Hadley Wickham: messy data
###############################################################


### tidy data definition
### structure
### columns hold variables -- e.g., name, homeworld, species
### rows hold observations
### cell hold values


### once the data is in the tidy format, you can use dplyr to do subsequent analysis
### filter subset rows
### mutate create new variables or overwriting existing ones
### group_by, summarize() for aggregation

### stepwise manner : using pipe operator
### 

### multiple variables in a single column
### separating variables over two columns
### separate function


### Multiple variables per column
### Multiple variables per column
# Being a busy person, you don't want to spend too much time on Netflix, so you decide to crunch some numbers on TV show and movie durations before deciding what to watch. You've managed to obtain a dataset named netflix_df, but its duration column has an issue. It contains strings with both a value and unit of duration ("min" or "Season").
# 
# You'll tidy this dataset so that each variable gets its own column.
# 
# As will always be the case in this course, the tidyr package has been pre-loaded for you.

netflix_df %>% 
    # Split the duration column into value and unit columns
    separate(duration, into = c("value", "unit"), sep = " ", convert = TRUE)


### columns with multiple values
### e.g., two variables stored in the single column
### convert = TRUE, automatically checks if a newly created column can be converted to numeric


### Combine multiple columns into one?
### unite function, first specify the new column name, and then the columns to be specified
### drink_df
### multiple values in a single cell
### values to variables, values to observations

### separating values over rows
### separate_rows() function
### count function to count the frequency of ingridients used and drinks used


### example:
# International phone numbers
# 
# You work for a multinational company that uses auto-dialer software to contact its customers. When new customers subscribe online they are asked for a phone number but they often forget to add the country code needed for international calls. You were asked to fix this issue in the database. You've been given a data frame with national numbers and country codes named phone_nr_df. Now you want to combine the country_code and national_number columns to create valid international numbers.
# 

phone_nr_df %>%
    # Unite the country_code and national_number columns
    unite("international_number", country_code, national_number, sep = " ")

# 
# tvshow_df %>% 
#     # Separate the actors in the cast column over multiple rows
#     separate_rows(cast, sep = ", ") %>% 
#     rename(actor = cast) %>% 
#     count(actor, sort = TRUE) %>% 
#     head(6)


### Separating into columns and rows
### 
# Remember the drink ingredients data from the video? 
# You've been given an similar version (drink_df) that also includes quantities and units. Now you want to create an overview of how much of each ingredient you should buy to make these drinks.
# 
# The dplyr package has been pre-loaded for you.


drink_df %>% 
    # Separate the ingredients over rows
    separate_rows(ingredients, sep = "; ") %>% 
    # Separate ingredients into three columns
    separate(
        ingredients, 
        into = c("ingredient", "quantity", "unit"), 
        sep = " ", 
        convert = TRUE
    ) %>% 
    # Group by ingredient and unit
    group_by(ingredient, unit) %>% 
    # Calculate the total quantity of each ingredient
    summarize(quantity = sum(quantity))


#### 





#### missing values in R
#### NA = not available
#### imputing with a default value, replace_na() function
#### typeof(0L): integer
#### typeof(0): double
#### imputing with the most recent value: fill()
#### fill(,.direction="down") = "up"
#### drop_na(): get rid of the rows with NAs
#### drop_na(people_on_moon)

# And the Oscar for best director goes to ... <NA>
#     
# You're working on a sample of the Netflix dataset pre-loaded as director_df. This time, the data frame contains just the directors and movie titles. Your goal is to identify the directors who created the most movies. Since the director column contains multiple names, you'll first separate its values over multiple rows and then count the directors.
# 
# Since you don't want movies without directors polluting your overview, you'll apply the drop_na() function.
# 
# The dplyr package has been pre-loaded for you.

director_df %>% 
    # Drop rows with NA values in the director column
    drop_na(director) %>% 
    # Spread the director column over separate rows
    separate_rows(director, sep = ", ") %>% 
    # Count the number of movies per director
    count(director, sort = TRUE)



###########################################
##########

# Imputing sales data
# 
# You've been asked to create a report that allows management to compare sales figures per quarter for two years. The problem is that the dataset (sales_df) contains missing values. You'll need to impute the values in the year column so that you can visualize the data.
# 
# The ggplot2 package has been pre-loaded for you.



# A tibble: 8 x 3
# year  quarter sales
# <fct> <chr>   <dbl>
#     1 2019  Q1      12498
# 2 2019  Q2      20461
# 3 2019  Q3      19737
# 4 2019  Q4      20314
# 5 2020  Q1      13494
# 6 2020  Q2      19314
# 7 2020  Q3      23640
# 8 2020  Q4      22920


# sales_df %>% 
#     # Impute the year column
#     fill(year, .direction = "up") %>%
#     # Create a line plot with sales per quarter colored by year.
#     ggplot(aes(x = quarter, y = sales, color = year, group = year)) +
#     geom_line()


sales_df %>% 
    # Impute the year column
    fill(year, .direction = "up") %>%
    # Create a line plot with sales per quarter colored by year.
    ggplot(aes(x = quarter, y = sales, color = year, group = year)) +
    geom_line()



##########################################################
############## 
# Nuclear bombs per continent
# 
# Since WWII, a number of nations have been detonating nuclear bombs for military research. A tally of bombs detonated per nation has been calculated from the Nuclear Explosion DataBase (NEDB) and provided as nuke_df. You are interested in finding out how many bombs have been detonated by nations grouped per continent. To achieve this goal, nuke_df will be joined to country_to_continent_df which is a mapping of nation to continent. You will need to overwrite missing values with zeros so that you can create a nice plot.
# 
# The dplyr and ggplot2 packages have been pre-loaded for you.
# 
# Side note 1: Bombs detonated by the Soviet Union were attributed to the Russian Federation.
# 
# Side note 2: The Russian Federation is solely mapped to Europe for simplicity.

country_to_continent_df %>% 
    left_join(nuke_df, by = "country_code") %>%  
    # Impute the missing values in the n_bombs column with 0L
    replace_na(list(n_bombs = 0L)) %>% 
    # Group the dataset by continent
    group_by(continent) %>% 
    # Sum the number of bombs per continent
    summarize(n_bombs_continent = sum(n_bombs)) %>% 
    # Plot the number of bombs per continent
    ggplot(aes(x=continent, y=n_bombs_continent)) +
    geom_col()



###############################################################
###############################################################