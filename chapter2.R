##################################
##################################
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg))
        install.packages(new.pkg, dependencies = TRUE)
    try(sapply(pkg, require, character.only = TRUE), silent = TRUE)
}

packages <- c( "ggplot2", "sparklyr","foreach",
               "doParallel", "dplyr",
               "xtable", "data.table",  "DescTools", "ggplot2", "devtools", "stringr", "stringi",
               "forcats")
ipak(packages)
###################################
###################################


# From wide to long data

# values in column headers
# 

# pivot_longer function 
# `1945`:`1951`
# -: specify the column not to be pivot
# pivot_longer(-country, names_to, values_to, values_drop_na)
# names_transform

# nuke_df %>% 
#     # Pivot the data to a longer format
#     pivot_longer(
#         -year, 
#         # Overwrite the names of the two new columns
#         names_to = "country", 
#         values_to = "n_bombs"
#     ) %>% 
#     # Replace NA values for n_bombs with 0L
#     replace_na(list(n_bombs = 0L)) %>% 
#     # Plot the number of bombs per country over time
#     ggplot(aes(x = year, y = n_bombs, color = country)) +
#     geom_line()


# obesity_df %>% 
#     # Pivot the male and female columns
#     pivot_longer(c(male, female),
#                  names_to = "sex",
#                  values_to = "pct_obese") %>% 
#     # Create a scatter plot with pct_obese per country colored by sex
#     ggplot(aes(x = pct_obese, color = sex,
#                y = forcats::fct_reorder(country, both_sexes))) +
#     geom_point() +
#     scale_y_discrete(breaks = c("India", "Nauru", "Cuba", "Brazil",
#                                 "Pakistan", "Gabon", "Italy", "Oman",
#                                 "China", "United States of America")) +
#     labs(x = "% Obese", y = "Country")


# Bond... James Bond
# 
# You've been given a James Bond movie dataset (bond_df) and want to visualize the number of movies that Bond actors have featured in per decade. However, the data is in an untidy format with the decade values captured in the column headers. You'll tidy this dataset to give each variable its own column.
# 
# The ggplot2 package has been pre-loaded for you.


# bond_df %>% 
#     # Pivot the data to long format
#     pivot_longer(
#         -Bond, 
#         # Overwrite the names of the two newly created columns
#         names_to = "decade", 
#         values_to = "n_movies", 
#         # Drop na values
#         values_drop_na = TRUE, 
#         # Transform the decade column data type to integer
#         names_transform = list(decade = as.integer)
#     ) %>% 
#     ggplot(aes(x = decade + 5, y = n_movies, fill = Bond))+
#     geom_col()



###############################################
###############################################
######## Deriving variables from column headers
###############################################
###############################################

### names_prefix


### two variables per column name


### names_sep
### 


# New-Zealand's bird of the year
# 
# Every year New Zealanders vote en masse to decide which species gets the bird of the year trophy. The contest is organized by the Forest & Bird agency which allows each person to give points to up to five birds (first pick gets 5 points, second 4, …). Your job is to decide this year's winner from the messy dataset that's been pre-loaded for you as bird_df.
# 
# The dplyr package has been pre-loaded for you.

bird_df %>%
    # Pivot the data to create a 2 column data frame
    pivot_longer(
        starts_with("points_"),
        names_to = "points",
        names_prefix = "points_",
        names_transform = list(points = as.integer),
        values_to = "species",
        values_drop_na = TRUE
    ) %>%
    group_by(species) %>% 
    summarize(total_points = sum(points)) %>% 
    slice_max(total_points, n = 5)


# 
# Big tech stock prices
# 
# You're an analyst at an investment firm and want to visualize the weekly closing prices of five big tech firms' stocks. However, the dataset you've been handed (stock_df) is messy and has the year and week variables stored in the column headers. You'll pivot this data into a tidy format, extract the variables from the headers, and create a line plot.
# 
# The ggplot2 package has been pre-loaded for you.


stock_df %>% 
    # Pivot the data to create 3 new columns: year, week, price
    pivot_longer(
        -company,
        names_to = c("year", "week"),
        values_to = "price",
        names_sep = "_week",
        names_transform = list(
            year = as.integer,
            week = as.integer)
    ) %>%
    # Create a line plot with price per week, color by company
    ggplot(aes(x = week, y = price, color = company)) +
    geom_line() +
    facet_grid(. ~ year)



### separating column headers into variables
### pivot_longer function
### ".value"
### names_sep
### uncount() function 
### .id
### duplicate
### drop the count

# Soviet space dogs, the dog perspective
# 
# You'll be working on an pre-processed sample of the USSR space dogs database compiled by Duncan Geere and pre-loaded for you as space_dogs_df. Each of the 42 rows in this dataset represents a test rocket launch which had one or two very brave dogs on board.
# 
# Your goal is to reshape this dataset so that for each launch, each dog has a row.
# 
# The challenge is that in the column headers (name_1, name_2, gender_1, and gender_2), the part before the _ separator can point to two different variables (name and gender), while the second part always points to the dog ID (1st or 2nd dog).

space_dogs_df %>% 
    pivot_longer(
        # Add the columns to pivot
        -c(`date`, `result`),
        names_sep = "_",
        # Complete the names_to argument to re-use the first part of the column headers
        names_to = c(".value", "dog_id"),
        # Make sure NA values are dropped
        values_drop_na = TRUE
    )


#
# WHO obesity vs. life expectancy
# 
# You've been given a sample of WHO data (who_df) with obesity percentages and life expectancy data per country, year, and sex. You want to visually inspect the correlation between obesity and life expectancy.
# 
# However, the data is very messy with four variables hidden in the column names. Each column name is made up of three parts separated by underscores: Values for the year, followed by those for sex, and then values for either pct.obese or life.exp. Since the third part of the column name string holds two variables you'll need to use the special ".value" value in the names_to argument.
# 
# You'll pivot the data into a tidy format and create the scatterplot.
# 
# The ggplot2 package has been pre-loaded for you.


who_df %>% 
    # Put each variable in its own column
    pivot_longer(
        -country,
        names_to = c("year", "sex", ".value"),
        names_sep = "_", 
        names_transform = list("year" = as.integer)
    ) %>%
    # Create a plot with life expectancy over obesity
    ggplot(aes(x = pct.obese, y = life.exp, color = sex)) + geom_point()




############################################
############################################

dog_df %>% 
    # Create one row for each participant and add the id
    uncount(n_participants, .id = "dog_id")



############################################
############################################
# from long to wide data
############################################
############################################
# variable names in a column
############################################
############################################
# pivot_wider()

# 
pivot_wider(names_from, values_from, names_prefix)
# 


# transposing a dataframe
# 
space_dogs_df %>% 
    # Pivot the data to a wider format
    pivot_wider(names_from = dog_id, values_from = gender, names_prefix = "gender_") %>% 
    # Drop rows with NA values
    drop_na() %>% 
    # Create a Boolean column on whether both dogs have the same gender
    mutate(same_gender = (gender_1 == gender_2)) %>% 
    summarize(pct_same_gender = mean(same_gender))

#
# >
#     space_dogs_df
# # A tibble: 81 x 3
# date       gender dog_id
# <date>     <chr>   <int>
#     1 1951-07-22 Male        1
# 2 1951-07-22 Male        2
# 3 1951-07-29 Male        1
# 4 1951-07-29 Female      2
# 5 1951-08-15 Male        1
# 6 1951-08-15 Male        2
# 7 1951-08-19 Male        1
# 8 1951-08-19 Male        2
# 9 1951-08-28 Male        1
# 10 1951-08-28 Male        2



# 
planet_df %>% 
    # Give each planet variable its own column
    pivot_wider(names_from = "metric", values_from = "value") %>% 
    # Plot planet temperature over distance to sun
    ggplot(aes(x = distance_to_sun, y = temperature)) +
    geom_point(aes(size = diameter)) +
    geom_text(aes(label = planet), vjust = -1) +
    labs(x = "Distance to sun (million km)", 
         y = "Mean temperature (°C)") +
    theme(legend.position = "none")



#
planet_df %>%
    # Pivot all columns except metric to long format
    pivot_longer(-metric, names_to = "planet") %>% 
    # Put each metric in its own column
    pivot_wider(names_from = metric, values_from = value) %>% 
    # Plot the number of moons vs planet diameter
    ggplot(aes(x = diameter, y = number_of_moons)) +
    geom_point(aes(size = diameter)) +
    geom_text(aes(label = planet), vjust = -1) +
    labs(x = "Diameter (km)", y = "Number of moons") +
    theme(legend.position = "none")