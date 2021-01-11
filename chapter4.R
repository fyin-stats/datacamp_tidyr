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
               "forcats", "rjson", "broom", "purrr")
ipak(packages)

######################################
######################################

# chapter 4, intro to non rectangular data
# use .csv format for rectangular data

# non-rectangular formats
# json
# xml


#######################################
#######################################
star_wars_list <- rjson::fromJSON(file = "star_wars.json")
tibble(character = star_wars_list) 
unnest_wider(character)
unnest_wider(films)

#######################################
#######################################

# Create a tibble with a movie column
tibble(movie = movie_planets_list) %>% 
    # Unnest the movie column
    unnest_wider(movie) %>% 
    # Unnest the planets column
    unnest_wider(planets)


#########################################
#########################################
# unnest_wider

# rectangling deeply nested data
# unnest_longer() for list
# unnest_wider() for name list
# 


planet_df %>% 
    # Unnest the moons list column over observations
    unnest_longer(moons) %>% 
    # Further unnest the moons column
    unnest_wider(moons) %>% 
    # Unnest the moon_data column
    unnest_wider(moon_data) %>% 
    # Get the top five largest moons by radius
    slice_max(radius, n = 5)



############################################
############################################
# selecting nested variable
############################################
############################################

#
# selective unnesting with hoist

# 
planet_df %>% hoist(moons, first_moon = list(), radius = list())

# unnesting google maps data

##############################################
##############################################

# hoist requires you know structures of your data

# character_df %>% 
#     hoist(metadata, first_film = list("films", 1))

movie_df %>% 
    hoist(
        movie,
        title = "Title",
        year = "Year",
        rating = list("Ratings", "Rotten Tomatoes")
    )


#################################################
#################################################
# nesting data for modeling
#################################################
#################################################

# 
# USA Olympics performance

# broom package


# broom + dplyr + tidyr

# group_by
# nest
# purrr::map()

# 
broom::glance(model)
tidy(model)

# 
ansur_df %>%
    # Group the data by sex
    group_by(sex) %>% 
    # Nest the data
    nest() %>% 
    mutate(
        fit = map(data, function(df) lm(weight_kg ~ waist_circum_m + stature_m, data = df)),
        glanced = map(fit, glance)
    ) %>% 
    # Unnest the glanced column
    unnest(glanced)












######### recap
######### chapter 1: separate + missing values
######### chapter 2: pivoting data
######### chapter 3: expanding data
######### chapter 4: turn nested data structures into tidy data format


