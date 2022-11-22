library(tidyverse)
library(dplyr)
library(stringr)
rm(list = ls())

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Load the incarceration data file.
incarceration_df <- get_data()

## Obtain the column names of the dataset.
features <- colnames(incarceration_df)

## Section 2  ----
#----------------------------------------------------------------------------#
# Data Summary
# This section is dedicated to understanding facets of incarceration inequality
# in the United States through an analysis of the Vera Institute data.
# In doing so, we will be answering the following questions:
#   1) How does the number of people in pretrial detention in 2014 compare to
#      the 1970?
#   2) When was the highest number of people in pretrial detention recorded?
#   3) Which counties had the highest pretrial detention rates in 2014,
#      and what were their relative sizes?
#   4) What was the average jail population rate of African Americans across
#      the United States in 2014?
#   5) What was the average jail population rate of White Americans across
#      the United States in 2014?
#   6) Which counties had the highest average prison population from 2000 to
#     2014?
#----------------------------------------------------------------------------#

# 1) How does the number of people in pretrial detention in 2014 compare to
#    the 1970?
num_pretrial_diff <- incarceration_df %>%
  filter(year == 1970 | year == 2014) %>%
  group_by(year) %>%
  summarize(pretrial_pop = sum(total_jail_pretrial, na.rm = TRUE)) %>%
  arrange(year) %>%
  pull(pretrial_pop) %>%
  diff() %>%
  round

# 2) When was the highest number of people in pretrial detention recorded?
year_highest_pretrial_pop <- incarceration_df %>%
  group_by(year) %>%
  summarize(pretrial_pop = sum(total_jail_pretrial, na.rm = TRUE)) %>%
  filter(pretrial_pop == max(pretrial_pop, na.rm = TRUE)) %>%
  pull(year)

# 3) Which counties had the highest pretrial detention rates in 2014,
#    and what were their relative sizes?
top_pretrial_counties_2014 <- incarceration_df %>%
  filter(year == 2014) %>%
  mutate(location = paste0(county_name, ", ", state)) %>%
  top_n(10, wt = total_jail_pretrial_rate) %>%
  select(location, urbanicity)
  
# 4) What was the average jail population rate of African Americans across
#    the United States in 2014?
avg_aa_jail_rate <- incarceration_df %>%
  filter(year == 2014) %>%
  pull(black_jail_pop_rate) %>%
  mean(na.rm = TRUE)

# 5) What was the average jail population rate of White Americans across
#    the United States in 2014?
avg_white_jail_rate <- incarceration_df %>%
  filter(year == 2014) %>%
  pull(white_jail_pop_rate) %>%
  mean(na.rm = TRUE)
  
# 6) Which counties had the highest average prison population from 2000 to
#    2014?
counties_highest_avg_prison_pop <- incarceration_df %>%
  filter(year >= 2000 & year <= 2014) %>%
  mutate(location = paste0(county_name, ", ", state)) %>%
  group_by(location, urbanicity) %>%
  summarize(avg_prison_pop = round(mean(total_prison_pop)),
            .groups = "drop") %>%
  top_n(5, wt = avg_prison_pop) %>%
  rename("Location" = location,
         "Prison Population" = avg_prison_pop,
         "Urbanicity" = urbanicity
  )

# 7) Which counties had the highest average prison population rates from 2000
#    to 2014?
counties_highest_avg_prison_rate <- incarceration_df %>%
  filter(year >= 2000 & year <= 2014) %>%
  mutate(location = paste0(county_name, ", ", state)) %>%
  group_by(location, urbanicity) %>%
  summarize(mean_prison_rate = round(mean(total_prison_pop_rate)),
            .groups = "drop") %>%
  top_n(5, wt = mean_prison_rate) %>%
  rename("Location" = location,
         "Population Rate" = mean_prison_rate,
         "Urbanicity" = urbanicity
  )

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# This section contains functions responsible for data wrangling and charting
# the growth of the U.S. prison population.
#----------------------------------------------------------------------------#
# Organizes 
get_year_jail_pop <- function() {
  jail_pop_df <- incarceration_df %>%
    group_by(year) %>%
    summarize(total_pop = sum(total_jail_pop, na.rm = TRUE))
  return(jail_pop_df)
}

# This function ... <todo:  update comment>
plot_jail_pop_for_us <- function()  {
  # TODO: Implement this function
  return()
} 

## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Prison Population by State
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 5  ----
#----------------------------------------------------------------------------#
# <variable comparison that reveals potential patterns of inequality>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#

## Section 6  ----
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
