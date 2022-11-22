library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
rm(list = ls())

# The functions might be useful for A4
source("../source/a4-helpers.R")

## Load the incarceration data file.
incarceration_df <- get_data()

## Obtain the column names of the dataset.
features <- colnames(incarceration_df)

## Load state names and codes for later use.
state_codes <- read.csv("../source/state_names_and_codes.csv")

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
# Growth of the U.S. Jail Population
# This section contains functions for wrangling data to chart
# the growth of the U.S. Jail population.
#----------------------------------------------------------------------------#
# Creates a tibble which organizes the total U.S. jail population by year.
#
# Returns:
#   A tibble mapping years to the total U.S. jail populations for those years
get_year_jail_pop <- function() {
  jail_pop_df <- incarceration_df %>%
    group_by(year) %>%
    summarize(total_pop = round(sum(total_jail_pop, na.rm = TRUE)))
  return(jail_pop_df)
}

# Generates an aesthetic plot of total U.S. jail population by year.
#
# Returns:
#   A plot of the total U.S. jail population by year for the range of years
#   from 1970-2018
plot_jail_pop_for_us <- function()  {
  plot_data <- get_year_jail_pop()
  jail_plot <- ggplot(data = plot_data) +
    geom_col(
      mapping = aes(
        x = year,
        y = total_pop,
        
      )
    ) +
    scale_y_continuous(labels = label_comma()) +
    labs(
      title = "Increase of Jail Population in the U.S. (1970-2018)",
      caption = "The U.S. jail population has seen a steady increase since
                 1980, peaking in 2007",
      x = "Year",
      y = "Total Jail Population"
    )
  return(jail_plot)
}

## Section 4  ----
#----------------------------------------------------------------------------#
# Growth of Jail Population by State
# This section contains functions for wrangling data to chart the growth
# of the U.S. jail population by state.
#----------------------------------------------------------------------------#
# Creates a tibble consisting of data on U.S. jail populations for the
# provided vector of states.
#
# Arguments:
#   states: a vector of state codes which represents the states whose data for
#           jail population sizes are to be collected
# Returns:
#   A tibble mapping the given states to their total jail populations for
#   the range of years from 1970-2018
get_jail_pop_by_states <- function(states) {
  jail_pop_by_states <- incarceration_df %>%
    filter(state %in% states) %>%
    group_by(state, year) %>%
    summarize(total_pop = sum(total_jail_pop, na.rm = TRUE),
              .groups = "drop") %>%
    mutate("Code" = state) %>%
    left_join(state_codes, by = "Code")
  return(jail_pop_by_states)
}

# Generates a line chart of the total jail populations for the given states
# from 1970-2018.
#
# Arguments:
#   states: a vector of state codes which represents the states to include in
#           the line chart
# Returns:
#   A line chart displaying the total jail populations for each given state
#   for the range of years from 1970-2018\
plot_jail_pop_by_states <- function(states) {
  if (length(states) > 10) {
    print("Number of states must be less than 10")
    return(null)
  }
  plot_data <- get_jail_pop_by_states(states)
  state_pop_plot <- ggplot(data = plot_data,
                           mapping = aes(
                             x = year,
                             y = total_pop,
                             color = State
                           )) +
    geom_point() +
    geom_smooth() +
    scale_y_continuous(labels = label_comma()) +
    labs(
      title = "Increase of U.S. Jail Population by State (1970-2018)",
      x = "Year",
      y = "Total Jail Population",
      caption = "Jail populations for states of various sizes are shown to
      capture how jail population counts vary by population size"
    )
  return(state_pop_plot)
}

## Section 5  ----
#----------------------------------------------------------------------------#
# Changes in Incarceration Rates by Gender Across Western States
# The following section contains data wrangling functions for analyzing how
# gender disparities in the incarceration rates of counties in the Western
# United States.
#----------------------------------------------------------------------------#

## Section 6  ----
#----------------------------------------------------------------------------#
# <a map shows potential patterns of inequality that vary geographically>
# Your functions might go here ... <todo:  update comment>
# See Canvas
#----------------------------------------------------------------------------#
