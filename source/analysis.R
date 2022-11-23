library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
library(scales)
library(leaflet)
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
# Changes in Incarceration Rates by Gender Across Regions of the United States
# The following section contains data wrangling functions for analyzing
# gender disparities in the incarceration rates of men and woman across
# each region of the United States (as indicated by the divisions Midwest,
# Northeast, South, West).
#----------------------------------------------------------------------------#
# Organizes data by region and jail population rates for men and
# women separately.
#
# Returns:
#   A tibble of data mapping regions of the United States to their respective
#   jail population rates for men and women, organized by year.
get_jail_gender_rates <- function() {
  gender_df <- incarceration_df %>%
    group_by(region, year) %>%
    summarize(avg_female_jail_rate = mean(female_jail_pop_rate,
                                          na.rm = TRUE),
              avg_male_jail_rate = mean(male_jail_pop_rate,
                                        na.rm = TRUE),
              .groups = "drop") %>%
    gather(
      key = gender,
      value = rate,
      -region, -year
    ) %>%
    mutate(gender = str_replace(gender, "avg_male_jail_rate", "Male Rate"),
           gender = str_replace(gender, "avg_female_jail_rate", "Female Rate"),
           region = paste(region, gender)) %>%
  return(gender_df)
}

# Generates a line chart of female to male jail population rates for each region
# of the United States from 1980-2015.
#
# Returns:
#   A line chart showing trends in jail population rates of men and women
#   in the United States across different regions of the United States
plot_jail_gender_rates <- function() {
  plot_data <- get_jail_gender_rates()
  gender_plot <- ggplot(data = plot_data, mapping = aes(
                                            x = year,
                                            y = rate,
                                            color = region)) +
    geom_point() +
    geom_smooth() +
    labs(
      title = "Average Jail Population Rates for Men and Women by U.S. Region
      (1980-2015)",
      caption = "Men experienced a larger growth in jail population rates than
      women, and certain regions of the U.S. saw larger changes in growth rates
      compared to others",
      x = "Year",
      y = "Jail Population Rate per 100k People",
      color = "Region"
    )
  return(gender_plot)
}

## Section 6  ----
#----------------------------------------------------------------------------#
# Racial Groups with the Highest Incarceration Rates by County
# This section contains functions for mapping the racial groups with the highest
# incarceration rates in each Southern county of the United States in 2015.
#----------------------------------------------------------------------------#
# Creates a tibble which maps U.S. counties in the South to the racial group
# with the highest jail and prison population rates in 2015.
#
# Returns:
#   A tibble mapping U.S. counties to the racial group with the highest
#   jail and prison population rate in 2015.
get_most_incarcerated <- function() {
  top_incarcerated_by_county <- incarceration_df %>%
    filter(year == 2015, state %in% states_in_region("South")) %>%
    replace(is.na(.), 0) %>%
    mutate(location = paste(county_name, state, sep = ", "),
           Code = state,
           subregion = tolower(str_sub(county_name, 0, -8)),
           "Asian American/Pacific Islander" = aapi_prison_pop_rate +
                                               aapi_jail_pop_rate,
           "Black" = black_prison_pop_rate + black_jail_pop_rate,
           "Latinx" = latinx_prison_pop_rate + latinx_jail_pop_rate,
           "Native American" = native_prison_pop_rate + native_jail_pop_rate,
           "White" = white_prison_pop_rate + white_jail_pop_rate) %>%
    left_join(state_codes, by = "Code") %>%
    mutate(region = tolower(State)) %>%
    gather(key = race, value = rate, `Asian American/Pacific Islander`,
           Black, Latinx, `Native American`, White) %>%
    select(location, subregion, race, rate, region) %>%
    group_by(location) %>%
    filter(rate > 0 & rate == max(rate)) %>%
    ungroup()
  return(top_incarcerated_by_county)
}

# Generates a choropleth map of counties and the most incarcerated racial group
# in Southern county for the year 2015.
#
# Returns:
#   A choropleth map which is colored based on racial group for each U.S. county
#   in the South in 2015.
plot_most_incarcerated_by_county <- function() {
  plot_data <- get_most_incarcerated() %>%
    left_join(map_data("county"), by = c("subregion", "region"))
  racial_plot <- ggplot(data = plot_data) +
    geom_polygon(
      mapping = aes(
        x = long,
        y = lat,
        fill = race,
        group = group
      ),
      color = "white",
      size = 0.3
    ) +
    coord_map() +
    theme(
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      axis.ticks = element_blank()
    ) +
    labs(
      title = "Map of Racial Group with Highest Incarceration Rate in Each
      Southern County of the U.S. (2015)",
      fill = "Racial Group",
      caption = "Black Americans consistently held the highest jail and
      prison population rates in Southern counties of the U.S. in 2015\
      Note: some counties are omitted for having unrecorded jail and prison
      population rates"
    )
  return(racial_plot)
}