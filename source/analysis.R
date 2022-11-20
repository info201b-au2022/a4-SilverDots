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
#   4) What was the average incarceration rate of African Americans across the
#      United States in 2014?
#   5) What was the average incarceration rate of White Americans across the
#      United States in 2014?
#   6) Which counties saw the highest change in incarceration rates from 1970 to
#     2014?
#----------------------------------------------------------------------------#

# 1) How does the number of people in pretrial detention in 2014 compare to
#    the 1970?
num_pretrial_diff <- incarceration_df %>%
  filter(year == 1970 | year == 2014) %>%
  group_by(year) %>%
  summarize(pretrial_pop = sum(total_jail_pretrial, na.rm = TRUE)) %>%
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
  top_n(10, wt = total_jail_pretrial_rate, na.rm = TRUE) %>%
  

## Section 3  ----
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population
# Your functions might go here ... <todo:  update comment>
#----------------------------------------------------------------------------#
# This function ... <todo:  update comment>
get_year_jail_pop <- function() {
  # TODO: Implement this function
return()
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
