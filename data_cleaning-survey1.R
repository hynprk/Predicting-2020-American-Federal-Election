#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from 
# Democracy Fund + UCLA Nationscape
# - Link to data set: https://www.voterstudygroup.org/publication/nationscape-data-set
# Author: Pamela De Vera, Hyoeun Park
# Data: 02 November 2020
# Contact: pamela.devera@mail.utoronto.ca, hyoeun.park@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data 
# from https://www.voterstudygroup.org/publication/nationscape-data-set 
# and save the folder that you're interested in to inputs
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)

# setwd("~/Desktop/STA304/Problem Set 3/election_data")
# Read in the raw data
survey_raw_data <- read_dta("inputs/ns20200625/ns20200625.dta")
# Add the labels
survey_raw_data <- labelled::to_factor(survey_raw_data)

### Selecting specific variables only ###
cleaned_data <- 
  survey_raw_data %>% 
  dplyr::select(vote_2020,
         gender,
         census_region,
         hispanic,
         race_ethnicity,
         household_income,
         education,
         age)

### More Data Cleaning ### 
# Remove any Missing entries (i.e., NA's)
cleaned_data <- na.omit(cleaned_data)

# Change variable name from 'census_region' to 'region
names(cleaned_data)[names(cleaned_data) == 'census_region'] <- 'region'

# Change variable name from 'gender' to 'sex'
## - so that variable name is the same as census data
names(cleaned_data)[names(cleaned_data) == 'gender'] <- 'sex'

# Binary Outcome for 'vote_2020'
## filter Donald Trump and Joe Biden only
cleaned_data <- cleaned_data %>%
  filter(vote_2020 == "Donald Trump" | vote_2020 == "Joe Biden")
## Create new variable
cleaned_data<-
  cleaned_data %>%
  mutate(vote_biden = 
           ifelse(vote_2020=="Joe Biden", 1, 0))

# Binary outcome for Hispanic
cleaned_data <- cleaned_data %>%
  mutate(is_hispanic = 
           ifelse(hispanic == "Not Hispanic", 0, 1))

# Stratifying: Race Ethnicity into 5 categories
cleaned_data <- cleaned_data %>% 
  mutate(race =
           ifelse(race_ethnicity == "White", 
                  "White", 
                  ifelse(race_ethnicity == "Black, or African American", 
                         "Black", 
                         ifelse(race_ethnicity == "American Indian or Alaska Native", 
                                "Indigenous", 
                                ifelse(race_ethnicity == "Some other race", 
                                       "Other", 
                                       "Asian/Pacific Islander")))))

# Stratifying: Education Levels
cleaned_data <- cleaned_data %>%
  mutate(education_level = 
           ifelse(education == "3rd Grade or less" | 
                    education == "Middle School - Grade 4 - 8" | 
                    education == "Completed some high school" | 
                    education == "High school graduate" | 
                    education == "Completed some college, but no degree", 
                  "Primary/Secondary", 
                  ifelse(education == "Other post high school vocational training" | 
                           education == "Associate Degree" | 
                           education == "College Degree (such as B.A., B.S.)" | 
                           education == "Completed some graduate, but no degree", 
                         "Associate/Bachelor", 
                         "Master/Doctorate")))

# Stratifying: Income Class
cleaned_data <- cleaned_data %>% 
  mutate(income_class = 
           ifelse(household_income == "$125,000 to $149,999" |
                    household_income == "$150,000 to $174,999" | 
                    household_income == "$175,000 to $199,999" |
                    household_income == "$200,000 to $249,999" |
                    household_income == "$250,000 and above", 
                  "High", 
                  ifelse(household_income == "Less than $14,999" |
                           household_income == "$15,000 to $19,999" | 
                           household_income == "$20,000 to $24,999" |
                           household_income == "$25,000 to $29,999" |
                           household_income == "$30,000 to $34,999" |
                           household_income == "$35,000 to $39,999" |
                           household_income == "$40,000 to $44,999", "Low", 
                         "Middle")))

# Age
cleaned_data <- cleaned_data %>% 
  mutate(age_group = ifelse(age >= 18 & age<=25, "18 to 25", 
                             ifelse(age >= 26 & age <= 35, "26 to 35",
                                    ifelse(age >= 36 & age <= 45, "36 to 45", 
                                           ifelse(age >= 46 & age <= 55, "46 to 55", 
                                                  ifelse(age >= 56 & age <= 65, "56 to 65", 
                                                         ifelse(age >= 66 & age <= 75, "66 to 75", 
                                                                ifelse(age >= 76 & age <= 85, "76 to 85",
                                                                       "86 to 95"))))))))

# Saving the survey/sample data as a csv file in my
# working directory
write_csv(cleaned_data, "outputs/survey_data.csv")
