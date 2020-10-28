#### Preamble ####
# Purpose: Prepare and clean the census data downloaded from IPUMS USA, University of Minnesota, www.ipums.org.
# Author: Pamela De Vera, Hyoeun Park
# Data: 22 October 2020
# Contact: pamela.devera@mail.utoronto.ca, hyoeun.park@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("C:/Users/Sammi-Jo/Desktop/PS3")
raw_data <- read_dta("inputs/usa_00001.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(region,
         sex, 
         age, 
         race, 
         hispan,
         citizen,
         educd,
         ftotinc)

# Filter out for respondents not eligible to vote since target is the voting population
reduced_data <-
  reduced_data %>%
  filter(citizen != 'not a citizen') %>%
  mutate(citizen = 'citizen')

# Convert age to integer values
reduced_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") %>%
  filter(age != "90 (90+ in 1980 and 1990)")

reduced_data$age <- as.integer(reduced_data$age)

# Changing to integer made every age one year higher, so we need to adjust.
# Filtering for people only of legal voting age
reduced_data <-
  reduced_data %>%
  mutate(age = age - 1) %>%
  filter(age >= 18)



#Change region values to general region instead of divisions within region.
reduced_data <-
  reduced_data %>%
  mutate(region = case_when(
    region %in% c('new england division','middle atlantic division')~1, #Northeast
    region %in% c('east north central div','west north central div')~2, #Midwest
    region %in% c('south atlantic division','east south central div', 'west south central div')~3, #South
    region %in% c('mountain division','pacific division')~4)) #West


#Change education values
reduced_data <-
  reduced_data %>%
  mutate(educd = case_when(
    educd %in% c(
      'no schooling completed','nursery school, preschool','kindergarten',
      'grade 1','grade 2','grade 3','grade 4','grade 5','grade 6','grade 7',
      'grade 8','grade 9','grade 10','grade 11','12th grade, no dilploma',
      'regular highschool diploma','ged or alternative credential',
      'some college, but less than 1 year','1 or more years of college credit, no degree')~
      'Primary/Secondary',
    educd %in% c("associate's degree, type not specified","bachelor's degree")~
      'Associate/Bachelor',
    educd %in% c("master's degree","professional degree beyond a bachelor's degree",
                 'doctoral degree')~ 'Master/Doctorate'))

#Filter n/a responses in education
reduced_data <- na.omit(reduced_data)

# Merge race category into 5 categories: white, black, indigenous peoples,
# asian/pacific islander, other.
reduced_data <-
  reduced_data %>%
    mutate(race = case_when(
    race == 'white'~'White',
    race == 'black/african american/negro'~'Black',
    race == 'american indian or alaska native'~'Indigenous Peoples',
    race %in% c('chinese', 'japanese', 'other asian or pacific islander')~'Asian/Pacific Islander',
    race %in% c('other race, nec','two major races','three or more major races')~'Other'))
    
# Merge hispan values into: hispanic and not hispanic
reduced_data <-
  reduced_data %>%
  mutate(hispan = ifelse(hispan == 'not hispanic','not hispanic','hispanic'))


#Merge income based on socio-economic status: low, middle, and high
reduced_data <-
  reduced_data %>%
  mutate(ftotinc = ifelse(
    ftotinc %in% (0:44999),"Low",
    ifelse(ftotinc%in% (45000:124999),"Middle",
           "High")))


count_data <- 
  reduced_data %>%
  count(region,
        sex, 
        age, 
        race, 
        hispan,
        educd,
        ftotinc)

# Saving the census data as a csv file in my
# working directory
write_csv(count_data, "outputs/census_data.csv")

         