#### Preamble ####
# Purpose: Prepare and clean the census data downloaded from IPUMS USA, University of Minnesota, www.ipums.org.
# Author: Pamela De Vera, Hyoeun Park
# Data: 22 October 2020
# Contact: pamela.devera@mail.utoronto.ca, hyoeun.park@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the IPUMS USA data and saved it to inputs


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
# setwd("~/Desktop/PS3")
raw_data <- read_dta("inputs/usa_00002.dta.gz")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Selecting variables of interest
reduced_data <-
  raw_data %>% 
  dplyr::select(region,
         sex, 
         age, 
         race, 
         hispan,
         citizen,
         educd,
         hhincome)

# Filter out for respondents not eligible to vote since target is the voting population
reduced_data <-
  reduced_data %>%
  filter(citizen != 'not a citizen') %>%
  mutate(citizen = 'citizen')

# Change sex values to match survey data
reduced_data <-
  reduced_data %>%
  mutate(sex = ifelse(sex == 'male','Male','Female'))

# Convert age to integer values
reduced_data <- 
  reduced_data %>% 
  filter(age != "less than 1 year old") 

reduced_data$age <- as.integer(reduced_data$age)

# Changing to integer made every age one year higher, so we need to adjust.
# Filtering for people only of legal voting age
reduced_data <-
  reduced_data %>%
  mutate(age = age - 1) %>%
  filter(age >= 18) %>%
  filter(age <=95)

# Change age into age groups
reduced_data <-
  reduced_data %>%
  mutate(age = case_when(
    age %in% (18:25)~'18 to 25',
    age %in% (26:35)~'26 to 35',
    age %in% (36:45)~'36 to 45',
    age %in% (46:55)~'46 to 55',
    age %in% (56:65)~'56 to 65',
    age %in% (66:75)~'66 to 75',
    age %in% (76:85)~'76 to 85',
    age %in% (86:95)~'86 to 95'))


#Change region values to general region instead of divisions within region.
reduced_data <-
  reduced_data %>%
  mutate(region = case_when(
    region %in% c('new england division','middle atlantic division')~'Northeast', 
    region %in% c('east north central div','west north central div')~'Midwest', 
    region %in% c('south atlantic division','east south central div', 'west south central div')~'South', 
    region %in% c('mountain division','pacific division')~'West'))


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

# Merge race category into 5 categories: white, black, indigenous,
# asian/pacific islander, other.
reduced_data <-
  reduced_data %>%
    mutate(race = case_when(
    race == 'white'~'White',
    race == 'black/african american/negro'~'Black',
    race == 'american indian or alaska native'~'Indigenous',
    race %in% c('chinese', 'japanese', 'other asian or pacific islander')~'Asian/Pacific Islander',
    race %in% c('other race, nec','two major races','three or more major races')~'Other'))
    
# Merge hispan values into: hispanic (1) and not hispanic (0)
reduced_data <-
  reduced_data %>%
  mutate(hispan = ifelse(hispan == 'not hispanic',0,1))


#Merge income based on socio-economic status: low, middle, and high
reduced_data <-
  reduced_data %>%
  mutate(hhincome = ifelse(
    hhincome %in% (0:44999),"Low",
    ifelse(hhincome %in% (45000:124999),"Middle",
           "High")))

# Change variable names to match survey data
names(reduced_data)[names(reduced_data) == 'hhincome'] <- 'income_class'
names(reduced_data)[names(reduced_data) == 'hispan'] <- 'is_hispanic'
names(reduced_data)[names(reduced_data) == 'educd'] <- 'education_level'
names(reduced_data)[names(reduced_data) == 'age'] <- 'age_group'

# Count the observations in each cell sorted by age group, income class, region,
# race, if hispanic, sex, and education level
count_data <- 
  reduced_data %>%
  count(region,
        sex, 
        age_group, 
        race, 
        is_hispanic,
        education_level,
        income_class)

# Saving the census data as a csv file in 
# working directory
write_csv(count_data, "outputs/census_data.csv")
