##### ---------------------------------------------------------------------####
# Code for Stats Class - Week 5
# Author: Marylis Fantoni
# E-mail: mfantoni@byu.edu
# Date 08/2025
##### ---------------------------------------------------------------------####
# 0.SYSTEM SETUP

if (!require("pacman")) {
  install.packages("pacman")
  library(pacman)
}
p_load(tidyverse, dplyr, data.table, ggplot2, fastDummies, readxl)

##### ---------------------------------------------------------------------####
# Goal 1: Calculate probabilities from a dataset

# let's first create a dataset (instead of loading one):
set.seed(123)

n <- 500

demographics <- data.frame(
  id = 1:n,
  gender = sample(c("Male", "Female"),
    size = n,
    replace = TRUE,
    prob = c(0.48, 0.52)
  ),
  marital_status = sample(c("Married", "Never Married"),
    size = n,
    replace = TRUE,
    prob = c(0.55, 0.45)
  ),
  age = round(rnorm(n, mean = 42, sd = 12)),
  race = sample(c("White", "Black", "Hispanic", "Asian", "Other"),
    size = n,
    replace = TRUE,
    prob = c(0.60, 0.13, 0.18, 0.06, 0.03)
  )
)

demographics$age[demographics$age < 18] <- 18
demographics$age[demographics$age > 85] <- 85

demographics <- as.data.frame(demographics)

# use a couple of codes to see the structure of your dataset.
# What did you learn about it?
summary(demographics)
glimpse(demographics)
names(demographics)

# The dataset has 500 rows and 5 variables:
#   gender, marital_status, age, race

# The gender variable has two levels:
#   Female and Male

# The marital_status variable has four levels:
#   Married, Divorced, Separated, and Never Married

# The age variable has a range of 18 to 85 years

# The race variable has five levels:
#   White, Black, Hispanic, Asian, and Other

# now let's create a contingency table similar to the one from our class:
table(demographics$gender, demographics$marital_status)

# MARGINAL PROBABILITY
# what's the probability a randomly selected adult is female?
mean(demographics$gender == "Female")
# The probability that a randomly selected adult is female is 0.542.

# what's the probability a randomly selected adult is married?
mean(demographics$marital_status == "Married")
# The probability that a randomly selected adult is married is 0.544.

# JOINT PROBABILITY
# what's the probability an adult is Female and Married? (independent)
mean(demographics$gender == "Female" & demographics$marital_status == "Married")
# The probability that an adult is Female and Married is 0.294.

# what's the probability an adult is Hispanic and Never Married?
mean(demographics$race == "Hispanic" & demographics$marital_status == "Never Married")
# The probability that an adult is Hispanic and Never Married is 0.090.

# UNION PROBABILITY
# what's the probability an adult is Male or Never Married?
mean(demographics$gender == "Male" | demographics$marital_status == "Never Married")
# The probability that an adult is Male or Never Married is 0.706.

# what's the probability an adult is under 30 or Asian?
mean(demographics$age < 30 | demographics$race == "Asian")
# The probability that an adult is under 30 or Asian is 0.230.


# CONDITIONAL PROBABILITY
# what's the probability an adult is Married given that they are Female?
mean(demographics$marital_status[demographics$gender == "Female"] == "Married")
# The probability that an adult is Married given Female is 0.5424354.

# what's the probability an adult is Never Married given that they are under 30?
mean(demographics$marital_status[demographics$age < 30] == "Never Married")
# The probability that an adult is Never Married given under 30 is 0.4880952.


# now let's create a conditional probability table
prop.table(table(demographics$gender, demographics$marital_status), margin = 1)

# this is a joint probability table (independent)
prop.table(table(demographics$gender, demographics$marital_status))


# why are these tables different?
# Because `margin = 1` is used to calculate the marginal probabilities,
# which are the probabilities of each variable independently.
# In contrast, not using `margin = 1` calculates the joint probabilities,
# which are the probabilities of both variables occurring together.

##### ---------------------------------------------------------------------####
# Goal 2: Advanced cleaning

# create a second dataset to merge it back with the first one
education <- data.frame(
  id = 1:n,
  education_level = sample(
    c("Less than HS", "High School", "Some College", "Bachelor's", "Graduate"),
    size = n,
    replace = TRUE,
    prob = c(0.10, 0.28, 0.27, 0.22, 0.13)
  )
)

education <- as.data.frame(education)

# now merge them
dt <- merge(demographics, education, by = "id")

# sorting data
dt <- dt[order(dt$age), ]

# checking for duplicates
duplicated(dt$id)

# using dplyr and summarize
dt |>
  group_by(gender) |>
  summarize(mean_age = mean(age, na.rm = TRUE))

dt |>
  filter(age == "18") |>
  summarize(count = n())


##### ---------------------------------------------------------------------##
# Goal 3: Now create some probabilities from your own data

# calculate marginal, joint, union, and conditional probabilities from your data and interpret them with one sentence


# generate a conditional and a joint probability table and interpret them wiht one sentence
