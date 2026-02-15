##### ---------------------------------------------------------------------####
# Code for Stats Class - Week 5
# Author: Marylis Fantoni
# E-mail: mfantoni@byu.edu
# Date: 08/2025
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

# use a couple of codes to see the structure of your dataset. What did you learn about it?
summary(demographics)
glimpse(demographics)
names(demographics)

# now let's create a contingency table similar to the one from our class:
table(data$gender, data$marital_status)

# MARGINAL PROBABILITY
# what's the probability a randomly selected adult is female?
mean(data$gender == "Female")

# what's the probability a randomly selected adult is married?


# JOINT PROBABILITY
# what's the probability an adult is Female and Married? (independent)
mean(data$gender == "Female" & data$marital_status == "Married")

# what's the probability an adult is Hispanic and Never Married?


# UNION PROBABILITY
# what's the probability an adult is Male or Never Married?
mean(data$gender == "Male" | data$marital_status == "Never Married")

# what's the probability an adult is under 30 or Asian?


# CONDITIONAL PROBABILITY
# what's the probability an adult is Married given that they are Female?
mean(data$marital_status[data$gender == "Female"] == "Married")

# what's the probability an adult is Never Married given that they are under 30?


# now let's create a conditional probability table
prop.table(table(data$gender, data$marital_status), margin = 1)

# this is a joint probability table (independent)
prop.table(table(data$gender, data$marital_status))


# why are these tables different?


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
dt %>%
  group_by(gender) %>%
  summarize(mean_age = mean(age, na.rm = TRUE))

dt %>%
  filter(age == "18") %>%
  summarize(count = n())


##### ---------------------------------------------------------------------##
# Goal 3: Now create some probabilities from your own data

# calculate marginal, joint, union, and conditional probabilities from your data and interpret them with one sentence


# generate a conditional and a joint probability table and interpret them wiht one sentence
