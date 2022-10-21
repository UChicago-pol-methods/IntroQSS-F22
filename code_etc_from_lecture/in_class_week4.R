library(tidyverse)

# Read in the data from github. 
file <- 'https://raw.githubusercontent.com/UChicago-pol-methods/IntroQSS-F22/main/data/Butler_Broockman_AJPS_2011_public.csv'
df <- read_csv(file)

# Explore the data. Use `View(df)`
# What do rows represent? How many observations are there?
# How many columns? What do you think the column `leg_party` represents? 
# `leg_republican`? `leg_black`?

# Go to https://isps.yale.edu/research/data and find the data archive for this 
# project. What is the population included in this data, and what are the 
# inclusion/exclusion criteria?

# In your own local data set, create a variable `D` that equals `1` if 
# `treat_deshawn` is 1 and 0 otherwise.

# As in class, create variables Y0 and Y1 to represent realized potential 
# outcomes when `D` is 0 and 1 respectively. 

# Calculate the means of Y0 *separately* among Democrats and Republicans. Do 
# they seem to have different responses under control?

# Calculate the difference in means estimates *separately* among Democrats
# and Republican legislators. Do they seem to have different treatment effects?
# Do you have any theories about why this might be the case?

