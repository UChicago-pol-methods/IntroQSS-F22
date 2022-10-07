library(tidyverse)

# download the data from this website: https://data.cdc.gov/NCHS/Provisional-COVID-19-Deaths-by-Race-and-Hispanic-O/ks3g-spdg
# and read it in locally, using `read_csv()`
# dat <- read_csv('<path to your data>')

# check out the data using `dat`, `summary(dat)`, and `View(dat)`. 

# filter the data so that you're just using data for the United states as a whole,
# and drop observations where the column `Race and Hispanic Origin Group` is 'Total Deaths'

# Consider the age groups in this data. Do you want to keep all of them? 
# Filter out any age groups you don't want to keep. 

# create a data set, `dat0`, that is just the data where the column `Age group`
# is 'All Ages'

# create another data set, `dat1`, that is the data where the column `Age group`
# is anything but 'All Ages'


# in dat0, plot COVID-19 deaths by `Race and Hispanic Origin Group` using `geom_col`. 
# put the x axis labels at 45 degrees (you can google how to do this)

# in dat1, plot COVID-19 deaths by `Race and Hispanic Origin Group` using `geom_col`,
# AND set the fill by `Age group`


# Do the last plot again, but try using the argument `position = 'dodge'` in 
# `geom_col()`. Which do you like better? Why?


# Try using `facet_grid()` where you have separate plots for each race and 
# hispanic origin group, with COVID deaths on the Y axis, and age on the X axis


# Which plot do you think is the best way to summarize this data? Why? 

# What other data would you need to say something about the relative risk for 
# by age and race/hispanic origin?

# Add any features/changes to color/aesthetics/labels. 
# Use ggsave() to save your best plot, and email it to mollyow@uchicago.edu 
# with this script, and the names of students in your group. 
