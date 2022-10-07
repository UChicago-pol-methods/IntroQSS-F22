library(tidyverse)

# download the data from this website: https://data.cdc.gov/NCHS/Provisional-COVID-19-Deaths-by-Race-and-Hispanic-O/ks3g-spdg
# and read it in locally, using `read_csv()`
dat <- read_csv('../data/Provisional_COVID-19_Deaths_by_Race_and_Hispanic_Origin__and_Age.csv')

# check out the data using `dat`, `summary(dat)`, and `View(dat)`. 
dat
summary(dat)
# View(dat)

# filter the data so that you're just using data for the United states as a whole,
# and drop observations where the column `Race and Hispanic Origin Group` is 'Total Deaths'
dat <- dat %>% 
  filter(State == 'United States',
         `Race and Hispanic Origin Group` != 'Total Deaths')

# Consider the age groups in this data. Do you want to keep all of them? 
# Filter out any age groups you don't want to keep. 
table(dat$`Age group`)


dat <- dat %>% 
  filter(`Age group` %in% c('15-24 years', '25-34 years', '35-44 years',
                            '45-54 years', '55-64 years', '65-74 years',
                            '75-84 years', '85 years and over', 'All Ages'))
# create a data set, `dat0`, that is just the data where the column `Age group`
# is 'All Ages'
dat0 <- dat %>% 
  filter(`Age group` == 'All Ages')

# create another data set, `dat1`, that is the date where the column `Age group`
# is anything but 'All Ages'

dat1 <- dat %>% 
  filter(`Age group` != 'All Ages')

# in dat0, plot COVID-19 deaths by `Race and Hispanic Origin Group` using `geom_col`. 
# put the x axis labels at 45 degrees (you can google how to do this)
ggplot(dat0, aes(y = `COVID-19 Deaths`, 
                x = `Race and Hispanic Origin Group`)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# in dat1, plot COVID-19 deaths by `Race and Hispanic Origin Group` using `geom_col`,
# AND set the fill by `Age group`
ggplot(dat1, aes(y = `COVID-19 Deaths`, 
                x = `Race and Hispanic Origin Group`,
                fill = `Age group`)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

# Do the last plot again, but try using the argument `position = 'dodge'` in 
# `geom_col()`. Which do you like better? Why?
ggplot(dat1, aes(y = `COVID-19 Deaths`, 
                 x = `Race and Hispanic Origin Group`,
                 fill = `Age group`)) +
  geom_col(position = 'dodge') +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


# Try using `facet_grid()` where you have separate plots for each race and 
# hispanic origin group, with COVID deaths on the Y axis, and age on the X axis
ggplot(dat1, aes(y = `COVID-19 Deaths`, 
                 x = `Age group`,
                 fill = `Age group`)) +
  facet_grid(vars(`Race and Hispanic Origin Group`), scales = 'free') + 
  geom_col(position = 'identity') +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


# Which plot do you think is the best way to summarize this data? Why? 

# What other data would you need to say something about the relative risk for 
# by age and race/hispanic origin?

# Use ggsave() to save your best plot, and email it to mollyow@uchicago.edu 
# with this script, and the names of students in your group. 