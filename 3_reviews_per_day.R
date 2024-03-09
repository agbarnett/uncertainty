# 3_reviews_per_day.R
# How many reviews per day does F1000 get?
# no longer used, needed for sample size
# Sep 2021
library(dplyr)

# from 0_read_api.R
load('data/historic.RData') 

#
start = as.Date('2020-01-01') # just since 2020
all_dates = data.frame(date = seq(start, today, 1)) # range of dates to consider, up to when data was collected
daily_counts = filter(all_reviews, date >= as.Date('2020-01-01')) %>%
  group_by(date) %>%
  tally() %>% # count per day
  right_join(all_dates, by='date') %>%
  mutate(n = ifelse(is.na(n), 0, n))
summary(daily_counts$n)

# quick checks
plot(daily_counts$date, daily_counts$n)
barplot(table(daily_counts$n))
table(all_reviews$journal)