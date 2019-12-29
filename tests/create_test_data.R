# simple script to create test data for this package
# output is CSV with schema (country, date, sales)

library(data.table)
library(stringi)
library(tidyr)

n_countries <- 3
countries <- stri_rand_strings(n_countries, 10, '[A-Z]')
dates <- seq(as.Date('2010-01-10'), as.Date('2010-01-30'), by="day")
dt <- tidyr::crossing(data.table(countries), data.table(dates))
dt[, sales := runif(.N, 0, 100) ]
fwrite(dt, "test_data.csv")







