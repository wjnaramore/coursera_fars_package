library(testthat)
library(fars)

#this test loads in the 2013 fars data file, and checks the length of the STATE column
data = fars_read(make_filename(2013))
expect_that(length(data$STATE),equals(30202))
