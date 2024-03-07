# Test to save csv on Cosmos server
library(tidyverse)

test <- tibble(a = c(1, 2, 3), b = c('one', 'two', 'three'))

write_csv(test, 'test.csv')