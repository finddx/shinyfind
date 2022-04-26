library(dplyr)

. <- get_data_all()
data_all <- .$data_all
country_last_update_info <- .$country_last_update_info

colnames_expected <- c(
  "set", "name", "unit", "time", "cum_tests_orig", "new_tests_orig",
  "pop_100k", "pop", "new_cases_orig", "new_deaths_orig", "cap_cum_cases",
  "cap_new_cases", "cap_cum_deaths", "cap_new_deaths", "cap_cum_tests",
  "cap_new_tests", "all_cum_cases", "all_new_cases", "all_cum_deaths",
  "all_new_deaths", "all_cum_tests", "all_new_tests", "pos"
)

sets <- c("country", "income", "region", "who_region", "world")

test_that("data_all is structured as expected", {
  
  expect_true(
    all(
      names(data_all) == colnames_expected
    )
  )
  
  # check that the expected sets are present in the data
  expect_true(
    all(
      unique(data_all$set) == sets
    )
  )
  
  # assert that countries that have NA in their last_update are not in data_all
  a <- country_last_update_info |>
    filter(!is.na(last_update)) |>
    arrange(name) |>
    pull(name)
  
  b <- data_all |>
    filter(set == 'country') |>
    filter(time == as.Date(Sys.Date() - 1)) |>
    arrange(name) |>
    pull(name)
    
  expect_true(
    all(
      a == b
    )
  ) 
    
})
