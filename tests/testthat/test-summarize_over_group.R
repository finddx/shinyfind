library(dplyr)

. <- get_data_all()
data_all <- .$data_all
country_last_update_info <- .$country_last_update_info

colnames_expected <- c(
  "unit","name","pos","cap_new_cases","sum_cap_new_cases","cap_new_deaths",
  "sum_cap_new_deaths","cap_new_tests","sum_cap_new_tests","cap100k_new_cases",
  "sum_cap100k_new_cases","cap100k_new_deaths","sum_cap100k_new_deaths",
  "cap100k_new_tests","sum_cap100k_new_tests","all_new_cases","sum_all_new_cases",
  "all_new_deaths","sum_all_new_deaths","all_new_tests","sum_all_new_tests",
  "cap_cum_cases","cap_cum_deaths","cap_cum_tests","cap100k_cum_cases",
  "cap100k_cum_deaths","cap100k_cum_tests","all_cum_cases","all_cum_deaths",
  "all_cum_tests","pop_100k","pop"
)

test_that("Summarize by group for a single day", {
  
  time_start <- as.Date("2021-06-17")
  time_end <- time_start
  data_filtered <-
    data_all |>
    filter(set == "country") |>
    select(-name, -set) |>
    left_join(country_last_update_info, by = "unit") |>
    filter(time == time_start)
  data_summarized_over_one_day <- shinyfind::summarize_over_time(data_filtered)
  data_group_single_day <- shinyfind::summarize_over_group(data_summarized_over_one_day, "income")
  
  
  a <- data_all |>
    filter(set == "income") |>
    filter(time == time_start) |>
    filter(unit == "High") |>
    pull(all_cum_cases)
  
  
  b <- data_group_single_day |>
    filter(unit == "High") |>
    pull(all_cum_cases)
  
  expect_equal(a, b)
  
  expect_s3_class(data_group_single_day, "data.frame")

  expect_true(
    all(
      names(data_group_single_day) == colnames_expected
    )
  )
  
})
