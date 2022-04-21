
library(dplyr)
. <- get_data_all()
data_all <- .$data_all
country_last_update_info <- .$country_last_update_info



test_that("Summarize of period for one day gives same result as already in data", {

  time_start <- as.Date("2021-06-17")
  time_end <- time_start
  data_filtered <-
    data_all |>
    filter(set == "country") |>
    select(-name, -set) |>
    left_join(country_last_update_info, by = "unit") |>
    filter(!is.na(last_update))|>
    filter(dplyr::between(
      time,
      as.Date(time_start),
      as.Date(time_end)
    ))
  data_summarized_over_one_day <- shinyfind::summarize_over_time(data_filtered)


  a <- data_all |>
    filter(set == "country") |>
    filter(time == time_start) |>
    filter(unit == "ARE") |>
    pull(all_cum_cases)


  b <- data_summarized_over_one_day |>
    filter(unit == "ARE") |>
    pull(all_cum_cases)

  expect_equal(a, b)

})


# Christophs expecation

# cummulative numbers are not aligned with new numbers


# test_that("Sum over period is equal to cummulative value at the end of a period", {

#   time_start <- as.Date("2020-01-01")
#   time_end <- as.Date("2021-06-17")
#   data_filtered <-
#     data_all |>
#     filter(set == "country") |>
#     select(-name, -set) |>
#     left_join(country_last_update_info, by = "unit") |>
#     filter(!is.na(last_update))|>
#     filter(dplyr::between(
#       time,
#       as.Date(time_start),
#       as.Date(time_end)
#     ))
#   data_summarized_over_one_day <- shinyfind::summarize_over_time(data_filtered)

#   a <- data_summarized_over_one_day |>
#     filter(unit == "ARE") |>
#     pull(all_cum_tests)

#   b <- data_summarized_over_one_day |>
#     filter(unit == "ARE") |>
#     pull(sum_all_new_tests)

#   expect_equal(a, b)

#   `actual`: 54259480
# `expected`:   117395

# })





# test_that("Sum over period is equal to cummulative value at the end of a period", {

#   time_start <- as.Date("2020-01-01")
#   time_end <- as.Date("2021-06-17")
#   data_filtered <-
#     data_all |>
#     filter(set == "country") |>
#     select(-name, -set) |>
#     left_join(country_last_update_info, by = "unit") |>
#     filter(!is.na(last_update))|>
#     filter(dplyr::between(
#       time,
#       as.Date(time_start),
#       as.Date(time_end)
#     ))
#   data_summarized_over_one_day <- shinyfind::summarize_over_time(data_filtered)

#   a <- data_summarized_over_one_day |>
#     filter(unit == "CHE") |>
#     pull(all_cum_tests)

#   b <- data_summarized_over_one_day |>
#     filter(unit == "CHE") |>
#     pull(sum_all_new_tests)

#   expect_equal(a, b)

# #   `actual`: 8058247
# # `expected`:   17021
# })


