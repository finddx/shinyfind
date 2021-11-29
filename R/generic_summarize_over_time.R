# This function summarizes country data over a time range
#' @export
generic_summarize_over_time_range <- function (data_all, country_info_matrix, start_date, end_date) {
  #as.Date(max(data_all$time) - 367),
  #as.Date(max(data_all$time) - 2)
  data_filtered <-
    data_all |>
    select(-ends_with("_orig")) |>
    filter(set == "country") |>
    select(-name, -set)
  
  # if time_range
  data_table  <-
    data_filtered |>
    filter(dplyr::between(
      time,
      as.Date(start_date), 
      as.Date(end_date)
    )) |>
    group_by(unit) |>
    summarize(
      pop_100k               = mean(pop_100k                  , na.rm = TRUE),
      pop                    = mean(pop                       , na.rm = TRUE),
      avg_cap_new_cases      = mean(all_new_cases / pop       , na.rm = TRUE),
      sum_cap_new_cases      = sum(all_new_cases / pop        , na.rm = TRUE),
      avg_cap_new_deaths     = mean(all_new_deaths / pop      , na.rm = TRUE),
      sum_cap_new_deaths     = sum(all_new_deaths / pop       , na.rm = TRUE),
      avg_cap_new_tests      = mean(all_new_tests / pop       , na.rm = TRUE),
      sum_cap_new_tests      = sum(all_new_tests / pop        , na.rm = TRUE),
      avg_all_new_cases      = mean(all_new_cases             , na.rm = TRUE),
      sum_all_new_cases      = sum(all_new_cases              , na.rm = TRUE),
      avg_all_new_deaths     = mean(all_new_deaths            , na.rm = TRUE),
      sum_all_new_deaths     = sum(all_new_deaths             , na.rm = TRUE),
      avg_all_new_tests      = mean(all_new_tests             , na.rm = TRUE),
      sum_all_new_tests      = sum(all_new_tests              , na.rm = TRUE),
      avg_pos                = avg_all_new_cases / avg_all_new_tests,
      avg_cap100k_new_cases  = mean(all_new_cases / pop_100k  , na.rm = TRUE),
      sum_cap100k_new_cases  = sum(all_new_cases / pop_100k   , na.rm = TRUE),
      avg_cap100k_new_deaths = mean(all_new_deaths / pop_100k , na.rm = TRUE),
      sum_cap100k_new_deaths = sum(all_new_deaths / pop_100k  , na.rm = TRUE),
      avg_cap100k_new_tests  = mean(all_new_tests / pop_100k  , na.rm = TRUE),
      sum_cap100k_new_tests  = sum(all_new_tests / pop_100k   , na.rm = TRUE),
      .groups = "drop"
    )
  
  
  data_table_full <- 
    data_table |>
    left_join(country_info_matrix, by = "unit")
  
  data_table_full
}


# This function summarizes country data on a single specific date

#' @export
generic_summarize_on_this_day <- function(data_all, country_info_matrix, day) {
  data_filtered <-
    data_all |>
    select(-ends_with("_orig")) |>
    filter(set == "country") |>
    select(-name, -set)
  
  # if single_day
  data_table  <-
    data_filtered |>
    filter(
      time == as.Date(day)
    ) |>
    mutate(
      cap100k_cum_cases      = all_cum_cases / pop_100k,
      cap100k_new_cases      = all_new_cases / pop_100k,
      cap100k_cum_deaths     = all_cum_deaths / pop_100k,
      cap100k_new_deaths     = all_new_deaths / pop_100k,
      cap100k_cum_tests      = all_cum_tests / pop_100k,
      cap100k_new_tests      = all_new_tests / pop_100k
    )
  data_table_full <- 
    data_table |>
    left_join(country_info_matrix, by = "unit")
  
  data_table_full
}