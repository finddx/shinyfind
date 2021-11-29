# This function summarizes data_all by grouping based on a defined category
# (e.g. income, continent, etc) first and then summarizes over a time range.
# Here we take average once.
# This seems to provide better results when compared to summarizing over time
# first, and later over group (taking averages twice)
# 
#' @export
summarize_over_group_and_time_range <- function (data_all, country_last_update_info, start_date, end_date, grouped = FALSE, category = "income") {
  
  data_filtered <-
    data_all |>
    select(-ends_with("_orig")) |>
    filter(set == "country") |>
    select(-name, -set) |>
    left_join(country_last_update_info, by = "unit") |>
    filter(!is.na(last_update))
  
  # if time_range
  data_table  <-
    data_filtered |>
    filter(dplyr::between(
      time,
      as.Date(start_date), 
      as.Date(end_date)
    ))
  
  if (grouped) {
    data_table <-
      data_table |>
      rename_with(function(x) "group", {{ category }}) |>
      filter(!is.na(group)) |>
      mutate(unit = group, name = group)
  } else {
    data_table <-
      data_table |>
      mutate(group = unit)
  }
  
  data_table <-
    data_table |>
    group_by(unit) |>
    summarize(
      name = name[1],
      group = group[1],
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
  data_table
}

