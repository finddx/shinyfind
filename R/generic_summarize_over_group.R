# This function summarizes country data for a single day by
# group (e.g. income, continent, who_region)

#' @export
generic_summarize_over_group <- function(data) {
  
  data |>
    mutate(unit = group, name = group) |>
    group_by(unit) |>
    summarize(
      name = name[1],
      group = group[1],
      all_new_cases = sum(all_new_cases, na.rm = TRUE),
      all_new_deaths = sum(all_new_deaths, na.rm = TRUE),
      all_new_tests = sum(all_new_tests, na.rm = TRUE),
      all_cum_cases = sum(all_cum_cases, na.rm = TRUE),
      all_cum_deaths = sum(all_cum_deaths, na.rm = TRUE),
      all_cum_tests = sum(all_cum_tests, na.rm = TRUE),
      pop_100k = sum(pop_100k, na.rm = TRUE),
      pop = sum(pop, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      cap_new_cases = all_new_cases / pop,
      cap_new_deaths = all_new_deaths / pop,
      cap_new_tests = all_new_tests / pop,
      cap_cum_cases = all_cum_cases / pop,
      cap_cum_deaths = all_cum_deaths / pop,
      cap_cum_tests = all_cum_tests / pop,
      cap100k_new_cases = all_new_cases / pop_100k,
      cap100k_new_deaths = all_new_deaths / pop_100k,
      cap100k_new_tests = all_new_tests / pop_100k,
      pos = all_new_cases / all_new_tests
    ) |>
    filter(!is.na(name))
}


# This function summarizes country data for a time range by
# group (e.g. income, continent, who_region)

#' @export
generic_summarize_over_group_range <- function(data) {
  data |>
    filter(!is.na(group)) |>
    mutate(unit = group, name = group) |>
    group_by(unit) |>
    summarize(
      name = name[1],
      group = group[1],
      avg_all_new_cases = mean(avg_all_new_cases, na.rm = TRUE),
      avg_all_new_deaths   = mean(avg_all_new_deaths, na.rm   = TRUE),
      avg_all_new_tests    = mean(avg_all_new_tests, na.rm    = TRUE),
      sum_all_new_cases = sum(sum_all_new_cases, na.rm = TRUE),
      sum_all_new_deaths   = sum(sum_all_new_deaths, na.rm   = TRUE),
      sum_all_new_tests    = sum(sum_all_new_tests, na.rm    = TRUE),
      pop_100k = sum(pop_100k, na.rm = TRUE),
      pop = sum(pop, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      avg_cap_new_cases  = avg_all_new_cases   / pop,
      avg_cap_new_deaths = avg_all_new_deaths     / pop,
      avg_cap_new_tests  = avg_all_new_tests      / pop,
      sum_cap_new_cases  = sum_all_new_cases   / pop,
      sum_cap_new_deaths = sum_all_new_deaths     / pop,
      sum_cap_new_tests  = sum_all_new_tests      / pop,
      avg_cap100k_new_cases  = avg_all_new_cases   / pop_100k,
      avg_cap100k_new_deaths = avg_all_new_deaths     / pop_100k,
      avg_cap100k_new_tests  = avg_all_new_tests      / pop_100k,
      sum_cap100k_new_cases  = sum_all_new_cases   / pop_100k,
      sum_cap100k_new_deaths = sum_all_new_deaths     / pop_100k,
      sum_cap100k_new_tests  = sum_all_new_tests      / pop_100k,
      avg_pos = avg_all_new_cases / avg_all_new_tests
    ) |>
    filter(!is.na(name))
}
