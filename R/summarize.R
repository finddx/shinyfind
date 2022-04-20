#' Summarize Data over Time and Groups
#'
#' All apps should use this functions to do aggregations!
#'
#' Aggregation over period: If data is missing during more than 25% of the most
#' recent observations, the period is considered incomplete, no aggregated
#' value is computed.
#'
#' Aggregation over group: Groups aggregations use all the countries for which
#' data is available. If a ratio is computed (e.g., per-captia measures,
#' positivity rate), we only consider observations that have values both for
#' the nominator and the denominator. E.g., in order to calculate tests
#' per-capita for a continent, a country is only used if reports both test and
#' population data.
#'
#' This is a deviation from an older discussion in: https://github.com/dsbbfinddx/shinyfindapps/issues/142#issuecomment-1023435405
#'
#' See discussiofn in: https://github.com/finddx/shinyfindapps/issues/142#issuecomment-1023435405
#' @export
#' @examples
#'
#' library(shinyfind)
#' library(dplyr)
#' . <- get_data_all()
#' data_all <- .$data_all
#' country_last_update_info <- .$country_last_update_info
#'
#' time_start <- as.Date(max(data_all$time) - 367)
#' time_end <- as.Date(max(data_all$time) - 2)
#' data_filtered <-
#'   data_all |>
#'   filter(set == "country") |>
#'   select(-name, -set) |>
#'   left_join(country_last_update_info, by = "unit") |>
#'   filter(!is.na(last_update))|>
#'   filter(dplyr::between(
#'     time,
#'     as.Date(time_start),
#'     as.Date(time_end)
#'   ))
#' data_summarized_over_time <- shinyfind::summarize_over_time(data_filtered)
#' shinyfind::summarize_over_group(data_summarized_over_time, "income")
#' shinyfind::summarize_over_group(data_summarized_over_time, "who_region")
summarize_over_time <- function (data_filtered) {

  data_filtered <-
    data_filtered |>
    arrange(unit, time)

  data_grouped <- 
    if ("period" %in% colnames(data_filtered)) {
      group_by(data_filtered, unit, period)
    } else {
      group_by(data_filtered, unit)
    }
  
  data_summarized_over_time <-
    data_grouped |> 
    mutate(world = "world") |>
    summarize(
      country = name[1],
      name = name[1],
      unit = unit[1],
      pop_100k                 = robust_time_series_mean(pop_100k),
      pop                      = robust_time_series_mean(pop),
      avg_cap_new_cases        = robust_time_series_mean(all_new_cases / pop),
      sum_cap_new_cases        = robust_time_series_sum(all_new_cases / pop),
      avg_cap_new_deaths       = robust_time_series_mean(all_new_deaths / pop),
      sum_cap_new_deaths       = robust_time_series_sum(all_new_deaths / pop),
      avg_cap_new_tests        = robust_time_series_mean(all_new_tests / pop),
      sum_cap_new_tests        = robust_time_series_sum(all_new_tests / pop),
      avg_all_new_cases        = robust_time_series_mean(all_new_cases),
      sum_all_new_cases        = robust_time_series_sum(all_new_cases),
      avg_all_new_deaths       = robust_time_series_mean(all_new_deaths),
      sum_all_new_deaths       = robust_time_series_sum(all_new_deaths),
      avg_all_new_tests        = robust_time_series_mean(all_new_tests),
      sum_all_new_tests        = robust_time_series_sum(all_new_tests),
      avg_pos                  = pmin(avg_all_new_cases / avg_all_new_tests, 1),
      all_cum_cases            = all_cum_cases[n()],
      all_cum_deaths           = all_cum_deaths[n()],
      all_cum_tests            = all_cum_tests[n()],
      cap_cum_cases            = all_cum_cases[n()] / pop,
      cap_cum_deaths           = all_cum_deaths[n()] / pop,
      cap_cum_tests            = all_cum_tests[n()] / pop,
      cap100k_cum_cases        = all_cum_cases[n()] / pop_100k,
      cap100k_cum_deaths       = all_cum_deaths[n()] / pop_100k,
      cap100k_cum_tests        = all_cum_tests[n()] / pop_100k,
      avg_cap100k_new_cases    = robust_time_series_mean(all_new_cases / pop_100k),
      sum_cap100k_new_cases    = robust_time_series_sum(all_new_cases / pop_100k),
      avg_cap100k_new_deaths   = robust_time_series_mean(all_new_deaths / pop_100k),
      sum_cap100k_new_deaths   = robust_time_series_sum(all_new_deaths / pop_100k),
      avg_cap100k_new_tests    = robust_time_series_mean(all_new_tests / pop_100k),
      sum_cap100k_new_tests    = robust_time_series_sum(all_new_tests / pop_100k),
      continent = continent[1],
      income = income[1],
      who_region = who_region[1],
      world = world[1],
      .groups = "drop"
    )
  data_summarized_over_time
}


# compute mean if data has reported until 'recently', e.g, until more than 3/4
# of the period.
# robust_mean(c(NA, NA, 1, 2, NA, NA))
# robust_mean(c(NA, NA, 1, 2, 1, NA))
# robust_mean(c(NA, NA))
robust_time_series_aggregation <- function(x, threshold = 0.75, fun) {

  pos_of_last_value <- tail(which(!is.na(x)), 1)
  if (length(pos_of_last_value) == 0) return(NA_real_)

  ans <-
    if (pos_of_last_value / length(x) < threshold) {
      NA
    } else {
      fun(x, na.rm = TRUE)
    }

  ans

}

robust_time_series_mean <- function(x, threshold = 0.75, fun)  {
  robust_time_series_aggregation(x, threshold = threshold, fun = mean)
}

robust_time_series_sum <- function(x, threshold = 0.75, fun)  {
  robust_time_series_aggregation(x, threshold = threshold, fun = sum)
}




#' Step 2: Summarize Data Over Groups
#' @name summarize_over_group
#' @export
summarize_over_group <- function (data_summarized_over_time, group = NULL) {
  
  is.periodic <- "period" %in% colnames(data_summarized_over_time)
  
  data_summarized_group <-
    data_summarized_over_time |>
    rename_with(function(x) "group", {{ group }}) |>
    filter(!is.na(group))
  
  data_grouped <- 
    if (is.periodic) {
      group_by(data_summarized_group, group, period)
    } else {
      group_by(data_summarized_group, group)
    }
  
  if ((is.null(group) || group == "country") & !is.periodic)  {
    colnames(data_summarized_over_time) <- 
      colnames(data_summarized_over_time) |>
      str_replace_all(c("avg_" = ""))
    
    ans <-
      data_summarized_over_time |>
      select(-starts_with("sum_")) |>
      arrange(name)
    return(ans)
  }
  
  if(group == "country" & is.periodic) {
    return(data_summarized_over_time)
  }
  
  stopifnot(inherits(group, "character"))
  
  data_summarized_over_group <-
    data_grouped |>
    summarize(
      unit = group[1],
      name = group[1],

      avg_cap_new_cases      = robust_ratio(avg_all_new_cases, pop),
      sum_cap_new_cases      = robust_ratio(sum_all_new_cases, pop),
      avg_cap_new_deaths     = robust_ratio(avg_all_new_deaths, pop),
      sum_cap_new_deaths     = robust_ratio(sum_all_new_deaths, pop),
      avg_cap_new_tests      = robust_ratio(avg_all_new_tests, pop),
      sum_cap_new_tests      = robust_ratio(sum_all_new_tests, pop),

      avg_all_new_cases      = mean(avg_all_new_cases       , na.rm = TRUE),
      sum_all_new_cases      = sum(sum_all_new_cases        , na.rm = TRUE),
      avg_all_new_deaths     = mean(avg_all_new_deaths      , na.rm = TRUE),
      sum_all_new_deaths     = sum(sum_all_new_deaths       , na.rm = TRUE),
      avg_all_new_tests      = mean(avg_all_new_tests       , na.rm = TRUE),
      sum_all_new_tests      = sum(sum_all_new_tests        , na.rm = TRUE),
      avg_pos                = robust_ratio(avg_all_new_cases, avg_all_new_tests),

      cap_cum_cases          = robust_ratio(all_cum_cases, pop),
      all_cum_cases          = sum(all_cum_cases, na.rm = TRUE),
      cap_cum_deaths         = robust_ratio(all_cum_deaths, pop),
      all_cum_deaths         = sum(all_cum_deaths, na.rm = TRUE),
      cap_cum_tests          = robust_ratio(all_cum_tests, pop),
      all_cum_tests          = sum(all_cum_tests, na.rm = TRUE),

      cap100k_cum_cases      = robust_ratio(all_cum_cases, pop_100k),
      cap100k_cum_deaths     = robust_ratio(all_cum_deaths, pop_100k),
      cap100k_cum_tests      = robust_ratio(all_cum_tests, pop_100k),

      avg_cap100k_new_cases  = robust_ratio(avg_all_new_cases, pop_100k),
      sum_cap100k_new_cases  = robust_ratio(sum_all_new_cases, pop_100k),
      avg_cap100k_new_deaths = robust_ratio(avg_all_new_deaths, pop_100k),
      sum_cap100k_new_deaths = robust_ratio(sum_all_new_deaths, pop_100k),
      avg_cap100k_new_tests  = robust_ratio(avg_all_new_tests, pop_100k),
      sum_cap100k_new_tests  = robust_ratio(sum_all_new_tests, pop_100k),

      # needs to be done in the end, pop and pop_100k above should be from non summarized values
      pop_100k               = sum(pop_100k,  na.rm = TRUE),
      pop                    = sum(pop, na.rm = TRUE),

      .groups = "drop"
    ) |>
    arrange(name)
  
  if (group == "income") {
    data_summarized_over_group <-
      data_summarized_over_group |>
      arrange(factor(group, levels = c("Low", "Lower middle", "Upper middle", "High")))
  }
  
  data_summarized_over_group <-
    data_summarized_over_group |>
    select(-group) |>
    filter(!is.na(name))
  
  data_summarized_over_group
}


# Only use units that appear above an below the line
#
# robust_ratio(c(2, 2, 3, NA), c(2, 2, NA, 100))
robust_ratio <- function(nominator, denominator) {
  is_missing <- is.na(nominator) | is.na(denominator)
  sum(nominator[!is_missing], na.rm = TRUE) / sum(denominator[!is_missing], na.rm = TRUE)
}



