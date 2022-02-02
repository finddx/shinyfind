#' Summarize Data over Time and Groups
#'
#' All apps should use this functions to do aggregations!
#'
#' In principle first it should be: (1) summarise by time (examples time range given
#' below for reference) and then (2) summarise by group.
#'
#' The avg_cap_new_tests of the groups should be calculated using median and not
#' mean.
#'
#' Problem is that if we calculate the means of groups using the sum of tests
#' divided by the sums of the population in all countries of the groups,
#' countries with low tests and high population skew downwards the mean and
#' vice versa.
#'
#' See discussiofn in: https://github.com/dsbbfinddx/shinyfindapps/issues/142#issuecomment-1023435405
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

  data_summarized_over_time <-
    data_filtered |>
    group_by(unit) |>
    summarize(
      name = name[1],
      pop_100k                 = mean(pop_100k, na.rm = TRUE),
      pop                      = mean(pop, na.rm = TRUE),
      avg_cap_new_cases        = mean(all_new_cases / pop       , na.rm = TRUE),
      sum_cap_new_cases        = sum(all_new_cases / pop        , na.rm = TRUE),
      avg_cap_new_deaths       = mean(all_new_deaths / pop      , na.rm = TRUE),
      sum_cap_new_deaths       = sum(all_new_deaths / pop       , na.rm = TRUE),
      avg_cap_new_tests        = mean(all_new_tests / pop       , na.rm = TRUE),
      sum_cap_new_tests        = sum(all_new_tests / pop        , na.rm = TRUE),
      avg_all_new_cases        = mean(all_new_cases             , na.rm = TRUE),
      sum_all_new_cases        = sum(all_new_cases              , na.rm = TRUE),
      avg_all_new_deaths       = mean(all_new_deaths            , na.rm = TRUE),
      sum_all_new_deaths       = sum(all_new_deaths             , na.rm = TRUE),
      avg_all_new_tests        = mean(all_new_tests             , na.rm = TRUE),
      sum_all_new_tests        = sum(all_new_tests              , na.rm = TRUE),
      avg_pos                  = avg_all_new_cases / avg_all_new_tests,
      all_cum_cases            = all_cum_cases[1],
      cap_cum_cases            = cap_cum_cases[1],
      all_cum_deaths           = all_cum_deaths[1],
      cap_cum_deaths           = cap_cum_deaths[1],
      all_cum_tests            = all_cum_tests[1],
      cap_cum_tests            = cap_cum_tests[1],
      cap100k_cum_cases        = all_cum_cases[1] / pop_100k,
      cap100k_cum_deaths       = all_cum_deaths[1] / pop_100k,
      cap100k_cum_tests        = all_cum_tests[1] / pop_100k,
      avg_cap100k_new_cases    = mean(all_new_cases / pop_100k  , na.rm = TRUE),
      sum_cap100k_new_cases    = sum(all_new_cases / pop_100k   , na.rm = TRUE),
      avg_cap100k_new_deaths   = mean(all_new_deaths / pop_100k , na.rm = TRUE),
      sum_cap100k_new_deaths   = sum(all_new_deaths / pop_100k  , na.rm = TRUE),
      avg_cap100k_new_tests    = mean(all_new_tests / pop_100k  , na.rm = TRUE),
      sum_cap100k_new_tests    = sum(all_new_tests / pop_100k   , na.rm = TRUE),
      continent = continent[1],
      income = income[1],
      who_region = who_region[1],
      .groups = "drop"
    )
  data_summarized_over_time
}


#' Step 2: Summarize Data Over Groups (using Median)
#' @name summarize_over_time
#' @export
summarize_over_group <- function (data_summarized_over_time, group = NULL) {

  if (is.null(group) || group == "country") {
      colnames(data_summarized_over_time) <- 
        colnames(data_summarized_over_time) |>
        str_replace_all(c("avg_" = ""))
      
     ans <-
       data_summarized_over_time |>
       select(-starts_with("sum_")) |>
       arrange(name)
    return(ans)
  }

  stopifnot(inherits(group, "character"))

  data_summarized_over_group <-
    data_summarized_over_time |>
    rename_with(function(x) "group", {{ group }}) |>
    filter(!is.na(group))|>
    group_by(group) |>
    summarize(
      unit = group[1],
      name = group[1],
      pop_100k               = sum(pop_100k,  na.rm = TRUE),
      pop                    = sum(pop, na.rm = TRUE),
      avg_cap_new_cases      = median(avg_cap_new_cases       , na.rm = TRUE),
      sum_cap_new_cases      = median(sum_cap_new_cases       , na.rm = TRUE),
      avg_cap_new_deaths     = median(avg_cap_new_deaths      , na.rm = TRUE),
      sum_cap_new_deaths     = median(sum_cap_new_deaths      , na.rm = TRUE),
      avg_cap_new_tests      = median(avg_cap_new_tests       , na.rm = TRUE),
      sum_cap_new_tests      = median(sum_cap_new_tests       , na.rm = TRUE),
      avg_all_new_cases      = median(avg_all_new_cases       , na.rm = TRUE),
      sum_all_new_cases      = sum(sum_all_new_cases          , na.rm = TRUE),
      avg_all_new_deaths     = median(avg_all_new_deaths      , na.rm = TRUE),
      sum_all_new_deaths     = sum(sum_all_new_deaths         , na.rm = TRUE),
      avg_all_new_tests      = median(avg_all_new_tests       , na.rm = TRUE),
      sum_all_new_tests      = sum(sum_all_new_tests          , na.rm = TRUE),
      avg_pos                = median(avg_pos                 , na.rm = TRUE),
      avg_cap100k_new_cases  = median(avg_cap100k_new_cases   , na.rm = TRUE),
      sum_cap100k_new_cases  = median(sum_cap100k_new_cases   , na.rm = TRUE),
      avg_cap100k_new_deaths = median(avg_cap100k_new_deaths  , na.rm = TRUE),
      sum_cap100k_new_deaths = median(sum_cap100k_new_deaths  , na.rm = TRUE),
      avg_cap100k_new_tests  = median(avg_cap100k_new_tests   , na.rm = TRUE),
      sum_cap100k_new_tests  = median(sum_cap100k_new_tests   , na.rm = TRUE),
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




