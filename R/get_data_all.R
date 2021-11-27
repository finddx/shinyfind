#' Get all Data
#'
#' Memoised Function, updates once a day
#'
#' @name get_data_all
get_data_all_ <- function(time = today_at_sunrise()) {

  check_for_update <- function(object, download_url, heartbeat_url, col_spec = readr::cols()) {

    if (exists(object)) {

      heartbeat_upstream <- curl::curl_fetch_memory(heartbeat_url)$modified

      if (isTRUE(heartbeat_upstream == heartbeat_local)) {
        message(sprintf("'%s.csv': data already up-to-date, skipping download.", object))
        return(get(object, envir = globalenv()))
      } else {
        object <- readr::read_csv(download_url, col_types = col_spec)
        heartbeat_local <<- curl::curl_fetch_memory(heartbeat_url)$modified
        return(object)
      }
    } else {
      object <- readr::read_csv(download_url, col_types = col_spec)
      heartbeat_local <<- curl::curl_fetch_memory(heartbeat_url)$modified
      return(object)
    }
  }


  codebook <- get_codebook_extended()

  # info on variable selection
  # (this has some info needed by the map application perhaps we can put it there)
  outcome_info <- tibble::tribble(
   ~calc, ~diff,     ~var,  ~ref, ~split_list,         ~info,
   NA, "cum",  "cases", "all", "huge",               p("The time series chart below shows the", strong("cumulative number"), "of", strong("all cases"),"."),
   NA, "cum",  "cases", "cap", "large",              p("The time series chart below shows the", strong("cumulative number"), "of", strong("cases per 1000 people"),"."),
   NA, "cum", "deaths", "all", "large",              p("The time series chart below shows the", strong("cumulative number"), "of", strong("all deaths"), " in each country."),
   NA,  "cum", "deaths", "cap", "medium",            p("The time series chart below shows the", strong("cumulative number"), "of", strong("deaths per 1000 people"),"."),
   NA,  "cum",  "tests", "all", "huge",              p("The time series chart below shows the", strong("cumulative number"), "of", strong("all tests"), " in each country."),
   NA,  "cum",  "tests", "cap", "large",             p("The time series chart below shows the", strong("cumulative number"), "of", strong("tests per 1000 people"), "."),
   NA,  "new",  "cases", "all", "medium",            p("The time series chart below shows the", strong("daily number"), "of", strong("cases"),". Due to data inconsistencies, we show a moving 7-day average."),
   NA,  "new",  "cases", "cap", "small",             p("The time series chart below shows the", strong("daily number"), "of", strong("cases per 1000 people"), ". Due to data inconsistencies, we show a moving 7-day average."),
   NA,  "new", "deaths", "all", "small",             p("The time series chart below shows the", strong("daily number"), "of", strong("deaths"), ". Due to data inconsistencies, we show a moving 7-day average."),
   NA,  "new", "deaths", "cap", "tiny",              p("The time series chart below shows the", strong("daily number"), "of", strong("deaths per 1000 people"),". Due to data inconsistencies, we show a moving 7-day average."),
   NA,  "new",  "tests", "all", "large",             p("The time series chart below shows the", strong("daily number"), "of", strong("tests"), ". Due to data inconsistencies, we show a moving 7-day average."),
   NA,  "new",  "tests", "cap", "small",             p("The time series chart below shows the", strong("daily number"), "of", strong("tests per 1000 people"),". Due to data inconsistencies, we show a moving 7-day average."),
   "avg",  "new",  "tests", "all", "large",          p("The time series chart below shows the", strong("average number"), "of", strong("tests for the selected time range"), ". Due to data inconsistencies, we show a moving 7-day average."),
   "avg",  "new",  "tests", "cap", "tiny",          p("The time series chart below shows the", strong("average number"), "of", strong("tests per 1000 people for the selected time range"), ". Due to data inconsistencies, we show a moving 7-day average."),
   "sum",  "new",  "tests", "all", "huge",           p("The time series chart below shows the", strong("cumulative number"), "of", strong("all tests for the selected time range"), " in each country."),
   "sum",  "new",  "tests", "cap", "large",          p("The time series chart below shows the", strong("cumulative number"), "of", strong("tests per 1000 people for the selected time range"), "."),
   "avg",  "new",  "cases", "all", "large",          p("The time series chart below shows the", strong("average number"), "of", strong("cases for the selected time range"), ". Due to data inconsistencies, we show a moving 7-day average."),
   "avg",  "new",  "cases", "cap", "tiny",          p("The time series chart below shows the", strong("average number"), "of", strong("cases per 1000 people for the selected time range"), ". Due to data inconsistencies, we show a moving 7-day average."),
   "sum",  "new",  "cases", "all", "huge",           p("The time series chart below shows the", strong("cumulative number"), "of", strong("all cases for the selected time range"), " in each country."),
   "sum",  "new",  "cases", "cap", "large",          p("The time series chart below shows the", strong("cumulative number"), "of", strong("cases per 1000 people for the selected time range"), "."),
   "avg",  "new",  "deaths", "all", "large",         p("The time series chart below shows the", strong("average number"), "of", strong("deaths for the selected time range"), ". Due to data inconsistencies, we show a moving 7-day average."),
   "avg",  "new",  "deaths", "cap", "tiny",         p("The time series chart below shows the", strong("average number"), "of", strong("deaths per 1000 people for the selected time range"), ". Due to data inconsistencies, we show a moving 7-day average."),
   "sum",  "new",  "deaths", "all", "huge",          p("The time series chart below shows the", strong("cumulative number"), "of", strong("all deaths for the selected time range"), " in each country."),
   "sum",  "new",  "deaths", "cap", "large",         p("The time series chart below shows the", strong("cumulative number"), "of", strong("deaths per 1000 people for the selected time range"), "."),
    # positivity rate: repeat same descr, info for 'cap' and 'all', despite being the same.
   NA, "cum",  "pos",   "all", "tiny",               p("The time series chart below shows the", strong("positivity rate."), "This is calculated as the ratio of 7 day average of daily number of cases to daily number of tests."),
   NA,  "cum",  "pos",   "cap", "tiny",              p("The time series chart below shows the", strong("positivity rate."), "This is calculated as the ratio of 7 day average of daily number of cases to daily number of tests."),
   NA,  "new",  "pos",   "all", "tiny",              p("The time series chart below shows the", strong("positivity rate."), "This is calculated as the ratio of 7 day average of daily number of cases to daily number of tests."),
   NA,  "new",  "pos",   "cap", "tiny",              p("The time series chart below shows the", strong("positivity rate."), "This is calculated as the ratio of 7 day average of daily number of cases to daily number of tests.")
  ) |>
    mutate(variable = if_else(is.na(calc), paste(ref, diff, var, sep = "_"), paste(calc, ref, diff, var, sep = "_"))) |>
    mutate(variable = if_else(grepl("_pos$", variable), "pos", variable)) |>
    left_join(codebook, by = "variable") |>
    rename(descr = description) |>
    select(-variable)




  # check if newer data is available in FINDCov19TrackerData
  data_all <- check_for_update("data_all",
    download_url = "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/data_all.csv",
    heartbeat_url = "https://api.github.com/repos/dsbbfinddx/FINDCov19TrackerData/commits?path=processed%2Fdata_all.csv&page=1&per_page=1",
    col_spec = list(
      .default = col_double(),
      set = col_character(),
      name = col_character(),
      unit = col_character(),
      time = col_date(format = "")
    )
  )


  unit_info = check_for_update("unit_info",
    download_url = "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/unit_info.csv",
    heartbeat_url = "https://api.github.com/repos/dsbbfinddx/FINDCov19TrackerData/commits?path=processed%2Funit_info.csv&page=1&per_page=1"
  )








  # data_all <- readr::read_csv("/Users/Anna/FIND_Onedrive/OneDrive - Foundation for Innovative New Diagnostics FIND/BB_Projects/Shinyapps_projects/FINDCov19TrackerData/processed/data_all.csv",
  #                             col_types = readr::cols()
  # )
  #
  # unit_info <- readr::read_csv("/Users/Anna/FIND_Onedrive/OneDrive - Foundation for Innovative New Diagnostics FIND/BB_Projects/Shinyapps_projects/FINDCov19TrackerData/processed/unit_info.csv",
  #                             col_types = readr::cols()
  # )

  shiny_data_wide <-
   data_all %>%
    select(
      set, unit, time, cap_cum_cases, cap_new_cases, cap_cum_deaths,
      cap_new_deaths, cap_cum_tests, cap_new_tests, all_cum_cases,
      all_new_cases, all_cum_deaths, all_new_deaths, all_cum_tests,
      all_new_tests, pos
    ) %>%
    # https://github.com/dsbbfinddx/FINDCov19TrackerShiny/issues/36
    mutate(pos = if_else(pos > 1, NA_real_, pos)) %>%
    # quadrupple pos series, to have all combinations
    mutate(pos = 100 * pos) %>%
    mutate(cap_cum_pos = pos, all_cum_pos = pos, cap_new_pos = pos, all_new_pos = pos) %>%
    select(-pos)

  shiny_data <-
    shiny_data_wide |>
    pivot_longer(-c(set, unit, time), names_to = c("ref", "diff", "var"), names_sep = "_") %>%
    filter(value >= 0)

  country_name <-
    countrycode::codelist %>%
    as_tibble() %>%
    select(
      name = country.name.en, country = iso3c
    ) %>%
    dplyr::mutate(country = dplyr::case_when(
      name == "Kosovo" ~ "XK",
      TRUE ~ country
    ))  %>%
    filter(!is.na(country))

  country_symbol <-
    countrycode::codelist %>%
      as_tibble() %>%
      select(
        country = iso3c, symbol = unicode.symbol
      ) %>%
      filter(!is.na(country))

    country_info_raw <- readr::read_csv(
      "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/raw/country_info.csv",
      col_types = readr::cols()
    )

  country_info <-
    country_info_raw |>
    select(alpha3, name) |>
    filter(!is.na(alpha3))

  country_info_matrix <-
    country_info_raw |>
    select(unit = alpha3, name, continent, continent, income, who_region) |>
    filter(!is.na(who_region))

  country_name <- country_info  %>%
    left_join(select(country_name, country), by = c("alpha3" = "country")) %>%
    rename(country = alpha3)

  # segregated_tests = check_for_update("unit_info",
  #   download_url = "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/automated/segregated_tests.csv",
  #   heartbeat_url = "https://api.github.com/repos/dsbbfinddx/FINDCov19TrackerData/commits?path=processed%2Fsegregated_tests.csv&page=1&per_page=1"
  # )

  segregated_data = check_for_update("segregated_data",
    download_url = "https://raw.githubusercontent.com/dsbbfinddx/FINDCov19TrackerData/master/processed/segregated-data.csv",
    heartbeat_url = "https://api.github.com/repos/dsbbfinddx/FINDCov19TrackerData/commits?path=processed%2Fsegregated_data.csv&page=1&per_page=1"
  )


  segregated_tests <-
    segregated_data %>%
    mutate(
      pcr_test_new_corrected = all_pcr_test_new,
      rapid_test_new_corrected =all_rapid_test_new
    )


  # countries_selected <- c("FR", "DE", "IT", "CN", "KR", "ES", "CH", "UK", "US")
  units_selected <- c("ITA", "KOR", "GBR", "CHE")

  segregated_tests_clean <-
    segregated_tests %>%
    rename(name = country) %>%
    left_join(country_name, by = "name") %>%
    rename(unit = country) %>%
    select(unit, date, pcr_test_new_corrected, rapid_test_new_corrected) %>%
    filter(!is.na(pcr_test_new_corrected))

  max_date <- max(segregated_tests_clean$date)



  pcr_rapid <-
    segregated_tests_clean %>%
    filter(date %in% (max_date-7):max_date) %>%
    # for now, latest value
    group_by(unit) %>%
    summarize(
      pcr = mean(pcr_test_new_corrected, na.rm = TRUE),
      rapid = mean(rapid_test_new_corrected, na.rm = TRUE),
      pcr_share = pcr / (pcr + rapid),
      .groups = "drop"
    )

  # overview of all countries (used in table)
  unit_info_exp <-
    unit_info %>%
    rename(region = continent) %>%
    # prettier NA values
    mutate(across(c(comment, tests_type_segregated, recurrent_reporting), coalesce, "")) %>%
    mutate(across(c(tests_type, tests_description), coalesce, "not specified")) %>%
    mutate(is_selected = unit %in% units_selected) %>%
    arrange(desc(is_selected)) %>%
    left_join(country_name, by = c("unit" = "country")) %>%
    mutate(name = coalesce(name, unit)) %>%
    left_join(country_symbol, by = c("unit" = "country")) %>%
    mutate(symbol = coalesce(symbol, " ")) %>%
    filter(!is.na(unit)) %>%
    # https://github.com/dsbbfinddx/FINDCov19TrackerShiny/issues/36
    mutate(pos = if_else(pos > 1, NA_real_, pos)) %>%
    mutate(pos = 100 * pos) %>%
    mutate_at(vars(pos, deaths, tests, cases), function(e) if_else(e > 50, round(e), round(e,1)))  %>%  # do not show decimals on large rates
    mutate(latest_test_date = as.character(latest_test_date)) %>%
    left_join(select(pcr_rapid, unit, pcr_share), by = "unit")


  map_names <-
    select(data_all, name, unit) %>%
    filter(!is.na(name)) %>%
    distinct()


  slider_date <- max(filter(shiny_data, var == "tests" & !is.na(value))$time)


  # set font in echart equal to rest
  # e_common(font_family = "roboto,sans-serif", theme = "westeros")


  last_data_update <- curl::curl_fetch_memory("https://api.github.com/repos/dsbbfinddx/FINDCov19TrackerData/commits?path=processed%2Fdata_all.csv&page=1&per_page=1")$modified

  lst(
    shiny_data,
    data_all,
    country_info,
    country_info_matrix,
    outcome_info,
    pcr_rapid,
    unit_info_exp,
    map_names,
    slider_date,
    segregated_tests_clean,
    last_data_update
  )

}

#' @export
get_data_all <- memoise::memoise(get_data_all_)


