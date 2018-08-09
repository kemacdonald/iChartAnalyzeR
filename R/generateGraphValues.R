#' Generate Graph Values
#'
#' This function aggregates iChart Proportion Looking for each trial and Accuracy for each participant, condition,
#' and frame, and returns this information in a data frame.
#'
#' @description \code{generateGraphValues()} computes the mean proportion looking each participant, condition, and frame.
#' @param iChart A data frame in iChart format with iChart column names.
#' @param startWindow An integer indicating the lower bound of the analysis window in milliseconds.
#' @param endWindow An integer indicating the upper bound of the analysis window in milliseconds.
#' @param group_cols A list of strings indicating the variable names  for any grouping variables.
#' @param filter_criteria A list of strings indicating the variables that should be used for filtering iChart. Options include:
#'   "GoodRT", "GoodFirstGap", and "GoodLongestGap." Or if you don't want to filter pass the string "none"
#' @param save_results A boolean indicating whether the results should be saved to disk.
#' @export
#' @examples
#' \dontrun{df_gvs <- generateGraphValues(d_analysis,
#'     filter_criteria = list("GoodRT", "GoodFirstGap", "GoodLongestGap"),
#'     group_cols = list("Sub.Num", "Condition"),
#'     startWindow = 0, endWindow = 3000,
#'     save_results = FALSE)}
#'

generateGraphValues <- function(iChart,
                                filter_criteria = list("GoodRT", "GoodFirstGap", "GoodLongestGap"),
                                group_cols = list("Sub.Num", "Condition"),
                                startWindow = 0, endWindow = 3000,
                                save_results = TRUE) {

  iChart_filtered <- filter_iChart(iChart, filter_criteria)
  df_by_ss <- aggregate_by_ss(iChart_filtered, group_cols, startWindow, endWindow)
  df_by_cond <- aggregate_by_cond(df_by_ss, group_cols)

  # save results of both the subject and condition aggregations
  if(save_results) {
    study_name <- iChart$StudyName %>% unique()
    n_particpants <- iChart$Sub.Num %>% unique() %>% length()
    save_graph_values(df_by_ss, df_by_cond, study_name = study_name, startWindow, endWindow, n_participants = n_particpants)
  }

  # return the by_condition data frame in wide format
  widen_gv(df_by_cond)
}

save_graph_values <- function(df_by_ss, df_by_cond, study_name, startWindow, endWindow, n_participants) {
  message('*saving subject and condition graph values to .txt files')
  readr::write_tsv(widen_gv(df_by_ss), path = make_file_name(study_name,startWindow, endWindow, n_participants, by_subs = TRUE))
  readr::write_tsv(widen_gv(df_by_cond), path = make_file_name(study_name,startWindow, endWindow, n_participants, by_subs = FALSE))
}

make_file_name <- function(study_name, startWindow, endWindow, n_partcipants, by_subs = TRUE) {
  if (by_subs) {
    stringr::str_c(study_name, "PP_graphValues_by_subs", startWindow, endWindow, "n", n_partcipants, sep = "_") %>% stringr::str_c(., ".txt")
  } else {
    stringr::str_c(study_name, "PP_graphValues_by_condition", startWindow, endWindow, "n", n_partcipants, sep = "_") %>% stringr::str_c(., ".txt")
  }
}

filter_iChart <- function(iChart, filter_criteria) {
  print_filters(filter_criteria)
  if (stringr::str_detect(filter_criteria, 'none')) {
    iChart
  } else {
    filters <- quote_filters(filter_criteria)
    iChart %>% dplyr::filter(!!! filters)
  }

}

print_filters <- function(filter_criteria) {
  message("*the user-specified filtering criteria include:")
  filter_criteria %>% purrr::map(function(x){message("  -", x)})
}

print_groupings <- function(group_cols) {
  message("*the user-specified grouping variables include:")
  group_cols %>% purrr::map(function(x){message("  -", x)})
}

print_window <- function(startWindow, endWindow) {
  message("*the user-specified plotting window is: ", startWindow, " to ", endWindow)
}

# this is some serious tidyeval dark magic
# but it takes a list of strings representing filtering criteria
# returns a list of quosures that can then be passed to dplyr's filter
quote_filters <- function(filter_criteria) {
  purrr::map(filter_criteria, function(x) dplyr::quo((!!(as.name(x))) == T | is.na(!!(as.name(x)))))
}

aggregate_by_ss <- function(iChart, group_cols, startWindow, endWindow) {
  time_selector <- dplyr::quo(!! as.character(startWindow) : !! as.character(endWindow))
  print_window(startWindow, endWindow)
  group_by_ss <- rlang::syms(group_cols)
  print_groupings(group_cols)

  iChart %>%
    dplyr::select(Sub.Num:Response, !! time_selector) %>%
    tidyr::gather(key = frame, value = look_code, !! time_selector) %>%
    dplyr::mutate(frame = as.integer(frame)) %>%
    dplyr::filter(look_code != 0.5) %>%
    dplyr::group_by(!!! group_by_ss, frame) %>%
    dplyr::summarise(m_ss = mean(look_code))
}

aggregate_by_cond <- function(df_by_ss, group_cols) {
  group_by_cond <- stringr::str_remove(group_cols, "Sub.Num") %>% .[. != ""]  %>% rlang::syms(.) # this code removes Sub.Num from grouping cols
  df_by_ss %>%
    dplyr::group_by(!!! group_by_cond, frame) %>%
    dplyr::summarise(m = mean(m_ss),
                     se = sd(m_ss) / sqrt(n()))

}

widen_gv <- function(df) {
  df_name_string <- deparse(substitute(df))

  if (stringr::str_detect(df_name_string, pattern = "cond")) {
    df_wide_m <- df %>%
      dplyr::select(-se) %>%
      tidyr::spread(frame, m) %>%
      dplyr::mutate(statistic = "mean")

    df_wide_se <- df %>%
      dplyr::select(-m) %>%
      tidyr::spread(frame, se) %>%
      dplyr::mutate(statistic = "se")

    dplyr::bind_rows(df_wide_m, df_wide_se) %>%
      dplyr::select(Condition, statistic, tidyselect::everything())
  } else {
    df %>%
      dplyr::rename(m = m_ss) %>%
      tidyr::spread(frame, m)
  }

}
