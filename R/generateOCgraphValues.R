#' Generate Onset Contingency Plot Graph Values
#'
#' This function aggregates iChart Proportion Looking for each trial and for each participant, condition,
#' and frame, and returns this information in a data frame.
#'
#' @description \code{generateOCgraphValues()} computes the mean proportion looking each participant, condition, frame.
#'   as a function of whether the participant was looking at the target or the distractor image at F0.
#' @param iChart A data frame in iChart format with iChart column names.
#' @param gvStartWindow An integer indicating the lower bound of the graph values window in milliseconds.
#' @param gvEndWindow An integer indicating the upper bound of the graph values window in milliseconds.
#' @param group_cols A list of strings indicating the variable names  for any grouping variables.
#' @param filter_criteria A list of strings indicating the variables that should be used for filtering iChart. Options include:
#'   "GoodRT", "GoodFirstGap", and "GoodLongestGap." Or if you don't want to filter pass the string "none"
#' @param save_results A boolean indicating whether the results should be saved to disk.
#' @export
#' @examples
#' \dontrun{df_gvs <- generateOCgraphValues(d_analysis,
#'     filter_criteria = list("GoodRT", "GoodFirstGap", "GoodLongestGap"),
#'     group_cols = list("Condition"),
#'     gvStartWindow = 0, gvEndWindow = 3000,
#'     save_results = FALSE)}
#'

generateOCgraphValues <- function(iChart,
                                  filter_criteria = list("GoodFirstGap", "GoodLongestGap"),
                                  group_cols = list("Condition"),
                                  gvStartWindow = 0, gvEndWindow = 3000,
                                  save_results = TRUE) {
  # extract some relevant information from iChart
  study_name <- iChart$StudyName %>% unique()
  n_participants <- iChart$Sub.Num %>% unique() %>% length()

  # filter and aggregate data
  iChart_filtered <- filter_iChart(iChart, filter_criteria)
  df_by_ss <- aggregate_by_ss_oc(iChart_filtered, group_cols, gvStartWindow, gvEndWindow)
  df_by_cond <- aggregate_by_cond_oc(df_by_ss, group_cols)

  # widen the data frames
  df_by_ss_wide <- widen_gv_oc(df_by_ss, study_name, n_participants)
  df_by_cond_wide <- widen_gv_oc(df_by_cond, study_name, n_participants)

  # save results of both the subject and condition aggregations
  if(save_results) {
    save_graph_values_oc(df_by_ss, df_by_cond,
                         study_name = study_name,
                         gvStartWindow, gvEndWindow,
                         n_participants = n_participants)
  }

  # return the by_condition data frame in wide format
  df_by_cond_wide
}

save_graph_values_oc <- function(df_by_ss, df_by_cond, study_name, gvStartWindow, gvEndWindow, n_participants) {
  message('*saving subject and condition graph values to .txt files')
  readr::write_tsv(df_by_ss, path = make_file_name_oc(study_name, gvStartWindow, gvEndWindow, n_participants, by_subs = TRUE))
  readr::write_tsv(df_by_cond, path = make_file_name_oc(study_name, gvStartWindow, gvEndWindow, n_participants, by_subs = FALSE))
}

make_file_name_oc <- function(study_name, gvStartWindow, gvEndWindow, n_participants, by_subs = TRUE) {
  if (by_subs) {
    stringr::str_c(study_name, "OC_graphValues_by_subs", gvStartWindow, gvEndWindow, "n", n_participants, sep = "_") %>%
      stringr::str_c("processed_data/", ., ".txt")
  } else {
    stringr::str_c(study_name, "OC_graphValues_by_condition", gvStartWindow, gvEndWindow, "n", n_participants, sep = "_") %>%
      stringr::str_c("processed_data/", ., ".txt")
  }
}

filter_iChart <- function(iChart, filter_criteria) {
  print_filters(filter_criteria)
  none_condition <- stringr::str_detect(filter_criteria, 'none') %>% sum() # sum will be greater than 0 if none appears in list of filters

  if (none_condition != 0) {
    iChart # if 'none' appears,then just return the iChart
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

print_window <- function(gvStartWindow, gvEndWindow) {
  message("*the user-specified plotting window is: ", gvStartWindow, " to ", gvEndWindow)
}

# this is some serious tidyeval dark magic
# but it takes a list of strings representing filtering criteria
# returns a list of quosures that can then be passed to dplyr's filter
quote_filters <- function(filter_criteria) {
  purrr::map(filter_criteria, function(x) dplyr::quo((!!(as.name(x))) == T | is.na(!!(as.name(x)))))
}

aggregate_by_ss_oc <- function(iChart, group_cols, gvStartWindow, gvEndWindow) {
  time_selector <- dplyr::quo(!! as.character(gvStartWindow) : !! as.character(gvEndWindow))
  print_window(gvStartWindow, gvEndWindow)
  group_by_ss <- rlang::syms(group_cols)
  print_groupings(group_cols)

  # compute proportion looking for d-initial and t-initial trials
  iChart %>%
    dplyr::filter(Response %in% c("D", "T")) %>%
    dplyr::select(Sub.Num:Response, !! time_selector) %>%
    tidyr::gather(key = frame, value = look_code, !! time_selector) %>%
    dplyr::mutate(frame = as.integer(frame)) %>%
    dplyr::filter(look_code != 0.5) %>%
    dplyr::mutate(look_code = dplyr::case_when(
      Response == "T" & look_code == 0 ~ 1,
      Response == "T" & look_code == 1 ~ 0,
      TRUE ~ look_code)) %>%
    dplyr::group_by(!!! group_by_ss, frame, Sub.Num, Response) %>%
    dplyr::summarise(m_ss = mean(look_code))
}

aggregate_by_cond_oc <- function(df_by_ss, group_cols) {
  group_by_cond <- stringr::str_remove(group_cols, "Sub.Num") %>% .[. != ""]  %>% rlang::syms(.) # this code removes Sub.Num from grouping cols
  df_by_ss %>%
    dplyr::group_by(!!! group_by_cond, frame, Response) %>%
    dplyr::summarise(m = mean(m_ss),
                     se = sd(m_ss) / sqrt(n()))

}

widen_gv_oc <- function(df, study_name, n_participants) {
  # get the name of the data frame as a string
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
      dplyr::mutate(study_name = study_name,
                    n_participants = n_participants) %>%
      dplyr::select(Condition, statistic, study_name, n_participants, tidyselect::everything())

  } else {
    df %>%
      dplyr::rename(m = m_ss) %>%
      tidyr::spread(frame, m) %>%
      dplyr::mutate(study_name = study_name,
                    n_participants = n_participants) %>%
      dplyr::select(Condition, study_name, n_participants, tidyselect::everything())
  }

}
