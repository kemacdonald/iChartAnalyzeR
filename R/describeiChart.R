#' Describe iChart
#'
#' This function returns a tabular summary of trial and prescreening counts for each participant and condition.
#'
#' @description \code{describeiChart()} provides a quick check of the number of trials
#'   for each participant and condition. It is also useful for checking the effects of any data
#'   filtering.
#' @param iChart A data frame created by the \code{readiChart()} function.
#' @export
#' @examples
#' \dontrun{d <- describeiChart(iChart)}

describeiChart <- function(iChart) {
  d_ps <- iChart %>%
    dplyr::filter(Prescreen.Notes != "good_trial") %>%
    dplyr::count(Sub.Num, Condition) %>%
    dplyr::rename(n_trials_prescreened = n)

  if(nrow(d_ps) == 0) {
    print("There are no trials to prescreen out, returning trial counts for each participant and condition")
    iChart %>%
      dplyr::count(Sub.Num, Condition) %>%
      dplyr::rename(n_trials = n)
  } else {
    print("There are prescreened out trials in the dataset, returning trial counts with prescreening information for each participant and condition")
    iChart %>%
      dplyr::count(Sub.Num, Condition) %>%
      dplyr::left_join(d_ps, by = c("Sub.Num", "Condition")) %>%
      dplyr::rename(n_trials = n) %>%
      dplyr::mutate(n_good_trials = n_trials - n_trials_prescreened)
  }
}


