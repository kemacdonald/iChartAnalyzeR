#' Filter Prescreened Trials From iChart
#'
#' This function returns an iChart with prescreened out trials removed
#'
#' @description \code{describeiChart()} provides a quick check of the number of trials
#'   for each participant and condition. It is also useful for checking the effects of any data
#'   filtering.
#' @param iChart A data frame created by the \code{readiChart()} function.
#' @param save_results A boolean indicating whether the results should be saved to disk.
#' @export
#' @examples
#' \dontrun{d <- filterPrescreened(iChart, save_results = TRUE)}

filterPrescreened <- function(iChart, save_results = TRUE) {
  # get summary of prescreening
  d_ps_summary <- iChart %>%
    dplyr::filter(.data$Prescreen.Notes != "good_trial") %>%
    dplyr::count(.data$Sub.Num, .data$Condition, .data$Prescreen.Notes)

  if(save_results) {
    dir.create("processed_data", showWarnings = FALSE)
    file_name <- paste(unique(iChart$StudyName), "prescreening_summary.txt", sep = '_')
    readr::write_delim(d_ps_summary,
                       path = paste("processed_data/", file_name, sep = ""),
                       delim = "\t")
    print("Saved prescreening results")
  }
  # filter
  iChart %>% dplyr::filter(.data$Prescreen.Notes == "good_trial")
}
