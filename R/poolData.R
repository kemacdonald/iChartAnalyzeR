#' Pool Data
#'
#' This function aggregates iChart RT and Accuracy for each trial, participant, and condition,
#' returning this information in a data frame.
#'
#' @description \code{poolData()} computes the mean accuracy or RT for each each participant and condition.
#' @param iChart A data frame in iChart format with iChart column names.
#' @param dependent A string indicating which dependent measure to use (Accuracy or RT).
#' @param include_T_initial A boolean indicating whether Target-initial trials should be included in RT computation.
#' @param RejectFirstGap A boolean indicating whether bad first gaps should be filtered out of the computation.
#' @param RejectLongestGap A boolean indicating whether bad longest gaps should be filtered out of the computation.
#' @param RejectRT A boolean indicating whether bad RTs should be filtered out of the computation.
#' @param save_results A boolean indicating whether the results should be saved to disk.
#' @export
#' @examples
#' \dontrun{acc <- poolData(d, RejectFirstGap=FALSE, RejectLongestGap=FALSE, RejectRT=FALSE,
#'   dependent="Accuracy", paired = TRUE, save_results = TRUE)}
#'

poolData <- function(iChart,
                     dependent = "Accuracy",
                     include_T_initial = TRUE,
                     RejectFirstGap = TRUE,
                     RejectLongestGap = TRUE,
                     RejectRT = FALSE,
                     save_results = TRUE) {

  GoodFirstGap <- RejectFirstGap
  GoodLongestGap <- RejectLongestGap
  GoodRT <- RejectRT

  if(include_T_initial) {
    filterediChart <- iChart[iChart$Response == "D" | iChart$Response == "T",]
  } else {
    filterediChart <- iChart[iChart$Response == "D",]
  }

  ## filtering
  if(GoodFirstGap) filterediChart <- filterediChart[filterediChart$GoodFirstGap | is.na(filterediChart$GoodFirstGap),]
  if(GoodLongestGap) filterediChart <- filterediChart[filterediChart$GoodLongestGap | is.na(filterediChart$GoodLongestGap),]
  if(GoodRT) filterediChart <- filterediChart[filterediChart$GoodRT,]

  ## aggregate depending on dependent variable
  if (dependent == "Accuracy") {
    results_table <- filterediChart %>%
      dplyr::group_by(Sub.Num, Condition) %>%
      dplyr::summarise(accuracy = mean(Accuracy, na.rm = T),
                       stdev = sd(Accuracy, na.rm = T),
                       n_trials = dplyr::n())
  } else if (dependent == "RT") {
    results_table <- filterediChart %>%
      dplyr::group_by(Sub.Num, Condition, Response) %>%
      dplyr::summarise(rt = mean(RT, na.rm = T),
                       stdev = stats::sd(RT, na.rm = T),
                       n_trials = dplyr::n()) %>%
      dplyr::filter(!is.na(Sub.Num))
  }
  ## save results
  if(save_results & dependent == "Accuracy") {
    npar <- length(unique(iChart$Sub.Num))
    dir.create("processed_data", showWarnings = FALSE)
    save_as_ta <- paste("processed_data/", iChart[1, "StudyName"], "_mean_", dependent, "_by_subs_", iChart[1, "StartWindowAcc"], "_", iChart[1, "EndWindowAcc"], "_n_", npar, ".txt", sep="")
    write.table(results_table, save_as_ta, sep="\t", row.names=F)
  } else {
    npar <- length(unique(iChart$Sub.Num))
    dir.create("processed_data", showWarnings = FALSE)
    save_as_ta <- paste("processed_data/", iChart[1, "StudyName"], "_mean_", dependent, "_by_subs_", iChart[1, "StartWindowRT"], "_", iChart[1, "EndWindowRT"], "_n_", npar, ".txt", sep="")
    write.table(results_table, save_as_ta, sep="\t", row.names=F)
  }
  results_table
}
