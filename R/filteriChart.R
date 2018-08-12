#' Filter iChart
#'
#' This function creates variables that can be used to
#' filter trials out of the iChart based on user-specified exlcusionary criteria.
#'
#' @description \code{filteriChart()} takes two values that indicated an analysis window and computes
#'   the following statistics for each trial: RT, longestLook, longestGap, Gap, longestGapPosition,
#'   NumberOfShifts, FirstFixation.
#' @param iChart A data frame in iChart format with iChart column names.
#' @param minRT An integer indicating the lower bound of the RT analysis window in milliseconds.
#' @param maxRT An integer indicating the upper bound of the RT analysis window in milliseconds.
#' @param maxfirstgap An integer indicating the upper bound of the first gap shift length frames.
#' @param maxlonggap An integer indicating the upper bound of the longest gap in a trial in frames.
#' @param save_results A boolean indicating whether the results should be saved to disk.
#' @export
#' @examples
#' \dontrun{d <- filteriChart(iChart, minRT=300, maxRT=1800, maxfirstgap=15,
#'   maxlonggap=15, save_results = TRUE)}
#'

filteriChart <- function(iChart,
                         minRT, maxRT,
                         maxfirstgap, maxlonggap,
                         save_results = TRUE) {

  iChart$StartWindowRT <- minRT
  iChart$EndWindowRT <- maxRT

  high_percentileRT <- maxRT
  low_percentileRT <- minRT
  percentileFirstGap <- maxfirstgap
  percentileLongestGap <- maxlonggap

  if(save_results) {
    dir.create("processed_data", showWarnings = FALSE)
    save_as <- paste("processed_data/", iChart[1, "StudyName"], "_filtering_Criteria.txt", sep="")
    sink(paste(save_as, sep=""))
  }

  goodTrials <- iChart$Response == "D" | iChart$Response == "T"
  T_Trials <- iChart$Response == "T"
  D_Trials <- iChart$Response == "D"

  # first Gap
  if(percentileFirstGap < 1) {
    longestFirstGap <- quantile(iChart$firstGap[goodTrials], percentileFirstGap, na.rm=T)
    print("")
    print(paste(percentileFirstGap, "quantile of First Gaps =", longestFirstGap))
  } else {
    longestFirstGap <- percentileFirstGap
  }

  print("First GAP")
  print(paste("Longest First Gap = ", longestFirstGap))
  print(summary(iChart$firstGap[goodTrials]))

  # longest Gap
  if(percentileLongestGap < 1) {
    longestLongestGap <- quantile(iChart$longestGap[goodTrials], percentileLongestGap, na.rm=T)
    print("")
    print(paste(percentileLongestGap, "quantile of Longest Gaps =", longestLongestGap))
  } else {
    longestLongestGap <- percentileLongestGap
  }

  print("Longest GAP")
  print(paste("Longest Gap = ", longestLongestGap))
  print(summary(iChart$longestGap[goodTrials]))

  # RTs
  if(low_percentileRT < 1) {
    shortestRT_D <- quantile(iChart$RT[D_Trials], low_percentileRT, na.rm=T)
    print(paste(low_percentileRT, "quantile of RTs (D) =", round(shortestRT_D)))
    shortestRT_T <- quantile(iChart$RT[T_Trials], low_percentileRT, na.rm=T)
    print(paste(low_percentileRT, "quantile of RTs (T) =", round(shortestRT_T)))
  } else {
    shortestRT_D <- low_percentileRT
    shortestRT_T <- low_percentileRT
  }

  if(high_percentileRT < 1) {
    longestRT_D <- quantile(iChart$RT[D_Trials], high_percentileRT, na.rm=T)
    print("")
    print(paste(high_percentileRT, "quantile of RTs (D) =", round(longestRT_D)))

    longestRT_T <- quantile(iChart$RT[T_Trials], high_percentileRT, na.rm=T)
    print("")
    print(paste(high_percentileRT, "quantile of RTs (T) =", round(longestRT_T)))

  } else {
    longestRT_D <- high_percentileRT
    longestRT_T <- high_percentileRT
  }

  print("RT D Trials (filtering based on these statistics)")
  print(paste("Longest RT D trials: ", longestRT_D))
  print(paste("Shortest RT D trials: ", shortestRT_D))
  print(summary(iChart$RT[D_Trials]))

  print("RT T Trials")
  print(paste("Longest RT T trials: ", longestRT_T))
  print(paste("Shortest RT T trials: ", shortestRT_T))
  print(summary(iChart$RT[T_Trials]))

  iChart$GoodFirstGap <- iChart$firstGap <= longestFirstGap

  print("Trials Rejected")
  print("")
  print(paste("First Gap = ", sum(iChart$GoodFirstGap,na.rm=T), " out of ", sum(iChart$GoodFirstGap,na.rm=T)+sum(!iChart$GoodFirstGap,na.rm=T), sep=""))


  iChart$GoodRT <- (iChart$RT <= longestRT_D)&(iChart$RT >= shortestRT_D)

  print("")
  print(paste("RT = ", sum(iChart$GoodRT,na.rm=T), " out of ", sum(iChart$GoodRT,na.rm=T)+sum(!iChart$GoodRT,na.rm=T),sep=""))


  iChart$GoodLongestGap <- iChart$longestGap <= longestLongestGap

  print("")
  print(paste("Longest Gap = ", sum(iChart$GoodLongestGap,na.rm=T), " out of ", sum(iChart$GoodLongestGap,na.rm=T)+sum(!iChart$GoodLongestGap, na.rm=T), sep=""))

  if(save_results) {
    sink()
    npar <- length(unique(iChart$Sub.Num))
    dir.create("processed_data", showWarnings = FALSE)
    save_as <- paste("processed_data/", iChart[1, "StudyName"], "_filterediChart", "_RT_",iChart[1, "StartWindowRT"], "_", iChart[1, "EndWindowRT"], "_minRT_",  shortestRT_D, "_maxRT_", longestRT_D, "_lg_", longestLongestGap, "_fg_", longestFirstGap, "_Acc_", iChart[1, "StartWindowAcc"], "_", iChart[1, "EndWindowAcc"], "_n_", npar, ".txt", sep="")
    write.table(iChart, save_as, sep="\t", row.names=F)
  }
  iChart
}
