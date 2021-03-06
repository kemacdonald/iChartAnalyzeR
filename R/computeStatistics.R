#' Compute Statistics On iChart
#'
#' This function computes the relevant stats on an iChart.
#'
#' @description \code{computeStatistics()} takes two values that indicated an analysis window and computes
#'   the following statistics for each trial: RT, longestLook, longestGap, Gap, longestGapPosition,
#'   NumberOfShifts, FirstFixation, and Accuracy.
#' @param iChart A data frame in iChart format with iChart column names.
#' @param cleanWindowStart An integer indicating the lower bound of the cleaning window in milliseconds.
#' @param cleanWindowEnd An integer indicating the upper bound of the cleaning window in milliseconds.
#' @param accuracyWindowStart An integer indicating the lower bound of the accuracy computation window in milliseconds.
#' @param accuracyWindowEnd An integer indicating the upper bound of the accuracy computation window in milliseconds.
#' @param save_results A boolean indicating whether the results should be saved to disk.
#' @export
#' @examples
#' \dontrun{d <- computeStatistics(iChart, cleanWindowStart=0, cleanWindowEnd=3000, accuracyWindowStart = 300, accuracyWindowEnd = 1800, save_results = TRUE)}

computeStatistics <- function(iChart,
                              cleanWindowStart = 300, cleanWindowEnd = 3000,
                              accuracyWindowStart = 300, accuracyWindowEnd = 1800,
                              save_results = TRUE) {

  iChart$StartCleaningWindow <- rep(cleanWindowStart, nrow(iChart))
  iChart$EndCleaningWindow <- rep(cleanWindowEnd, nrow(iChart))

  accuracyWindowStart <- as.character(accuracyWindowStart)
  accuracyWindowEnd <- as.character(accuracyWindowEnd)
  iChart$StartWindowAcc <- accuracyWindowStart
  iChart$EndWindowAcc <- accuracyWindowEnd

  cleanWindowStart <- which(names(iChart)==cleanWindowStart)

  if (cleanWindowEnd == FALSE) cleanWindowEnd <- length(iChart)
  else cleanWindowEnd <- which(names(iChart)==cleanWindowEnd)

  iChart$longestLook <- rep(NA, nrow(iChart))
  iChart$longestGap <- rep(NA, nrow(iChart))
  iChart$Gap <- rep(NA, nrow(iChart))
  iChart$longestGapPosition <- rep(NA, nrow(iChart))
  iChart$firstGap <- rep(NA, nrow(iChart))
  iChart$RT <- rep(NA, nrow(iChart))
  iChart$durationLongestLook <- rep(NA, nrow(iChart))
  iChart$StartlongestLook_T <- rep(NA, nrow(iChart))
  iChart$NumberOfShifts <- rep(NA, nrow(iChart))
  iChart$FirstFixation <- rep(NA, nrow(iChart))

  endRows <- nrow(iChart)
  print("### Trials left ###")

  for (row in 1:endRows) {

    if((endRows-row)%%50 == 0)print(endRows-row)
    consecutiveAways <- 0
    containsAway <- FALSE
    currentLook <- 0
    longestLook <- 0
    firstGap <- 0
    longestGap <- 0
    longestGap_col <- NA
    Gap <- 0
    longestLook_T <- 0
    StartlongestLook_T <- NA
    NumberOfShifts <- 0
    FirstFixation <- 0
    isFirst <- TRUE
    isShift <- TRUE

    start_elem <- iChart[row, cleanWindowStart]

    rt_notdetected <- TRUE
    for (col in cleanWindowStart:cleanWindowEnd) {
      curr_elem <- iChart[row, col]

      if (is.na(curr_elem) || curr_elem == ""){
        col = cleanWindowEnd
      } else if (curr_elem == 0.5) {

        currentLook <- 0

        if(isShift) {
          NumberOfShifts <- NumberOfShifts + 1
          isShift <- FALSE
        }

        if(FirstFixation > 0) {
          isFirst <- FALSE
        }


        if(consecutiveAways==0) rt <- col
        consecutiveAways <- consecutiveAways + 1
        Gap <- Gap + 1

        if(!containsAway && (curr_elem == 0.5)) containsAway <- TRUE

        if (containsAway && (consecutiveAways > longestGap)) {
          longestGap_col <- col
          longestGap <- consecutiveAways
        }


      } else if (curr_elem== 1 || curr_elem == 0) {

        isShift <- TRUE
        #define longest look
        currentLook <- currentLook + 1


        if (currentLook > longestLook) {
          longestLook <- currentLook
          longestLook_elem <- curr_elem
        }
        if(rt_notdetected){
          if(curr_elem != start_elem && iChart$Response[row] != "R") {
            iChart$RT[row] <- as.numeric(colnames(iChart)[rt])
            iChart$firstGap[row] <- consecutiveAways
            rt_notdetected <- FALSE
          }

        }

        if(!rt_notdetected & isFirst){
          FirstFixation <- currentLook
        }

        if(!rt_notdetected && (currentLook > longestLook_T) && curr_elem == 1) {
          longestLook_T <- currentLook
          StartlongestLook_T <- rt
        }

        consecutiveAways <- 0
        containsAway <- FALSE

      } # if it is an away or off trial
    } # col
    iChart$durationLongestLook[row] <- longestLook
    iChart$longestLook[row] <- longestLook_elem
    iChart$longestGap[row] <- longestGap
    iChart$Gap[row] <- Gap

    iChart$NumberOfShifts[row] <- NumberOfShifts

    iChart$FirstFixation[row] <- FirstFixation


    if(!is.na(StartlongestLook_T)) iChart$StartlongestLook_T[row] <- as.numeric(colnames(iChart)[StartlongestLook_T])
    if(!is.na(longestGap_col))iChart$longestGapPosition[row] <- as.numeric(colnames(iChart)[longestGap_col-longestGap+1])
  }

  ## compute accuracy for each participant and each trial
  df_accuracy <- iChart %>%
    dplyr::select(Sub.Num, Tr.Num, accuracyWindowStart:accuracyWindowEnd) %>%
    tidyr::gather(key = "time_stamp", value = "look_type", -Sub.Num, -Tr.Num, accuracyWindowStart:accuracyWindowEnd) %>%
    dplyr::filter(look_type != 0.5) %>%  # this removes aways from the accuracy computation
    dplyr::group_by(Sub.Num, Tr.Num) %>%
    dplyr::summarise(Accuracy = mean(look_type),
              FramesAccuracyComputation = n()) # this computes the number of frames that went into computation

  iChart <- dplyr::left_join(iChart, df_accuracy, by = c("Sub.Num", "Tr.Num"))

  #iChart$Accuracy <- rowMeans(data.matrix(iChart[,cleanWindowStart:cleanWindowEnd]), na.rm=T)

  longestFirstGap <- max(iChart$firstGap[iChart$Response == "D" | iChart$Response == "T"], na.rm=T)
  longestLongestGap <- max(iChart$longestGap[iChart$Response == "D" | iChart$Response == "T"], na.rm=T)
  longestRT_D <- max(iChart$RT[iChart$Response == "D"], na.rm=T)
  shortestRT_D <- min(iChart$RT[iChart$Response == "D"], na.rm=T)

  if (save_results) {
    npar <- length(unique(iChart$Sub.Num))
    dir.create("processed_data", showWarnings = FALSE)
    save_as <- paste("processed_data/", iChart[1, "StudyName"], "_originaliChart", "_RT_",iChart[1, "StartCleaningWindow"], "_", iChart[1, "EndCleaningWindow"], "_minRT_",  shortestRT_D, "_maxRT_", longestRT_D, "_lg_", longestLongestGap, "_fg_", longestFirstGap, "_Acc_", iChart[1, "StartWindowAcc"], "_", iChart[1, "EndWindowAcc"], "_n_", npar, ".txt", sep="")
    write.table(iChart, save_as, sep="\t", row.names=F)
  }
  # returrn the iChart
  iChart
}
