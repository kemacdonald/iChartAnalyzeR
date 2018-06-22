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
#' @param group A string indicating the variable name for any grouping variables (e.g., conditions).
#' @param paired A boolean indicating whether the computation should be done over paired data.
#' @param save_results A boolean indicating whether the results should be saved to disk.
#' @export
#' @examples
#' \dontrun{acc <- poolData(d, RejectFirstGap=FALSE, RejectLongestGap=FALSE, RejectRT=FALSE,
#'   dependent="Accuracy", paired = TRUE, save_results = TRUE)}
#'

poolData <- function(iChart,
                     dependent = "Accuracy",
                     include_T_initial = FALSE,
                     RejectFirstGap = FALSE,
                     RejectLongestGap =FALSE,
                     RejectRT =FALSE,
                     group = "",
                     paired = TRUE,
                     save_results = FALSE) {

  GoodFirstGap <- RejectFirstGap
  GoodLongestGap <- RejectLongestGap
  GoodRT <- RejectRT

  if(include_T_initial) {
    iChart <- iChart[iChart$Response == "D" | iChart$Response == "T",]
  } else {
    iChart <- iChart[iChart$Response == "D",]
  }

  # this line controls which dependent measure goes into pooling computation
  dep <- which(names(iChart)==dependent)
  iChart$Accuracy <- iChart[,dep]
  groupName <- group

  if(group=="") {group <- FALSE} else {group <- as.factor(iChart[, group])}


  # pool Accuracy across subjects
  filteredAccuracy <- iChart

  if(group[1]!=FALSE) filteredAccuracy <- cbind(filteredAccuracy, group)
  if(GoodFirstGap) filteredAccuracy <- filteredAccuracy[filteredAccuracy$GoodFirstGap | is.na(filteredAccuracy$GoodFirstGap),]
  if(GoodLongestGap) filteredAccuracy <- filteredAccuracy[filteredAccuracy$GoodLongestGap | is.na(filteredAccuracy$GoodLongestGap),]
  if(GoodRT) filteredAccuracy <- filteredAccuracy[filteredAccuracy$GoodRT,]

  valuesBySub <- list(Sub.Num = filteredAccuracy$Sub.Num, Condition = filteredAccuracy$Condition)
  if(group[1]!=FALSE) valuesBySub <- c(valuesBySub, list(group = filteredAccuracy$group))

  sumAccuracy <- aggregate(list(n= filteredAccuracy$Accuracy), valuesBySub, function(value){sum(!is.na(value))})
  pooledAccuracy <- aggregate(list(Accuracy=as.numeric(filteredAccuracy$Accuracy)), valuesBySub, mean, na.rm=T)
  sdAccuracy <- aggregate(list(sd=as.numeric(filteredAccuracy$Accuracy)), valuesBySub, sd)
  tableAccuracy <- cbind(pooledAccuracy, sumAccuracy["n"], sdAccuracy["sd"])

  if ("firstGap" %in% names(filteredAccuracy)) {longestFirstGap <- max(filteredAccuracy$firstGap, na.rm=T) } else {longestFirstGap <-NA }
  if ("longestGap" %in% names(filteredAccuracy)) {longestLongestGap <- max(filteredAccuracy$longestGap, na.rm=T) } else { longestLongestGap <- NA }

  longestRT_D <- max(filteredAccuracy$RT, na.rm=T)
  shortestRT_D <- min(filteredAccuracy$RT, na.rm=T)

  if(save_results) {
    npar <- length(unique(iChart$Sub.Num))
    save_as_ta <- paste(iChart[1, "Directory"], iChart[1, "StudyName"], "_mean_", dependent, "_by_subs_", iChart[1, "StartWindowAcc"], "_", iChart[1, "EndWindowAcc"], "_lg_", longestLongestGap, "_n_", npar, ".txt", sep="")
  }

  averaging <- "Sub.Num~Condition"
  if(group[1]!=FALSE) averaging <- paste(averaging, "+group")

  tableAccuracyPaired_2 <- reshape::cast(pooledAccuracy,as.formula(averaging), mean,  value = 'Accuracy')
  tableAccuracyPairedse <- reshape::cast(sdAccuracy,as.formula(averaging), mean,  value = 'sd')
  tableAccuracyPairedNs <- reshape::cast(sumAccuracy,as.formula(averaging), mean, value = 'n')
  names(tableAccuracyPairedNs)[2:ncol(tableAccuracyPairedNs)] <- paste(names(tableAccuracyPairedNs)[2:ncol(tableAccuracyPairedNs)], "n", sep="_")
  names(tableAccuracyPairedse)[2:ncol(tableAccuracyPairedse)] <- paste(names(tableAccuracyPairedse)[2:ncol(tableAccuracyPairedse)], "sd", sep="_")
  tableAccuracyPaired <- merge(tableAccuracyPaired_2, tableAccuracyPairedse, by="Sub.Num")
  tableAccuracyPaired <- merge(tableAccuracyPaired, tableAccuracyPairedNs, by="Sub.Num")

  if(save_results) {
    save_as <- paste(iChart[1, "Directory"], iChart[1, "StudyName"], "_mean_", dependent, "_by_subs_paired_", iChart[1, "StartWindowAcc"], "_", iChart[1, "EndWindowAcc"], "_lg_", longestLongestGap, "_n_", npar, ".txt", sep="")
  }

  ## build the table to return
  indexGroup <- which(names(tableAccuracy)=="group")
  if("group" %in% names(tableAccuracy)) {names(tableAccuracy)[indexGroup] <- groupName}
  tableAccuracyPaired <- tableAccuracyPaired[,c(1,order(names(tableAccuracyPaired)[2:ncol(tableAccuracyPaired)])+1)]

  if(paired==TRUE) {
    if(save_results) {
      write.table(tableAccuracyPaired, save_as_ta, sep="\t", row.names=F)
    }
    tableAccuracyPaired_2
  } else {
    if(save_results) {
      write.table(tableAccuracy, save_as_ta, sep="\t", row.names=F)
    }
    tableAccuracy
  }

}
