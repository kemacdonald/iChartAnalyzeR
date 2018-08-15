#' Define Onsets For iChart
#'
#' This function defines the type of trial (D-, T-, or A-initial) based on a user-specified
#' critical onset value.
#'
#' @description \code{defineOnset()}the type of trial (D-, T-, or A-initial) based on a user-specified
#'   critical onset value provided in milliseconds.
#' @param iChart A data frame in iChart format with iChart column names.
#' @param critonset An integer indicating the critical onset value in milliseconds.
#' @param includeAways A boolean indicating whether Away responses should be inlcluded.
#' @export
#' @examples
#' \dontrun{d <- defineOnset(iChart, critonset = 0, includeAways = FALSE)}


defineOnset <- function(iChart, critonset = 0, includeAways = FALSE) {
  F0 <- critonset
  F0 <- which(names(iChart)==F0)
  iChart$Response <- ifelse(iChart[,F0] == "1", "T", ifelse(iChart[,F0] == "0", "D", "A"))

  if(includeAways) {
    iChart$Response <- ifelse(iChart[,F0] == "1", "T", ifelse(iChart[,F0] == "0", "D", "T"))
  }
  iChart
}
