#' Rename Condition In iChart
#'
#' This function renames the conditions in an iChart
#'
#' @description \code{renameCondition()} provides a quick way to rename conditions in an iChart
#' @param iChart A data frame created by the \code{readiChart()} function.
#' @param oldCondition A string indicating the old condition name
#' @param newCondition A string indicating the new condition name
#' @export
#' @examples
#' \dontrun{d <- renameCondition(iChart, oldCondition = "Experimental", newCondition = "Vanilla")}

renameCondition <- function(iChart, oldCondition, newCondition) {
  d_new_condition <- iChart %>%
    dplyr::mutate(Condition = ifelse(Condition == oldCondition,
                                     newCondition,
                                     Condition))

  message("renaming conditions in iChart")

  d_new_condition
}
