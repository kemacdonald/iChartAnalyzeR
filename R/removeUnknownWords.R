#' Remove Unknown Words From iChart
#'
#' This function removes unknown words for each participant
#'
#' @description \code{removeUnknownWords()} provides a quick way to rename conditions in an iChart
#' @param iChart A data frame created by the \code{readiChart()} function.
#' @param df_unknown_words A data frame of unknown words for each participant that should be filtered from the analysis.
#' @export
#' @examples
#' \dontrun{d <- removeUnknownWords(iChart, df_unknown_words)}
#'

removeUnknownWords <- function(iChart, df_unknown_words) {
  print("Removing unknown words for each participant")
  iChart %>%
    split(.$Sub.Num) %>%
    purrr::map_df(remove_unknown_ss, df_unknown_words)
}

## helper function to remove unknown words at the individual
## participant level
remove_unknown_ss <- function(df_ss, df_unknown_words) {
  id <- df_ss$Sub.Num %>% unique()
  unknown_words <- df_unknown_words %>%
    dplyr::filter(.data$Sub.Num == id) %>%
    dplyr::pull(.data$unknown_word)

  df_ss %>% dplyr::filter(!(target_word %in% unknown_words))
}
