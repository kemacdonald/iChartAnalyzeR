#' Remove Unknown Words From iChart
#'
#' This function removes unknown words for each participant
#'
#' @description \code{removeUnknownWords()} provides a quick way to rename conditions in an iChart
#' @param iChart A data frame created by the \code{readiChart()} function.
#' @param knownWords_file A file with a table of unknown words for each participant
#' @param knows_threshold An integer indicating the threshold value for whether a word is known.
#' @export
#' @examples
#' \dontrun{d <- removeUnknownWords(iChart, knownWords_file = "knows.txt, knows_threshold = 3)}
#'

removeUnknownWords <- function(iChart, knownWords_file, knows_threshold = 3) {
  print("Removing unknown words for each participant")
  df_unknown_words <- read_known_words(knownWords_file, knows_threshold)

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
    dplyr::pull(.data$word)

  df_ss %>% dplyr::filter(!(target_word %in% unknown_words))
}

## helper function to read in Lang Leanring Lab style known words data file
read_known_words <- function(knownWords_file, knows_threshold) {
  df <- readr::read_delim(knownWords_file, delim = "\t")
  # convert to long format
  df_long <- df %>% tidyr::gather(key = word, value = value, -Sub.Num, -response)
  # return data frame with uknown words for each ss
  df_long %>%
    dplyr::mutate(known_status = ifelse(value >= knows_threshold, "known", "unknown")) %>%
    dplyr::filter(known_status == "unknown", response == "understands") %>%
    dplyr::arrange(Sub.Num)
}


