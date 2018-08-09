#' Make Profile Plot
#'
#' This function takes graph values and generates a profile plot (proportion looking to target as a function of
#' time).
#'
#' @description \code{makeProfilePlot()} creates a profile plot
#' @param df_gvs A data frame of graph values created by the \code{generateGraphValues()} function.
#' @param startWindow An integer indicating the lower bound of the plotting window in milliseconds.
#' @param endWindow An integer indicating the upper bound of the plotting window in milliseconds.
#' @param bin_width An integer indicating the sampling rate or smoothing rate for the plot. Values
#'   currently supported include 17 or 33 ms.
#' @export
#' @examples
#' \dontrun{makeProfilePlot(df_gvs, startWindow = 0, endWindow = 3000, bin_width = 17)}
#'

makeProfilePlot <- function(df_gvs, startWindow, endWindow, bin_width = 17) {
  window_selector <- dplyr::quo(!! as.character(startWindow) : !! as.character(endWindow))
  message(window_selector)
  # get the means at each time bin
  d_long <- df_gvs %>%
    dplyr::select(Condition, statistic, !! window_selector) %>%
    tidyr::gather(key = time_ms, value = m,  !! window_selector) %>%
    dplyr::mutate(time_bin = ggplot2::cut_width(time_ms, width = bin_width, boundary = 0))

  # separate the standard errors from the mean
  d_se <- d_long %>%
    dplyr::filter(statistic == "se") %>%
    dplyr::rename(se = m) %>%
    dplyr::select(-statistic)

  # make plot
  d_long %>%
    dplyr::filter(statistic == "mean") %>%
    dplyr::left_join(d_se) %>%
    dplyr::group_by(time_bin) %>%
    dplyr::summarise(m = mean(m),
                     se = mean(se)) %>%
    dplyr::mutate(time_ms = seq(0, endWindow, by = bin_width)) %>%
    ggplot2::ggplot(ggplot2::aes(x = time_ms, y = m)) +
    ggplot2::geom_line(group = 1) +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = m - se, ymax = m + se)) +
    ggplot2::geom_hline(yintercept = 0.5, lty = "dashed") +
    ggplot2::lims(y = c(0.3, 1)) +
    ggplot2::labs(x = "time (ms)", y = "prop. looking") +
    ggplot2::theme_classic()
}


