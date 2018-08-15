#' Make a Profile Plot
#'
#' This function takes graph values and generates a profile plot: proportion looking to target as a function of
#' time.
#'
#' @description \code{makeProfilePlot()} creates a profile plot
#' @param df_gvs A data frame of graph values created by the \code{generatePPgraphValues()} function.
#' @param plotStartWindow An integer indicating the lower bound of the plotting window in milliseconds.
#' @param plotEndWindow An integer indicating the upper bound of the plotting window in milliseconds.
#' @param sample_rate An integer indicating the sampling rate of the eye tracker in milliseconds (likely 17 or 33 ms).
#' @param smoothing_factor  An integer controlling the number of data points that are displayed on the Profile Plot.
#'   Higher values display fewer data points. A value of 1 shows all data points.
#' @param save_results A boolean indicating whether the results should be saved to disk.
#' @export
#' @examples
#' \dontrun{makeProfilePlot(df_gvs, plotStartWindow = 0, plotEndWindow = 3000, sample_rate = 17, smoothing_factor = 6,
#'   save_results = TRUE)}
#'

makeProfilePlot <- function(df_gvs,
                            plotStartWindow = 0, plotEndWindow = 3000,
                            sample_rate = 17,
                            smoothing_factor = 5,
                            save_results = TRUE) {
  # extract relevant information from data frame
  study_name <- df_gvs$study_name %>% unique()
  n_participants <- df_gvs$n_participants %>% unique()
  window_selector <- dplyr::quo(!! as.character(plotStartWindow) : !! as.character(plotEndWindow))

  # get the means at each time bin
  d_long <- df_gvs %>%
    dplyr::select(Condition, statistic, !! window_selector) %>%
    tidyr::gather(key = time_ms, value = m,  !! window_selector) %>%
    dplyr::mutate(time_bin = ggplot2::cut_width(time_ms, width = sample_rate, boundary = 0))

  # separate the standard errors from the means
  d_se <- d_long %>%
    dplyr::filter(statistic == "se") %>%
    dplyr::rename(se = m) %>%
    dplyr::select(-statistic)

  # make data frame for the plot
  plot_vals <- d_long %>%
    dplyr::filter(statistic == "mean") %>%
    dplyr::left_join(d_se) %>%
    dplyr::group_by(time_bin, Condition) %>%
    dplyr::summarise(m = mean(m),
                     se = mean(se))

  # extract the number of time bins
  n_time_bins <- plot_vals %>%
    dplyr::ungroup() %>%
    dplyr::select(Condition) %>%
    dplyr::count(Condition) %>%
    dplyr::pull(n) %>%
    unique()

  # create clean time variable for x-axis
  plot_vals <- plot_vals %>%
    dplyr::group_by(Condition) %>%
    dplyr::mutate(time_ms = seq.int(plotStartWindow, plotEndWindow, length.out = n_time_bins) %>% round()) %>%
    dplyr::ungroup()

  # handle case when user supplies negative smoothing factor
  if (smoothing_factor < 1) {smoothing_factor = 1}
  time_bins <- plot_vals %>% dplyr::pull(time_ms) %>% unique()
  breaks <- time_bins[seq(1, length(time_bins), by = smoothing_factor)]

  # make plot
  final_plot <- plot_vals %>%
    ggplot2::ggplot(ggplot2::aes(x = time_ms, y = m, color = Condition, group = Condition)) +
    ggplot2::geom_line() +
    ggplot2::geom_pointrange(ggplot2::aes(ymin = m - se, ymax = m + se),
                             data = dplyr::filter(plot_vals, time_ms %in% breaks)) +
    ggplot2::geom_hline(yintercept = 0.5, lty = "dashed") +
    ggplot2::lims(y = c(-0.05, 1.05)) +
    ggplot2::labs(x = "time (ms)", y = "prop. looking") +
    ggplot2::theme_minimal() +
    ggplot2::theme(panel.border = ggplot2::element_rect(colour = "grey", fill=NA, size=1))

  if(save_results) {
    dir.create("plots", showWarnings = FALSE)
    ggplot2::ggsave(plot = final_plot,
                    path = "plots/",
                    filename = make_file_name_pp_plot(study_name, plotStartWindow, plotEndWindow, n_participants),
                    width = 8,
                    height = 5)
  }

  final_plot
}

make_file_name_pp_plot <- function(study_name, gvStartWindow, gvEndWindow, n_participants, by_subs = TRUE) {
    stringr::str_c(study_name, "PP", gvStartWindow, gvEndWindow, "n", n_participants, sep = "_") %>% stringr::str_c(., ".pdf")
}


