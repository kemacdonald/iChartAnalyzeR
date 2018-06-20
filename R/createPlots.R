#' Create LWL Plots
#'
#' This function generates two types of plots for PEEK data: Profile Plots and Onset Contingency Plots
#'
#' @description \code{createPlots()} generates either a Profile Plot or Onset Contingency Plot.
#' @param iChart A data frame in iChart format with iChart column names.
#' @param startWindow An integer indicating the lower bound of the analysis window in milliseconds.
#' @param endWindow An integer indicating the upper bound of the analysis window in milliseconds.
#' @param RejectFirstGap A boolean indicating whether bad first gaps should be filtered out of the computation.
#' @param RejectLongestGap A boolean indicating whether bad longest gaps should be filtered out of the computation.
#' @param RejectRT A boolean indicating whether bad RTs should be filtered out of the computation.
#' @param plotStats A string indicating which type of plot to generate measure to use (PP, OC, OC_T, OC_D).
#' @param group A string indicating the variable name for any grouping variables (e.g., conditions).
#' @param ... Additional style arguments for tweaking the ggplot output.
#' @export
#' @examples
#' \dontrun{acc <- createPlots(d, startWindow=0, endWindow=3000,
#'   RejectFirstGap=FALSE, RejectLongestGap=FALSE, RejectRT=FALSE,
#'   plotStats = "PP", group = "", save_results = TRUE)}
#'

createPlots <- function(iChart,
                        startWindow,
                        endWindow,
                        RejectLongestGap,
                        RejectFirstGap,
                        RejectRT,
                        plotStats = "PP",
                        group="",
                        ...) {

  ## define plotting variables
  color <- FALSE
  smooth <- 100
  targetEnd <- 800
  carrier <- ""
  targets <- ""
  miny <- -1
  maxy <- -1
  legendPosition <- FALSE
  size <- 13
  legend.direction <- "vertical"
  legend.position <- "right"
  breaks <- -1
  x.target <- 0.30

  ## define global variables for filtering and grouping steps
  GoodLongestGap <- RejectLongestGap
  GoodFirstGap <- RejectFirstGap
  GoodRT <- RejectRT
  endTarget <- targetEnd

  if(group == "") {group <- FALSE} else {group <- iChart[,group]}

  startW <- startWindow

  if(group[1]!=FALSE) iChart <- cbind(iChart, group)
  if(GoodLongestGap) iChart <- iChart[iChart$GoodLongestGap | is.na(iChart$GoodLongestGap),]
  if(GoodFirstGap) iChart <- iChart[iChart$GoodFirstGap | is.na(iChart$GoodFirstGap),]
  if(GoodRT) iChart <- iChart[iChart$GoodRT,]

  sw <- startWindow
  ew <- endWindow

  onsetBy <- startWindow
  startWindow <- which(names(iChart)==startWindow)
  endWindow <- which(names(iChart)==endWindow)

  iChart <- iChart[iChart$Response == "D" | iChart$Response == "T", ]

  if(plotStats == "OC_D") iChart <- iChart[iChart$Response == "D", ]
  if(plotStats == "OC_T") iChart <- iChart[iChart$Response == "T", ]

  longestFirstGap <- max(iChart$firstGap, na.rm=T)
  longestLongestGap <- max(iChart$longestGap, na.rm=T)
  longestRT_D <- max(iChart$RT, na.rm=T)
  shortestRT_D <- min(iChart$RT, na.rm=T)
  npar <- length(unique(iChart$Sub.Num))

  # Profile Plot
  value <- list(Sub.Num = iChart$Sub.Num, Condition = iChart$Condition)
  if(group[1]!=FALSE) value <- c(value, list(groupping=iChart$group))
  if(plotStats=="OC") value <- c(value, list(Response=iChart$Response))

  pooledPlotAccuracy <- stats::aggregate(data.matrix(iChart[,startWindow:endWindow]), value, mean, na.rm=T)

  startPlot <- which(names(pooledPlotAccuracy)==sw)
  endPlot <- which(names(pooledPlotAccuracy)==ew)

  if(plotStats=="OC") pooledPlotAccuracy[pooledPlotAccuracy$Response == "T",startPlot:endPlot] <- 1 - pooledPlotAccuracy[pooledPlotAccuracy$Response == "T",startPlot:endPlot]
  if(plotStats=="OC_T") pooledPlotAccuracy[,startPlot:endPlot] <- 1 - pooledPlotAccuracy[,startPlot:endPlot]

  save_as <- paste(iChart[1, "Directory"], iChart[1, "StudyName"], "_", plotStats,"_graphValues_by_subs_",sw, "_", ew, "_minRT_",  shortestRT_D, "_maxRT_", longestRT_D, "_lg_", longestLongestGap, "_fg_", longestFirstGap, "_n_", npar, ".txt", sep="")

  if(plotStats %in% c("PP","OC")) {
    write.table(pooledPlotAccuracy, save_as, sep="\t", row.names=F)
  }

  value <- list(Condition = pooledPlotAccuracy$Condition)
  if(group[1]!=FALSE) value <- c(value, list(groupping= pooledPlotAccuracy$group))
  if(plotStats=="OC") value <- c(value, list(Response= pooledPlotAccuracy$Response))

  startPlot <- which(names(pooledPlotAccuracy)==sw)
  endPlot <- which(names(pooledPlotAccuracy)==ew)

  plotProfilemean <- stats::aggregate(data.matrix(pooledPlotAccuracy[,startPlot:endPlot]), value, mean, na.rm=T)
  plotProfilemean <- cbind(statistic = rep("mean", length(plotProfilemean$Condition)), plotProfilemean)
  plotProfilese <- stats::aggregate(data.matrix(pooledPlotAccuracy[,startPlot:endPlot]), value, function(value) { sd(value, na.rm=TRUE)/sqrt(sum(!is.na(value)))})
  plotProfilese <- cbind(statistic = rep("se", length(plotProfilese$Condition)), plotProfilese)
  plotProfile <- rbind(plotProfilemean, plotProfilese)

  save_as <- paste(iChart[1, "Directory"], iChart[1, "StudyName"], "_", plotStats, "_graphValues_",sw, "_", ew, "_minRT_",  shortestRT_D, "_maxRT_", longestRT_D, "_lg_", longestLongestGap, "_fg_", longestFirstGap, "_n_", npar, ".txt", sep="")

  if(plotStats %in% c("PP","OC")) {
    write.table(plotProfile, save_as, sep="\t", row.names=F)
  }

  idvalue <- c("Condition")
  if(group[1]!=FALSE) idvalue <- c("Condition", "groupping")
  if(plotStats == "OC") idvalue <- c(idvalue, "Response")

  tablePlotProfile = reshape::melt(plotProfilemean[,2:endPlot], id=idvalue)
  tablePlotProfilese = reshape::melt(plotProfilese[,2:endPlot], id=idvalue)
  tablePlotProfile$ucl <- tablePlotProfile$value + tablePlotProfilese$value
  tablePlotProfile$lcl <- tablePlotProfile$value - tablePlotProfilese$value
  tablePlotProfile$variable <- as.numeric(as.character(tablePlotProfile$variable))

  if (smooth > 33) {
    tablePlotProfile$lcl[tablePlotProfile$variable %% smooth != 0] <- NA
    tablePlotProfile$ucl[tablePlotProfile$variable %% smooth != 0] <- NA
  }

  if(plotStats == "OC") {
    plotMeansAccuracy <- ggplot2::qplot(variable, value, data = tablePlotProfile, shape= Response, colour=Condition, group=interaction(Condition, Response),geom="blank") + ggplot2::geom_errorbar(ggplot2::aes(ymin = lcl, ymax = ucl, fill=Condition), data=tablePlotProfile, stat="identity", width=100) + ggplot2::xlab("Time (ms) from noun onset") + ggplot2::labs(colour = "", shape = "", fill = "") + ggplot2::ylab("Proportion\n  Looking\n  to target") + ggplot2::theme(panel.background = ggplot2::element_blank()) + ggplot2::theme(axis.title.x = ggplot2::element_text(vjust = -0.5, size=13))  + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust=-0.5, vjust=0.5, size=13)) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) + ggplot2::theme(axis.line = ggplot2::element_line()) + ggplot2::theme(plot.title = ggplot2::element_text(vjust = 1.3, size=18)) + ggplot2::theme(plot.margin = ggplot2::unit(c(1, 1, 1, 3), "lines")) + ggplot2::theme(legend.key = ggplot2::element_blank()) + ggplot2::geom_line(size = 0.8) + ggplot2::theme(strip.text.y = ggplot2::element_text(angle = -90, size=13)) + ggplot2::theme(legend.text = ggplot2::element_text(size=12))+ggplot2::geom_point(ggplot2::aes(x=variable, y=value, colour=Condition, shape=Response), size=4, data= tablePlotProfile[!is.na(tablePlotProfile$ucl),]) + ggplot2::geom_vline(xintercept = startW, linetype = "dashed", size=0.3)+ ggplot2::geom_vline(xintercept = endTarget, linetype = "dashed", size=0.3)
  } else {
    if (length(unique(iChart$Condition)) > 6) {
      plotMeansAccuracy <- ggplot2::qplot(variable, value, data = tablePlotProfile, colour=Condition, group=Condition,geom="line") + ggplot2::geom_errorbar(ggplot2::aes(ymin = lcl, ymax = ucl, fill=Condition), data=tablePlotProfile, stat="identity", width=100) + ggplot2::xlab("Time (ms) from noun onset") + ggplot2::labs(colour = "", shape = "", fill = "") + ggplot2::ylab("Proportion\n  Looking\n  to target") + ggplot2::theme(panel.background = ggplot2::element_blank()) + ggplot2::theme(axis.title.x = ggplot2::element_text(vjust = -0.5, size=13))  + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust=-0.5, vjust=0.5, size=13)) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) + ggplot2::theme(axis.line = ggplot2::element_line()) + ggplot2::theme(plot.title = ggplot2::element_text(vjust = 1.3, size=18)) + ggplot2::theme(plot.margin = ggplot2::unit(c(1, 1, 1, 3), "lines")) + ggplot2::theme(legend.key = ggplot2::element_blank()) + ggplot2::geom_line(size = 0.8)  + ggplot2::theme(strip.text.y = ggplot2::element_text(angle = -90, size=13)) + ggplot2::theme(legend.text = ggplot2::element_text(size=12))+ggplot2::geom_point(ggplot2::aes(x=variable, y=value, colour=Condition), size=4, data= tablePlotProfile[!is.na(tablePlotProfile$ucl),]) + ggplot2::geom_vline(xintercept = startW, linetype = "dashed", size=0.3)+ ggplot2::geom_vline(xintercept = endTarget, linetype = "dashed", size=0.3)
    } else {
      plotMeansAccuracy <- ggplot2::qplot(variable, value, data = tablePlotProfile, colour=Condition, group=Condition, geom="line") + ggplot2::geom_errorbar(ggplot2::aes(ymin = lcl, ymax = ucl, fill=Condition), data=tablePlotProfile, stat="identity", width=100) + ggplot2::xlab("Time (ms) from picture onset") + ggplot2::labs(colour = "", shape = "", fill = "") + ggplot2::ylab("Proportion looking to target") + ggplot2::theme(panel.background = ggplot2::element_blank()) + ggplot2::theme(axis.title.x = ggplot2::element_text(vjust = -0.5, size=13))  + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust=-0.5, vjust=0.5, size=13)) + ggplot2::theme(panel.grid.minor = ggplot2::element_blank()) + ggplot2::theme(axis.line = ggplot2::element_line()) + ggplot2::theme(plot.title = ggplot2::element_text(vjust = 1.3, size=18)) + ggplot2::theme(plot.margin = ggplot2::unit(c(1, 1, 1, 3), "lines")) + ggplot2::theme(legend.key = ggplot2::element_blank()) + ggplot2::geom_line(size = 0.8) + ggplot2::theme(strip.text.y = ggplot2::element_text(angle = -90, size=13)) + ggplot2::theme(legend.text = ggplot2::element_text(size=12))+ggplot2::geom_point(ggplot2::aes(x=variable, y=value, colour=Condition, shape=Condition), size=4, data= tablePlotProfile[!is.na(tablePlotProfile$ucl),]) + ggplot2::geom_vline(xintercept = startW, linetype = "dashed", size=0.3)+ ggplot2::geom_vline(xintercept = endTarget, linetype = "dashed", size=0.3)
    }
  }


  if(group[1]!=FALSE) plotMeansAccuracy <- plotMeansAccuracy + ggplot2::facet_grid(groupping~.) + ggplot2::theme(panel.background = element_rect()) + ggplot2::theme(axis.line = ggplot2::element_blank()) + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust=-0.5, vjust=0.8, size=13))
  if(legendPosition == TRUE) plotMeansAccuracy <-  plotMeansAccuracy + ggplot2::theme(legend.position=c(0.86,0.90))
  if(plotStats == "PP") plotMeansAccuracy <-  plotMeansAccuracy + ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", size=0.3)
  if(breaks != -1) plotMeansAccuracy <- plotMeansAccuracy + scale_y_continuous(breaks = breaks)
  if(maxy != -1) plotMeansAccuracy <-  plotMeansAccuracy + coord_cartesian(ylim = c(miny, maxy))
  if(!color) {plotMeansAccuracy <- plotMeansAccuracy + ggplot2::scale_colour_grey()} else { plotMeansAccuracy <- plotMeansAccuracy + scale_colour_hue()}

  plotMeansAccuracy <- plotMeansAccuracy + ggplot2::theme(axis.title.x = ggplot2::element_text(vjust = -0.5, size=size))  + ggplot2::theme(axis.title.y = ggplot2::element_text(hjust=-0.5, vjust=0.5, size=size)) + ggplot2::theme(axis.line = ggplot2::element_line()) + ggplot2::theme(strip.text.y = ggplot2::element_text(angle = 30))+ ggplot2::theme(strip.text.x = ggplot2::element_text(angle = 0, size=size))+ ggplot2::theme(axis.text.x=ggplot2::element_text(size=size-2, vjust=1))+ ggplot2::theme(axis.text.y=ggplot2::element_text(size=size-2, hjust=1, colour="black"))

  plotMeansAccuracy <- plotMeansAccuracy + ggplot2::theme(legend.direction = legend.direction, legend.position=legend.position) + ggplot2::theme(legend.text= ggplot2::element_text(size = size))

  plotMeansAccuracy <- plotMeansAccuracy + ggplot2::theme(legend.text= ggplot2::element_text(size = size))

  if(group!="")plotMeansAccuracy <- plotMeansAccuracy +  ggplot2::theme(strip.text.y = ggplot2::element_text(size = size, angle = 90)) + ggplot2::theme(strip.text.x = ggplot2::element_text(angle = 0,size=size)) + ggplot2::theme(axis.title.y = ggplot2::element_text(size=size, angle = 90, vjust=-0.2, hjust= - 0.1))

  save_as <- paste(iChart[1, "Directory"], iChart[1, "StudyName"], "_", plotStats, "_plot_",sw, "_", ew, "_minRT_",  shortestRT_D, "_maxRT_", longestRT_D, "_lg_", longestLongestGap, "_fg_", longestFirstGap, "_n_", npar, ".pdf", sep="")


  grDevices::pdf(file=save_as, width=10, height=5)
  print(plotMeansAccuracy)

  x <- 0.07
  y <- 0.93
  grid::grid.text(carrier, x=x, y=y, gp = grid::gpar(fontsize=21, col="black", fontface="italic"))

  x <- x.target
  y <- 0.99
  for(i_word in 1:length(targets)) {
    y <- y - 0.06
    grid::grid.text(targets[i_word], x=x, y=y, gp=grid::gpar(fontsize=21, col="black"))
    if(i_word > 1){grid::grid.text(targets[i_word], x=x, y=y, gp=grid::gpar(fontsize=21, col="grey"))} else {grid::grid.text(targets[i_word], x=x, y=y, gp=grid::gpar(fontsize=21, col="black"))}
  }

  dev.off()

}
