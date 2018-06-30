# iChartAnalyzeR

`iChartAnalyzeR` is a R-package that contains utility functions for
analyzing eyetracking studies in the style of the 
Language Learning Lab at Stanford University.

## Setup

### Install iChartAnalyzeR

Install `iChartAnalyzeR` from this GitHub repository:

```r
install.packages('devtools')
devtools::install_github("kemacdonald/iChartAnalyzeR")
```

## Usage 

For detailed usage information, see the package vignette by running the following line of code 

```r
library(iChartAnalyzeR)
vignette(topic = "iChartAnalyzer_vignette", package = 'iChartAnalyzeR')
```

The following code snippet shows how to use the basic functionality of `iChartAnalyzeR`. Note that
for any of these functions to work, the data have to be in iChart format with the same column naming
conventions.

```r
library(iChartAnalyzeR)

## Read and Preprocess iChart
d <- readiChart(iChartFile = "Habla2_25_iChart_wide.txt", sampling_rate = 17)
d <- computeStatistics(d, startWindow=0, endWindow=2300, save_results = TRUE)
d <- filteriChart(d, minRT=300, maxRT=1800, maxfirstgap=15, maxlonggap=15, save_results = TRUE)
d <- defineOnset(d, critonset = 300, includeAways = FALSE)

## Describe the iChart
describeiChart(d)

## Filter out Prescreened Trials
d_analysis <- filterPrescreened(d, save_results = TRUE)
describeiChart(d_analysis)

## Rename condition
d_analysis <- renameCondition(d_analysis, oldCondition = "Experimental", newCondition = "Vanilla")
describeiChart(d_analysis)

## Filter out unknown words
d_unknown <- readr::read_delim("Habla2_25_unknown_words.txt", delim = "\t")
d_analysis <- removeUnknownWords(d_analysis, d_unknown)

## Compute aggregate accuracy and RT
acc <- poolData(d_analysis,
               dependent="Accuracy",
               include_T_initial = TRUE,
               RejectFirstGap=TRUE,
               RejectLongestGap=TRUE,
               RejectRT=FALSE,
               paired = TRUE,
               save_results = TRUE)

rt <- poolData(d_analysis,
              dependent = "RT",
              include_T_initial = TRUE,
              RejectFirstGap = TRUE,
              RejectLongestGap = TRUE,
              RejectRT = TRUE,
              paired = TRUE,
              save_results = TRUE)

## Make profile plot
createPlots(d_analysis,
            plotStats="PP",
            startWindow=300, endWindow=3000,
            RejectLongestGap=FALSE,
            RejectFirstGap = FALSE,
            RejectRT = FALSE,
            group="")
```
