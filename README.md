# iChartAnalyzeR

`iChartAnalyzeR` is a R-package that contains utility functions for
analyzing eyetracking studies in the style of the 
Language Learning Lab at Stanford University.

## Setup

### Install iChartAnalyzeR

Install `iChartAnalyzeR` from this GitHub repository:

```S
install.packages('devtools')
devtools::install_github("kemacdonald/iChartAnalyzeR")
```

## Usage 

The following code snippet shows how to use the basic functionality of `iChartAnalyzeR`. Note that
for any of these functions to work, the data have to be in iChart format with the same column naming
conventions.

```r
library(iChartAnalyzeR)

# Read and Preprocess iChart
d <- readiChart(iChartFile = "Habla2_25_iChart_wide.txt") 
d <- computeStatistics(d, startWindow=0, endWindow=2300, save_results = TRUE)  
d <- filteriChart(d, minRT=300, maxRT=1800, maxfirstgap=15, maxlonggap=15, save_results = TRUE)
d <- defineOnset(d, critonset = 300, includeAways = FALSE)

## Compute aggregate accuracy and RT 
acc <- poolData(d, 
                RejectFirstGap=FALSE,
                RejectLongestGap=FALSE, 
                RejectRT=FALSE, 
                dependent="Accuracy",
                paired = TRUE,
                save_results = TRUE)

rt <- poolData(d[d$Response == "D",], 
               RejectFirstGap = FALSE,
               RejectLongestGap = FALSE, 
               RejectRT = TRUE, 
               dependent = "RT",
               paired = TRUE,
               save_results = TRUE)

## Make profile plot
createPlots(d, startWindow=300, endWindow=3000, 
            RejectLongestGap=FALSE, 
            RejectFirstGap = FALSE, 
            RejectRT = FALSE, 
            group="", 
            plotStats="PP")
```
