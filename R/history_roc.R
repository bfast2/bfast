########################################
## Reversely Ordered CUSUM (ROC) test ##
########################################

## A technique to verify whether or not the historical period is stable or not
## reversely order sample and perform
## recursive CUSUM test
history_roc <- function(formula, data, level = 0.05) {
  n <- nrow(data)
  data_rev <- data[n:1,]
  data_rev$response <- ts(data_rev$response)
  y_rcus <- efp(formula, data = data_rev, type = "Rec-CUSUM")

  y_start <- if(sctest(y_rcus)$p.value < level) {
    length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
  } else {
    1    
  }
  data$time[y_start]
}