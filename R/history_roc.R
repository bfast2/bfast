########################################
## Reversely Ordered CUSUM (ROC) test ##
########################################


history_roc <- function (x, ...) {
  UseMethod("history_roc", x)
}


history_roc.matrix <- function(X, y,time, level = 0.05) {
  n <- nrow(X)
  #data_rev <- data[n:1,]
  #data_rev$response <- ts(data_rev$response)
  X_rev  <- X[n:1,]
  y_rev <-  y[n:1]
  time_rev <-  time[n:1]
  
  if (!is.ts(y)) y <- ts(y) # needed?
  y_rcus <- efp.matrix(X_rev,y_rev,time_rev, type = "Rec-CUSUM") ## TODO
  
  pval = sctest(y_rcus)$p.value
  y_start <- if(!is.na(pval) && pval < level) {
    length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
  } else {
    1    
  }
  time[y_start] 
}


## A technique to verify whether or not the historical period is stable or not
## reversely order sample and perform
## recursive CUSUM test
history_roc.formula <- function(formula, data, level = 0.05) {
  n <- nrow(data)
  data_rev <- data[n:1,]
  data_rev$response <- ts(data_rev$response)
  y_rcus <- efp(formula, data = data_rev, type = "Rec-CUSUM")

  pval = sctest(y_rcus)$p.value
  y_start <- if(!is.na(pval) && pval < level) {
    length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
  } else {
    1    
  }
  data$time[y_start]
}
