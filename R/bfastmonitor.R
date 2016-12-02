bfastmonitor <- function(data, start,
  formula = response ~ trend + harmon,
  order = 3, lag = NULL, slag = NULL,
  history = c("ROC", "BP", "all"),
  type = "OLS-MOSUM", h = 0.25, end = 10, level = 0.05,
  hpc = "none", verbose = FALSE, plot = FALSE)
{
  ## PREPROCESSING
  ## two levels needed: 1. monitoring, 2. in ROC (if selected)
  level <- rep(level, length.out = 2)
  
  if(!is.ts(data)) data <- as.ts(data)
  
  ## frequency of data
  freq <- frequency(data)
  ## start on natural scale (if necessary)
  time2num <- function(x) if(length(x) > 1L) x[1L] + (x[2L] - 1)/freq else x
  start <- time2num(start)

  ## full data
  data_tspp <- bfastpp(data, order = order, lag = lag, slag = slag)
    
  ## SELECT STABLE HISTORY  
  ## full history period
  history_tspp <- subset(data_tspp, time < start)

  ## find start of history period
  ## (may be specified via character, function, or time index directly)
  if(is.null(history)) {
    history <- start(history_tspp$response)
  } else if(all(is.character(history))) {
    history <- match.arg(history)
    history <- switch(history,    
      "all" = start(history_tspp$response),      
      "ROC" = history_roc(formula, data = history_tspp, level = level[2]),
      "BP" = history_break(formula, data = history_tspp, hpc = hpc)
    )
  } else if(all(is.function(history))) {
    history <- history(formula, data = history_tspp)
  }
  history <- time2num(history)

  ## compute subset
  history_tspp <- subset(history_tspp, time >= history)

  ## output information (if desired)
  if(verbose) {
    cat("\nBFAST monitoring\n\n1. History period\n")
    cat(sprintf("Stable period selected: %i(%i)--%i(%i)\n",
      start(history_tspp$response)[1], start(history_tspp$response)[2],
      end(history_tspp$response)[1], end(history_tspp$response)[2]))
    cat(sprintf("Length (in years): %f\n", NROW(history_tspp)/freq))
  }


  ## MODEL HISTORY PERIOD
  test_tspp <- history_tspp
  test_mefp <- mefp(formula, data = test_tspp, 
    type = type, period = end, h = h, alpha = level[1])
  test_lm <- lm(formula, data = test_tspp)
  if(floor(h * NROW(test_tspp)) <= 1 | NROW(test_tspp) <= length(coef(test_lm))) {
    ok <- FALSE
    warning("too few observations in selected history period")
  } else {
    ok <- TRUE
  }
  if(verbose) {
    cat("Model fit:\n")
    print(coef(test_lm))
  }

  ## MONITOR CHANGES IN THE MONITORING PERIOD
  test_tspp <- subset(data_tspp, time >= history)
  if(ok) {
    test_mon <- monitor(test_mefp, data = test_tspp, verbose = FALSE)
    tbp <- if(is.na(test_mon$breakpoint)) NA else test_tspp$time[test_mon$breakpoint]
    if(verbose) {
      cat("\n\n2. Monitoring period\n")
      cat(sprintf("Monitoring starts at: %i(%i)\n", floor(start), round((start - floor(start)) * freq) + 1))
      if(is.na(tbp)) {      
          cat("Break detected at: -- (no break)\n\n")
        } else {
          cat(sprintf("Break detected at: %i(%i)\n\n", floor(tbp), round((tbp - floor(tbp)) * freq) + 1))
      }
    }
  } else {
    test_mon <- NA
    tbp <- NA
  }
  
  ## the magnitude of change
  if(ok) {
    test_tspp$prediction <- predict(test_lm, newdata = test_tspp)
    new_data <- subset(test_tspp, time>=start) ## only data from the monitoring period
    magnitude <- median(new_data$response - new_data$prediction,na.rm=TRUE)
  } else {
    test_tspp$prediction <- NA
    magnitude <- NA
  }
  
  ## set up return object
  rval <- list(
    data = data,
    tspp = test_tspp,
    model = test_lm,
    mefp = test_mon,
    history = c(head(history_tspp$time, 1), tail(history_tspp$time, 1)),
    monitor = c(start, tail(test_tspp$time, 1)),
    breakpoint = tbp,
    magnitude = magnitude
  )
  class(rval) <- "bfastmonitor"
  
  ## plot if desired
  if(plot) plot(rval)
  
  ## return object
  return(rval)
}
