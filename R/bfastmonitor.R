
bfastmonitor <- function(data, start,
                         formula = response ~ trend + harmon,
                         order = 3, lag = NULL, slag = NULL,
                         history = c("ROC", "BP", "all"),
                         type = "OLS-MOSUM", h = 0.25, end = 10, level = 0.05,
                         hpc = "none", verbose = FALSE, plot = FALSE)
{
  
  if (getOption("bfast.prefer_matrix_methods", FALSE)) {
    return(.bfastmonitor.matrix(data,start,formula,order, lag, slag,
                                 history, type, h, end, level, hpc, verbose, plot))
  }
  else {
    return(.bfastmonitor.default(data,start,formula,order, lag, slag,
                                history, type, h, end, level, hpc, verbose, plot))
  }
}



.bfastmonitor.matrix <- function(data, start,
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
  data_tspp <- bfastpp(data, order = order, lag = lag, slag = slag, formula = formula)
  X = data_tspp$X
  y = data_tspp$y
  time = data_tspp$t
  
  ## SELECT STABLE HISTORY  
  ## full history period
  history_X =  X[time < start,]
  history_y =  y[time < start]
  history_time = time[time < start]
  
  ## find start of history period
  ## (may be specified via character, function, or time index directly)
  if(is.null(history)) {
    history <- start(history_y)
  } else if(all(is.character(history))) {
    history <- match.arg(history)
    history <- switch(history,    
                      "all" = time[1],  # or time[1]   
                      "ROC" = history_roc.matrix(history_X,history_y, history_time, level = level[2]), # TODO: make these functions work with X and y
                      "BP" = history_break.matrix(history_X,history_y, history_time, hpc = hpc) # TODO: make these functions work with X and y
    )
  } else if(all(is.function(history))) {
    history <- history(history_X,history_y, history_time) # TODO: make these functions work with X and y
  }
  history <- time2num(history)
  
  ## compute subset
  history_X = history_X[history_time >= history,] # TODO: maybe use window to maintain t info?
  history_y = history_y[history_time >= history] # TODO: maybe use window to maintain t info?
  history_time = history_time[history_time >= history]
  
  ## output information (if desired)
  if(verbose) {
    cat("\nBFAST monitoring\n\n1. History period\n")
    cat(sprintf("Stable period selected: %i(%i)--%i(%i)\n",
                start(history_y)[1], start(history_y)[2], 
                end(history_y)[1], end(history_y)[2]))
    cat(sprintf("Length (in years): %f\n", NROW(history_y)/freq))
  }
  
  
  ## MODEL HISTORY PERIOD
  test_mefp <- mefp.matrix(history_X,history_y,  
                           type = type, period = end, h = h, alpha = level[1])
  test_lm = lm.fit(x = history_X, y=history_y) 
  
  class(test_lm) <- "lm"
  test_lm$na.action <- na.omit
  test_lm$offset <- NULL
  test_lm$contrasts <- NULL
  test_lm$xlevels <- NULL
  test_lm$assign <- 0:(ncol(history_X)-1)
  test_lm$call <- call("lm",formula = formula)
  test_lm$terms <- terms(formula)
  test_lm$model = cbind(history_y,history_X)
  
  if(floor(h * NROW(history_X)) <= 1 | NROW(history_X) <= length(test_lm$coefficients)) {
    ok <- FALSE
    warning("too few observations in selected history period")
  } else {
    ok <- TRUE
  }
  if(verbose) {
    cat("Model fit:\n")
    print(test_lm$coefficients)
  }
  
  ## MONITOR CHANGES IN THE MONITORING PERIOD
  test_tspp_X <- X[time >= history,]
  test_tspp_y <- y[time >= history]
  test_tspp_time <- time[time >= history]
  
  if(ok) {
    test_mon <- monitor.matrix(test_mefp, test_tspp_X,as.matrix(test_tspp_y), verbose = FALSE) 
    tbp <- if(is.na(test_mon$breakpoint)) NA else test_tspp_time[test_mon$breakpoint]
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
    prediction <- test_tspp_X %*% test_lm$coefficients
    new_data_y <- test_tspp_y[test_tspp_time >= start]
    new_data_prediction <- prediction[test_tspp_time >= start]
    magnitude <- median(new_data_y - new_data_prediction,na.rm=TRUE)
  } else {
    prediction <- NA
    magnitude <- NA
  }

  ## set up return object
  rval <- list(
    data = data,
    tspp = cbind(test_tspp_y, time = test_tspp_time, prediction=prediction,test_tspp_X),
    model = test_lm, 
    mefp = test_mon,
    history = c(head(history_time, 1), tail(history_time, 1)),
    monitor = c(start, tail(test_tspp_time, 1)),
    breakpoint = tbp,
    magnitude = magnitude
  )
  class(rval) <- "bfastmonitor"
  
  ## plot if desired
  if(plot) plot(rval)
  
  ## return object
  return(rval)
}










.bfastmonitor.default <- function(data, start,
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







