#' Near Real-Time Disturbance Detection Based on BFAST-Type Models
#' 
#' Monitoring disturbances in time series models (with trend/season/regressor
#' terms) at the end of time series (i.e., in near real-time). Based on a model
#' for stable historical behaviour abnormal changes within newly acquired data
#' can be detected. Different models are available for modeling the stable
#' historical behavior. A season-trend model (with harmonic seasonal pattern)
#' is used as a default in the regresssion modelling.
#' 
#' \code{bfastmonitor} provides monitoring of disturbances (or structural
#' changes) in near real-time based on a wide class of time series regression
#' models with optional season/trend/autoregressive/covariate terms. See
#' Verbesselt at al. (2011) for details.
#' 
#' Based on a given time series (typically, but not necessarily, with frequency
#' greater than 1), the data is first preprocessed for regression modeling.
#' Trend/season/autoregressive/covariate terms are (optionally) computed using
#' \code{\link[bfast]{bfastpp}}. Second, the data is split into a history and
#' monitoring period (starting with \code{start}). Third, a subset of the
#' history period is determined which is considered to be stable (see also
#' below).  Fourth, a regression model is fitted to the preprocessed data in
#' the stable history period.  Fifth, a monitoring procedure is used to
#' determine whether the observations in the monitoring period conform with
#' this stable regression model or whether a change is detected.
#' 
#' The regression model can be specified by the user. The default is to use a
#' linear trend and a harmonic season: \code{response ~ trend + harmon}.
#' However, all other terms set up by \code{bfastpp} can also be omitted/added,
#' e.g., \code{response ~ 1} (just a constant), \code{response ~ season}
#' (seasonal dummies for each period), etc. Further terms precomputed by
#' \code{bfastpp} can be \code{lag} (autoregressive terms of specified order),
#' \code{slag} (seasonal autoregressive terms of specified order), \code{xreg}
#' (covariates, if \code{data} has more than one column).
#' 
#' For determining the size of the stable history period, various approaches
#' are available.  First, the user can set a start date based on subject-matter
#' knowledge. Second, data-driven methods can be employed. By default, this is
#' a reverse-ordered CUSUM test (ROC). Alternatively, breakpoints can be
#' estimated (Bai and Perron method) and only the data after the last
#' breakpoint are employed for the stable history. Finally, the user can also
#' supply a function for his/her own data-driven method.
#' 
#' @param data A time series of class \code{\link[stats]{ts}}, or another
#' object that can be coerced to such. For seasonal components, a frequency
#' greater than 1 is required.
#' @param start numeric. The starting date of the monitoring period. Can either
#' be given as a float (e.g., \code{2000.5}) or a vector giving period/cycle
#' (e.g., \code{c(2000, 7)}).
#' @param formula formula for the regression model. The default is
#' \code{response ~ trend + harmon}, i.e., a linear trend and a harmonic season
#' component. Other specifications are possible using all terms set up by
#' \code{\link[bfast]{bfastpp}}, i.e., \code{season} (seasonal pattern with
#' dummy variables), \code{lag} (autoregressive terms), \code{slag} (seasonal
#' autoregressive terms), or \code{xreg} (further covariates). See
#' \code{\link[bfast]{bfastpp}} for details.
#' @param order numeric. Order of the harmonic term, defaulting to \code{3}.
#' @param lag numeric. Order of the autoregressive term, by default omitted.
#' @param slag numeric. Order of the seasonal autoregressive term, by default
#' omitted.
#' @param history specification of the start of the stable history period. Can
#' either be a character, numeric, or a function. If character, then selection
#' is possible between reverse-ordered CUSUM (\code{"ROC"}, default), Bai and
#' Perron breakpoint estimation (\code{"BP"}), or all available observations
#' (\code{"all"}). If numeric, the start date can be specified in the same form
#' as \code{start}. If a function is supplied it is called as
#' \code{history(formula, data)} to compute a numeric start date.
#' @param type character specifying the type of monitoring process. By default,
#' a MOSUM process based on OLS residuals is employed. See
#' \code{\link[strucchangeRcpp]{mefp}} for alternatives.
#' @param h numeric scalar from interval (0,1) specifying the bandwidth
#' relative to the sample size in MOSUM/ME monitoring processes.
#' @param end numeric. Maximum time (relative to the history period) that will
#' be monitored (in MOSUM/ME processes). Default is 10 times the history
#' period.
#' @param level numeric vector. Significance levels of the monitoring and ROC (if
#' selected) procedure, i.e., probability of type I error.
#' @param hpc character specifying the high performance computing support.
#' Default is \code{"none"}, can be set to \code{"foreach"}. See
#' \code{\link[strucchangeRcpp]{breakpoints}} for more details.
#' @param verbose logical. Should information about the monitoring be printed
#' during computation?
#' @param plot logical. Should the result be plotted?
#' @param sbins numeric. Number of seasonal dummies, passed to
#' \code{\link{bfastpp}}.
#' @return \code{bfastmonitor} returns an object of class
#' \code{"bfastmonitor"}, i.e., a list with components as follows.
#' \item{data}{original \code{"ts"} time series,} \item{tspp}{preprocessed
#' \code{"data.frame"} for regression modeling,} \item{model}{fitted
#' \code{"lm"} model for the stable history period,} \item{mefp}{fitted
#' \code{"mefp"} process for the monitoring period,} \item{history}{start and
#' end time of history period,} \item{monitor}{start and end time of monitoring
#' period,} \item{breakpoint}{breakpoint detected (if any).}
#' \item{magnitude}{median of the difference between the data and the model
#' prediction in the monitoring period.}
#' @author Achim Zeileis, Jan Verbesselt
#' @seealso \code{\link[strucchangeRcpp]{monitor}},
#' \code{\link[strucchangeRcpp]{mefp}}, \code{\link[strucchangeRcpp]{breakpoints}}
#' @references \insertRef{janbfastmonitor}{bfast}
#' @keywords ts
#' @example examples/bfastmonitor.r
#' 
#' @export bfastmonitor
bfastmonitor <- function(data, start,
                         formula = response ~ trend + harmon,
                         order = 3, lag = NULL, slag = NULL,
                         history = c("ROC", "BP", "all"),
                         type = "OLS-MOSUM", h = 0.25, end = 10, level = c(0.05, 0.05),
                         hpc = "none", verbose = FALSE, plot = FALSE, sbins = 1)
{
  
  if (getOption("bfast.prefer_matrix_methods", FALSE)) {
    return(.bfastmonitor.matrix(data,start,formula,order, lag, slag,
        history, type, h, end, level, hpc, verbose, plot, sbins))
  }
  else {
    return(.bfastmonitor.default(data,start,formula,order, lag, slag,
        history, type, h, end, level, hpc, verbose, plot, sbins))
  }
}



.bfastmonitor.matrix <- function(data, start,
                         formula = response ~ trend + harmon,
                         order = 3, lag = NULL, slag = NULL,
                         history = c("ROC", "BP", "all"),
                         type = "OLS-MOSUM", h = 0.25, end = 10, level = c(0.05, 0.05),
                         hpc = "none", verbose = FALSE, plot = FALSE, sbins = 1)
{
  ## PREPROCESSING
  ## two levels needed: 1. monitoring, 2. in ROC (if selected)
  if (length(level) == 1) # Backwards compatibility, assume both are the same
    level <- rep(level, length.out = 2)
    
  if(!is.ts(data)) data <- as.ts(data)
  
  ## frequency of data
  freq <- frequency(data)
  ## start on natural scale (if necessary)
  time2num <- function(x) if(length(x) > 1L) x[1L] + (x[2L] - 1)/freq else x
  start <- time2num(start)
  

  ## full data
  data_tspp <- bfastpp(data, order = order, lag = lag, slag = slag, sbins = sbins)
  data_tsmat = model.matrix(formula, data_tspp)
  X = data_tsmat
  y = data_tspp$response
  time = data_tspp$time
  rm(data_tspp, data_tsmat)
  
  ## SELECT STABLE HISTORY  
  ## full history period
  history_X =  X[time < start,]
  history_y =  y[time < start]
  history_time = time[time < start]
  if (length(history_y) <= ncol(history_X))
      stop("Fewer observations in the history period than number of regressors")
  
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
                         type = "OLS-MOSUM", h = 0.25, end = 10, level = c(0.05, 0.05),
                         hpc = "none", verbose = FALSE, plot = FALSE, sbins = 1)
{
  ## PREPROCESSING
  ## two levels needed: 1. monitoring, 2. in ROC (if selected)
  if (length(level) == 1) # Backwards compatibility, assume both are the same
    level <- rep(level, length.out = 2)
  
  if(!is.ts(data)) data <- as.ts(data)
  
  ## frequency of data
  freq <- frequency(data)
  ## start on natural scale (if necessary)
  time2num <- function(x) if(length(x) > 1L) x[1L] + (x[2L] - 1)/freq else x
  start <- time2num(start)
  
  ## full data
  data_tspp <- bfastpp(data, order = order, lag = lag, slag = slag, sbins = sbins)
  
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







