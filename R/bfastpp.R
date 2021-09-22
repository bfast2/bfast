#' Time Series Preprocessing for BFAST-Type Models
#' 
#' Time series preprocessing for subsequent regression modeling.  Based on a
#' (seasonal) time series, a data frame with the response, seasonal terms, a
#' trend term, (seasonal) autoregressive terms, and covariates is computed.
#' This can subsequently be employed in regression models.
#' 
#' To facilitate (linear) regression models of time series data, \code{bfastpp}
#' facilitates preprocessing and setting up regressor terms. It returns a
#' \code{data.frame} containing the first column of the \code{data} as the
#' \code{response} while further columns (if any) are used as covariates
#' \code{xreg}. Additionally, a linear trend, seasonal dummies, harmonic
#' seasonal terms, and (seasonal) autoregressive terms are provided.
#' 
#' Optionally, each column of \code{data} can be seasonally adjusted and/or
#' trend-adjusted via STL (season-trend decomposition via LOESS smoothing)
#' prior to preprocessing. The idea would be to capture season and/or trend
#' nonparametrically prior to regression modelling.
#' 
#' @param data A time series of class \code{\link[stats]{ts}}, or another
#' object that can be coerced to such. For seasonal components, a frequency
#' greater than 1 is required.
#' @param order numeric. Order of the harmonic term, defaulting to \code{3}.
#' @param lag numeric. Orders of the autoregressive term, by default omitted.
#' @param slag numeric. Orders of the seasonal autoregressive term, by default
#' omitted.
#' @param na.action function for handling \code{NA}s in the data (after all
#' other preprocessing).
#' @param stl character. Prior to all other preprocessing, STL (season-trend
#' decomposition via LOESS smoothing) can be employed for trend-adjustment
#' and/or season-adjustment.  The \code{"trend"} or \code{"seasonal"} component
#' or both from \code{\link[stats]{stl}} are removed from each column in
#' \code{data}. By default (\code{"none"}), no STL adjustment is used.
#' @param decomp "stlplus" or "stl": use the NA-tolerant decomposition package
#' or the reference package (which can make use of time series with 2-3
#' observations per year)
#' @param sbins numeric. Controls the number of seasonal dummies. If integer > 1,
#' sets the number of seasonal dummies to use per year.
#' If <= 1, treated as a multiplier to the number of observations per year, i.e.
#' `ndummies = nobs/year * sbins`.
#' @return If no formula is provided, \code{bfastpp} returns a
#' \code{"data.frame"} with the following variables (some of which may be
#' matrices).  \item{time}{numeric vector of time stamps,}
#' \item{response}{response vector (first column of \code{data}),}
#' \item{trend}{linear time trend (running from 1 to number of observations),}
#' \item{season}{factor indicating season period,} \item{harmon}{harmonic
#' seasonal terms (of specified \code{order}),} \item{lag}{autoregressive terms
#' (or orders \code{lag}, if any),} \item{slag}{seasonal autoregressive terms
#' (or orders \code{slag}, if any),} \item{xreg}{covariate regressor (all
#' columns of \code{data} except the first, if any).}
#' 
#' If a formula is given, \code{bfastpp} returns a \code{list} with components
#' \code{X}, \code{y}, and \code{t}, where \code{X} is the design matrix of the
#' model, \code{y} is the response vector, and \code{t} represents the time of
#' observations. \code{X} will only contain variables that occur in the
#' formula. Columns of \code{X} have names as decribed above.
#' @author Achim Zeileis
#' @seealso \code{\link[bfast]{bfastmonitor}}
#' @references \insertRef{janbfastmonitor}{bfast}
#' @keywords ts
#' @example examples/bfastpp.r
#' 
#' @export bfastpp
bfastpp<- function(data, order = 3,
                   lag = NULL, slag = NULL, na.action = na.omit,
                   stl = c("none", "trend", "seasonal", "both"),
                   decomp = c("stl", "stlplus"), sbins = 1)
{
  decomp <- match.arg(decomp)
  stl <- match.arg(stl)
  if (stl != "none" && decomp == "stlplus" && !requireNamespace("stlplus", quietly = TRUE))
    stop("Please install the stlplus package or set decomp = 'stl' or stl = 'none'.")

  ## double check what happens with 29-02 if that happens...
  ## we should keep it simple an remove the datum if that happens
  
  if(!is.ts(data)) data <- as.ts(data)
  
  ## STL pre-processing to try to adjust for trend or season
  stl <- match.arg(stl)
  if(stl != "none") {
    stl_adjust <- function(x) {
      x_stl <- if (decomp=="stlplus") {
        stlplus::stlplus(x, s.window = "periodic")$data
      } else {
        stats::stl(x, s.window = "periodic")$time.series
      }
      switch(stl,
             "trend" = x - x_stl[, "trend"],
             "seasonal" = x - x_stl[, "seasonal"],
             "both" = x - x_stl[, "trend"] - x_stl[, "seasonal"])
    }
    if(NCOL(data) > 1L) {
      for(i in 1:NCOL(data)) data[,i] <- stl_adjust(data[,i])
    } else {
      data <- stl_adjust(data)
    }
  }
  
  ## check for covariates
  if(NCOL(data) > 1L) {
    x <- coredata(data)[, -1L]
    y <- data[, 1L]
  } else {
    x <- NULL
    y <- data
  }
  
  ## data with trend and season factor
  seasonfreq <- if (sbins > 1) sbins else frequency(y)*sbins
  
  rval <- data.frame(
    time = as.numeric(time(y)),
    response = y,
    trend = 1:NROW(y),
    season = if (seasonfreq > 1) cut(cycle(y), seasonfreq, ordered_result = TRUE) else factor("no seasonality")
  )
  
  ## set up harmonic trend matrix as well
  freq <- frequency(y)
  order <- min(freq, order)
  harmon <- outer(2 * pi * as.vector(time(y)), 1:order)
  harmon <- cbind(apply(harmon, 2, cos), apply(harmon, 2, sin))
  colnames(harmon) <- if(order == 1) {
    c("cos", "sin")
  } else {
    c(paste("cos", 1:order, sep = ""), paste("sin", 1:order, sep = ""))
  }
  if((2 * order) == freq) harmon <- harmon[, -(2 * order)]
  rval$harmon <- harmon
  
  ## add lags
  nalag <- function(x, k) c(rep(NA, k), head(x, -k))
  if(!is.null(lag)) {
    rval$lag <- sapply(lag, function(k) nalag(as.vector(y), k))
    colnames(rval$lag) <- lag
  }
  if(!is.null(slag)) {
    rval$slag <- sapply(slag * freq, function(k) nalag(as.vector(y), k))
    colnames(rval$slag) <- slag
  }
  
  ## add regressors
  rval$xreg <- x
  
  ## omit missing values
  rval <- na.action(rval)
  
  ## return everything
  return(rval)
}
