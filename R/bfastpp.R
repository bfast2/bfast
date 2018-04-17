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
#' @param formula regression model to be used (see
#' \code{\link[bfast]{bfastmonitor}}).  If given, only independent variables
#' that occur in the formula will be computed and the output will be a list of
#' the design matrix, the response vector, and the vector of times instead of a
#' single \code{data.frame}. Providing a formula may reduce relative expensive
#' calls of \code{model.matrix} and \code{model.frame} in following operations.
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
#' If a formula is given,\code{bfastpp} returns a \code{list} with components
#' \code{X}, \code{y}, and \code{t}, where \code{X} is the design matrix of the
#' model, \code{y} is the response vector, and \code{t} represents the time of
#' observations. \code{X} will only contain variables that occur in the
#' formula. Columns of \code{X} have names as decribed above.
#' @author Achim Zeileis
#' @seealso \code{\link[bfast]{bfastmonitor}}
#' @references Verbesselt J, Zeileis A, Herold M (2011).  Near Real-Time
#' Disturbance Detection in Terrestrial Ecosystems Using Satellite Image Time
#' Series: Drought Detection in Somalia.  Working Paper 2011-18. Working Papers
#' in Economics and Statistics, Research Platform Empirical and Experimental
#' Economics, Universitaet Innsbruck.
#' \url{http://EconPapers.RePEc.org/RePEc:inn:wpaper:2011-18}.  Submitted to
#' Remote Sensing and Environment.
#' @keywords ts
#' @examples
#' 
#' ## set up time series
#' library(zoo)
#' ndvi <- as.ts(zoo(cbind(a = som$NDVI.a, b = som$NDVI.b), som$Time))
#' ndvi <- window(ndvi, start = c(2006, 1), end = c(2009, 23))
#' 
#' ## parametric season-trend model
#' d1 <- bfastpp(ndvi, order = 2)
#' d1lm <- lm(response ~ trend + harmon, data = d1)
#' summary(d1lm)
#' 
#' ## autoregressive model (after nonparametric season-trend adjustment)
#' d2 <- bfastpp(ndvi, stl = "both", lag = 1:2)
#' d2lm <- lm(response ~ lag, data = d2)
#' summary(d2lm)
#' 
#' ## provide a formula and use the lower level lm.fit function
#' d3 <- bfastpp(ndvi, stl = "both", lag = 1:2, formula = response ~ lag)
#' d3lm <- lm.fit(d3$X, d3$y)
#' d3lm$coefficients
#' 
#' @export bfastpp
bfastpp<- function(data, order = 3,
                   lag = NULL, slag = NULL, na.action = na.omit,
                   stl = c("none", "trend", "seasonal", "both"),
                   formula = NULL) {
  
  if (is.null(formula)) {
    return(.bfastpp.full(data, order, lag, slag, na.action, stl))
  }
  else {
    return(.bfastpp.formula(data, order, lag, slag, na.action, stl, formula))
  }
  
}


.bfastpp.full <- function(data, order = 3,
                    lag = NULL, slag = NULL, na.action = na.omit,
                    stl = c("none", "trend", "seasonal", "both"))
{
  ## double check what happens with 29-02 if that happens...
  ## we should keep it simple an remove the datum if that happens
  
  if(!is.ts(data)) data <- as.ts(data)
  
  ## STL pre-processing to try to adjust for trend or season
  stl <- match.arg(stl)
  if(stl != "none") {
    stl_adjust <- function(x) {
      x_stl <- stats::stl(x, s.window = "periodic")$time.series
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
  rval <- data.frame(
    time = as.numeric(time(y)),
    response = y,
    trend = 1:NROW(y),
    season = factor(cycle(y))
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





# this function builds the design matrix X for bfast based on a given formula. Only terms 
# given in the formula will be added to X.
.bfastpp.formula <- function(data, order = 3,
                            lag = NULL, slag = NULL, na.action = na.omit,
                            stl = c("none", "trend", "seasonal", "both"),
                            formula = NULL)
{
  ## double check what happens with 29-02 if that happens...
  ## we should keep it simple an remove the datum if that happens
  
  if(!is.ts(data)) data <- as.ts(data)
  
  ## STL pre-processing to try to adjust for trend or season
  stl <- match.arg(stl)
  if(stl != "none") {
    stl_adjust <- function(x) {
      x_stl <- stats::stl(x, s.window = "periodic")$time.series
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
  
  
  rval = as.numeric(y) # remove ts attrs in order to avoid cbind.ts
  rval.names = "response"
  
  rval = cbind(rval, as.numeric(time(y)))
  rval.names = c(rval.names, "time")
  
  
  if (attr(terms(formula), "intercept")) {
    rval = cbind(rval, 1)
    rval.names = c(rval.names, "(Intercept)")
  }
  
  
  if ("trend" %in% attr(terms.formula(formula),"term.labels")){ 
    rval = cbind(rval,1:NROW(y))
    rval.names = c(rval.names, "trend")
  }
  
  if ("season" %in% attr(terms.formula(formula),"term.labels")){
    rval = cbind(rval,1 + seq(as.numeric(cycle(y[1]))-1,length.out=max(length(y),nrow(y))) %% frequency(y))
    rval.names = c(rval.names, "season")
  }
  
  
  ## set up harmonic trend matrix as well
  if ("harmon" %in% attr(terms.formula(formula),"term.labels")){
    
    freq <- frequency(y)
    order <- min(freq, order)
    harmon <- outer(2 * pi * rval[,2], 1:order) # rval[,2] is as.numeric(time(y))
    harmon <- cbind(apply(harmon, 2, cos), apply(harmon, 2, sin))
    colnames(harmon) <- if(order == 1) {
      c("harmoncos", "harmonsin")
    } else {
      c(paste("harmon.cos", 1:order, sep = ""), paste("harmon.sin", 1:order, sep = ""))
    }
    if((2 * order) == freq) harmon <- harmon[, -(2 * order)]
    rval = cbind(rval,harmon)
    rval.names = c(rval.names, colnames(harmon))
  }
  nalag <- function(x, k) c(rep(NA, k), head(x, -k))
  if ("lag" %in% attr(terms.formula(formula),"term.labels")){
    if(!is.null(lag)) {
      rval = cbind(rval,sapply(lag, function(k) nalag(y, k)))
      rval.names = c(rval.names, paste0("lag",lag))
    }
  }
  if ("slag" %in% attr(terms.formula(formula),"term.labels")){
    if(!is.null(slag)) {
      rval = cbind(rval, sapply(slag * frequency(y), function(k) nalag(as.vector(y), k)))
      rval.names = c(rval.names, paste0("slag",slag))
    }
  }
  
  if ("xreg" %in% attr(terms.formula(formula),"term.labels")){
    rval = cbind(rval, x)
    rval.names = c(rval.names, paste0("xreg.",colnames(x)))
  }
  
  ## omit missing values
  #if (!is.ts(rval)) rval <- ts(rval,start = start(data), frequency = frequency(data))
  colnames(rval) <- rval.names
  
  #class(rval) <- "matrix" # prevent calling na.omit.ts
  rval <- na.action(rval)
  
  return(list(y=rval[,1L],X=rval[,-c(1L,2L)], t=rval[,2L]))
}





