

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
      c("harmon.cos", "harmon.sin")
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





