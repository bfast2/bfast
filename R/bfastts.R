## for now only for conversion to a time series with 365 days
## to do solve the issue of 29 Februari
## to do and functionality for other types of time series
## e.g. 10 day or 16 day time series


#' Create a regular time series object by combining data and date information
#' 
#' Create a regular time series object by combining measurements (data) and
#' time (dates) information.
#' 
#' \code{bfastts} create a regular time series
#' 
#' @param data A data vector or matrix where columns represent variables
#' @param dates Optional input of dates for each measurement in the 'data'
#' variable. In case the data is a irregular time series, a vector with 'dates'
#' for each measurement can be supplied using this 'dates' variable. The
#' irregular data will be linked with the dates vector to create daily regular
#' time series with a frequency = 365. Extra days in leap years might cause
#' problems. Please be careful using this option as it is experimental.
#' Feedback is welcome.
#' @param type (\code{"irregular"}) indicates that the data is collected at
#' irregular dates and as such will be converted to a daily time series.
#' (\code{"16-day"}) indicates that data is collected at a regular time interval
#' (every 16-days e.g. like the MODIS 16-day data products). (\code{"10-day"}) 
#' indicates that data is collected at a 10-day time interval of the SPOT VEGETATION 
#' (S10) product. Warning: Only use this function for the SPOT VEGETATION S10 time series,
#' as for other 10-day time series a different approach might be required.
#' @return \code{bfastts} returns an object of class \code{"ts"}, i.e., a list
#' with components as follows.  \item{zz}{ a regular \code{"ts"} time series
#' with a frequency equal to 365 or 23 i.e. 16-day time series.}
#' @author Achim Zeileis, Jan Verbesselt
#' @seealso \code{\link[strucchangeRcpp]{monitor}}, \code{\link[strucchangeRcpp]{mefp}}, 
#' \code{\link[strucchangeRcpp]{breakpoints}}
#' @keywords ts
#' 
#' @example examples/bfastts.r
#' @examples
#'
#'
#' \dontrun{
#' # Example of use with a raster
#'
#' library("raster")
#' f <- system.file("extdata/modisraster.grd", package="bfast")
#' modisbrick <- brick(f)
#' ndvi <- bfastts(as.vector(modisbrick[1]), dates, type = c("16-day")) ## data of pixel 1
#' plot(ndvi/10000) 
#' 
#' # Time series of 4 pixels
#' modis_ts = t(as.data.frame(modisbrick))[,1:4]
#' # Data with multiple columns, 2-4 are external regressors
#' ndvi <- bfastts(modis_ts, dates, type = c("16-day"))
#' plot(ndvi/10000)
#' }
#' 
#' @export
bfastts <- function(data, dates, type = c("irregular", "16-day", "10-day")) {
  
  if (!is.vector(data) && !is.matrix(data)) {
    stop("data must be of type vector or matrix")
  }
      
  if (getOption("bfast.use_bfastts_modifications", FALSE)) {
    return(.bfastts.new (data, dates, type))
  }
  else 
  {
  	yday365 <- function(x) {
  		x <- as.POSIXlt(x)
  		mdays_sum <- c(0L, 31L, 59L, 90L, 120L, 151L, 181L, 212L, 243L, 273L, 304L, 334L, 365)
  		mdays_sum[1L + x$mon] + x$mday
  	}
  	
  	if (type == "irregular") {
  	zz <- zoo(data,1900 + as.POSIXlt(dates)$year + (yday365(dates) - 1)/365, frequency = 365)
  	}
  	
  	if (type == "16-day") {
  		z <- zoo(data, dates)
  		yr <- as.numeric(format(time(z), "%Y"))
  		jul <- as.numeric(format(time(z), "%j"))
  		delta <- min(unlist(tapply(jul, yr, diff))) # 16
  		zz <- aggregate(z, yr + (jul - 1) / delta / 23)
  	}
  	
  	if (type == "10-day") {
  	  tz <- as.POSIXlt(dates)
  	  zz <- zoo(data,
  	           1900L + tz$year + round((tz$yday - 1L)/ 10L)/36L,
  	           frequency = 36L)
  	}
    
  	tso <- as.ts(zz)
  	return(tso)
  }
}

.bfastts.new <- function(data, dates, type = c("irregular", "16-day", "10-day")) {
  
  yday365 <- function(x) {
    x <- as.POSIXlt(x)
    mdays_sum <- c(0L, 31L, 59L, 90L, 120L, 151L, 181L, 212L, 243L, 273L, 304L, 334L, 365)
    mdays_sum[1L + x$mon] + x$mday
  }
  
  # sort by date if needed
  if (is.unsorted(dates)) {
    ord = order(dates)
    dates = dates[ord]
    if (is.matrix(data)) {
      data = data[ord,]
    }
    else {
      data = data[ord]
    }
  }
  
  if (type == "irregular") {
    idx = 1900 + as.POSIXlt(dates)$year + (yday365(dates) - 1)/365
    freq = 365
  }
  else if (type == "16-day") {
    yr <- as.numeric(format(dates, "%Y"))
    jul <- as.numeric(format(dates, "%j"))
    delta <- min(unlist(tapply(jul, yr, diff))) # 16
    idx = yr + (jul - 1) / delta / 23
    freq = 23
  }
  else if (type == "10-day") {
    tz = as.POSIXlt(dates)
    freq = 36
    idx = 1900L + tz$year + round((tz$yday - 1L)/ 10L)/36L
  }
  else {
    stop("no valid time series type specified.")
  }
  
  # define target ts and fill with NAs
  start = idx[1]
  end = idx[length(idx)]
  
  if (is.matrix(data)) {
    A = ts(matrix(NA, ncol=ncol(data), nrow=1), start = start,end = end, frequency = freq) 
  }
  else {
    A = ts(NA, start = start,end = end, frequency = freq) 
  }
 
  # find closest points in the target time series for all observations
  matches = .bfast_cpp_closestfrom(idx,time(A), TRUE)
  
  # replace NAs with the observations at corresponding indexes
  if (is.matrix(data)) {
    A[matches,] = data
  }
  else {
    A[matches] = data
  }
  return(A)
}









# this is an alternative (faster) implementation that tries to work more precisely 
# with leap years and arbitrary frequencies but needs more testing. 
.bfast_construct_ts <- function(data, dates, freq=23) {
  if (class(dates) != "Date") {
    dates = as.Date(dates)
  }
  
  # find out year and fractional year of the observations
  data.yr <- as.numeric(format(dates, "%Y"))
  data.doy <- as.numeric(format(dates, "%j"))
  ndays = function(year){
   ifelse ((((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)), 366, 365)
  }
  data.time = data.yr + (data.doy-1)/ndays(data.yr)
  
  
  
  # define a NA time series with given frequency 
  start = c(data.yr[1], 1)
  end = c(data.yr[length(data)], freq)
  A = ts(start = start,end = end, frequency = freq)
  
  
  # find closest points in the target time series for all osbervations
  idx = .bfast_cpp_closestfrom(data.time,time(A), TRUE)
  if (anyDuplicated(idx)) 
    stop("multiple observations belong to the same point cycle in the time series")
  
  # replace NAs with the observations at corresponding indexes
  A[idx] = data
  
  #  crop time series and return
  bounds = range(which(!is.na(A)))
  return(window(A, start = time(A)[bounds[1]], end = time(A)[bounds[2]]))
}



