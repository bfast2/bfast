## for now only for conversion to a time series with 365 days
## to do solve the issue of 29 Februari
## to do and functionality for other types of time series
## e.g. 10 day or 16 day time series
bfastts <- function(data,dates, type = c("irregular", "16-day", "10-day")) {
	
	yday365 <- function(x) {
		x <- as.POSIXlt(x)
		mdays <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
		cumsum(c(0L, mdays))[1L + x$mon] + x$mday
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
