## a function to create 16-day time series

create16dayts <- function(data,dates) {
	z <- zoo(data,dates)
	yr <- as.numeric(format(time(z), "%Y"))
	jul <- as.numeric(format(time(z), "%j"))
	delta <- min(unlist(tapply(jul, yr, diff))) # 16
	zz <- aggregate(z, yr + (jul - 1) / delta / 23)
	(tso <- as.ts(zz))
	return(tso)  
}