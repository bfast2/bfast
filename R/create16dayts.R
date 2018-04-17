## a function to create 16-day time series



#' A helper function to create time series
#' 
#' Time series creation
#' 
#' 
#' @param data A vector
#' @param dates A vector ....
#' @author Achim Zeileis, Jan Verbesselt
#' @seealso \code{\link[bfast]{bfastmonitor}}
#' @keywords ts
#' @examples
#' 
#' ## set up a 16-day time series
#' #ndvi <- create16dayts(modisraster[1],dates)
#' #plot(ndvi)
#' 
#' @export create16dayts
create16dayts <- function(data,dates) {
	z <- zoo(data,dates)
	yr <- as.numeric(format(time(z), "%Y"))
	jul <- as.numeric(format(time(z), "%j"))
	delta <- min(unlist(tapply(jul, yr, diff))) # 16
	zz <- aggregate(z, yr + (jul - 1) / delta / 23)
	(tso <- as.ts(zz))
	return(tso)  
}
