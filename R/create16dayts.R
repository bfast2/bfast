# create16dayts - a function to create 16-day time series
# Copyright (C) 2019  Dainius Masiliūnas, Jan Verbesselt
#
# This file is part of Foobar.
#
# Foobar is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# Foobar is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with Foobar.  If not, see <http://www.gnu.org/licenses/>.
#

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
