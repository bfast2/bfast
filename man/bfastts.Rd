\name{bfastts}
\alias{bfastts}
\title{Create a regular time series object by combining data and date information}
\description{
  Create a regular time series object by combining measurements (data) and time (dates) information.
}

\usage{
bfastts(data, dates, 
		type = c("irregular", "16-day", "10-day"))
}

\arguments{
  \item{data}{A data vector }
  \item{dates}{Optional input of dates for each measurement in the 'data' variable. In 
    case the data is a irregular time series, a vector with 'dates' for 
  	each measurement can be supplied using this 'dates' variable. The irregular data will be linked
  	with the dates vector to create daily regular time series with a frequency = 365. Extra days
  	in leap years might cause problems. Please be carefull using this option as it is 
  	experimental. Feedback is welcome.}
  \item{type}{ (\code{"irregular"}) indicates that the data is collected at irregular dates and as such will be converted to a daily time series. (\code{"16-day"})indicates that data is collected at a regular time interval (every 16-days e.g. like the MODIS 16-day data products)}. (\code{"10-day"}) indicates that data is collected at a 10-day time interval of the SPOT VEGETATION (S10) product. Warning: Only use this function for the SPOT VEGETATION S10 time series, as for other 10-day time series a different approach might be required.
}

\details{
  \code{bfastts} create a regular time series
}

\value{
  \code{bfastts} returns an object of class \code{"ts"}, i.e., a list with components as follows.
  \item{zz}{ a regular \code{"ts"} time series with a frequency equal to 365 or 23 i.e. 16-day time series.}
}

\author{Achim Zeileis, Jan Verbesselt}

\seealso{\code{\link[strucchange]{monitor}}, \code{\link[strucchange]{mefp}}, \code{\link[strucchange]{breakpoints}}}

\examples{

library("raster")
f <- system.file("extdata/modisraster.grd", package="bfast")
modisbrick <- brick(f)
ndvi <- bfastts(as.vector(modisbrick[1]), dates, type = c("16-day")) ## data of pixel 1
plot(ndvi/10000) 

}

\keyword{ts}

