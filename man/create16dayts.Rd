\name{create16dayts}
\alias{create16dayts}
\title{A helper function to create time series}

\description{
  Time series creation
}

\usage{
create16dayts(data, dates)
}

\arguments{
  \item{data}{A vector}
   \item{dates}{A vector ....}
}

\author{Achim Zeileis, Jan Verbesselt}

\seealso{\code{\link[bfast]{bfastmonitor}}}

\examples{
## set up a 16-day time series
#ndvi <- create16dayts(modisraster[1],dates)
#plot(ndvi)
}

\keyword{ts}

