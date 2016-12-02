\name{bfastpp}
\alias{bfastpp}
\title{Time Series Preprocessing for BFAST-Type Models}

\description{
  Time series preprocessing for subsequent regression modeling.
  Based on a (seasonal) time series, a data frame with the response,
  seasonal terms, a trend term, (seasonal) autoregressive terms,
  and covariates is computed. This can subsequently be employed in
  regression models.
}

\usage{
bfastpp(data, order = 3,
  lag = NULL, slag = NULL, na.action = na.omit,
  stl = c("none", "trend", "seasonal", "both"))
}

\arguments{
  \item{data}{A time series of class \code{\link[stats]{ts}}, or another object that
    can be coerced to such. For seasonal components, a frequency greater than 1 is
    required.}
  \item{order}{numeric. Order of the harmonic term, defaulting to \code{3}.}
  \item{lag}{numeric. Orders of the autoregressive term, by default omitted.}
  \item{slag}{numeric. Orders of the seasonal autoregressive term, by default omitted.}
  \item{na.action}{function for handling \code{NA}s in the data (after all other
    preprocessing).}
  \item{stl}{character. Prior to all other preprocessing, STL (season-trend decomposition
    via LOESS smoothing) can be employed for trend-adjustment and/or season-adjustment.
    The \code{"trend"} or \code{"seasonal"} component or both from \code{\link[stats]{stl}}
    are removed from each column in \code{data}. By default (\code{"none"}), no STL
    adjustment is used.}
}

\details{
  To facilitate (linear) regression models of time series data, \code{bfastpp} facilitates
  preprocessing and setting up regressor terms. It returns a \code{data.frame} containing the
  first column of the \code{data} as the \code{response} while further columns (if any) are
  used as covariates \code{xreg}. Additionally, a linear trend, seasonal dummies, harmonic
  seasonal terms, and (seasonal) autoregressive terms are provided.
  
  Optionally, each column of \code{data} can be seasonally adjusted and/or trend-adjusted via
  STL (season-trend decomposition via LOESS smoothing) prior to preprocessing. The idea would
  be to capture season and/or trend nonparametrically prior to regression modelling.
}

\value{
  \code{bfastpp} returns a \code{"data.frame"} with the following variables (some of which may be matrices).
  \item{time}{numeric vector of time stamps,}
  \item{response}{response vector (first column of \code{data}),}
  \item{trend}{linear time trend (running from 1 to number of observations),}
  \item{season}{factor indicating season period,}
  \item{harmon}{harmonic seasonal terms (of specified \code{order}),}
  \item{lag}{autoregressive terms (or orders \code{lag}, if any),}
  \item{slag}{seasonal autoregressive terms (or orders \code{slag}, if any),}
  \item{xreg}{covariate regressor (all columns of \code{data} except the first, if any).}
}

\references{
Verbesselt J, Zeileis A, Herold M (2011).
  Near Real-Time Disturbance Detection in Terrestrial Ecosystems Using Satellite
  Image Time Series: Drought Detection in Somalia.
  Working Paper 2011-18. Working Papers in Economics and Statistics,
  Research Platform Empirical and Experimental Economics, Universitaet Innsbruck.
  \url{http://EconPapers.RePEc.org/RePEc:inn:wpaper:2011-18}.
  Submitted to Remote Sensing and Environment.
}

\author{Achim Zeileis}

\seealso{\code{\link[bfast]{bfastmonitor}}}

\examples{
## set up time series
library(zoo)
ndvi <- as.ts(zoo(cbind(a = som$NDVI.a, b = som$NDVI.b), som$Time))
ndvi <- window(ndvi, start = c(2006, 1), end = c(2009, 23))

## parametric season-trend model
d1 <- bfastpp(ndvi, order = 2)
d1lm <- lm(response ~ trend + harmon, data = d1)
summary(d1lm)

## autoregressive model (after nonparametric season-trend adjustment)
d2 <- bfastpp(ndvi, stl = "both", lag = 1:2)
d2lm <- lm(response ~ lag, data = d2)
summary(d2lm)
}

\keyword{ts}

