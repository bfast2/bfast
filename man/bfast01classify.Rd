\name{bfast01classify}
\alias{bfast01classify}
\title{Change type analysis of the bfast01 function}

\description{
  A function to determine the change type
}

\usage{
bfast01classify(object, alpha = 0.05, pct_stable = NULL)
}

\arguments{
  \item{object}{ \code{\link[bfast]{bfast01}} object, i.e. the output of the \code{\link[bfast]{bfast01}} function.}
  \item{alpha}{ threshold for significance tests, default 0.05}
  \item{pct_stable}{threshold for segment stability, unit: percent change per unit time (0-100), default NULL}
}

\details{
  \code{bfast01classify} 
}

\value{
  \code{bfast01classify} returns a data.frame with the following elements:
  \item{flag_type}{Type of shift: 
  (1) monotonic increase, 
  (2) monotonic decrease, 
  (3) monotonic increase (with positive break),
  (4) monotonic decrease (with negative break),
  (5) interruption: increase with negative break,
  (6) interruption: decrease with positive break, 
  (7) reversal: increase to decrease,
  (8) reversal: decrease to increase
  }
  \item{flag_significance}{SIGNIFICANCE FLAG:
(0) both segments significant (or no break and significant),
(1) only first segment significant,
(2) only 2nd segment significant,
(3) both segments insignificant (or no break and not significant)
  }
  \item{flag_pct_stable}{STABILITY FLAG:
  (0) change in both segments is substantial (or no break and substantial),
  (1) only first segment substantial,
  (2) only 2nd segment substantial
  (3) both segments are stable (or no break and stable)
  } 
  and also significance and percentage of both segments before and after the potentially detected break: "p_segment1", "p_segment2", "pct_segment1", "pct_segment2".
}

\references{
  de Jong R, Verbesselt J, Zeileis A, Schaepman M (2013). 
  Shifts in global vegetation activity trends. 
  \emph{Remote Sensing}, \bold{5}, 1117--1133.
  \url{http://dx.doi.org/10.3390/rs5031117}
}

\author{Rogier de Jong, Jan Verbesselt}

\seealso{\code{\link[bfast]{bfast01}}}

\examples{
library(zoo)
## define a regular time series
ndvi <- as.ts(zoo(som$NDVI.a, som$Time))
## fit variations
bf1 <- bfast01(ndvi)
bfast01classify(bf1, pct_stable = 0.25)
}
\keyword{ts,bfast01}

