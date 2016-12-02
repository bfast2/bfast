\name{bfast01}
\alias{bfast01}
\title{Checking for one major break in the time series}

\description{
  A function to select a suitable model for the data by choosing either a model with 0 or with 1 breakpoint.
}

\usage{
bfast01(data, formula = NULL,
  test = "OLS-MOSUM", level = 0.05, aggregate = all,
  trim = NULL, bandwidth = 0.15, functional = "max",
  order = 3, lag = NULL, slag = NULL, na.action = na.omit, stl = "none")
}

\arguments{
  \item{data}{A time series of class \code{\link[stats]{ts}}, or another object that
    can be coerced to such. The time series is processed by \code{\link[bfast]{bfastpp}}. A time series of class \code{\link[stats]{ts}} can be prepared by a convenience function \code{\link[bfast]{bfastts}} in case of daily, 10 or 16-daily time series.
    }
  \item{formula}{formula for the regression model. 
    The default is intelligently guessed      based on the arguments order/lag/slag i.e.  \code{response ~ trend + harmon}, i.e., a linear trend and a harmonic season component. Other specifications are possible using all terms set up by \code{\link[bfast]{bfastpp}}, i.e., \code{season} (seasonal pattern with dummy variables), \code{lag} (autoregressive terms), \code{slag} (seasonal autoregressiv terms), or \code{xreg} (further covariates). See \code{\link[bfast]{bfastpp}} for details.}
  \item{test}{ character specifying the type of test(s) performed. Can be one or more of BIC, supLM, supF, OLS-MOSUM, ..., or any other test supported by \code{\link[strucchange]{sctest.formula}} }
  \item{level}{numeric. Significance for the \code{\link[strucchange]{sctest.formula}} performed.}
  \item{aggregate}{function that aggregates a logical vector to a single value. This is used for aggregating the individual test decisions from \code{test} to a single one.}
  \item{trim}{numeric. The mimimal segment size passed to the \code{from} argument of the \code{\link[strucchange]{Fstats}} function. }
  \item{bandwidth}{numeric scalar from interval (0,1), functional. The \code{bandwidth} argument is passed to 
  the \code{h} argument of the \code{\link[strucchange]{sctest.formula}}. }
  \item{functional}{arguments passed on to \code{\link[strucchange]{sctest.formula}}}
  
  \item{order}{numeric. Order of the harmonic term, defaulting to \code{3}.}
  \item{lag}{numeric. Order of the autoregressive term, by default omitted.}
  \item{slag}{numeric. Order of the seasonal autoregressive term, by default omitted.}
  \item{na.action}{arguments passed on to \code{\link[bfast]{bfastpp}}}
  \item{stl}{argument passed on to \code{\link[bfast]{bfastpp}}}
}

\details{
  \code{bfast01} tries to select a suitable model for the data by choosing
either a model with 0 or with 1 breakpoint. It proceeds in the following
steps: 

1. The data is preprocessed with bfastpp using the arguments
order/lag/slag/na.action/stl.

2. A linear model with the given formula is fitted. By default a suitable
formula is guessed based on the preprocessing parameters.

3. The model with 1 breakpoint is estimated as well where the breakpoint
is chosen to minimize the segmented residual sum of squares.

4. A sequence of tests the null hypothesis of zero breaks is performed.
Each test results in a decision for FALSE (no breaks) or TRUE (structural
break(s)). The test decisions are then aggregated to a single decision
(by default using all() but any() or some other function could also be used).

Available methods for the object returned include standard methods for
linear models (coef, fitted, residuals, predict, AIC, BIC, logLik, deviance,
nobs, model.matrix, model.frame), standard methods for breakpoints (breakpoints,
breakdates), coercion to a zoo series with the decomposed components (as.zoo),
and a plot method which plots such a zoo series along with the confidence
interval (if the 1-break model is visualized). All methods take a 'breaks'
argument which can either be 0 or 1. By default the value chosen based on the
'test' decisions is used.

Note that the different tests supported have power for different types of
alternatives. Some tests (such as supLM/supF or BIC) assess changes in all
coefficients of the model while residual-based tests (e.g., OLS-CUSUM or
OLS-MOSUM) assess changes in the conditional mean. See Zeileis (2005) for
a unifying view.
}

\value{
  \code{bfast01} returns a list of class \code{"bfast01"} with the following elements:
  \item{call}{the original function call.}
  \item{data}{the data preprocessed by \code{"bfastpp"}.}
  \item{formula}{the model formulae.}
  \item{breaks}{the number of breaks chosen based on the \code{test} decision (either 0 or 1).}
  \item{test}{the individual test decisions.}
  \item{breakpoints}{the optimal breakpoint for the model with 1 break.}
  \item{model}{A list of two 'lm' objects with no and one breaks, respectively.}
}

\references{
  de Jong R, Verbesselt J, Zeileis A, Schaepman M (2013). 
  Shifts in global vegetation activity trends. 
  \emph{Remote Sensing}, \bold{5}, 1117--1133.
  \url{http://dx.doi.org/10.3390/rs5031117}

  Zeileis A (2005). A unified approach to structural change tests
  based on ML scores, F statistics, and OLS residuals.
  \emph{Econometric Reviews}, \bold{24}, 445--466.
  \url{http://dx.doi.org/10.1080/07474930500406053}.
}

\author{Achim Zeileis, Jan Verbesselt}

\seealso{\code{\link[bfast]{bfastmonitor}}, \code{\link[strucchange]{breakpoints}}}

\examples{
library(zoo)
## define a regular time series
ndvi <- as.ts(zoo(som$NDVI.a, som$Time))

## fit variations
bf1 <- bfast01(ndvi)
bf2 <- bfast01(ndvi, test = c("BIC", "OLS-MOSUM", "supLM"), aggregate = any)
bf3 <- bfast01(ndvi, test = c("OLS-MOSUM", "supLM"), aggregate = any, bandwidth = 0.11) 

## inspect test decisions
bf1$test
bf1$breaks
bf2$test
bf2$breaks
bf3$test
bf3$breaks

## look at coefficients
coef(bf1)
coef(bf1, breaks = 0)
coef(bf1, breaks = 1) 

## zoo series with all components
plot(as.zoo(ndvi))
plot(as.zoo(bf1, breaks = 1))
plot(as.zoo(bf2))
plot(as.zoo(bf3))

## leveraged by plot method
plot(bf1, regular = TRUE)
plot(bf2)
plot(bf2, plot.type = "multiple",
     which = c("response", "trend", "season"), screens = c(1, 1, 2))
plot(bf3)

}
\keyword{ts}

