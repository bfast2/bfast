#
# This file is part of BFAST.
#
# BFAST is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# BFAST is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with BFAST.  If not, see <http://www.gnu.org/licenses/>.

#' Detect multiple breaks in a time series
#'
#' A combination of \code{\link{bfastpp}} and \code{\link[strucchangeRcpp]{breakpoints}}
#' to do light-weight detection of multiple breaks in a time series
#' while also being able to deal with NA values by excluding them
#' via \code{bfastpp}.
#' 
#' @param level numeric; threshold value for the \link[strucchangeRcpp]{sctest.efp}
#' test for at least one break in a time series, to save processing time for
#' no-change areas. The test is skipped if set to <= 0.
#' @param type character, indicating the type argument to
#' \link[strucchangeRcpp]{efp}.
#' @param ... Additional arguments to \code{\link[strucchangeRcpp]{breakpoints}}.
#' @inheritParams bfastpp
#' @inheritParams strucchangeRcpp::breakpoints
#' @return An object of class \code{bfastlite}, with three elements:
#' \item{breakpoints}{output from \code{\link[strucchangeRcpp]{breakpoints}},
#' containing information about the estimated breakpoints.}
#' \item{data_pp}{preprocessed data as output by \code{\link{bfastpp}}.}
#' \item{sctest}{output from \code{\link[strucchangeRcpp]{sctest.efp}},
#' contatining information about the likelihood of the time series
#' having at least one break.}
#' @author Dainius Masiliunas, Jan Verbesselt
#' @references \insertRef{dainiusbfastlite}{bfast}
#' @example examples/bfastlite.r
#'
#' @export

bfastlite <- function(data, formula = response ~ trend + harmon,
                   order = 3, breaks = "LWZ",
                   lag = NULL, slag = NULL, na.action = na.omit,
                   stl = c("none", "trend", "seasonal", "both"),
                   decomp = c("stl", "stlplus"), sbins = 1,
                   h = 0.15, level = 0, type = "OLS-MOSUM", ...)
{
  data_pp <- bfastpp(data, order = order,
                   lag = lag, slag = slag, na.action = na.action,
                   stl = stl, decomp = decomp, sbins = sbins)

  # If requested by the user, run an sctest before running anything else
  if (level > 0) {
    sct <- strucchangeRcpp::sctest(strucchangeRcpp::efp(
      formula = formula, h = h, type = type, data = data_pp))
    if (!is.null(sct$p.value) && sct$p.value > level)
      return(structure(list(breakpoints = NA,
        data_pp = data_pp, sctest = sct), class = "bfastlite"))
  } else sct <- NA

  breakpoints <- strucchangeRcpp::breakpoints(
    formula = formula, data = data_pp, breaks = breaks, h = h, ...)
  Result <- structure(list(breakpoints = breakpoints,
    data_pp = data_pp, sctest = sct), class = "bfastlite")
  return(Result)
}

#' @rdname bfastlite
#' @export
bfast0n <- bfastlite

#' Plot the time series and results of BFAST Lite
#' 
#' The black line represents the original input data,
#' the green line is the fitted model,
#' the blue lines are the detected breaks, and
#' the whiskers denote the magnitude (if `magstat` is specified).
#' 
#' @param x          bfastlite object from [bfastlite()]
#' @param breaks     number of breaks or optimal break selection method, see [strucchangeRcpp::breakpoints()]
#' @param magstat    name of the magnitude column to plot (e.g. `RMSD`, `MAD`, `diff`), see the `Mag` component of [strucchangeRcpp::magnitude.breakpointsfull()]
#' @param magcomp    name of the component (i.e. column in `x$data_pp`) to plot magnitudes of
#' @param ...        other parameters to pass to [plot()]
#' @return Nothing, called for side effects.
#' @method plot bfastlite
#' @export
plot.bfastlite = function(x, breaks = NULL, magstat = NULL, magcomp = "trend", ...)
{
    # Plot the original time series
    plot(response ~ time, data = x$data_pp, type = "l", ...)
    
    if (!identical(x$breakpoints, NA)) {
        # Plot the fitted model in green
        lines(fitted(x$breakpoints, breaks = breaks) ~ x$data_pp$time, col = "green")
        
        # Get the requested breaks
        bpOptim <- breakpoints(x$breakpoints, breaks = breaks)
        
        if (length(bpOptim$breakpoints) > 0 && !all(is.na(bpOptim$breakpoints))) {
            bpTimes <- x$data_pp[bpOptim$breakpoints, "time"]
            abline(v = bpTimes, col = "blue") # Detected breakpoints in blue
            
            # If magnitudes requested, plot whiskers denoting magnitude
            if (!is.null(magstat)) {
                Mag <- strucchangeRcpp::magnitude(x$breakpoints, breaks = breaks, component = magcomp)$Mag
                bpMag <- Mag[, colnames(Mag) %in% magstat]
                bpY <- x$data_pp[bpOptim$breakpoints, "response"]
                arrows(bpTimes, bpY-bpMag, bpTimes, bpY+bpMag, length = 0.05, angle = 90, code = 3, col = "blue")
            }
        }
    }
}

#' @method print bfastlite
#' @export
print.bfastlite <- function(x, ...)
{
    if (identical(x$breakpoints, NA)) {
        cat("The structural change test found no breakpoints for this time series,\ntherefore processing was skipped.\n")
    } else {
        print(x$breakpoints)
    }
}
