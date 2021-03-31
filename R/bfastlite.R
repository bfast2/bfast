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
#' @param ... Additional arguments to \code{\link[strucchangeRcpp]{breakpoints}}.
#' @inheritParams bfastpp
#' @inheritParams strucchangeRcpp::breakpoints
#' @return An object of class \code{bfastlite}, with two elements:
#' \item{breakpoints}{output from \code{\link[strucchangeRcpp]{breakpoints}},
#' containing information about the estimated breakpoints.}
#' \item{data_pp}{preprocessed data as output by \code{\link{bfastpp}}.}
#' @example examples/bfastlite.r
#'
#' @export

bfastlite <- function(data, formula=response ~ trend + harmon, order = 3,
                   lag = NULL, slag = NULL, na.action = na.omit,
                   stl = c("none", "trend", "seasonal", "both"),
                   decomp=c("stlplus", "stl"), sbins=1, ...)
{
  data_pp <- bfastpp(data, order = order,
                   lag = lag, slag = slag, na.action = na.action,
                   stl = stl, decomp=decomp, sbins=sbins)
  breakpoints <- strucchangeRcpp::breakpoints(formula=formula, data = data_pp, ...)
  Result <- structure(list(breakpoints=breakpoints, data_pp=data_pp), class="bfastlite")
  return(Result)
}

#' @rdname bfastlite
bfast0n <- bfastlite
