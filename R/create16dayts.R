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
#' A deprecated alias to bfastts.
#' Please use \code{bfastts(type="16-day")} instead.
#' 
#' @param data Passed to bfastts.
#' @param dates Passed to bfastts.
#' @author Achim Zeileis, Jan Verbesselt
#' @seealso \code{\link[bfast]{bfastmonitor}}
#' @keywords ts
#' @name create16dayts-deprecated
#' @usage create16dayts(data, dates)
#' @seealso \code{\link{bfast-deprecated}}
NULL

#' @rdname bfast-deprecated
#' @section \code{create16dayts}:
#' Use \code{\link{bfastts}} with the parameter \code{type="16-day"}.
#' @export
create16dayts <- function(data,dates) {
  .Deprecated("bfastts")
  return(bfastts(data, dates, type="16-day"))
}
