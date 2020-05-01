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
#' A combination of \code{\link{bfastpp}} and \code{\link[strucchange]{breakpoints}}
#' to do light-weight detection of multiple breaks in a time series
#' while also being able to deal with NA values by excluding them
#' via \code{bfastpp}.
#'
#' @export bfast0n

bfast0n <- function()
{
  data_pp <- bfastpp()
  strucchange::breakpoints(data_pp)
}
