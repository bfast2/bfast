# zzz
# Copyright (C) 2019  Dainius Masiliūnas, Marius Appel
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

.onLoad <- function(libname, pkgname) {
  op <- options()
  op.bfast <- list(
    bfast.prefer_matrix_methods = TRUE,
    bfast.use_bfastts_modifications = TRUE,
    strucchange.use_armadillo=TRUE
  )
  toset <- !(names(op.bfast) %in% names(op))
  if(any(toset)) options(op.bfast[toset])
  
  invisible()
}

#' Set package options with regard to computation times
#' 

#' These functions set options of the bfast and strucchangeRcpp packages to enable
#' faster computations. By default (\code{set_default_options}), these optimizations are 
#' enabled. Notice that only some functions of the \code{bfast}
#' package make use of these options. \code{set_fast_options} is an alias for \code{set_default_options}.
#' 
#' @name setoptions
#' @aliases set_default_options set_fallback_options set_fast_options
#' @return A list of modified options and their new values.
#' @examples
#' 
#' 
#' # run bfastmonitor with different options and compare computation times
#' library(zoo)
#' NDVIa <- as.ts(zoo(som$NDVI.a, som$Time))
#' 
#' set_default_options()
#' \dontrun{
#' system.time(replicate(100,  bfastmonitor(NDVIa, start = c(2010, 13))))
#' }
#' 
#' set_fallback_options()
#' \dontrun{
#' system.time(replicate(100,  bfastmonitor(NDVIa, start = c(2010, 13))))
#' }
#' 
#' @export
set_default_options <- function() {
  return(options(strucchange.use_armadillo=TRUE, 
                 bfast.prefer_matrix_methods=TRUE,
                 bfast.use_bfastts_modifications=TRUE))
}

#' @rdname setoptions
#' @export
set_fast_options <- set_default_options

#' @rdname setoptions
#' @export
set_fallback_options <- function() {
  return(options(strucchange.use_armadillo=FALSE, 
                 bfast.prefer_matrix_methods=FALSE,
                 bfast.use_bfastts_modifications=FALSE))
}
