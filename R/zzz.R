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
