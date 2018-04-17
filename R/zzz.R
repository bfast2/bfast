.onLoad <- function(libname, pkgname) {
  op <- options()
  op.bfast <- list(
    bfast.prefer_matrix_methods = FALSE,
    bfast.use_bfastts_modifications = FALSE,
    strucchange.use_armadillo=FALSE
  )
  toset <- !(names(op.bfast) %in% names(op))
  if(any(toset)) options(op.bfast[toset])
  
  invisible()
}

#' Set package options with regard to computation times
#' 
#' These functions set options of the bfast and strucchange package to enable
#' faster computations. The fast options should return equal results but
#' require a compatible version of the \code{strucchange} package with
#' matrix-based functions. Notice that only some functions of the \code{bfast}
#' package make use of these options.
#' 
#' @name setoptions
#' @aliases set_fast_options set_default_options
#' @return A list of modified options and their new values.
#' @author Marius Appel
#' @examples
#' 
#' 
#' # run bfastmonitor with different options and compare computation times
#' library(zoo)
#' NDVIa <- as.ts(zoo(som$NDVI.a, som$Time))
#' 
#' set_fast_options()
#' system.time(replicate(100,  bfastmonitor(NDVIa, start = c(2010, 13))))
#' 
#' set_default_options()
#' system.time(replicate(100,  bfastmonitor(NDVIa, start = c(2010, 13))))
#' 
#' @export set_fast_options set_default_options
set_fast_options <- function() {
  env.strucchange = as.environment("package:strucchange")
  
  # check existence of required strucchange functions
  flist = c("monitor.matrix","efp.matrix","mefp.matrix",
            "breakpoints.matrix", "root.matrix.crossprod")
  if (!all(sapply(flist, function(x) exists(x, envir = env.strucchange))))
  {
    stop("failed to set fast options, please reinstall strucchange from http://github.com/appelmar/strucchange")
  }
  return(options(strucchange.use_armadillo=TRUE, 
                 bfast.prefer_matrix_methods=TRUE,
                 bfast.use_bfastts_modifications=TRUE))
}



set_default_options <- function() {
  return(options(strucchange.use_armadillo=FALSE, 
                 bfast.prefer_matrix_methods=FALSE,
                 bfast.use_bfastts_modifications=FALSE))
}
