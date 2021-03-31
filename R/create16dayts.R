## a function to create 16-day time series



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
