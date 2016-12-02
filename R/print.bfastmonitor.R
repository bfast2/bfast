print.bfastmonitor <- function(x, ...)
{
  freq <- frequency(x$data)
  cat("\nBFAST monitoring\n\n1. History period\n")
  cat(sprintf("Stable period selected: %i(%i)--%i(%i)\n",
    floor(x$history[1]),
    round((x$history[1] - floor(x$history[1])) * freq) + 1,
    floor(x$history[2]),
    round((x$history[2] - floor(x$history[2])) * freq) + 1))
  cat(sprintf("Length (in years): %f\n", diff(x$history)))

  cat("Model fit:\n")
  print(coef(x$model))
  cat(sprintf("R-squared: %f\n", summary(x$model)$r.squared))

  cat("\n\n2. Monitoring period\n")
  cat(sprintf("Monitoring period assessed: %i(%i)--%i(%i)\n",
    floor(x$monitor[1]),
    round((x$monitor[1] - floor(x$monitor[1])) * freq) + 1,
    floor(x$monitor[2]),
    round((x$monitor[2] - floor(x$monitor[2])) * freq) + 1))
  cat(sprintf("Length (in years): %f\n", diff(x$monitor)))
  if(is.na(x$breakpoint)) {  
      cat("Break detected at: -- (no break)\n\n")
    } else {
      cat(sprintf("Break detected at: %i(%i)\n\n", floor(x$breakpoint), round((x$breakpoint - floor(x$breakpoint)) * freq) + 1))
  }

  invisible(x)
}