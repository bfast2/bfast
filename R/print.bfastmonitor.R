# print.bfastmonitor
# Copyright (C) 2019  Dainius Masiliūnas, Marius Appel, Jan Verbesselt
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

#' @method print bfastmonitor
#' @export
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
