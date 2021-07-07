# print.bfast
# Copyright (C) 2019  Dainius Masiliūnas, Dongdong Kong
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

#' @method print bfast
#' @export
print.bfast <- function(x, ...)
{    
    cat("\n  TREND BREAKPOINTS")
    niter <- length(x$output)
    if(x$output[[niter]]$Vt.bp[1] != 0)
        print(x$output[[niter]]$ci.Vt)
    else
        cat(":  None\n")
    cat("\n  SEASONAL BREAKPOINTS")
    if(x$output[[niter]]$Wt.bp[1] != 0)
        print(x$output[[niter]]$ci.Wt)
    else
        cat(":  None\n")
    cat("\n")
}
