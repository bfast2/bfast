# history_break - Bai & Perron last breakpoint
# Copyright (C) 2019  Marius Appel, Jan Verbesselt
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

history_break <- function(formula, data, h = NULL, hpc = "none") {
  n <- nrow(data)
  ## rule of thumb for minimal segment size
  if(is.null(h)) h <- 6 * NCOL(model.matrix(formula, data = data[0,]))

  ## conduct breakpoints estimation
  bp <- breakpoints(formula, data = data, h = h, hpc = hpc)

  y_start <- tail(breakpoints(bp)$breakpoints, 1)
  y_start <- if(is.na(y_start)) 1 else y_start + 1
  data$time[y_start]
}



history_break.matrix <- function(X,y,time, h = NULL, hpc = "none") {
  n <- nrow(X)
  ## rule of thumb for minimal segment size
  if(is.null(h)) h <- 6 * NCOL(X)
  
  ## conduct breakpoints estimation
  bp <- breakpoints.matrix(X,y, h = h, hpc = hpc) # TODO
  
  y_start <- tail(breakpoints(bp)$breakpoints, 1)
  y_start <- if(is.na(y_start)) 1 else y_start + 1
  time[y_start]
}