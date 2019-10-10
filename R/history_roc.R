# history_roc - Reversely Ordered CUSUM (ROC) test
# Copyright (C) 2019  Jan Verbesselt, Marius Appel
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

history_roc <- function (x, ...) {
  UseMethod("history_roc", x)
}


history_roc.matrix <- function(X, y,time, level = 0.05) {
  n <- nrow(X)
  #data_rev <- data[n:1,]
  #data_rev$response <- ts(data_rev$response)
  X_rev  <- X[n:1,]
  y_rev <-  y[n:1]
  time_rev <-  time[n:1]
  
  if (!is.ts(y)) y <- ts(y) # needed?
  y_rcus <- efp.matrix(X_rev,y_rev,time_rev, type = "Rec-CUSUM") ## TODO
  
  y_start <- if(sctest(y_rcus)$p.value < level) {
    length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
  } else {
    1    
  }
  time[y_start] 
}


## A technique to verify whether or not the historical period is stable or not
## reversely order sample and perform
## recursive CUSUM test
history_roc.formula <- function(formula, data, level = 0.05) {
  n <- nrow(data)
  data_rev <- data[n:1,]
  data_rev$response <- ts(data_rev$response)
  y_rcus <- efp(formula, data = data_rev, type = "Rec-CUSUM")

  y_start <- if(sctest(y_rcus)$p.value < level) {
    length(y_rcus$process) - min(which(abs(y_rcus$process)[-1] > boundary(y_rcus)[-1])) + 1
  } else {
    1    
  }
  data$time[y_start]
}