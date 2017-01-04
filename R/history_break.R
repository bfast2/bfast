##################################
## Bai & Perron last breakpoint ##
##################################

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