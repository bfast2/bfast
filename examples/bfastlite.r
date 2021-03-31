plot(simts) # stl object containing simulated NDVI time series
datats <- ts(rowSums(simts$time.series))
# sum of all the components (season,abrupt,remainder)
tsp(datats) <- tsp(simts$time.series) # assign correct time series attributes
plot(datats)

# Detect breaks
bp = bfastlite(datats)

# Default method of estimating breakpoints
bp[["breakpoints"]][["breakpoints"]]
# Custom method
strucchangeRcpp::breakpoints(bp[["breakpoints"]], breaks = 2)
