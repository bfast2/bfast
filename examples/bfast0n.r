plot(simts) # stl object containing simulated NDVI time series
datats <- ts(rowSums(simts$time.series))
# sum of all the components (season,abrupt,remainder)
tsp(datats) <- tsp(simts$time.series) # assign correct time series attributes
plot(datats)

# Detect breaks
bp = bfast0n(datats)

# Default method of estimating breakpoints
bp[["breakpoints"]]
# Custom method
strucchange::breakpoints(bp, breaks=2)
