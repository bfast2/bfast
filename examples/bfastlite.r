plot(simts) # stl object containing simulated NDVI time series
datats <- ts(rowSums(simts$time.series))
# sum of all the components (season,abrupt,remainder)
tsp(datats) <- tsp(simts$time.series) # assign correct time series attributes
plot(datats)

# Detect breaks
bp = bfastlite(datats)

# Default method of estimating breakpoints
bp[["breakpoints"]][["breakpoints"]]

# Plot
plot(bp)

# Custom method of estimating number of breaks (request 2 breaks)
strucchangeRcpp::breakpoints(bp[["breakpoints"]], breaks = 2)

# Plot including magnitude based on RMSD for the cos1 component of harmonics
plot(bp, magstat = "RMSD", magcomp = "harmoncos1", breaks = 2)

# Try with a structural change test
bp <- bfastlite(datats, level=0.05)
print(bp)
plot(bp)

# Details of the structural change test with the type RE
bfastlite(datats, level=0.05, type="RE")$sctest
