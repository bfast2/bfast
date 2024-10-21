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

\donttest{
## Run bfastlite() on a raster
f <- system.file("extdata/modisraster.tif", package="bfast")
modisbrick <- terra::rast(f)

# Run on pixel 10
data <- unlist(modisbrick[10])
ndvi <- bfastts(data, dates, type = c("16-day"))
bfl <- bfastlite(ndvi, breaks = "BIC")
# Get max magnitude by RMSD
max(magnitude(bfl[["breakpoints"]])$Mag[,"RMSD"])

# Wrapper function
bflSpatial <- function(pixels)
{
    ts <- bfastts(pixels, dates, type = c("16-day"))
    bfl <- bfastlite(ts, breaks="BIC")
    bp <- bfl[["breakpoints"]]
    # Return number of breakpoints and max breakpoint magnitude
    if (length(bp[["breakpoints"]]) == 1 && is.na(bp[["breakpoints"]]))
        return(c(0, 0))
    
    return(c(length(bp[["breakpoints"]]), max(magnitude(bp)$Mag[,"RMSD"])))
}

# Run function on each raster pixel
rastbfl <- terra::app(modisbrick, bflSpatial)
terra::plot(rastbfl)
}
