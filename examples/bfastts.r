# 16-day time series (i.e. MODIS)
timedf <- data.frame(y = som$NDVI.b, dates = dates[1:nrow(som)])
bfastts(timedf$y, timedf$dates, type = "16-day")

# Irregular
head(bfastts(timedf$y, timedf$dates, type = "irregular"), 50)

\donttest{
# Example of use with a raster
f <- system.file("extdata/modisraster.tif", package="bfast")
modisbrick <- terra::rast(f)
ndvi <- bfastts(unlist(modisbrick[1]), dates, type = c("16-day")) ## data of pixel 1
plot(ndvi/10000) 

# Time series of 4 pixels
modis_ts = t(modisbrick[1:4])
# Data with multiple columns, 2-4 are external regressors
ndvi <- bfastts(modis_ts, dates, type = c("16-day"))
plot(ndvi/10000)
}
