# 16-day time series (i.e. MODIS)
timedf <- data.frame(y = som$NDVI.b, dates = dates[1:nrow(som)])
bfastts(timedf$y, timedf$dates, type = "16-day")

# Irregular
head(bfastts(timedf$y, timedf$dates, type = "irregular"), 50)
