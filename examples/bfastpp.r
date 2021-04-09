## set up time series
ndvi <- as.ts(zoo::zoo(cbind(a = som$NDVI.a, b = som$NDVI.b), som$Time))
ndvi <- window(ndvi, start = c(2006, 1), end = c(2009, 23))

## parametric season-trend model
d1 <- bfastpp(ndvi, order = 2)
d1lm <- lm(response ~ trend + harmon, data = d1)
summary(d1lm)
# plot visually (except season, as it's a factor)
plot(zoo::read.zoo(d1)[,-3],
  # Avoid clipping plots for pretty output
  ylim=list(c(min(d1[,2]), max(d1[,2])),
            c(min(d1[,3]), max(d1[,3])),
            c(-1, 1), c(-1, 1), c(-1, 1), c(-1, 1),
            c(min(d1[,6]), max(d1[,6]))
       ))

## autoregressive model (after nonparametric season-trend adjustment)
d2 <- bfastpp(ndvi, stl = "both", lag = 1:2)
d2lm <- lm(response ~ lag, data = d2)
summary(d2lm)

## use the lower level lm.fit function
d3 <- bfastpp(ndvi, stl = "both", lag = 1:2)
d3mm <- model.matrix(response ~ lag, d3)
d3lm <- lm.fit(d3mm, d3$response)
d3lm$coefficients
