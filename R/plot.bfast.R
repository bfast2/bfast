# modifications:
# 1: plot layout (Tt higer, St and et lower)
# 2: new parameter ANOVA determines significance of trend slopes
# 3: plot-labels changed into Yt, Tt, St and et
# 4: trend slope and significance printed in plot


plot.bfast <- function (x, type = c("components", "all", "data", "seasonal", 
    "trend", "noise"), sim = NULL, largest = FALSE, main, ANOVA = FALSE, ...) 
{
    type <- match.arg(type)
    #  opar <- par()
    realdata <- is.null(sim)
    Trend.bp <- !x$nobp$Vt
    if (type == "largest" & !Trend.bp) 
        stop("No trend breakpoints")
    title <- !missing(main)
    niter <- length(x$output)
    out <- x$output[[niter]]
    Tt <- out$Tt
    St <- out$St
    noise <- out$Nt
    if (type == "data") {
        if (!title) 
            main <- "Yt"
        plot(x$Yt, main = main, ...)
    }
    else if (type == "components") {
        ft <- cbind(seasonal = out$St, trend = out$Tt, remainder = out$Nt)
        tsp(ft) <- tsp(x$Yt)
        ft <- list(time.series = ft)
        if (!title) 
            main <- paste("no. iterations to estimate breakpoints:", 
                niter)
        # fit = x passes the BFAST object to seasonal() for ANOVA
        if (ANOVA == TRUE) { seasonal(ft, out, sim = sim, main = main, fit = x) }
        else { seasonal(ft, out, sim = sim, main = main) }
    }
    else if (type == "noise") {
        if (!title) 
            main <- "Noise component"
        tsdisplay(noise, main = main, ...)
    }
    else {
        if (type == "all") {
            idx <- 1:niter
            opar <- par(mfrow = c(2, niter))
        }
        else idx <- niter
        for (i in idx) {
            out <- x$output[[i]]
            if (type != "seasonal") {
                if (type == "trend" & !title) 
                  main <- "Trend component"
                else if (!title) 
                  main <- paste("Iteration ", i, ": Trend", sep = "")
                plot(out$Vt, main = main, ylab = "Vt", ...)
                lines(out$Tt, col = 4)
                if (Trend.bp) {
                  lines(out$bp.Vt)
                  lines(out$ci.Vt)
                  legend("topright", paste("Time of BP(s)", paste(out$Vt.bp, 
                    collapse = ",")), col = 2)
                }
                if (!realdata) {
                  lines(sim$time.series[, "abrupt"], col = 1, 
                    lty = 2)
                  legend("bottomleft", c("estimated", "simulated"), 
                    lty = c(1, 2), col = 1)
                }
                if (largest) {
                  legend("bottomright", c("Magnitude of most sign change"), 
                    lty = c(1), col = 6)
                  lines(x$jump, col = 6)
                  points(x$jump, pch = 14, cex = 1, col = 6)
                }
            }
            if (type != "trend") {
                if (type == "seasonal" & !title) 
                  main <- "Seasonal component"
                else if (!title) 
                  main <- paste("Iteration ", i, ": Seasonal", 
                    sep = "")
                plot(out$Wt, main = main, ylab = "Wt", ...)
                lines(out$St, col = 2)
                Seas.bp <- !x$nobp$Wt
                if (Seas.bp) {
                  lines(out$bp.Wt)
                  lines(out$ci.Wt)
                  legend("topright", paste("Time of BP(s)", paste(out$Wt.bp, 
                    collapse = ",")), col = 2)
                }
                if (!realdata) {
                  lines(sim$time.series[, "seasonal"], col = 1, 
                    lty = 2)
                  legend("bottomleft", c("first run seasonality", 
                    "first run estimated", "simulated"), lty = c(1, 
                    1, 2), col = c(1, 2, 1))
                }
            }
        }
        if (type == "all") 
            par(opar)
    }
    # par(opar)
}
