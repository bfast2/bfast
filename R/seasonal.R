seasonal <- function (x, out, sim = NULL, labels = colnames(X),
set.pars = list(tck = -0.01, mar = c(0, 6, 0, 6), oma = c(6, 0, 4, 0)),
main = NULL, range.bars = FALSE, ..., col.range = "light gray", fit = NULL)
{
     # define plot layout
     # notice: mfrow parameter removed from set.pars = list()
    layout(matrix(c(1,2,3,4),4,1,byrow=TRUE), heights = c(1,0.75,1.5,0.75), TRUE)
    sers <- x$time.series
    ncomp <- ncol(sers)
    data <- drop(sers %*% rep(1, ncomp))
    X <- cbind(data, sers)
    #colnames(X) <- c("data", colnames(sers))
    colnames(X) <- c("Yt","St","Tt", "et")
    nplot <- ncomp + 1
    if (range.bars)
        mx <- min(apply(rx <- apply(X, 2, range), 2, diff))
    if (length(set.pars)) {
        oldpar <- do.call("par", as.list(names(set.pars)))
        on.exit(par(oldpar))
        do.call("par", set.pars)
    }
    ## ANOVA
    if (is.null(fit) == F) {
      niter <- length(fit$output) # nr of iterations
      out <- fit$output[[niter]]  # output of results of the final fitted seasonal and trend models and nr of breakpoints in both.
      out_ANOVA <- array()
      out_breakdates <- array()
    if (out$Vt.bp[1] > 0) {breaks <- length(out$Vt.bp) } else {breaks <- 0}  # number of breaks
    if (breaks > 0) {
      breakdates <- out$Vt.bp # breakdates
      coefs <- coef(out$bp.Vt) # output coefficients per segment
      sl <- coefs[,2] # slopes
   }

   TS_anova <- fit$Yt - out$St   # time series Yt - St for ANOVA test
   dataframe <- data.frame(TIME=c(1:length(fit$Yt)),DATA=TS_anova)

   # determine segment startpoint and endpoint, calculate ANOVA
   for (m in 1:(breaks+1)) {
     startpoint <- if(m==1)  1 else breakdates[[m-1]]
     endpoint <- if(m==(breaks+1)) length(fit$Yt) else breakdates[m]-1
     df2 <- dataframe[startpoint:endpoint,]  # subset of dataframe (section)
     model <- lm(DATA~TIME, data=df2)        # linear model
     modelAnova <- anova(model)              # ANOVA
     out_ANOVA[m] <- modelAnova$Pr[1]        # save p-value
     if(breaks==0) {sl <- model$coefficients[2]}  ## JV updated -- this was causing problems !# slope Tt if breaks == 0
   }
}
## end ANOVA

    for (i in 1:nplot) {

        if(i == 4) {
          par(mar = c(0, 6, 0, 6))
        }

        plot(X[, i], col = if(i == 1) "black"
           else "red",
          ylim = if (i == 1 | i == 3)
            range(X[, 1], na.rm = TRUE)
        else range(X[, i], sim$time.series[, i - 1], na.rm = TRUE),
            type = if (i < nplot)
                "l"
            else "h", xlab = "", ylab = "", axes = FALSE, ...)

        if (range.bars) {
            dx <- 1/64 * diff(ux <- par("usr")[1:2])
            y <- mean(rx[, i])
            rect(ux[2] - dx, y + mx/2, ux[2] - 0.4 * dx, y -
                mx/2, col = col.range, xpd = TRUE)
        }
        if (i == 1 && !is.null(main)) {
            #title(main, line = 2, outer = par("oma")[3] > 0)
            mtext(main,side=3,font=2,line=1.25,cex=1.1)
            #lines(X[, i], col = "black", type = "l")
            if (!is.null(sim)) {
                lines(X[, i + 1] + X[, i + 2], col = "red", type = "l")
                legend("bottom", c("input", "estimated seasonal + trend "),
                  col = c("black", "red"), lty = 1)
            }
        }
        if (i == 2) {
            lines(sim$time.series[, "seasonal"], col = "black")
            lines(out$bp.Wt)
            lines(out$ci.Wt)
        }
        if (i == 3) {
            lines(sim$time.series[, "abrupt"], col = "black")
            lines(out$bp.Vt)
            lines(out$ci.Vt)

            ## plot ANOVA
            if(is.null(fit) == FALSE) {
              for(m in 1:(breaks+1)) {
                # coordinates based on start time series and breakpoints
                # x_coor <-  out$bp.Wt$datatsp[[1]]
                x_coor <- tsp(out$Wt)[1]
                if(m > 1) { x_coor <- x_coor + breakdates[[m-1]] /
                              frequency(fit$Yt) }
                y_range <- range(X[, 1])
                y_sl <- y_range[2] - (y_range[2] - y_range[1]) / 10 # 10% from top
                y_Pr <- y_range[2] - (y_range[2] - y_range[1]) / 5  # 20% from top
                # print slope
                beta <- formatC(sl[m],format="f",digits=3)
                text(x_coor,y_sl, bquote(beta == .(beta)), pos=4)
                # print p-value
                Pr <- formatC(out_ANOVA[m],format="f",digits=3)
                text(x_coor,y_Pr, bquote(p == .(Pr)), pos=4)
              }
            }
            ## end plot ANOVA
        }
        if (i == nplot) {
            abline(h = 0)
            lines(sim$time.series[, "remainder"], col = "black")
        }
        box()
        right <- i%%2 == 0
        axis(2, labels = !right)
        axis(4, labels = right)
        axis(1, labels = i == nplot)
        mtext(labels[i], side = 2, 3)
    }
    mtext("Time", side = 1, line = 3)
    invisible()
    if (is.null(fit) == FALSE) {
      return(data.frame(slope = sl, prob = out_ANOVA))}
    layout(matrix(1))
}
