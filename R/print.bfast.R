print.bfast <- function(x, ...)
{    
    cat("\n  TREND BREAKPOINTS")
    niter <- length(x$output)
    if(x$output[[niter]]$Vt.bp[1] != 0)
        print(x$output[[niter]]$ci.Vt)
    else
        cat(":  None\n")
    cat("\n  SEASONAL BREAKPOINTS")
    if(x$output[[niter]]$Wt.bp[1] != 0)
        print(x$output[[niter]]$ci.Wt)
    else
        cat(":  None\n")
    cat("\n")
}
