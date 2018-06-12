## Purpose:   robust F-test: Wald test for several coefficients of
##            an rlm object
## -------------------------------------------------------------------------
## Arguments:
##   object   result of rlm(...)
##   var      variables. Either their names or their indices
##		  Default: all *but* the intercept
## -------------------------------------------------------------------------
## Author: Werner Stahel, Date: 14 Jul 2000;  MM, 2000-07-14
##- From: Werner Stahel <stahel@stat.math.ethz.ch>
##- To: holzer@stat.math.ethz.ch, maechler@stat.math.ethz.ch
##- Subject: robust F-test
##- Date: Fri, 14 Jul 2000 17:01:55 +0200 (CEST)

## Taken from sfsmisc package 
anova.rlm <- function(object, var = -1)
{
  if (!inherits(object, "rlm"))
    stop("anova.rlm only works for 'rlm' objects")
  
  ## determine and check coefficients to be tested
  cf <- object$coef
  iind <- if(is.character(var)) match(var,names(cf)) else seq(length(cf))[var]
  wrong <- is.na(iind) | iind > length(cf) | iind < 1
  if (any(wrong))
    stop(paste("variable ",var[wrong]," not found"))
  cf <- cf[iind]
  if (0 == (t.nv <- length(cf)))
    stop("no variables to be tested")
  ## covariance matrix of estimated coefficients: calls summary.rlm():
  stopifnot(requireNamespace("MASS"))
  t.r <- summary(object, method="XtWX")
  ## Nota BENE: vcov() calls vcov.lm() which uses $sigma instead of $stddev !
  t.cov <- t.r$cov.unscaled[iind,iind] * t.r$stddev ^ 2
  
  ## Instead of   c(cf %*% solve(t.cov) %*% cf)/t.nv
  ## quite a bit more efficient (for larger p):  x' A^-1 x :
  t.f <- sum(cf * solve(t.cov, cf))/t.nv
  df <- c(t.nv, t.r$df[2])
  
  ## MM: Return an object of class "htest"  ---> nice print.*() method !
  structure(list(statistic = c(F = t.f), df = df,
                 data.name = paste("from", deparse(object$call)),
                 method = "robust F-test (as if non-random weights)",
                 alternative = "two.sided",
                 null.value = {c0 <- cf; c0[] <- 0; c0},
                 `Pr(>F)` = pf(t.f, df[1], df[2], lower.tail = FALSE)),
            class = "htest")
}
