## ----------------------------------------------------
## classification function based on the BFAST01 object
## ----------------------------------------------------
# Input: BFAST01 object (object)
# Return: integer representing the class
## classification function for 1-break bfast output
## object: bfast01 object
## alpha: threshold for significance tests, default 0.05
## pct_stable: threshold for segment stability, unit: percent change per unit time (0-100), default NULL



#' Change type analysis of the bfast01 function
#' 
#' A function to determine the change type
#' 
#' \code{bfast01classify}
#' 
#' @param object \code{\link[bfast]{bfast01}} object, i.e. the output of the
#' \code{\link[bfast]{bfast01}} function.
#' @param alpha threshold for significance tests, default 0.05
#' @param pct_stable threshold for segment stability, unit: percent change per
#' unit time (0-100), default NULL
#' @return \code{bfast01classify} returns a data.frame with the following
#' elements: \item{flag_type}{Type of shift: (1) monotonic increase, (2)
#' monotonic decrease, (3) monotonic increase (with positive break), (4)
#' monotonic decrease (with negative break), (5) interruption: increase with
#' negative break, (6) interruption: decrease with positive break, (7)
#' reversal: increase to decrease, (8) reversal: decrease to increase }
#' \item{flag_significance}{SIGNIFICANCE FLAG: (0) both segments significant
#' (or no break and significant), (1) only first segment significant, (2) only
#' 2nd segment significant, (3) both segments insignificant (or no break and
#' not significant) } \item{flag_pct_stable}{STABILITY FLAG: (0) change in both
#' segments is substantial (or no break and substantial), (1) only first
#' segment substantial, (2) only 2nd segment substantial (3) both segments are
#' stable (or no break and stable) } and also significance and percentage of
#' both segments before and after the potentially detected break: "p_segment1",
#' "p_segment2", "pct_segment1", "pct_segment2".
#' @author Rogier de Jong, Jan Verbesselt
#' @seealso \code{\link[bfast]{bfast01}}
#' @references de Jong R, Verbesselt J, Zeileis A, Schaepman M (2013).  Shifts
#' in global vegetation activity trends.  \emph{Remote Sensing}, \bold{5},
#' 1117--1133.  \url{http://dx.doi.org/10.3390/rs5031117}
#' @keywords ts,bfast01
#' @examples
#' 
#' library(zoo)
#' ## define a regular time series
#' ndvi <- as.ts(zoo(som$NDVI.a, som$Time))
#' ## fit variations
#' bf1 <- bfast01(ndvi)
#' bfast01classify(bf1, pct_stable = 0.25)
#' 
#' @export bfast01classify

bfast01classify <- function(object, alpha=0.05, pct_stable=NULL, typology="standard") { 
   ## output array 
   out <- rep(NA,7)
   if(typology=="standard"){
   names(out) <- c("flag_type","flag_significance","p_segment1","p_segment2",
                   "pct_segment1","pct_segment2","flag_pct_stable")
      }else if(typology=="drylands"){
      names(out) <- c("flag_type","flag_subtype","flag_significance",
                  "p_segment1","p_segment2","pct_segment1","pct_segment2")
   }
      
   ## Segment and break point parameters
   object.zoo <- as.zoo(object) # data series
   ## if break, list segment and break point parameters (p$..)
   if(object$breaks != 0) {
     ToB <- as.numeric(object$breakpoints[[1]])  # time of break
     s1 <- object$model[[2]]$coefficients[3] # slope segment 1
     s2 <- object$model[[2]]$coefficients[4] # slope segment 2
     m <- as.numeric(object.zoo$trend[ToB+1]) - as.numeric(object.zoo$trend[ToB]) # magnitude of abrupt change
   }  
   
   ## ANOVA and PCTCHANGE
   p.seg<-c()
   seg.pct.chg<-c()
   for (segment in 1:(object$breaks+1)) {
      # subset zoo object for segment
      date.start <- if(segment==1)  object$data$time[1] else object$data$time[ToB+1]
      date.end <- if(segment==2 || object$breaks==0) object$data$time[nrow(object$data)] else object$data$time[ToB]
      object.zoo.subset <- window(object.zoo, start=date.start, end=date.end)
      # Anova
      segment.anova <- anova(lm((object.zoo.subset$response-object.zoo.subset$season)~time(object.zoo.subset))) 
      # linear model of deseasonalized trend versus time
      p.seg[segment] <- segment.anova$Pr[1]
      # PctChange
      obs.start <- if(segment==1)  1 else ToB+1
      obs.end <- if(segment==2 || object$breaks==0) nrow(object$data) else ToB
      if(object.zoo$trend[[obs.end]] / object.zoo$trend[[obs.start]] > 0){
         segment.pctchange <- 
           ( (object.zoo$trend[[obs.end]] / object.zoo$trend[[obs.start]])^(1/(date.end-date.start)) -1) * 100      
      } else {
         if(object.zoo$trend[[obs.start]] < object.zoo$trend[[obs.end]]){
            value.start <- object.zoo$trend[[obs.start]] + 2 * abs(object.zoo$trend[[obs.start]])
            value.end <- object.zoo$trend[[obs.end]] + 2 * abs(object.zoo$trend[[obs.start]])
            segment.pctchange <- ( (value.end / value.start)^(1/(date.end-date.start)) -1) * 100               
         } else {
            value.start <- object.zoo$trend[[obs.start]] + 2 * abs(object.zoo$trend[[obs.end]])
            value.end <- object.zoo$trend[[obs.end]] + 2 * abs(object.zoo$trend[[obs.end]])
            segment.pctchange <- ( (value.end / value.start)^(1/(date.end-date.start)) -1) * 100                           
         }
      }
      seg.pct.chg[segment] <- segment.pctchange
   }
   
   if(typology=="standard"){
      out[3] <- p.seg[1]
      out[4] <- p.seg[2]
      }else if(typology=="drylands"){
      out[4] <- p.seg[1]
      out[5] <- p.seg[2]
   }
   
   if(typology=="standard"){
      out[5] <- seg.pct.chg[1]
      out[6] <- seg.pct.chg[2]
      }else if(typology=="drylands"){
      out[6] <- seg.pct.chg[1]
      out[7] <- seg.pct.chg[2]
   }
   
   ## CLASSIFICATION 
   ## Standard Typo
   if(typology=="standard"){
      ## monotonic if no break
      if(object$breaks == 0) {     
         slope <- object$model[[1]]$coefficients[2] # slope
         if(slope > 0) out[1] <- 1
         if(slope < 0) out[1] <- 2
      }else{
         ## classes with break
         # with break, but still monotonic
         if(s1 > 0 && s2 > 0 && m > 0) out[1] <- 3
         if(s1 < 0 && s2 < 0 && m < 0) out[1] <- 4
         # interrupted gradual change (setback or boost)
         if(s1 > 0 && s2 > 0 && m < 0) out[1] <- 5
         if(s1 < 0 && s2 < 0 && m > 0) out[1] <- 6
         # trend reversal (greening to browning v.v.)
         if(s1 > 0 && s2 < 0) out[1] <- 7
         if(s1 < 0 && s2 > 0) out[1] <- 8
      }
   }   
      
   # Drylands Typo
   if(typology=="drylands"){
      ## monotonic if no break
      if(object$breaks == 0) {
         if(p.seg[1] > alpha){
         out[1] <- 0 #fluctuating/no change
      }else{
         slope <- object$model[[1]]$coefficients[2] # slope
         if(slope > 0) out[1] <- 1 # monotonic increase
         if(slope < 0) out[1] <- 2 # monotonic decrease  
      }
      } else {
         ## classes with break
         if(p.seg[1] > alpha && p.seg[2] > alpha){
            out[1] <- 0 #fluctuating/no change
         }else{
            # with break, but still same direction
            if(s1 > 0 && s2 > 0) out[1] <- 3 # non-monotonic increase
            if(s1 < 0 && s2 < 0) out[1] <- 4 # non-monotonic decrease
            # trend reversal (greening to browning v.v.)
            if(s1 > 0 && s2 < 0) out[1] <- 5 # reversal increase to decrease
            if(s1 < 0 && s2 > 0) out[1] <- 6 # reversal decrease to increase
         }
      }
  
      ## Sub-classification
      if(object$breaks != 0 && out[1] != 0) { #if there's a break and at least one sign. trend
         if( (s1 > 0 && s2 > 0) || (s1 < 0 && s2 < 0)){
         # non mono trends (sub-types 1 and 2, "slowing down" and "accelerating")
         if( (s1 > 0 && s2 > 0) || (s1 < 0 && s2 < 0) ){
            if(abs(s1) > abs(s2)) out[2] <- 1 # slowing down
            if(abs(s1) < abs(s2)) out[2] <- 2 # accelerating
         }
      }else if( (s1 < 0 && s2 > 0) || (s1 > 0 && s2 < 0)){
         # reversal trend (sub-types 3 and 4, "transition" and "complete")
         if( (p.seg[1] <= alpha && p.seg[2] > alpha) || (p.seg[1] > alpha && p.seg[2] <= alpha) ) out[2] <- 3 # transition
         if(p.seg[1] <= alpha && p.seg[2] <= alpha) out[2] <- 4 # complete
         }
      }
   }
    
   ## Segment significance flag
   # code: 0 = both segments significant (or no break and significant), 
   # 1 = only first segment significant, 
   # 2 = only 2nd segment significant, 
   # 3 = both segments insignificant (or no break and not significant)
   
   if(typology=="standard"){ isf = 0 }
   if(typology=="drylands"){ isf = 1 }
   # no break
   if(object$breaks == 0) {     
      if(out[3+isf] <= alpha) out[2+isf] <- 0
      if(out[3+isf] > alpha) out[2+isf] <- 3
   # with break
   } else {
      if(out[3+isf] <= alpha && out[4+isf] <= alpha) out[2+isf] <- 0
      if(out[3+isf] <= alpha && out[4+isf] > alpha) out[2+isf] <- 1
      if(out[3+isf] > alpha && out[4+isf] <= alpha) out[2+isf] <- 2
      if(out[3+isf] > alpha && out[4+isf] > alpha) out[2+isf] <- 3
   }   

   if(typology=="standard"){
      ## Segment stability flag
      # code: 0 = both segments beyond stable (or no break and not stable), 
      # 1 = only first segment beyond stable, 
      # 2 = only 2nd segment beyond stable, 
      # 3 = both segments stable (or no break and stable)
   
      if(!is.null(pct_stable)) {
         # no break
         if(object$breaks == 0) {     
            if(abs(out[5]) > pct_stable) out[7] <- 0
            if(abs(out[5]) <= pct_stable) out[7] <- 3
         # with break
         } else {
            if(abs(out[5]) > pct_stable && abs(out[6]) > pct_stable) out[7] <- 0
            if(abs(out[5]) > pct_stable && abs(out[6]) <= pct_stable) out[7] <- 1
            if(abs(out[5]) <= pct_stable && abs(out[6]) > pct_stable) out[7] <- 2
            if(abs(out[5]) <= pct_stable && abs(out[6]) <= pct_stable) out[7] <- 3
         }        
      }else{
      out[7] <- NA
      }
   }
     
   return(as.data.frame(t(out)))
}

# ## print the flag labels for the standard typology
# classlabels.bfast01 <- function() {
#    cat("\n*** TYPE OF SHIFT *** \n")
#    class_names <- c('monotonic increase','monotonic decrease','monotonic increase (with positive break)','monotonic decrease (with negative break)','interruption: increase with negative break','interruption: decrease with positive break','reversal: increase to decrease','reversal: decrease to increase')
#    for (i in 1:8) cat(i, " -- ", class_names[i], "\n")
# 
#    cat("\n*** SIGNIFICANCE FLAG *** \n")
#    class_names <- c('both segments significant (or no break and significant)','only first segment significant','only 2nd segment significant','both segments insignificant (or no break and not significant)')
#    for (i in 0:3) cat(i, " -- ", class_names[i+1], "\n")
# 
#    cat("\n*** STABILITY FLAG *** \n")
#    class_names <- c('change in both segments is substantial (or no break and substantial)','only first segment substantial','only 2nd segment substantial','both segments are stable (or no break and stable)')
#    for (i in 0:3) cat(i, " -- ", class_names[i+1], "\n")
# }

# ## print the flag labels for the typology optimized for drylands
# classlabels.bfast01 <- function() {
#   cat("\n*** TYPE OF SHIFT *** \n")
#   class_names <- c('fluctuating/no change','monotonic increase','monotonic decrease',
#                    'non-monotonic increase','non-monotonic decrease',
#                    'reversal: increase to decrease','reversal: decrease to increase')
#   for (i in 0:6) cat(i, " -- ", class_names[i+1], "\n")
#   
#   cat("\n*** SUBTYPE OF SHIFT *** \n")
#   class_names <- c('slowing down','accelerating','transition','complete')
#   for (i in 1:4) cat(i, " -- ", class_names[i], "\n")
#   
#   cat("\n*** SIGNIFICANCE FLAG *** \n")
#   class_names <- c('both segments significant (or no break and significant)',
#                    'only first segment significant','only 2nd segment significant',
#                    'both segments insignificant (or no break and not significant)')
#   for (i in 0:3) cat(i, " -- ", class_names[i+1], "\n")
# }
#
## ----------------------------------------------------------------------------------------
## Examples
#
# # A time series which would be classified as interruption: increase with negative break by
# # the standard typology, and as non-monotonic increase, accelerating after the breakpoint,
# # by the typology optmized for drylands.
# 
# set.seed(11)
# 
# # Simulating data for a time series using an AR(1) model
# x<-arima.sim(model= list(order = c(1,1,0), ar=0.68), n = 200)
# 
# # Creating the time series object
# ts1<-ts(x, start = 1801, end = 2000, frequency = 1)
# 
# # Creating bfast01 object
# bf1 <- bfast01(ts1, formula = response ~ trend)
# 
# # Applying the classification
# bfast01classify(bf1, typology = "standard") # Interruption: increase with negative break
# bfast01classify(bf1, typology = "drylands") # Non-monotonic increase, Accelerating
# 
# # Visualizing the time series with break and fitted trends
# plot(bf1) 
# 
# ##
# 
# # A time series which would be classified as reversal from decrease to increase by the 
# # standard typology, and as a complete reversal from decrease to increase by the 
# # typology optmized for drylands.
# 
# # Simulating data for a time series with decreasing trend
# set.seed(3)
# y1 <- arima.sim(list(order = c(0,1,12), ma = c(-0.8,rep(0,10),-0.8)), n = 359, sd = 0.2)
# 
# # Simulating data for a time series with increasing trend
# set.seed(4)
# y2 <- arima.sim(list(order = c(0,1,12), ma = c(-0.8,rep(0,10),-0.8)), n = 359, sd = 0.2)
# 
# # Joining data and creating a time series object
# ts2 <- ts(c(y1,y2), start = c(1941,1), end = c(2000,12), frequency = 12)
# 
# # Creating bfast01 object
# bf2 <- bfast01(ts2, formula = response ~ trend)
# 
# # Applying the classification
# bfast01classify(bf2, typology = "standard") # Reversal from decrease to increase
# bfast01classify(bf2, typology = "drylands") # Reversal from decrease to increase, Complete
# 
# # Visualizing the time series with break and fitted trends
# plot(bf2) 
