## ----------------------------------------------------
## classification function based on the BFAST01 object
## ----------------------------------------------------
# Input: BFAST01 object (object)
# Return: integer representing the class
## classification function for 1-break bfast output
## object: bfast01 object
## alpha: threshold for significance tests, default 0.05
## pct_stable: threshold for segment stability, unit: percent change per unit time (0-100), default NULL

bfast01classify <- function(object, alpha=0.05, pct_stable=NULL) { 
   ## output array 
   out <- rep(NA,7)
   names(out) <- c("flag_type","flag_significance","p_segment1","p_segment2",
                   "pct_segment1","pct_segment2","flag_pct_stable")
   ## classification
   object.zoo <- as.zoo(object) # data series
   ## monotonic if no break
   if(object$breaks == 0) {     
      slope <- object$model[[1]]$coefficients[2] # slope
      if(slope > 0) out[1] <- 1
      if(slope < 0) out[1] <- 2
   } else {
     ## if break, list segment and break point parameters (p$..)
     ToB <- as.numeric(object$breakpoints[[1]])  # time of break
     s1 <- object$model[[2]]$coefficients[3] # slope segment 1
     s2 <- object$model[[2]]$coefficients[4] # slope segment 2
     m <- as.numeric(object.zoo$trend[ToB+1]) - as.numeric(object.zoo$trend[ToB]) # magnitude of abrupt change
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
   ## ANOVA and PCTCHANGE      
   for (segment in 1:(object$breaks+1)) {
      # subset zoo object for segment
      date.start <- if(segment==1)  object$data$time[1] else object$data$time[ToB+1]
      date.end <- if(segment==2 || object$breaks==0) object$data$time[nrow(object$data)] else object$data$time[ToB]
      object.zoo.subset <- window(object.zoo, start=date.start, end=date.end)
      # Anova
      segment.anova <- anova(lm((object.zoo.subset$response-object.zoo.subset$season)~time(object.zoo.subset))) 
      # linear model of deseasonalized trend versus time
      out[segment+2] <- segment.anova$Pr[1]
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
      out[segment+4] <- segment.pctchange
   }
   ## Segment significance flag
   # code: 0 = both segments significant (or no break and significant), 
   # 1 = only first segment significant, 
   # 2 = only 2nd segment significant, 
   # 3 = both segments insignificant (or no break and not significant)
   
   # no break
   if(object$breaks == 0) {     
      if(out[3] <= alpha) out[2] <- 0
      if(out[3] > alpha) out[2] <- 3
   # with break
   } else {
      if(out[3] <= alpha && out[4] <= alpha) out[2] <- 0
      if(out[3] <= alpha && out[4] > alpha) out[2] <- 1
      if(out[3] > alpha && out[4] <= alpha) out[2] <- 2
      if(out[3] > alpha && out[4] > alpha) out[2] <- 3
   }   

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
   } else {
     out[7] <- NA
   }   
   return(as.data.frame(t(out)))
}

# ## print the flag labels
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