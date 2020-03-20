# winsor(df[["ratio_p_indiv"]], fractionLow = 0, fractionHigh = 0.95)

winsor <- function (x, fractionLow=0.05, fractionHigh=.95, verbose = FALSE, varname = "var", pos_only = FALSE)
{
  #check inputs
  if(length(fractionLow) !=1 || fractionLow < 0 ||
       fractionLow > 0.5) {
    stop("bad value for 'fractionLow'")
  }
  if(length(fractionHigh) != 1 || fractionHigh < 0 ||
       fractionHigh < 0.5) {
    stop("bad value for 'fractionHigh'")
  }
  
  #compute limits
  if(pos_only == TRUE) {
    x_quantile_sample <- x[x>0]
  }
  else {
    x_quantile_sample <- x
  }
  lim <- quantile(x_quantile_sample, probs=c(0,fractionLow, fractionHigh,1), na.rm = TRUE)
  min_str <- min(x)
  max_str <- max(x)
  
  #winsorize
  lim[2] <-  ifelse(fractionLow == 0,min_str,lim[2])
  lim[3] <- ifelse(fractionHigh == 1, max_str, lim[3])
  x[ x< lim[2] ] <- lim[2]
  x[ x> lim[3] ] <- lim[3]
  if (verbose == TRUE) {
    if (fractionLow != 0) {
      print(paste0(varname," min was ",lim[1],", now winsorized to ", fractionLow," percentile of ",min(x)))
    }
    if (fractionHigh != 1) {
      print(paste0(varname," max was ",lim[4],", now winsorized to ",fractionHigh," percentile of ",max(x)))
    }
  }
  
  return(x)
}
