#caveats:
#function assumes discontinuity point is at zero and that range is symmetric about that point
#assumes that running variable is named x

#origin: this is built by modifying code from Daniel Dimmery's RDD command.
#his source code is here: https://github.com/ddimmery/rdd/blob/master/plot.RD.R

loc_lin <- function(df, y, bw, range, cutoff = 0, gran = 40, kern = "triangular") {
  require(rdd) #this is needed for the kernelwts only
  d.l<-data.frame(X=df[df$x<cutoff,"x"],Y=df[df$x<cutoff,y])
  lval<-seq(range[1],cutoff,length.out=(gran%/%2 + 1))
  width <- abs(range[1])/(gran%/%2)
  line_minus <- data.frame(x = lval, y = NA, lwr = NA, upr = NA)
  for(i in 1:(gran%/%2 + 1)) {
    sub<-d.l$X>=(lval[i]-bw) & d.l$X<=(lval[i]+bw)
    w<-kernelwts(X=d.l$X[sub],center=lval[i],bw=bw,kernel=kern)
    ly<-d.l$Y[sub]
    lx<-d.l$X[sub]
    if(length(lx)<=2)
      pred<-rep(NA,3)
    else
      pred<-predict(lm(ly~lx,weights=w),interval="confidence",newdata=data.frame(lx=lval[i]))
    pred
    line_minus[i,c("y","lwr","upr")] = pred
  }
  
  d.r<-data.frame(X=df[df$x>cutoff,"x"],Y=df[df$x>cutoff,y])
  rval<-seq(cutoff,range[2],length.out=(gran%/%2 + 1))
  width <- abs(range[2])/(gran%/%2)
  line_plus <- data.frame(x = rval, y = NA, lwr = NA, upr = NA)
  for(i in 1:(gran%/%2  + 1)) {
    sub<-d.r$X>=(rval[i]-bw) & d.r$X<=(rval[i]+bw)
    w<-kernelwts(X=d.r$X[sub],center=rval[i],bw=bw,kernel=kern)
    ry<-d.r$Y[sub]
    rx<-d.r$X[sub]
    if(length(rx)<=2)
      pred<-rep(NA,3)
    else
      pred<-predict(lm(ry~rx,weights=w),interval="confidence",newdata=data.frame(rx=rval[i]))
    line_plus[i,c("y","lwr","upr")] = pred
  }
  return_list <- list("line_minus" = line_minus, "line_plus" = line_plus)
  return(return_list)
}
