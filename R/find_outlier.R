
find_outlier <- function(x,mult=1.5) {
  return(x < quantile(x, .25,na.rm=T) - mult*IQR(x,na.rm=T) | x > quantile(x, .75,na.rm=T) + mult*IQR(x,na.rm=T))
}
