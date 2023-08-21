
read96wells<-function(mat){

  v<-NULL
  for(i in 1:12){
    v<-c(v,mat[,i])
  }
  return(data.frame(well=paste0(
    rep(c("A","B","C","D","E","F","G","H"),12),
    sapply(c("01","02","03","04","05","06",
             "07","08","09","10","11","12"),
           function(x){rep(x,8)})),
    Value=v))
}
