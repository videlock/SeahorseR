create_column_interval <-
  function(dt,
           timeVar="time",
           Interval.list=list(Int0=c(1),
                          Int1=c(2:3),
                          Int2=c(4:6),
                          Int3=c(7:10),
                          Int4=c(11:14))) {
    dt<-as.data.frame(dt)
    y <- as.numeric(dt[,timeVar])
    x <- character(length = length(y))


  for(int in names(Interval.list)){
    for (i in 1:length(y)){
      if(y[i]%in%Interval.list[[int]]){
        x[i] <- int
      }
    }

  }
  return(x)
}

