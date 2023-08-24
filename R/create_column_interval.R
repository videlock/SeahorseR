#' create_column_interval
#'
#' Adds column with intervals spanning multiple time points
#'
#' @param df data frame
#' @param timeVar character-column name for time points (not time exact)
#' @param Interval.list list of vectors specifying time points to include for each interval
#' @export


create_column_interval <-
  function(df,
           timeVar="time",
           Interval.list=list(Int0=c(1),
                          Int1=c(2:3),
                          Int2=c(4:6),
                          Int3=c(7:10),
                          Int4=c(11:14))) {
    df<-as.data.frame(df)
    y <- as.numeric(df[,timeVar])
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

