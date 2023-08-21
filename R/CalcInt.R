IntSummaryCalc <- function(vec, summaryFunc){
  ifelse(length(vec[!is.na(vec)]) == 0,
         NA,
         do.call(summaryFunc, list(vec,na.rm=TRUE))
  )
}

calcInt<-function(x,varname=NULL,
                  groupingVar="samp_int",
                  intVar="interval",
                  intervals=c("Int1","Int2","Int3","Int4"),
                  measure="ocr",
                  calcFunctions=c("mean","min", "max", "min")){
  y<-x %>%
    group_by(.data[[groupingVar]]) %>%
    mutate("{varname}" := ifelse(.data[[intVar]]==intervals[1],
                           IntSummaryCalc(.data[[measure]],calcFunctions[1]),
                           ifelse(.data[[intVar]]==intervals[2],
                                  IntSummaryCalc(.data[[measure]],calcFunctions[2]),
                                  ifelse(.data[[intVar]]==intervals[3],
                                         IntSummaryCalc(.data[[measure]], calcFunctions[3]),
                                         ifelse(.data[[intVar]]==intervals[4],
                                                IntSummaryCalc(.data[[measure]],calcFunctions[4]),
                                                NA)))))

  return(y)
}
