#' calcInt
#'
#' calculates the summarized data over the time points in each interval
#'
#' @param x data frame
#' @param varname character, new name for column
#' @param groupingVar character, variable to group by before summarizing
#' @param intVar character-column name for intervals
#' @param intervals character vector of intervals to include
#' @param measure character, which column to summarize
#' @param calcFunctions character vector of the summary functions corresponding to the intervals, e.g. c("mean","min", "max", "min")
#' @importFrom rlang :=
#' @export

calcInt<-function(x,varname="ocr",
                  groupingVar="samp_int",
                  intVar="interval",
                  intervals=c("Int1","Int2","Int3","Int4"),
                  measure="ocr",
                  calcFunctions=c("mean","min", "max", "min")){


  IntSummaryCalc <- function(vec, summaryFunc){
    ifelse(length(vec[!is.na(vec)]) == 0,
           NA,
           do.call(summaryFunc, list(vec,na.rm=TRUE))
    )
  }


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
