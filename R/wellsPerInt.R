#' wellsPerInt
#'
#' table of well counts per sample for each interval
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @param dat data frame
#' @param measure character, what data column should be not na
#' @param wellVar character, column name for well
#' @param groupingVars character vector of variables to group by
#' @param tableVars character vector of variables to show results by
#' @param summaryColName name for the result column
#' @param intVar character, name of column with intervals
#' @importFrom rlang :=
#' @export

wellsPerInt<-function(dat, measure="ocr",
                      wellVar="well",
                      groupingVars=c("AgeGrp","interval","mID"),
                      tableVars=c("AgeGrp","GT"),
                      summaryColName="nWells",
                      intVar="interval"
                      ){
  dat2<-dat %>%
    filter(!is.na(measure)) %>%
    select(all_of(union(wellVar,union(tableVars,groupingVars)))) %>%
    distinct() %>%
    group_by_at(groupingVars) %>%
    mutate("{summaryColName}":=n()) %>%
    select(all_of(union(tableVars,c(summaryColName,groupingVars)))) %>%
    distinct() %>%
    filter(.data[[intVar]]!="Int0") %>%
    pivot_wider(.,names_from = intVar,values_from = summaryColName) %>%
    group_by_at(tableVars) %>%
    arrange_at(tableVars)

}

