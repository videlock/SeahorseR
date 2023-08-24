#' remove_wells
#'
#' removes background and user-specified.
#'
#' @param dt dataframe
#' @param method character
#' @param background character vector
#' @param exclude character vector
#' @param nMeasures integer - how many timepoints
#' @export


remove_wells <- function(dt, method = "without transformation",
                         background = c("A01", "H01", "A12", "H12"), exclude=NULL, nMeasures=14) {
  a <- nrow(dt)
  print(paste("Input dt has", a, "rows (measurements)"))
  dt2 <- dt[!(dt$well %in% c(background,exclude)),]
  b <- a - nrow(dt2)
  print(paste0(b, " measurements removed (", b/nMeasures, " wells)"))

  if (method == "without transformation") {
    dt3 <- subset.data.frame(dt2, dt2$ocr >= 0)
    c <- nrow(dt2) - nrow(dt3)
    d <- c/nrow(dt2) *100
    print(paste0(c, " measurements with negative OCR removed (", round(d,3), "%)"))
  } else if (method == "with transformation") {
    abs_min_ocr <- abs(min(dt2$ocr))
    dt2$ocr <- dt2$ocr + abs_min_ocr + 0.0001
    print(paste0("All OCR values transformed by adding abs(min(dt$ocr)) and 0.0001"))
  } else {
    print("Error: not a valid input for method-variable in remove_wells")
  }
  dt3
}
