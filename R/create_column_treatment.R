#' create_column_treatment
#'
#' Adds column with inhibitors
#'
#' @param df data frame
#' @param Treatment.list list with names equal to intervals
#' @param IntVar character-column name for intervals
#' @export



create_column_treatment <- function(df,
                                    Treatment.list=list(
                                      Int1="basal",
                                      Int2="oligomycin",
                                      Int3="FCCP",
                                      Int4="Rot+AA",
                                      Int0="Start"
                                    ),
                                    IntVar="interval"
                                    ) {
  df<-as.data.frame(df)


   y <- df[,IntVar]
  x <- character(length = length(y))


  for(int in names(Treatment.list)){
    for (i in 1:length(y)){
      if(y[i]==int){
        x[i] <- Treatment.list[[int]]
      }
    }

  }
  return(x)

  }

