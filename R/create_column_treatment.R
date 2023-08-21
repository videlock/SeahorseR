create_column_treatment <- function(dt,
                                    Treatment.list=list(
                                      Int1="basal",
                                      Int2="oligomycin",
                                      Int3="FCCP",
                                      Int4="Rot+AA",
                                      Int0="Start"
                                    ),
                                    IntVar="interval"
                                    ) {
  dt<-as.data.frame(dt)


   y <- dt[,IntVar]
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

