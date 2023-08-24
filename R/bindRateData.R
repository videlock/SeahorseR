#' bindRateData
#'
#' collects data exported from wave
#'
#' @param data.dir character-path of files
#' @param norm.meth character
#' @param out.name character
#' @param returnData Boolean
#' @importFrom data.table fread
#' @importFrom readr write_delim

#' @export





bindRateData<-function(data.dir="rate_data", norm.meth="none", out.name="all_rate_data.txt", returnData=TRUE){
  file_list <- list.files(path = data.dir)
  all_rate_data <- data.frame()
  for (i in 1:length(file_list)){
    temp_data <- data.frame()
    temp_data <- fread(file.path(data.dir,file_list[i]), data.table = F)
    colnames(temp_data) <- c("time", "well", "wave.id",
                             "time_exact", "ocr", "ecar", "ppr")
    if(nchar(file_list[i])==10){
      temp_data$plate_id<-"p1"
    }else{
      temp_data$plate_id <- gsub("-","p",substr(file_list[i],11,nchar(file_list[i])))
    }
    temp_data$input<-file.path(data.dir,file_list[i])
    temp_data$batch<-gsub(".+/","",temp_data$input)
    temp_data$norm.meth<-norm.meth
    all_rate_data <- rbind(all_rate_data, temp_data)
    rm(temp_data)
  }


  write_delim(all_rate_data, file = out.name, delim = "\t")

  if(returnData){
    return(all_rate_data)
  }

}
