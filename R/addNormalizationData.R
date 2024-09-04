#' add normalization data
#'
#' adds a column with normalization factors.
#'
#' @param raw.dat dataframe
#' @param norm.dat.dir character-path of files. files are plain text with 8 rows and 12 columns
#' @param norm.meth character
#' @param batch.col character, matches file names
#' @param na what are NA symbols, defaults to "NA"
#' @export




for(normdat in normfiles){

  ndat<-read96wells(mat = read.table(
    file.path(norm.dat.dir,normdat),na.strings = na))

  rdat<-raw.dat[raw.dat[,batch.col]==normdat,]

  for(i in 1:nrow(rdat)){

    y=ndat$Value[ndat$well==rdat$well[i]]
    if(!is.na(y)&rdat$wave.id[i]!="Background"){
      rdat[i,norm.meth]<-y
    }else{rdat[i,norm.meth]<-NA}
    
  dat_norm<-rbind(dat_norm,rdat)
}
return(dat_norm)
}
