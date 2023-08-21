add.norm<-function(raw.dat,norm.dat.dir,norm.meth,batch.col){
  raw.dat <- as.data.frame(raw.dat)
  raw.dat[,batch.col] <- as.factor(raw.dat[,batch.col])
  normfilelist<-list.files(norm.dat.dir)
  dat_norm<-data.frame()

  for(normdat in levels(raw.dat[,batch.col])){
    ndat<-read96wells(mat = read.table(
      file.path(norm.dat.dir,normdat)))

    rdat<-raw.dat[raw.dat[,batch.col]==normdat,]

    for(i in 1:nrow(rdat)){

      y=ndat$Value[ndat$well==rdat$well[i]]
      if(y>0&rdat$wave.id[i]!="Background"){
        rdat[i,norm.meth]<-y
      }else{rdat[i,norm.meth]<-NA}
    }
    dat_norm<-rbind(dat_norm,rdat)
  }
  return(dat_norm)
}

