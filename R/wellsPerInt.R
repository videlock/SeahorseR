
wellsPerInt<-function(dat,ShowAge=FALSE){
  if(ShowAge){
    dat2<-dat %>%
      filter(!is.na(ocr)) %>%
      dplyr::select(interval,well,mID, GT, AgeGrp,Age)
  }else{
    dat2<-dat %>%
      filter(!is.na(ocr)) %>%
      dplyr::select(interval,well,mID, GT, AgeGrp)
  }
  dat2 %>%
    distinct() %>%
    group_by(AgeGrp,interval,mID) %>%
    mutate(nWells=n()) %>%
    dplyr::select(-well) %>%
    distinct() %>%
    filter(interval!="Int0") %>%
    pivot_wider(.,names_from = interval,values_from = nWells) %>%
    group_by(AgeGrp,GT) %>%
    arrange(AgeGrp,GT)

}
