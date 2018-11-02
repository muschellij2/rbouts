doSummary=function(intervalReference,intervalBout,by="day"){
  #intervalReference=df %>% intervals4Summary_daily(first=FALSE,offsetLabels=dhours(0),starts=dhours(0),duration=dhours(24))
  intervalBout=intervalReference %>% intervalIntersect(intervalBout) %>%
    mutate(from=fromNew,to=toNew) %>%
    select(from,to,everything(),-fromNew,-toNew)




  tableRef=intervalReference  %>% mutate(duration=difftime(to,from,units="secs"))       %>% group_by_(by)  %>% summarise(durationRef=as.integer(sum(duration,na.rm=T)))
  tableBout=intervalBout       %>% mutate(duration=difftime(to,from,units="secs")) %>%
    group_by_(by) %>%
    summarise(maxdur=max(duration),
              num=n(),
              duration=as.integer(sum(duration,na.rm=T)),
              )


tableRef %>% left_join(tableBout,by="day") %>%
    ungroup() %>%
    mutate(weight=durationRef/sum(durationRef),
           duration=ifelse(is.na(duration),0,duration),
           num=ifelse(is.na(num),0,num),
           maxdur=ifelse(is.na(maxdur),0,maxdur))

  }


