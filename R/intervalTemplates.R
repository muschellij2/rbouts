intervals4Summary_daily=function(df,label="24h",first=TRUE,last=TRUE,offsetLabels=dhours(0),starts=dhours(0),duration=dhours(23)+dminutes(59)+dseconds(59)){
  start=head(df$timestamp,1)
  end=tail(df$timestamp,1)
  timezone=tz(start)

  # problematic with summer time... Probably need another calendar to deal with this
  from=(start-dhours(hour(start))-dminutes(minute(start))-dseconds(second(start)))+starts+ddays(0:as.integer(as_date(end)-as_date(start)))
  #
  #from=ymd(as.character(as_date(start)+0:8),tz=tz(start))+starts
  #to=ymd(as.character(as_date(start)+0:8),tz=tz(start))+starts
  #day(from[1])
  if(!first) from=from[-1]
  if(!last) from=from[- length(from)]
  numDias=length(from)-1

  if(numDias>0){
   data.frame(from=from) %>% mutate(
    to=from+duration,
    day=as_date(from[1]+offsetLabels)+0:numDias,
    label=label) %>%
    as_tibble()
  }
  else {
    #Void/Trivial data frame
    data.frame(from=df$timestamp[1],to=df$timestamp[nrow(df)],day=as.Date(df$timestamp[1]),label=label) %>% as_tibble()# %>% filter(row_number()>1)
  }
}


intervals4Summary_strict=function(df,label="start_end"){
  data.frame(
             from=head(df$timestamp,1),
             to=tail(df$timestamp,1)) %>% mutate(
             day=as_date(from),
             label=label) %>% as_tibble()
}


intervals4Summary_dailystrict=function(df,label="24h",first=TRUE,last=TRUE,offsetLabels=dhours(0),starts=dhours(0),duration=dhours(23)+dminutes(59)+dseconds(59)){
  intervals4Summary_daily(df,label=label,first=first,last=last,offsetLabels=offsetLabels,starts=starts,duration=duration) %>%
    intervalIntersect(intervals4Summary_strict(df)) %>%
    transmute(day=day,from=fromNew,to=toNew)
}


intervals4Summary_awake=function(df,cama,label="awake",first=FALSE){
  start=head(df$timestamp,1)
  end=tail(df$timestamp,1)
  timezone=tz(start)

  cama  %>%
    transmute(from=endSleep+dseconds(1),to=lead(startSleep,1)+dseconds(-1),day=day,label=label) %>%
    filter(row_number()>1 & row_number()<nrow(.) | row_number()==1 & first) %>%
    filter(!is.na(to))
}

intervals4Summary_inBed=function(df,cama,label="bed",first=FALSE){
  start=head(df$timestamp,1)
  end=tail(df$timestamp,1)
  timezone=tz(start)

  cama  %>%
    transmute(from=startBed,to=endBed,day=day,label=label) %>%
    filter(row_number()>1  | row_number()==1 & first )
}



####No se usarán de aquí para abajo

BORRARintervals4Summary_WDvsWE=function(df,first=TRUE,last=TRUE,offsetLabels=dhours(0),starts=dhours(0),duration=dhours(23)+dminutes(59)+dseconds(59)){
  intervals4Summary_daily(df,first=first,last=last,offsetLabels=offsetLabels,starts=starts,duration=duration) %>%
    mutate(group=wday(label)==1 | wday(label)==7,
           group=c("WD","WE")[1+as.integer(group)])
}








