#' Title
#'
#' @param bouts 
#' @param label 
#' @param first 
#' @param last 
#' @param offsetLabels 
#' @param starts 
#' @param duration 
#'
#' @return
#' @export
genCalendar_24h=function(bouts,label="24h",first=TRUE,last=TRUE,offsetLabels=dhours(0),starts=dhours(0),duration=dhours(23)+dminutes(59)+dseconds(59)){
  start=bouts$isOn[["from"]]
  end=bouts$isOn[["to"]]
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
    if(numDias==0){
      data.frame(from=from) %>% mutate(
        to=from+duration,
        day=as_date(from[1]+offsetLabels),
        label=label) %>%
        as_tibble()
    } else {
      #Void/Trivial data frame
      data.frame(from=df$timestamp[1],to=df$timestamp[nrow(df)],day=as.Date(df$timestamp[1]),label=label) %>%
        as_tibble()
    }
  }
}



#' Title
#'
#' @param horario1 
#' @param horario2 
#' @param label 
#'
#' @return
#' @export
genCalendar_HorarioToHorarioFrom=function(horario1,horario2,label="horario_1_2"){
  horario1  %>%  transmute(from=to,day=day,label=label) %>%
    left_join(horario2 %>% transmute(to=from,day=day),by="day") %>%
    filter(from<to) %>% select(from,to,day)%>% mutate(label=label)
}



#' Title
#'
#' @param bouts 
#' @param label 
#' @param first 
#' @param last 
#' @param offsetLabels 
#' @param starts 
#' @param duration 
#'
#' @return
#' @export
genCalendar_daily=function(bouts,label="daily",first=TRUE,last=TRUE,offsetLabels=dhours(0),starts=dhours(0),duration=dhours(23)+dminutes(59)+dseconds(59)){
  genCalendar_24h(bouts,label=label,first=first,last=last,offsetLabels=offsetLabels,starts=starts,duration=duration) %>%
    intervalIntersectv2(bouts$isOn) %>%
    transmute(from=fromNew,to=toNew,day=day,label=label)
}


#' Title
#'
#' @param bouts 
#' @param label 
#'
#' @return
#' @export
genCalendar_start2end=function(bouts,label="start2end"){
  start=bouts$isOn[["from"]]
  end=bouts$isOn[["to"]]
  timezone=tz(start)
  
  data.frame(
    from=start,
    to=end) %>% mutate(
      day=as_date(from),
      label=label) %>% as_tibble()
}




#' Title
#'
#' @param bouts 
#' @param label 
#'
#' @return
#' @export
genCalendar_mostQuiet=function(bouts,label="mostQuiet"){
  bouts$SIB2 %>% intervalReposo(
    bouts$MuyQuieto,
    distance1=parametrosAcelerometria$distanceMiniSib,
    distance2=parametrosAcelerometria$distanceMiniQuiet) %>% 
    intervalBiggerOfDay(bouts$SIB) %>% mutate(label=label)
}


#' Title
#'
#' @param bouts 
#' @param label 
#' @param durMin 
#' @param durMax 
#'
#' @return
#' @export
genCalendar_bed=function(bouts,label="bed",durMin=dminutes(160),durMax=dhours(24)){
  mayorReposo=genCalendar_mostQuiet(bouts)
  SIBDormir=bouts$SIB %>% intervalIntersectv2(mayorReposo) %>% select(from,to,day)
  QuietoDormir=bouts$MuyQuieto %>% intervalIntersectv2(mayorReposo) %>% select(from,to,day)
  
  SIBDormir %>% intervalReposo( QuietoDormir, 
                                distance1=parametrosAcelerometria$distanceSib,
                                distance2=parametrosAcelerometria$distanceQuiet) %>%
    connectOverDistanceV2(parametrosAcelerometria$distanceSedentaryEarlyMorning,
                          parametrosAcelerometria$distanceSedentaryRestOfDay,
                          parametrosAcelerometria$earlyMorning) %>%
    intervalBiggerOfDay() %>% mutate(label=label) %>% select(from,to,day,label) %>%
    filter(difftime(to,from)<=durMax & difftime(to,from)>=durMin) %>% ungroup()
}


#' Title
#'
#' @param bouts 
#' @param calendar 
#' @param label 
#'
#' @return
#' @export
genCalendar_complementaryInternal=function(bouts,calendar,label="complementaryInternal"){
  start=bouts$isOn[["from"]]
  end=bouts$isOn[["to"]]
  timezone=tz(start)
  
  calendar  %>%
    transmute(from2=to+dseconds(1),to2=lead(from,1)+dseconds(-1),day=day) %>%
    filter(row_number()>1 & row_number()<nrow(.) | row_number()==1) %>%
    filter(!is.na(to2)) %>% transmute(from=from2,to=to2,day=day,label=label)
}




#' Title
#'
#' @param bouts 
#' @param bed 
#' @param label 
#'
#' @return
#' @export
genCalendar_awake=function(bouts,bed,label="awake"){
  genCalendar_complementaryInternal(bouts,bed,label=label)
}


#' Title
#'
#' @param bouts 
#' @param calendar 
#' @param label 
#'
#' @return
#' @export
genCalendar_complementary=function(bouts,calendar,label="complementary"){
  start=bouts$isOn[["from"]]
  end=bouts$isOn[["to"]]
  timezone=tz(start)
  
  interno=genCalendar_complementaryInternal(bouts,calendar,label=label)
  
  datePre=min(calendar$day %>% first()-1, as.Date(start))
  datePost=calendar$day %>% last()
  primero=data.frame(from=start,to=calendar$from %>% first()-dseconds(1)) %>%
    mutate (day=datePre,label=label) %>%
    filter(from<to) 
  ultimo=data.frame(from=calendar$to %>% last()+dseconds(1),to=end)%>%
    mutate(day=datePost,label=label) %>% filter(from<to) 
  rbind(primero,interno,ultimo)
}

