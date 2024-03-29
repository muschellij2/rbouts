#' Obtain the intervals formed by the best candidates every day to be considered as time and sleeping in bed
#'
#' For each day of accelerometry recorded choose an interval as best candidate to be considered as interval in bed, and inside of it, mark another interval as time from first sleep to awake before going out of bed
#'
#' @param intervalSib dataframe of intervals considered as SIB
#' @param distance1 distance allowed in SIB intervals to consider that the form part of the same sleep period and not different sleeping periods
#' @param distance2 distance allowed in intervalQuiet to connect intervals of low activity, considered as taking part in the same low activity period.
#'
#' @return A dataframe of intervals representing the intervals of being bed and sleeping for every day of accelerometry data
#'
#' @export
intervalBED=function(intervalSIB,intervalQuiet,distance1=dminutes(30), distance2=dminutes(5),distance3=dhours(4)){



  if(nrow(intervalSIB)>0){
  sibOverQuiet=intervalSIB %>% connectOverDistance(distance1)  %>%
    connectOverInterval(intervalQuiet %>% connectOverDistance(distance2))
  } else {sibOverQuiet=intervalSIB %>% mutate(fromPre=from, toPost=to)}


  sibOverQuiet_justSIB=sibOverQuiet %>% intervalIntersect(intervalSIB)


  if(nrow(sibOverQuiet_justSIB)>0) sibOverQuiet_justSIB_hours = sibOverQuiet_justSIB%>%
                              mutate(hours=as.numeric(difftime(toNew,fromNew,units="hours"))) %>%
                              group_by(from,to) %>%
                              summarise(hours=sum(hours),center=first(fromNew)+difftime(last(toNew),first(fromNew))/2) %>%
                              mutate(day=as_date(center+dhours(2))) %>% ungroup()
  else
    sibOverQuiet_justSIB_hours = sibOverQuiet_justSIB%>%
    mutate(hours=0.0,center=fromNew,day=as_date(center+dhours(2)))





  ###Interesante poner aqui unión de tramos según horas de sueño y distancia

  ##sibOverQuiet_justSIB_hours= connectOverVariableDistance(sibOverQuiet_justSIB_hours,fraccion = 0.333)


  sibOverQuiet_justSIB_Maxhours = sibOverQuiet_justSIB_hours %>%
    group_by(day) %>%
    summarise(hours=max(hours))


  resultado<- sibOverQuiet_justSIB_hours %>% inner_join(sibOverQuiet_justSIB_Maxhours,by=c("hours", "day")) %>%
    select(from,to,day,hours) %>%
    left_join (sibOverQuiet,by = c("from", "to"))


  resultado <- resultado %>%
    mutate(
      startBed=fromPre,
      startSleep=from,
      endSleep=to,
      endBed=toPost,
      sleepHours=hours)

  if (nrow(resultado)>0){
  resultado <- resultado %>% mutate(
      sleepEfficiency=sleepHours/as.numeric(difftime(endSleep,startSleep,units="hours"))*100,
      bedHours=as.numeric(difftime(endBed,startBed,units="hours")),
      minutesBedToSleep=as.numeric(difftime(startSleep,startBed,units="mins"))
  )} else {
    resultado <- resultado %>% mutate(
      sleepEfficiency=NA_real_,
      bedHours=0,
      minutesBedToSleep=NA_real_
    )
    }

  resultado <- resultado %>%
    select(day,startBed,startSleep,endSleep,endBed,sleepHours,sleepEfficiency,bedHours,minutesBedToSleep)


  resultado
}
