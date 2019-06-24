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
intervalBEDNew=function(bedNew,SIBDormir){

  dormir=SIBDormir%>%mutate(duration=as.integer(difftime(to,from,units="secs"))) %>% group_by(day) %>% summarise(startSleep=first(from),endSleep=last(to),sleepHours=round(sum(duration)/3600,2)) %>% ungroup()

  resultado=bedNew %>% transmute(day=day,startBed=from,endBed=to) %>% left_join(dormir,by="day") %>%
    select(day,startBed,startSleep,endSleep,endBed,sleepHours) %>%
    mutate(
      bedHours=round(as.numeric(difftime(endBed,startBed,units="hours")),2),
      sleepEfficiency=round(sleepHours/as.numeric(difftime(endSleep,startSleep,units="hours"))*100,2),
      minutesBedToSleep=round(as.numeric(difftime(startSleep,startBed,units="mins")),2)
    )

  resultado
}
