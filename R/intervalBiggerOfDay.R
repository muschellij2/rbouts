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
intervalBiggerOfDay=function(intervalo,intervalo2=NULL){
 if (is.null(intervalo2)) intervalo2=intervalo
 interseccion=intervalo %>% intervalIntersectv2(intervalo2)


 if(nrow(interseccion)>0) {
   interseccion_hours = interseccion %>%
     mutate(hours=as.numeric(difftime(toNew,fromNew,units="hours")),
            duracion=as.numeric(difftime(to,from,units="hours"))) %>%
     group_by(from,to) %>%
     summarise(hours=sum(hours),duracion=sum(duracion),center=first(fromNew)+difftime(last(toNew),first(fromNew))/2) %>%
     mutate(day=as_date(center+dhours(5))) %>% ungroup() %>%
     mutate(peso=round((1+0.5*(hour(center)<8))*(duracion+3*hours),6))
 } else {
   interseccion_hours = interseccion%>% mutate(hours=0.0,duracion=0.0,center=fromNew,day=as_date(center+dhours(5)),peso=0) %>% filter(FALSE)
 }

 almostThere=interseccion_hours %>% group_by(day) %>% filter(peso==max(peso))
 if(nrow(almostThere)<0 )almostThere=almostThere %>% summarise_all(first)
  almostThere %>% select(day,from,to)

  #
  #
  #
  # if(nrow(intervalo)>0) {
  #   intervalo_hours = intervalo %>% mutate(  center=from+difftime(to,from)/2,
  #                                            day=as_date(center+dhours(2)),
  #                                            hours=as.numeric(difftime(to,from,units="hours")))
  #   } else {
  #   intervalo_hours = intervalo %>% mutate(day=today(), center=from,hours=0.0,duracion=0.0) %>% filter(FALSE)
  #   }
  #
  # intervalo_hours %>% group_by(day) %>% filter(hours==max(hours)) %>% summarise_all(first) %>%
  #   select(day,from,to)
}
