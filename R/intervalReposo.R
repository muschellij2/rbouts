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
intervalReposo=function(intervalo1,intervalo2,distance1=dminutes(40), distance2=dminutes(5),distance3=dhours(4),distance4=dminutes(1)){

  if(nrow(intervalo1)>0){
  intervalo3=intervalo1 %>% connectOverDistance(distance1)  %>%
    connectOverInterval(intervalo2 %>% connectOverDistance(distance2)) %>%
    transmute(from=fromPre,to=toPost)
  } else {intervalo3=intervalo1 %>% select(from,to) }



  ###Interesante poner aqui unión de tramos según horas de sueño y distancia

  intervalo3=intervalo3 %>%
    connectOverDistanceV2(distanciaNoche=distance3,distanciaDia=distance4) %>% select(from,to)
  intervalo3
  }
