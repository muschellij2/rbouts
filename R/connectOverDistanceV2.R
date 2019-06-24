#' Connect intervals that are close enough
#' Generates a smaller dataframe of connected intervals using a data frame of given intervals and a distance that allows tow of them to be connected
#'
#' @param interval Initial dataframe of intervals to be connected
#' @param distanciaNoche distance between ttwo intervals to allo the connection of both in just one.
#' @param distanciaDia distance between ttwo intervals to allo the connection of both in just one.
#'
#' @return A dataframe of intervals, having less or equal rows tan the original
#'
#' @export


 cercaElastico=function(to1,from2,distanciaNoche=dhours(4),distanciaDia=dminutes(30),earlyMorning=c(1,9)){
   distancia=difftime(from2,to1,units="hours")

   ( (hour(to1)+minute(to1)/60) >=earlyMorning[1] & (hour(from2)+minute(from2)/60) <=earlyMorning[2]  & distancia<=distanciaNoche) | distancia<=distanciaDia

 }

connectOverDistanceV2=function(interval,distanciaNoche=dhours(4),distanciaDia=dminutes(0), earlyMorning=c(1,9)){
#  secDistance=distance/dseconds(1)
  interval=interval %>%
    mutate(lag=difftime(from,lag(to,1),units="secs"),
           lead=difftime(lead(from,1),to,units="secs"),
           closeEnough=  cercaElastico(to,lead(from,1),distanciaNoche,distanciaDia,earlyMorning)
             )


  interval %>%
    mutate(change=!lag(closeEnough),
           change=ifelse(is.na(change),0,change),
           bout=cumsum(change) ) %>%
    group_by(bout) %>%
    summarise(from=first(from),to=last(to))
}
