#' Connect intervals that are close enough
#' Generates a smaller dataframe of connected intervals using a data frame of given intervals and a distance that allows tow of them to be connected
#'
#' @param interval Initial dataframe of intervals to be connected
#' @param distance distance between ttwo intervals to allo the connection of both in just one.

#' @return A dataframe of intervals, having less or equal rows tan the original
#'
#' @export

connectOverVariableDistance=function(interval,fraccion=0.33){
  secDistance=distance/dseconds(1)

    interval2=interval %>%
    mutate(lag=difftime(from,lag(to,1),units="hours"),
           lead=difftime(lead(from,1),to,units="hours"),
           leadhours=lead(hours,1))
   interval2$distanciaReferencia=interval2 %>% select(hours,leadhours) %>% pmap(min,na.rm=T) %>% unlist()*fraccion


   interval2 =interval2 %>% mutate(closeEnough=lead<=distanciaReferencia)


  interval2 %>%
    mutate(change=!lag(closeEnough),
           change=ifelse(is.na(change),0,change),
           bout=cumsum(change) ) %>%
    group_by(bout) %>%
    summarise(from=first(from),to=last(to),hours=sum(hours),center=last(center),day=last(day))
}
