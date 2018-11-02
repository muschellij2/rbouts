#' Connect intervals that are close enough
#' Generates a smaller dataframe of connected intervals using a data frame of given intervals and a distance that allows tow of them to be connected
#'
#' @param interval Initial dataframe of intervals to be connected
#' @param distance distance between ttwo intervals to allo the connection of both in just one.

#' @return A dataframe of intervals, having less or equal rows tan the original
#'
#' @export

connectOverDistance=function(interval,distance=dminutes(30)){
  secDistance=distance/dseconds(1)
  interval=interval %>%
    mutate(lag=difftime(from,lag(to,1),units="secs"),
           lead=difftime(lead(from,1),to,units="secs"),
           closeEnough=lead<=distance)


  interval %>%
    mutate(change=!lag(closeEnough),
           change=ifelse(is.na(change),0,change),
           bout=cumsum(change) ) %>%
    group_by(bout) %>%
    summarise(from=first(from),to=last(to))
}
