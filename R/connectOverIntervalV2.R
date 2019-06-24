#'Connect intervals of a first dataframe using a second dataframe of intervals
#'
#' Connect the intervals of a first dataframe given that the can be considered connected if the separation between two of them are covered by a interval of a second dataframe.
#'
#' @param interval1 first dataframe
#' @param interval2 second dataframe
#'
#' @return A dataframe of intervals representing the resulting connected intervals
#'
#' @export
connectOverInterval=function(interval1,interval2){
  interval1=interval1 %>%
    mutate( falta = lead(from,1),.dummy=1)

  ##Preparativos sobre interval2 para que contenga todos los intervalos de interval1


    interval2=intervalMelt(interval1 %>% select(from,to), interval2%>% select(from,to)) %>%
    transmute(fromSup = from, toSup=to,.dummy=1)

previo=  interval1 %>%
    left_join(interval2,by=".dummy") %>%
  #    filter(to>=fromSup & to <=toSup) %>%
  #Overlap=  (StartA <= EndB) and (EndA >= StartB)
       filter( from<=toSup & to >=fromSup ) %>%
  group_by(from,to,falta) %>%
  summarise(fromSup=min(fromSup),toSup=max(toSup)) %>% ungroup() %>%
      mutate(closeEnough = falta<=toSup) %>%
      mutate(change=!lag(closeEnough),
             change=ifelse(is.na(change),0,change),
             bout=cumsum(change) ) %>%
      group_by(bout) %>%
      summarise(from=first(from),to=last(to),toPost=last(toSup)) %>%
      select(-bout) %>%
  mutate(.dummy=1)%>%
      left_join(interval2,by=".dummy") %>%
      filter( from<=toSup & to >=fromSup ) %>%
      mutate(fromPre=fromSup) %>%
      select(fromPre,from,to,toPost) %>%
      rowwise() %>%
      mutate(fromPre=min(fromPre,from),toPost=max(to,toPost) ) %>%
    ungroup() %>%
    group_by(from,to) %>%
    summarise(fromPre=min(fromPre),toPost=max(toPost)) %>% ungroup() %>%
  simplifyIntervals()

previo

}
