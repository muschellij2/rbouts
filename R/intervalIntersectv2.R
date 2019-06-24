#' Intersect the intervals given by two dataframes
#' Generates the intersection dataframe resulting from the intersection of the intervals represented by two dataframes of intervals
#'
#' @param interval1 first dataframe
#' @param interval2 second dataframe

#' @return A dataframe of intervals representing the inersection
#'
#' @export
intervalIntersectv2=function(interval1,interval2){
  interval1=interval1 %>% mutate(.dummyColumn=1)

  interval2=interval2 %>% mutate(.fromCut=from,.toCut=to) %>% mutate(.dummyColumn=1) %>% select(.fromCut,.toCut,.dummyColumn,everything(),-from,-to)
  resultado=interval1 %>% full_join(interval2,by=".dummyColumn") %>%
    mutate(solapan=as.integer(as.period(lubridate::intersect(interval(from,to),interval(.fromCut,.toCut)),"seconds"))) %>%
    filter(!is.na(solapan)) %>% as_tibble() %>%
    mutate(fromNew=pmax(from,.fromCut),toNew=pmin(to,.toCut)) %>%
    select(from,to,fromNew,toNew,everything(),-.dummyColumn, -.fromCut, -.toCut,-solapan,)
  resultado
}
