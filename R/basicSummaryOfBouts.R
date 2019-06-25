#' Title
#'
#' @param bouts 
#' @param listCalendarsAndBouts 
#' @param progreso 
#'
#' @return
#' @export
#'
basicSummaryOfBouts=function(bouts,listCalendarsAndBouts,progreso=NULL){
  
  if("data.frame" %in% class(bouts[[1]])){
    if(!is.null(progreso)) progreso$tick()$print()
    validcalendars=computeValidCalendars(bouts,listFunctionalCalendars)
    bouts=bouts %>% append( map(validcalendars, ~ .x %>% select(from,to)))
    
    listCalendarsAndBouts=map2(listCalendarsAndBouts,names(listCalendarsAndBouts),~append(.x,.y))
    
    summaryBoutsByCalendar(bouts,validcalendars,listCalendarsAndBouts) %>%
      mutate(ratio=duration/durationRef) %>%
      mutate(time=duration/seconds2Unit(variable)) 
  } else {
    progreso=dplyr::progress_estimated(length(bouts), min_time = 0)
    map( bouts, ~ basicSummaryOfBouts(.x,listCalendarsAndBouts,progreso)) %>% list2df("fileBIN")
  }
}
