#' Title
#'
#' @param bouts 
#' @param listCalendarsAndBouts 
#' @param progreso 
#'
#' @return
#' @export
#'
basicSummaryOfBouts=function(bouts,generateCalendars,listCalendarsAndBouts,progreso=NULL){
  
  if("data.frame" %in% class(bouts[[1]])){
    if(!is.null(progreso)) progreso$tick()$print()
    idBIN=bind_rows(bouts) %>% select(idBIN) %>% unique()%>%first()
    validcalendars=computeValidCalendars(bouts,generateCalendars,idBIN=idBIN)
    
#    print(validcalendars)
    bouts=bouts %>% append( map(validcalendars, ~ .x %>% select(from,to)))
    
    listCalendarsAndBouts=map2(listCalendarsAndBouts,names(listCalendarsAndBouts),~append(.x,.y))
    
    previo=summaryBoutsByCalendar(bouts,validcalendars,listCalendarsAndBouts)
    if(ncol(previo)==0){message("\tSummary without columns. Nothing to get from this.")
      
    return(NULL)
      }
    previo %>%
      mutate(ratio=duration/durationRef) %>%
      mutate(time=duration/seconds2Unit(variable)) 
  } else {
    progreso=dplyr::progress_estimated(length(bouts), min_time = 0)
    map( bouts, ~ basicSummaryOfBouts(.x,generateCalendars,listCalendarsAndBouts,progreso)) %>% list2df("fileBIN")
  }
}
