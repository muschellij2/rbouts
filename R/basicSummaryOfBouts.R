#' Title
#'
#' @param listBouts 
#' @param listCalendarsAndBouts 
#'
#' @return
#' @export
#'
#' @examples
basicSummaryOfBouts=function(listBouts,listCalendarsAndBouts){
  tmpVariables=c(listCalendarsAndBouts %>% unlist(),names(listCalendarsAndBouts))
  names(tmpVariables)=NULL
  missingUnits=tmpVariables %>% setdiff(names(secondsPerUnit))
  if(length(missingUnits)>0){
    tmpUnits=rep(1,length(missingUnits))
    names(tmpUnits)=missingUnits
  }
  secondsPerUnit=c(secondsPerUnit,tmpUnits)
  
  listBouts %>% summaryListOfBoutsByCalendar(listCalendarsAndBouts) %>%
    mutate(variable=ifelse(variable=="isOn",calendar,variable)) %>%
    mutate(ratio=duration/durationRef) %>%
    mutate(time=duration/secondsPerUnit[variable])
}
