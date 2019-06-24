#' Title
#'
#' @param listBouts 
#' @param listCalendarsAndBouts 
#' @param partition 
#' @param label 
#'
#' @return
#' @export
#'
#' @examples
basicSummaryOfPartitions=function(listBouts,listCalendarsAndBouts,partition,label="partition"){
  tmpVariables=c(listCalendarsAndBouts %>% unlist(),names(listCalendarsAndBouts))
  names(tmpVariables)=NULL
  missingUnits=tmpVariables %>% setdiff(names(secondsPerUnit))
  if(length(missingUnits)>0){
    tmpUnits=rep(1,length(missingUnits))
    names(tmpUnits)=missingUnits
  }
  secondsPerUnit=c(secondsPerUnit,tmpUnits)
  
  listBouts %>% summaryListOfBoutsByPartition(listCalendarsAndBouts,partition,label) %>%
    mutate(variable=ifelse(variable=="isOn",calendar,variable)) %>%
    mutate(ratio=duration/durationRef) %>%
    mutate(time=duration/secondsPerUnit[variable])
}
