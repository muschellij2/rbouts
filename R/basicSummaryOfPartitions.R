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

  listBouts %>% summaryListOfBoutsByPartition(listCalendarsAndBouts,partition,label) %>%
    mutate(variable=ifelse(variable=="isOn",calendar,variable)) %>%
    mutate(ratio=duration/durationRef) %>%
    mutate(time=duration/seconds2Unit(variable))
}
