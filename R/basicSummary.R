#' Title
#'
#' @param listBouts 
#' @param listCalendarsAndBouts 
#' @param partition 
#' @param label 
#'
#' @return
#' @export
basicSummary=function(listBouts,listCalendarsAndBouts,partition=NULL,label="partition"){
  if(is.null(partition))basicSummaryOfBouts(listBouts,listCalendarsAndBouts) else basicSummaryOfPartitions(listBouts,listCalendarsAndBouts,partition,label)
}