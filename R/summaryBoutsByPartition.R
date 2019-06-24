#' Title
#'
#' @param bouts 
#' @param calendars 
#' @param listCalendarsAndBouts 
#' @param partition 
#' @param label 
#'
#' @return
#' @export
#'
#' @examples
summaryBoutsByPartition=function(bouts,calendars,listCalendarsAndBouts,partition,label="partition") {
  calendars %>% partition() %>% compact() %>% map(~ summaryBoutsByCalendar(bouts, . ,listCalendarsAndBouts)) %>% list2df(label=label)
}