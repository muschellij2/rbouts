#' Title
#'
#' @param listBouts 
#' @param listCalendarsAndBouts 
#'
#' @return
#' @export
#'
#' @examples
summaryListOfBoutsByCalendar=function(listBouts,listCalendarsAndBouts){
  resultado=listBouts %>%
    map2_df(names(listBouts), ~ summaryBoutsByCalendar(.x,
                                                       .x %>% filterInvalidCalendars(computeCalendars(.x,listFunctionalCalendars)),
                                                       listCalendarsAndBouts) %>% mutate(fileBIN=.y)
    )
}
