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
summaryListOfBoutsByPartition=function(listBouts,listCalendarsAndBouts,partition,label="partition"){
  resultado=listBouts %>%
    map2_df(names(listBouts), 
            function(bouts,fichero) {
              calendars=bouts %>% filterInvalidCalendars(computeCalendars(bouts,listFunctionalCalendars))
              particion=calendars %>% partition()
              particion %>% map2_df(names(particion), 
                                    ~ {df=summaryBoutsByCalendar(bouts,
                                                                 .x,
                                                                 listCalendarsAndBouts) %>% mutate(.label=.y)
                                    df[[label]]=df[[".label"]]
                                    df %>% select(-.label)
                                    }) %>%
                mutate(fileBIN=fichero)}
    )
}