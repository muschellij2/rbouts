#' Dado una lista de bouts y un calendario devuelve una lsista de calendarios donde se cumplen las reglas de
#'  validez. Por tanto lo que obtenemos es un alista de calendarios más limitada.
#'
#' @param calendars 
#' @param bouts 
#' @param labelbed 
#'
#' @return
#' @export
filterInvalidCalendars=function(calendars, bouts, labelbed="bed"){
  calendars %>% list2df() %>% 
    left_join(computeInvalidCalendars(calendars,bouts,labelbed), by = c("day", "label")) %>%
    filter(is.na(exclusion)) %>% select(-exclusion) %>% df2list("label")
}