#' Dado una lista de bouts, calendarios y otra lista de que bouts queremos medir para cada calendario
#' nos devuelve un dataframe con un resumen de las variables de interés (bouts) medidas en sus respectivos calendarios
#'
#' @param bouts 
#' @param calendars 
#' @param listCalendarsAndBouts 
#'
#' @return
#' @export
#'
#' @examples
summaryBoutsByCalendar=function(bouts,calendars,listCalendarsAndBouts) {
  oldw <- getOption("warn")
  options(warn = -1)
  varsToGenerate=listCalendarsAndBouts %>%
    map(~ data.frame(variable=.x,stringsAsFactors = FALSE))  %>%
    list2df(label = "calendar") %>%
    as_tibble()
  result=map2_df(varsToGenerate[["variable"]]%>% as.list(),
                 varsToGenerate[["calendar"]]%>% as.list(),
                 ~ {df=doSummary(intervalReference = calendars[[.y]],
                                 intervalBout = bouts[[.x]])
                 if(!is.null(df)) df%>% mutate(variable=.x,calendar=.y) else NULL
                 }
  )
  options(warn = oldw)
  result
}