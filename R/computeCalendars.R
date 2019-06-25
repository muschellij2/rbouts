#' Dado una lista de bouts, y una lista que contiene funciones de generación de calendarios,
#' devuelve lista de calendarios
#'
#' @param bouts 
#' @param listFunctionalCalendars 
#' @param progreso 
#'
#' @return
#' @export
computeCalendars=function(bouts,listFunctionalCalendars,progreso=NULL){
  if( "data.frame" %in% class(bouts[[1]]) )  {
    if(!is.null(progreso)) progreso$tick()$print()
    map(listFunctionalCalendars, function(calendarFunction) calendarFunction(bouts)) 
  }
  else
  {
    progreso=dplyr::progress_estimated(length(bouts), min_time = 0)
    map( bouts, ~ computeCalendars(.x,listFunctionalCalendars,progreso))
  }
}
