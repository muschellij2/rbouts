#' Dado una lista de bouts, y una lista que contiene funciones de generación de calendarios,
#' devuelve lista de calendarios
#'
#' @param bouts 
#' @param listFunctionalCalendars 
#' @param progreso 
#'
#' @return
#' @export
computeCalendars=function(bouts,generateCalendars,progreso=NULL,idBIN=NA_character_){
  if( "data.frame" %in% class(bouts[[1]]) )  {
    if(!is.null(progreso)) progreso$tick()$print()
    generateCalendars(bouts,idBIN=idBIN) 
  }
  else
  {
    progreso=dplyr::progress_estimated(length(bouts), min_time = 0)
    map( bouts, ~ computeCalendars(.x,generateCalendars,progreso,idBIn=idBIN))
  }
}
