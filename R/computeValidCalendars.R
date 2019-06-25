#' Dado una lista de bouts, y una lista que contiene funciones de generación de calendarios,
#' devuelve lista de calendarios
#'
#' @param bouts 
#' @param listFunctionalCalendars 
#' @param progreso 
#' @param labelbed 
#'
#' @return
#' @export
computeValidCalendars=function(bouts,listFunctionalCalendars,labelbed="bed",progreso=NULL){

    if( "data.frame" %in% class(bouts[[1]]) )  {
    if(!is.null(progreso)) progreso$tick()$print()
      computeCalendars(bouts,listFunctionalCalendars) %>% filterInvalidCalendars(bouts,labelbed=labelbed)
  }
  else
  {
    progreso=dplyr::progress_estimated(length(bouts), min_time = 0)
     map (bouts, ~ computeValidCalendars(.x,listFunctionalCalendars,labelbed=labelbed,progreso)  )
  }
}
