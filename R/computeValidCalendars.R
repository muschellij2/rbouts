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
computeValidCalendars=function(bouts,generateCalendars,labelbed="bed",progreso=NULL,idBIN=NA_character_){

    if( "data.frame" %in% class(bouts[[1]]) )  {
    if(!is.null(progreso)) {progreso$tick()$print(); cat ("\t",idBIN)}

      computeCalendars(bouts,generateCalendars,idBIN=idBIN) %>% filterInvalidCalendars(bouts,labelbed=labelbed)
  }
  else
  {
    progreso=dplyr::progress_estimated(length(bouts), min_time = 0)
     map2(bouts,names(bouts), ~ computeValidCalendars(.x,generateCalendars,labelbed=labelbed,progreso,idBIN=.y)  )
  }
}
