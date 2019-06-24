#' Dado una lista de bouts, y una lista que contiene funciones de generación de calendarios,
#' devuelve lista de calendarios
#'
#' @param bouts 
#' @param listFunctionalCalendars 
#' @param mc.cores 
#'
#' @return
#' @export
computeCalendars=function(bouts,listFunctionalCalendars,mc.cores=numCores(4)){
  listFunctionalCalendars %>% mclapply(function(calendario) {calendario(bouts)},mc.cores=mc.cores)
}
