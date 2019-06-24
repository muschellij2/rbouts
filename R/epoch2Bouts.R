#' Genera una lista de todos los bouts para un dataframe con los nodos y ficheros para los que tenemos los epochs ya calculados
#'
#' @param dfTareas 
#'
#' @return
#' @export
epoch2Bouts=function(dfTareas){
progreso=dplyr::progress_estimated(nrow(dfTareas), min_time = 0)
listBouts=map2(dfTareas$Nodo,dfTareas$fileBIN, ~ obtenerBouts( .x,.y,progreso=progreso))
names(listBouts)=dfTareas$fileBIN
listBouts
}