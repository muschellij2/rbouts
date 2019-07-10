#' Genera una lista de todos los bouts para un dataframe con los nodos y ficheros para los que tenemos los epochs ya calculados
#'
#' @param dfTareas 
#'
#' @return
#' @export
epoch2Bouts=function(dfTareas){
progreso=dplyr::progress_estimated(nrow(dfTareas), min_time = 0)
listBouts=pmap(list(Nodo=dfTareas$Nodo,
                    fileBIN=dfTareas$fileBIN,
                    desde=dfTareas$desde,
                    hasta=dfTareas$hasta), 
               function(Nodo,fileBIN,desde,hasta) obtenerBouts(Nodo,fileBIN,desde,hasta,progreso=progreso))
names(listBouts)=dfTareas$fileBIN
listBouts
}