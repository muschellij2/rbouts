#' Title
#'
#' @param Nodo 
#' @param fileBIN 
#' @param nucleos 
#'
#' @return
#' @export
obtenerBouts=function(Nodo,fileBIN,desde=NA,hasta=NA,nucleos=numCores(4),progreso=NULL){
  dirMeta=str_c(baseEpoch,"/",Nodo,"/output_2.archivos_bin_procesados/meta/")
  if(!is.null(progreso))progreso$tick()$print()
  cat("\t",progreso$i,"/",progreso$n," ", Nodo,":",fileBIN)
  if(!is.na(desde) & !is.na(hasta)) cat(" [",as.character(desde),"=>",as.character(hasta),"]")

  ###Leyendo datos desde GGIR  
  lecturaGGIR=obtenerDF(Nodo,fileBIN,dirMeta,desde=desde,hasta=hasta)
  
  resultado=parallel::mclapply(basicVariables,function(x)x(lecturaGGIR[["FASE1"]])[["intervals"]],mc.cores=nucleos)
  names(resultado)=names(basicVariables)
  resultado
}
