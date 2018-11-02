#' Convert a dataframe of intervals, given a vector of timestamps representing epochs to
#' a boolean vector representing if that epoch belong to one of the intervals
#'
#' @param ts vector of timestamps
#' @param intervalos dataframe of intervals, with columns \code{to} and \code {from}
#' @param durEpoch amount of time that represents each row of the dataframe (duration of a epoch usually)

#' @return a logical vector of length(ts) indicating if that time velong to a interval
#'
#' @export


interval2criterio=function(ts,intervalos,durEpoch=dseconds(5)){
  #ts=df$timestamp
  #intervalos=intervalosNW
  criterio=vector("logical",length(ts))

  if(is.null(intervalos)  | nrow(intervalos)==0) return(criterio)

  for(i in 1:nrow(intervalos)){
    iDesde = difftime(intervalos[["from"]][i],ts[1])/durEpoch
    iHasta = difftime(intervalos[["to"]][i],ts[1])/durEpoch
    criterio[1+iDesde:iHasta]=1
  }

  criterio
}
