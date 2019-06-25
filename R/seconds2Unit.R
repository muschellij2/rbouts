#' Title
#'
#' @param variable 
#'
#' @return
#' @export
seconds2Unit=function(variable){
  result=unitPerVariable[variable] %>% as.numeric()
  if(is.na(result)) result=1
  result
}