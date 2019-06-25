#' Title
#'
#' @param variable 
#'
#' @return
#' @export
seconds2Unit=function(variable){
  if(length(variable)==1){
  result=unitPerVariable[variable] %>% as.numeric()
  if(is.na(result)) result=1
  } else {
    result=map_dbl(variable, ~ seconds2Unit(.x)) %>% as.numeric()
  }
  result
}
