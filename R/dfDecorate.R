#' Take a bout or a list of them and return a list with the same structure, but adding columns
#' with extra information (files and variable measured) in each dataframe containing bout information.
#'
#' @param element 
#' @param label 
#' @param value 
#'
#' @return
#' @export
dfDecorate=function(element,label=c(),value=c()){
  if("data.frame" %in% class(element)) {
    for (i in seq_along(label)) element[[ label[i] ]]=rep(value[[i]],nrow(element))
    element
  }
  else 
    map2(element %>% compact(), names(element), ~ dfDecorate(.x,label,c(value,.y )))
}
