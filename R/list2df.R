#' Convert a recursive list of dataframes to one dataframe.
#'
#' @param lista 
#' @param label 
#'
#' @return
#' @export
list2df=function(lista,label) {
  if("data.frame" %in% class(lista)) return(lista)
  if("list" %in% class(lista) & "data.frame" %in% class(lista[[1]])) {
    bind_rows(lista,.id = label[1])
  }
  else{
    map(lista, ~ list2df(.x,label[-1])) %>% list2df(label[1])
  }
}


