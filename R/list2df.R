#' Convert a recursive list of dataframes to one dataframe.
#'
#' @param lista 
#' @param label 
#'
#' @return
#' @export
list2df=function(lista,label="") {
#  print(lista)
#  dfVacio=data.frame(void=NA) %>% filter(FALSE) %>% select(-1)
  if(is.null(lista)) return(NULL)
  if("data.frame" %in% class(lista)) return(lista)
  if("list" %in% class(lista)){
    if(length(lista)==0) return(NULL)
    if(length(lista)==1 & is.null(lista[[1]])) return(NULL)
    if("data.frame" %in% class(lista[[1]])) {
      if(label=="") bind_rows(lista) else bind_rows(lista,.id = label[1])
  } else {
    map(lista, ~ list2df(.x,label[-1])) %>% list2df(label[1])
  }
  } else {
  error("No es una lista? ni un data. frame?")
  }
}


