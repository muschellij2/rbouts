#' convert a dataframe to a list of dataframes
#'
#' @param df 
#' @param label 
#'
#' @return
#' @export
df2list=function(df,label){
  if(length(label)==1) df %>% split(df[label]) %>% map( ~ .x %>% select(-c(label))) 
  else  df2list(df,label[1]) %>% map(~ df2list(.x,label[-1]))
}
