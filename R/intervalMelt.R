#'Melt intervals of a first dataframe using a second dataframe of intervals
#'
#'
#' @param interval1 first dataframe
#' @param interval2 second dataframe
#'
#' @return A dataframe of intervals representing the resulting connected intervals
#'
#' @export
intervalMelt=function(interval1,interval2){

  rbind(interval1 %>% select(from,to),interval2%>% select(from,to)) %>%
    arrange(to,from) %>% intervalSimplify()
}
