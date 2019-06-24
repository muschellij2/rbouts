#'Simplify intervals of a first dataframe using a second dataframe of intervals
#'
#'
#' @param interval first dataframe
#'
#' @return A dataframe of intervals representing the resulting connected intervals
#'
#' @export
intervalSimplify=function(interval){
  if(nrow(interval)<=1) return (interval)
  interval=interval %>% arrange(from,to)
  referencia=interval$from %>% first()
  interval %>% mutate_all(~difftime(.x,referencia,units="secs") %>% as.integer() )%>%
    arrange(from) %>%
    group_by(g = cumsum(cummax(lag(to, default = first(to))) < from)) %>%
    summarise(from = first(from), to = max(to)) %>%
    mutate_all(~referencia + dseconds(.x)) %>% select(from,to)
}

