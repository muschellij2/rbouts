#' Title
#'
#' @param dfDiario 
#' @param aggregateCalendar2BIN 
#' @param partition 
#'
#' @return
#' @export
aggregateByfileBIN=function(dfDiario,aggregateCalendar2BIN,partition=NULL){
  if (!is.null(partition)) dfDiario=dfDiario %>% left_join(partition)
  resultado=names(aggregateCalendar2BIN) %>% map_df(function(sufijo){
    resultadoTMP=
      dfDiario %>% filter(str_detect(variable,aggregateCalendar2BIN[[sufijo]]$patronVariable),
                          str_detect(calendar,aggregateCalendar2BIN[[sufijo]]$patronCalendar)) 
    if(is.null(partition)){
      resultadoTMP=resultadoTMP     %>%
        nest(-fileBIN,-variable,-calendar)}
    else { resultadoTMP=resultadoTMP     %>%
      nest(-fileBIN,-variable,-calendar,-partition)}
    
    resultadoTMP  %>% 
      mutate(summary=sufijo,
             value=map_dbl(data,aggregateCalendar2BIN[[sufijo]]$funcion)) %>%
      select(-data)
  })
  resultado
}
