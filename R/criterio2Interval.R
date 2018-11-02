#' Convert a data frame with a column called .criterio to a data frame of intervals of bouts of that criteria
#'
#' data frame of intervals of Bouts meeting certain criteria,
#'
#' @param df data frame with columns .criterio
#' @param pctBout represents fraction of time that the .criterio must be TRUE
#' @param durBoutMin minimum amount of time that the condition muest be met to be aconsidered a Bout
#' @param durEpoch amount of time that represents each row of the dataframe (duration of a epoch usually)
#' @param units Units of time to show certain summaries. Une of c("secs","mins","hours","days")
#' @return a list with data frame of intervals and certain summaries.
#'
#' @export

criterio2Interval=function(df,pctBouts=1,durBoutMin=dseconds(5),durEpoch=dseconds(5),units="mins"){
  #df needs to have two columns "timestamp" & .criterioBout
  #Ejemplo: df %>% mutate(.criterio=criterioENMO(.,limInf = 10/100)) %>% criterio2Interval() %>% .$intervals
  windowSize=durBoutMin/durEpoch


  resultado=list(intervals=NULL,numIntervals=NA,totalTime=NA,propTime=NA,dailyTime=NA,numberOfDays=NA)

  if(is.null(df) | nrow(df)==0){
    warning("No hay datos para buscar bouts.")
    return(resultado)
  }

  resultado$numberOfDays=  as.numeric(difftime(df[["timestamp"]][nrow(df)],df[["timestamp"]][1],units="days"))

  df$.criterio=df$.criterioBout
  df$.criterio[df$timestamp <= df$timestamp[1]+durBoutMin | df$timestamp >= df$timestamp[nrow(df)]-durBoutMin]=FALSE


  df=df %>%
    mutate(
      from_pct = caTools::runmean(.criterio, k=windowSize, alg="C", align = "left"),
      from_stat = from_pct >= pctBouts,
      from_tramo=caTools::runmax(from_stat & .criterio,k=windowSize,alg="C",align="right"),
      from = .criterio & from_stat & !shiftRight(from_tramo ))

  result_from =df %>% filter(from) %>% select(timestamp) %>% .[[1]]

  # Y aquí el final de los bouts:
  df=df %>%
    mutate(
      to_pct=caTools::runmean(.criterio, k=windowSize, alg="C", align = "right"),
      to_stat = to_pct >= pctBouts ,
      to_tramo=caTools::runmax(to_stat& .criterio,k=windowSize, alg="C", align="left"),
      to = .criterio & to_stat & !shiftLeft(to_tramo))

  result_to= df %>% filter(to) %>% select(timestamp) %>% .[[1]]




  #if(length(result_from)==0){
    #message("No hay tramos que mostrar")
    #  return(NULL)
  #}

  if(length(result_to)!=length(result_from)){
    stop("No miden lo mismo 'from' y 'to'")
    #  return(NULL)
  }

data.frame(from=result_from,to=result_to) %>%as_tibble()
}
