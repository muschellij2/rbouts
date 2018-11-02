#' Indicates which rows meets the criteria for be considered part of a Bout meeting certain criteria
#'
#' Generates a vector that indicates for each row of a dataframe (usually epoch or BIN file)
#' if that row meets the criteria to be considered part of a Bout (veryfy a criteria for ca percent of time of a minimum duration),
#'
#'
#' @param df data frame with columns .criterio and eventually .criterioNW (that represents NonWear time as TRUE/FALSE)
#' @param pctBout represents fraction of time that the .criterio must be TRUE
#' @param durBoutMin minimum amount of time that the condition muest be met to be aconsidered a Bout
#' @param durEpoch amount of time that represents each row of the dataframe (duration of a epoch usually)
#' @return a boolean vector (TRUE/FAlSE) indicating if the condition of belonging to a Bout is met.
#'
#' @export
criterioBout=function(df,pctBouts=1,durBoutMin=dseconds(5),durEpoch=dseconds(5)){
  windowSize=durBoutMin/durEpoch
  if(! (df %>%assertthat::has_name(".interrupcion"))) df$.interrupcion=FALSE
  df %>%
    mutate(
      .criterio=.criterioRaw & (!.interrupcion),
      from_pct = caTools::runmean(.criterio, k=windowSize, alg="C", align = "left")*(1-caTools::runmax(.interrupcion, k=windowSize, alg="C", align = "left")),
      from_stat = .criterio & from_pct >= pctBouts,
      from_tramo=caTools::runmax(from_stat ,k=windowSize,alg="C",align="right"), #& .criterio,
      to_pct = caTools::runmean(.criterio, k=windowSize, alg="C", align = "right")*(1-caTools::runmax(.interrupcion, k=windowSize, alg="C", align = "right")),
      to_stat = .criterio & to_pct >= pctBouts,
      to_tramo=caTools::runmax(to_stat ,k=windowSize,alg="C",align="left"), #& .criterio,
      to_from=to_tramo & from_tramo
    ) %>% .[["to_from"]]
}
