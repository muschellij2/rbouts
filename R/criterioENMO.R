#' Indicates which rows meets certain limits for ENMO
#'
#' Generates a vector that indicates for each row of a dataframe (usually epoch or BIN file) if that row verifies the condition
#'    limInf<=df[["ENMO"]] <=limSup
#'
#'
#' @param df data frame with columns ENMO and eventually .criterioNW (that represents NonWear time as TRUE/FALSE)
#' @param limInf inferior limit for ENMO
#' @param limSup superior limit for ENMO
#'
#' @return a boolean vector (TRUE/FAlSE) indicating if the condition is met.
#'
#' @export
criterioENMO=function(df,limInf=0,limSup=Inf,useNW=TRUE){
  if(!useNW | ! (df %>%assertthat::has_name(".criterioNW"))) df$.criterioNW=FALSE

  df %>% mutate(.criterio= ENMO>=limInf & ENMO<=limSup & (!.criterioNW)) %>%.[[".criterio"]]
}
