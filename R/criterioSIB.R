#' Indicates which rows meets the criteria for be considered part of a SIB (Sustained Inactivity Bouts)
#'
#' Generates a vector that indicates for each row of a dataframe (usually epoch or BIN file) if that row verifies the condition to be considered a SIB,
#' i.e Anglez variying a few degrees for a certain amount of time (durBoutMin, and ENMO lower than a low value)
#'
#'
#' @param df data frame with columns ANGLEZ, ENMO and eventually .criterioNW (that represents NonWear time as TRUE/FALSE)
#' @param critAnglez represents maximum of deviation (in both directios) of angle Z that it is allowed during a SIB
#' @param limSup superior limit for ENMO
#' @param durBoutMin minimum amount of time that the conditions must be met to beconsidered a SIB period
#' @param durEpoch amount of time that represents each row of the dataframe (duration of a epoch usually)
#' @return a boolean vector (TRUE/FAlSE) indicating if the condition of belonging to a SIB is met.
#'
#' @export
criterioSIB=function(df,critAnglez=5, limSup=30/1000, durBoutMin=dminutes(5),durEpoch=dseconds(5)){
  windowSize=durBoutMin/durEpoch
  df %>%
    mutate(
      tmp_nearConstantFuture = ((caTools::runmax(anglez,k=windowSize, alg="C", align="left")-caTools::runmin(anglez,k=windowSize, alg="C", align="left"))<critAnglez) & caTools::runmin((!.criterioNW) & ENMO<= limSup,k=windowSize, alg="C", align="left"),
      .criterio= caTools::runmax(tmp_nearConstantFuture,k=windowSize, alg="C", align="right")
    ) %>% .[[".criterio"]]

}
