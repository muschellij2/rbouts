#' Indicates which rows can not meet the criteria for be considered part of a MVPA or higher, because there is not enough angle z mouvement (bic accelerations without change in angle,
#'  means probably the subject is in a car)
#'
#' Generates a vector that indicates for each row of a dataframe (usually epoch or BIN file) if that row verifies the condition to be considered a SIB,
#' #'
#'
#' @param df data frame with columns ANGLEZ, ENMO
#' @param critAnglez represents minimum of deviation (in both directios) of angle Z that must happen in a MVPA bout
#' @param durBoutMin minimum amount of time that the conditions must be met.
#' @param durEpoch amount of time that represents each row of the dataframe (duration of a epoch usually)
#' @return a boolean vector (TRUE/FAlSE) indicating if the condition of belonging to a SIB is met.
#'
#' @export
criterioNoMVPA=function(df,critAnglez=5, durBoutMin=dseconds(120),durEpoch=dseconds(5)){
  windowSize=durBoutMin/durEpoch
  df %>%
    mutate(
#      .criterio =  ((caTools::runmax(anglez,k=windowSize, alg="C", align="left")-caTools::runmin(anglez,k=windowSize, alg="C", align="left"))<critAnglez)) %>%
    .criterio =  caTools::runsd(anglez,k=windowSize, center=anglez, align="center")<critAnglez) %>%
    .[[".criterio"]]

}
