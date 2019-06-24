#' Create a summery for the column .criterioRaw and .criterioBout of a data frame representing accelerometer data
#'
#' Generates a summary for a epoch or BIN file
#'
#'
#' @param df data frame with columns timestamp, .criterioRaw & .criterioBout
#' @param offset Will add a quantity to the timestamp to assign the instant of time to the appropiate day
#' @param Variable minimum amount of time that the condition muest be met to be aconsidered a Bout
#' @param durEpoch amount of time that represents each row of the dataframe (duration of a epoch usually)
#' @param minimoHorasValidas explicar
#' @param maximoHorasNonWear explicar
#' @return a list with a summary. This is the info that we use to define activity variables on a daily basis and on a global value.
#'
#' @export
getSummary=function(df,offset=dhours(0),minimoHorasValidas=20,maximoHorasNonWear=2){
  firstDay=lubridate::as_date(df[["timestamp"]][1])
  df= df %>%
    mutate(
      day=lubridate::as_date(timestamp+offset),
      diasPasados=as.integer(difftime(day,firstDay,units="days"))
    )


  dailyTable=df %>% group_by(diasPasados,day) %>%
    summarise(Suma=sum(.criterioRaw & .criterioBout)*dseconds(5)/dminutes(1),
              Duracion=as.character(dminutes(Suma)),
              HorasNonWear=sum(.criterioNW)*dseconds(5)/dhours(1),
              HorasWear=n()*dseconds(5)/dhours(1)-HorasNonWear,
              Valido=HorasWear>=minimoHorasValidas & HorasNonWear < maximoHorasNonWear
    ) %>% ungroup() %>%
    mutate(dayHuman=strftime(day,format = "%a %d-%m-%Y"))

  validDays=dailyTable  %>% filter(Valido)
  totalValidHours=validDays  %>%  .[["HorasWear"]] %>% sum(na.rm=T)
  average=validDays %>% .[["Suma"]] %>% mean(na.rm=T)
  weightedaverage=validDays %>% mutate(Contribution=Suma*HorasWear) %>% .[["Contribution"]] %>% sum(na.rm=T)/totalValidHours
  list(dailyTable=dailyTable, average=as.numeric(average),weightedaverage=as.numeric(weightedaverage),totalValidHours=totalValidHours,intervals=df %>% criterio2Interval())
}
