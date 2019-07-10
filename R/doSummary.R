#' Title
#'
#' @param intervalReference 
#' @param intervalBout 
#' @param by 
#'
#' @return
#' @export
#'
doSummary=function(intervalReference,intervalBout,by="day"){
  if(is.null(intervalReference)|is.null(intervalBout)) return(NULL)

  #intervalReference=df %>% intervals4Summary_daily(first=FALSE,offsetLabels=dhours(0),starts=dhours(0),duration=dhours(24))

#  message(intervalBout %>% dim() %>% paste0(collapse = ":"))
  intervalBout=intervalReference %>% intervalIntersectv2(intervalBout) %>%
    mutate(from=fromNew,to=toNew) %>%
    select(from,to,everything(),-fromNew,-toNew)
#  message("\tOK:",intervalBout %>% dim() %>% paste0(collapse = ":"))

  tableRef=intervalReference  %>% mutate(duration=difftime(to,from,units="secs"))       %>%
    group_by_(by)  %>%
    summarise(durationRef=as.integer(sum(duration,na.rm=T)))

  if(nrow(intervalReference)>0) timezone=tz(intervalReference$from[1]) else timezone="Europe/Madrid"

  tableBout=intervalBout       %>%
            mutate(
              duration=difftime(to,from,units="secs"),
              fecha_datetime= ymd_h(str_c(as.character(day)," 00"),tz=timezone),
              hfrom=as.numeric(difftime(from,fecha_datetime,units="hours")),
              hto=as.numeric(difftime(to,fecha_datetime,units="hours"))
            ) %>%
    group_by_(by) %>%
    summarise(maxdur=max(duration),
              num=n(),
              duration=as.integer(sum(duration,na.rm=T)),
              hfrom=first(hfrom),
              hto=last(hto)
              )


tableRef %>% left_join(tableBout,by="day") %>%
    ungroup() %>%
    mutate(weight=durationRef/sum(durationRef),
           duration=ifelse(is.na(duration),0,duration),
           num=ifelse(is.na(num),0,num),
           maxdur=ifelse(is.na(maxdur),0,maxdur))

}


