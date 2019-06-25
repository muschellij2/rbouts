

#' Title
#'
#' @param calendars 
#'
#' @return
#' @export
#'
#' @examples
part_WEWD=function(calendars){
  if("data.frame" %in% class(calendars)) calendars %>% mutate(.part=factor(wday(day,week_start = 1)<=5,levels=c(FALSE,TRUE),labels=c("WE","WD"))) %>%
    split(.[[".part"]]) %>% map( ~ select(.,-.part))
  else calendars %>% map(part_WEWD) %>% transpose()
}


#' Title
#'
#' @param calendars 
#'
#' @return
#' @export
#'
part_weekday=function(calendars){
  if("data.frame" %in% class(calendars)) calendars %>% mutate(.part=sprintf("%d-%s",wday(day,label = FALSE,abbr = FALSE,week_start = 1),wday(day,label = TRUE,abbr = TRUE,week_start = 1))) %>%
    split(.[[".part"]]) %>% map( ~ select(.,-.part))
  else calendars %>% map(part_weekday) %>% transpose()
}



#' Title
#'
#' @param calendars 
#'
#' @return
#' @export
#'
part_date=function(calendars){
  if("data.frame" %in% class(calendars)) calendars %>%
    mutate(.part=sprintf("%s-%s",day,wday(day,label = TRUE,abbr = TRUE,week_start = 1))) %>%
    split(.[[".part"]]) %>% map( ~ select(.,-.part))
  else calendars %>% map(part_weekday) %>% transpose()
}


#' Title
#'
#' @param calendars 
#'
#' @return
#' @export
#'
part_dayNumber=function(calendars,referenceDay=NA){
  if("data.frame" %in% class(calendars)){
    if(is.na(referenceDay)) referenceDay= calendars[["day"]] %>% first()
    calendars %>% mutate(.part=sprintf("d%02d",as.integer(difftime(day,referenceDay,units = "days")))) %>%
      split(.[[".part"]]) %>% map( ~ select(.,-.part)) %>% compact()
  }
  else {
    if(!is.null(calendars) & "data.frame" %in% class(calendars[[1]])){
      referenceDay=calendars %>% map( ~ .[["day"]] %>% first())  %>% reduce(min)
      calendars %>% map( ~ part_dayNumber(.,referenceDay=referenceDay)) %>% transpose() 
    } else{
      calendars %>% map(part_dayNumber) %>% compact()
    }
  }
}


#' Title
#'
#' @param calendars 
#'
#' @return
#' @export
#'
part_SatSun=function(calendars){
  if("data.frame" %in% class(calendars)) calendars %>% mutate(.part=as.integer(wday(day,week_start = 1))) %>%
    filter(.part>=6) %>%
    mutate(.part=as.character(wday(day,label = TRUE,abbr = FALSE,week_start = 1))) %>%
    split(.[[".part"]]) %>% map( ~ select(.,-.part))
  else calendars %>% map(part_SatSun) %>% transpose()
}
