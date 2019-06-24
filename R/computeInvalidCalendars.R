#' Calcula un dataframe con los calendarios que no son válidos. Depende de una lista de bouts de entrada y de calendarios. 
#' Si alguno hace el papel de contener la información de cual es la cama del día, hay que indicar su nombre
#'
#' @param calendars 
#' @param bouts 
#' @param labelbed 
#'
#' @return
#' @export
computeInvalidCalendars=function(calendars,bouts,labelbed="bed"){
  
  ##paso 1)
  ## Eliminar las camas del primer dia. También los calendarios donde se den condiciones de invalidez, por NonWear, 
  ## o por duración fuera de los límites.
  ## Esto crea dfInvalidoDirecto
  
  bedOfFirstDay=calendars[[labelbed]] %>% mutate(label=labelbed,exclusion="bedOfFirstDay") %>%
    filter(row_number()==1) %>%
    filter(day<= as.Date(bouts$isOn$from[1])) %>% select(day,label,exclusion)
  
  
  
  invalidNonWear <- intersect(names(validityParameters$maxNonWear), names(calendars)) %>% 
    map_df( ~ calendars[[.x]]  %>% 
              intervalIntersectv2(bouts[["nonWear"]]) %>% 
              mutate(duration=difftime(toNew,fromNew)) %>%
              group_by(day) %>% summarise(duration=sum(as.integer(duration))) %>%
              mutate(valid = duration < as.integer(validityParameters$maxNonWear[[.x]])) %>%
              mutate(label=.x,exclusion="nonWear") %>%
              filter(!valid) %>% select(day,label,exclusion) 
    )
  
  
  
  
  invalidDuration <- intersect(names(validityParameters$validDuration), names(calendars)) %>% 
    map_df( ~ calendars[[.x]] %>% mutate(duration=as.integer(difftime(to,from,units="secs"))) %>%
              mutate(valid = duration <= as.integer(validityParameters$validDuration[[.x]][["max"]]) &
                       duration >= as.integer(validityParameters$validDuration[[.x]][["min"]])) %>%
              mutate(label=.x,exclusion="duration") %>%
              filter(!valid) %>% select(day,label,exclusion)
    )
  
  
  explicitInvalids=invalidNonWear %>% rbind(invalidDuration) %>% rbind(bedOfFirstDay)
  
  cascadeInvalids=explicitInvalids %>% select(day,label) %>% group_by(day,label) %>% tally() %>% select(-n) %>% ungroup() 
  
  
  ## paso 2) Eliminar en cascada para cada dia, los valores de los calendarios que se anulan a partir
  ## de los InvalidosDirecto. Esto puede ocasionar invalidaciones en casacada, por tanto el proceso
  ## es iterativo hasta que se encuentra un punto fijo
  
  invalidMapping=names(validityParameters$cascadeInvalid) %>%
    map_df( ~ data.frame(label=as.character(.x),
                         affects=as.character(validityParameters$cascadeInvalid[[.x]]),stringsAsFactors = FALSE))
  
  
  
  iterate =cascadeInvalids
  
  repeat{
    
    BEGIN=iterate
    invalidCascade=iterate %>% left_join(invalidMapping,by="label") %>% 
      filter(!is.na(affects)) %>% 
      transmute(day=day,label=affects)
    
    iterate=iterate %>% rbind(invalidCascade) %>%
      group_by(day,label) %>% tally() %>% ungroup()%>% select(-n)
    
    if(isTRUE(all_equal(BEGIN,iterate))) break
  }
  
  
  invalidCascade = iterate %>% 
    left_join(cascadeInvalids %>% mutate(.previous=TRUE), by = c("day", "label")) %>%
    mutate(exclusion="cascade") %>%
    filter(is.na(.previous)) %>% 
    select(-.previous) 
  
  
  
  invalidByParameters=explicitInvalids %>% rbind(invalidCascade) 
  

  validDaily=invalidByParameters %>% filter(label=="daily") %>% group_by(day,label) %>% tally() %>% ungroup %>% select(-n,-label)
  
  tooMuchMVPA_B10=bouts$MVPA_B10 %>% intervalIntersectv2(
    calendars$daily %>% left_join(validDaily,by="day") %>% filter(is.na(label)) %>% select(-label)
  ) %>%
    mutate(duration=difftime(toNew,fromNew,units="secs")) %>% group_by (day) %>% summarise(duration=sum(duration)) %>%
    filter(duration>=validityParameters$maxMVPA_B10) %>% nrow()>0
  
  calendarsThatCanBeErased=names(calendars) %>% setdiff("total") 
  
  excludedByTooMuchMVPA_B10=calendarsThatCanBeErased %>% map_df( 
    ~ calendars[[.x]]  %>%
      transmute(day=day,label=.x,exclusion="tooMuchMVPA_B10")%>% filter(tooMuchMVPA_B10) )
  
  

  daysWithTooMuchMVPA_B1=bouts$MVPA_B1 %>% intervalIntersectv2(calendars$daily %>% left_join(validDaily,by="day")) %>% 
    mutate(duration=difftime(toNew,fromNew,units="secs")) %>% group_by (day) %>% summarise(duration=sum(duration)) %>%
    filter(duration>validityParameters$maxMVPA_B1) 
  
  
  excludedByTooMuchMVPA_B1=calendarsThatCanBeErased %>% map_df( 
    ~ calendars[[.x]] %>% select(day) %>% left_join(daysWithTooMuchMVPA_B1,by="day") %>% filter(!is.na(duration)) %>%
      transmute(day=day,label=.x,exclusion="tooMuchMVPA_B1") )
  
  
  
  excludedCalendars=invalidByParameters %>% rbind(excludedByTooMuchMVPA_B1)%>%
    rbind(excludedByTooMuchMVPA_B10) %>% 
    left_join(calendars %>% list2df(), by = c("day", "label")) %>%
    filter(!is.na(from)) %>% select(day,label,exclusion)
  
  excludedCalendars%>% group_by(day,label) %>% summarise(exclusion=paste(exclusion,sep=",",collapse=",")) %>% ungroup() 
}
