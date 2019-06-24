obtenerDF=function(Nodo,BIN,dirMeta,desde=NA,hasta=NA){
  ficFase1=str_c(dirMeta,"basic/meta_",BIN,".RData")
  ficFase5=str_c(dirMeta,   "ms5.out/",BIN,".RData")  
  
  df=NULL
  dfGGIR=data.frame(from=now(),to=now(),day=today(),label="bedGGIRtime") %>% filter(FALSE) %>% as_tibble()
  
  try({
    
    load(ficFase1)
    
    df=M$metashort %>%
      mutate(
        timestamp = with_tz(ymd_hms(timestamp),tz = "Europe/Madrid")
      ) %>%  select (timestamp,ENMO,anglez) %>% as_tibble()
    
    
    ##NonWear
    dfNW=M$metalong  %>%
      mutate(
        timestamp = with_tz(ymd_hms(timestamp),tz = "Europe/Madrid"),
        .criterioRaw=as.integer(nonwearscore!=0),#Ponemos el primer instante como nonWear
        .criterioBout=.criterioRaw) %>%
      select(timestamp,.criterioRaw,.criterioBout) %>% as_tibble()
    
    intervalosNW=dfNW %>% criterio2Interval() %>% #Eliminando nonWear cortos de noche
      filter(! (  (difftime(to,from)<dminutes(120) & ( hour(from)>22 | hour(to)<=8)) | difftime(to,from)<dminutes(40)))
    
    df=df %>% mutate(.criterioNW= interval2criterio(df$timestamp,intervalosNW))
  })
  
  
  
  
  ##Si es posible, añadimo el criterio de Cama GGIR 
  
  if(!is.null(df)){    
    df$.criteriocamaGGIR=NA_integer_
    if(!file.exists(ficFase5)){
      message("No hay fase 5 calculada para ", ficFase5)}
    else {
      try({
        load(ficFase5)
        dfGGIR= output %>% transmute(from=fechaHora2datetime(calendardate,acc_onset),
                                     to=fechaHora2datetime(calendardate,acc_wake),
                                     day=as.Date(ymd(calendardate)),label="GGIR") %>%
          as_tibble() %>% filter(complete.cases(.)) 
        df=df %>% mutate(.criteriocamaGGIR=as.logical(interval2criterio(df$timestamp,dfGGIR)))
      })
    }
  } 
  
  
  dfNuevo=df
  # Si al hacer la restricción de fechas no dejamos al bin sin datos, hacerla. Si no quedarnos al menos un dia
  # que haga de testigo
  if(!is.na(desde) & !is.na(hasta) & difftime(hasta,desde)>=dhours(24)) {
    df=df %>% filter(timestamp >= desde & timestamp <=hasta)
  }
  
  
  
  inicio=df$timestamp[1]
  fin=last(df$timestamp)
  
  list(FASE1=df,FASE5=dfGGIR,inicio=inicio,fin=fin)
}
