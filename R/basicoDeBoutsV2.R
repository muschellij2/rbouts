# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'


shiftLeft=function(x)c(x[-1],0)
shiftRight=function(x)c(0,x[-length(x)])





timeQuantile_1=function(intervals,probs=0.5){
  intervals %>% filter(probs<=probTo & probs>probFrom) %>%
    mutate(probs=probs) %>%
    mutate(quantile=from+difftime(to,from)*((probs-probFrom)/(probTo-probFrom)))
}


intervalsQuantiles=function(intervals,probs=c(0.5)){
  tmp=intervals %>% as_tibble() %>%
    mutate(duracion=difftime(to,from,units="secs"),
           probTo=cumsum(as.integer(duracion))/sum(as.integer(duracion)),
           probFrom=(function(x)c(-1e-9,x[-length(x)]))(probTo)
    )

  reduce(map(probs,function(probs)timeQuantile_1(tmp,probs)),rbind)
}






resumenHoraDiaSemana=function(df,pctBouts=1,durBout=dminutes(1),durEpoch=dseconds(5)){
  #df Debe terner columna timestamp + criterio
  windowSize=durBout/durEpoch
  resumen=df %>%
    mutate(
      from_pct = caTools::runmean(criterio, k=windowSize, alg="C", align = "left"),
      from_stat = as.integer(from_pct >= pctBouts),
      from_tramo=caTools::runmax(from_stat & criterio,k=windowSize,alg="C",align="right"),
      hora=hour(timestamp),
      diasPasados=as.integer(difftime(timestamp,timestamp[1],units="days"))
    )

  resumenHorario=resumen %>%
    group_by(diasPasados,hora) %>%
    summarise(
      fecha=date(first(timestamp)),
      diaSem=str_sub(wday(fecha,label=T),1,2),
      Fraccion=mean(from_tramo)*100) %>%
    ungroup() %>%
    arrange(fecha,hora) %>%
    mutate(
      Minutos=round(Fraccion/100*60,0),
      fechaFormateada=ordered(strftime(fecha,"%Y-%m-%d %a"))
    )

  levels(resumenHorario$fechaFormateada)=str_replace(levels(resumenHorario$fechaFormateada),
                                                     "([0-9][0-9][0-9][0-9])-([0-9][0-9])-([0-9][0-9]) (.*)","\\4 \\3-\\2-\\1")
  resumenHorario
}


plotHorario=function(dfHorario,low="white",high = "steelblue",label=""){
  totalMinutos=sum(dfHorario$Minutos,na.rm=T)
  if (is.na(totalMinutos) | totalMinutos<3) high=low
  ggplot(dfHorario,aes(x=hora,y=fechaFormateada))+ geom_tile(aes(fill = Minutos),colour = "black") +
    scale_fill_gradient(low = low, high = high)+
    geom_text(aes(label = str_replace(round(Minutos,0),"^0$","")),size=4)+
    scale_x_continuous(breaks=0:23)+
    scale_y_discrete(limits = rev(levels(dfHorario$fechaFormateada)))+
    theme_classic()+ggtitle(label = label)+ ylab("")+
    theme(legend.position="none")
}



grafico2html=function(grafico,width=800,height=640,res=150){
  tf1 <- tempfile(fileext = ".png")
  png(tf1,width=width,height=height,res=res)
  print(grafico)
  dev.off()

  graficotxt <- RCurl::base64Encode(readBin(tf1, "raw", file.info(tf1)[1, "size"]), "txt")
  cat(sprintf('<img src="data:image/png;base64,%s">', graficotxt))
}


maquetaTabla=function(dailyTable,nombreVariable){
  tabla=dailyTable %>% ungroup() %>% filter(Valido) %>%
    select(diasPasadosRef,fechaRef,Suma)
  filaNueva=nrow(tabla)+1
  tabla[filaNueva,1]=99
  tabla[filaNueva,2]="Total"
  tabla[filaNueva,3]=mean(tabla$Suma,na.rm=T)

  tabla[[nombreVariable]]=as.character(dminutes(tabla$Suma))
  tabla %>%select(diasPasadosRef,fechaRef,nombreVariable)
}



