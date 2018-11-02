intervalPlotInteractive=function(listaIntervalos,horario,listaDeVariablesElegidas,paleta){


  lasVariables=listaDeVariablesElegidas %>% unlist(use.names=F)
  variable2Color=map2(listaDeVariablesElegidas,names(listaDeVariablesElegidas), ~ paleta[[.y]][1:length(.x)]) %>% unlist(use.names=F)
  names(variable2Color)=lasVariables
  variable2Altura=1:length(lasVariables); names(variable2Altura)=lasVariables



  variable2Familia=names(listaDeVariablesElegidas %>% unlist()) %>% str_replace("[0-9]$","")
  names(variable2Familia)=listaDeVariablesElegidas %>% unlist()


  Familia2Orden=seq_along(listaDeVariablesElegidas)
  names(Familia2Orden)=names(listaDeVariablesElegidas)

  ordenLocalVariables=listaDeVariablesElegidas %>% map( function(x) {resultado=seq_along(x);names(resultado)=x;resultado})
  names(ordenLocalVariables)=NULL

  variable2Orden=ordenLocalVariables %>% unlist()


  dfInt=listaIntervalos %>% map2(names(listaIntervalos),function(x,y)x %>%
                                   mutate(content=y,
                                          Familia=variable2Familia[content],
                                          variable=variable2Orden[content],
                                          #altura=Familia2Orden[Familia]*12+3*variable2Orden[content],
                                          altura=variable2Altura[content],
                                          medicion=sprintf("%02d-%s",as.integer(altura),content),
                                          data_id=str_c(content,";",from,";",to)
                                   )
  ) %>%
    reduce(rbind) %>%
    filter(!is.na(Familia))


  miPaleta=dfInt %>% group_by(Familia,medicion) %>% summarise(variable=first(variable)) %>% arrange(medicion) %>%
    mutate(elColor=map2_chr(Familia,variable,function(x,y)paleta[[x]][y])) %>% .[["elColor"]]

zona=tz(dfInt$from[1])
primero=as_date(min(dfInt$from))
ultimo=as_date(max(dfInt$to))
dias=primero+(0:(1+ultimo-primero))

desdeGrafico=as.POSIXct(primero)
hastaGrafico=desdeGrafico+dhours(24)


dfIntDias=intervalIntersectv2(dfInt,horario) %>% #left_join(dfInt) %>%
  transmute(from=fromNew,to=toNew,content=content,Familia=Familia,variable=variable,altura=altura,medicion=medicion,day=day,data_id=data_id) %>%
  mutate(
    from_b= from-(day-primero),
    to_b= to-(day-primero),
    dia= str_c(str_sub(as.character(as_date(day)),3,10),"\n",weekdays(day,abbreviate=TRUE))) %>%
  arrange(medicion,from_b)





grafico=ggplot(dfIntDias,aes(x=from_b,y=altura))+
  geom_segment_interactive(aes(xend=to_b,yend=altura,color=medicion,tooltip=data_id,data_id = data_id),size=1)+
  scale_x_datetime(labels=date_format("%H",tz=zona),date_minor_breaks="30 mins",date_breaks="1 hours",position = "top")+
  #,limits=c(desdeGrafico,hastaGrafico)
  theme_stata() +
   scale_y_continuous(breaks=NULL)+
  xlab("Hora")+
  scale_color_manual(values=miPaleta)+
   theme(
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank(),
    axis.text.x=element_text(size=8),
    panel.grid.major.x=element_line(colour = 'lightblue'),
    axis.title.y=element_blank(),
    legend.position="right",
    legend.title = element_blank(),
    strip.text.y = element_text(size = 7,margin=margin()),
    panel.spacing.y=unit(0.05, "lines")
    )


if (sum(!is.na(dfIntDias$dia))>0)  grafico=grafico+facet_wrap(~dia,ncol = 1,strip.position="left")

grafico
}
