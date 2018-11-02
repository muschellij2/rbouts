simplifyIntervals=function(entrada){
  entrada  %>% mutate(.anterior=lag(to,1)) %>% 
    mutate(.solapa=.anterior>from) %>%
    filter(is.na(.solapa) | !.solapa) %>% select(-.solapa,-.anterior)
}
