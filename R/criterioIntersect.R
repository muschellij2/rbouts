criterioIntersect=function(df,interval1,interval2){
interval1 %>% intervalIntersect(interval2) %>% 
    transmute(from=fromNew,to=toNew) %>%
    interval2criterio(ts=df[["timestamp"]])
}
