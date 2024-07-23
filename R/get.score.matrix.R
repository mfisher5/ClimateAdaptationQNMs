get.score.matrix <- function(x,pattern="---"){
  x <- x %>% mutate(model2=ifelse(grepl(pattern,model),"m1","m0")) %>%
    dplyr::select(variable,model2,response.num) %>% 
    pivot_wider(names_from=model2,values_from=response.num) %>%
    mutate(score=abs(m0-m1)) %>%
    mutate(score=ifelse((m0<0)| (m0==0 & m1<0), score*-1,score))
}