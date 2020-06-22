#' computes min and max gene values for primary
#'
#' @param x the gene data
#'
#' @return a table of mins, maxes, min_maxes etc by gene (and state)
#' @export

primaryDataDistribution=function(x){

  require(tidyverse)

  a1 = x %>%
    filter(cell_type=="primary" & gene != "") %>%
    group_by(gene,state) %>%
    do(.,{
      data.frame(min=min(.$value),
                 max=max(.$value),
                 n=length(.$value))
    })

  a2_0 = a1 %>% filter(state=="p0") %>%
    ungroup() %>%
    plyr::rename(c("min"="min0","max"="max0","n"="n0")) %>%
    select(-state)

  a2_6 = a1 %>% filter(state=="p6") %>%
    ungroup() %>%
    plyr::rename(c("min"="min6","max"="max6","n"="n6")) %>%
    select(-state)

  a3 = merge(a2_0,a2_6,by="gene") %>%
    mutate(max_min=pmax(min0,min6),
           min_max=pmin(max0,max6))
}

