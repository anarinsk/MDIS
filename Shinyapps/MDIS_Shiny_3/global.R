### Initialize ----
### Load packages and prepare tdf 
library(tidyverse)
library(conflicted) 
library(wrapr) 
library(here)
library(readxl)
library(ggthemes)
library(spatstat)
library(shiny)

readRDS("./data/tbl_gdpd.rds") -> tbl 
tbl -> tbl0
id <- 0

### Define funcs ----
assign_percentile1 <- function(var, df, n_tile=100){
  #  
  df %.>% 
    dplyr::mutate(., target = get(var)) %.>% 
    group_by(., V1) %.>% 
    mutate(.,
           tTarget = ntile(-target, n_tile), 
           tType = 1) %.>% 
    select(., V1, V3, V99, target, tTarget, tType)
}
assign_percentile2 <- function(var, df, n_tile=100){
  #  
  df %.>% 
    dplyr::mutate(., target = get(var)) %.>% 
    select(., V1, V3, V99, target) -> ttl1 
  
  list(x1=ttl1$V1, x2=ttl1$V3, y=ttl1$target, z=ttl1$V99) %.>% 
    pmap_df(., function(x1,x2,y,z){
      tibble(V1 = rep(x1,z), V3 = rep(x2,z), target = rep(y,z))}) -> ttl2
  
  ttl2 %.>% 
    group_by(., V1) %.>% 
    mutate(., 
           tTarget = ntile(-target, n_tile), 
           V99 = 1, 
           tType = 2) %.>% 
    distinct(., V1, V3, tTarget, .keep_all=T) %.>% 
    select(., V1, V3, V99, target, tTarget, tType)
}

gen_tincome <- function(df){
  #
  df %.>% 
    group_by(., V1, tType, tTarget) %.>% 
    summarise(., 
              tincome_mn = mean(target),
              tincome_md = median(target), 
              tincome_wtn = weighted.mean(target, V99),
              tincome_wtd = weighted.median(target, V99),
              n = n()
    )
  #  
}
assign_PCT <- function(var, df, n_tile=100){
  df %.>% assign_percentile1(var, ., n_tile) -> ttl1 
  df %.>% assign_percentile2(var, ., n_tile) -> ttl2
#  df %.>% assign_percentile3(var, .) -> ttl3
  bind_rows(ttl1, ttl2) 
}
cal_dff <- function(var1, df){
 
  df %.>% 
    group_by(., tType, tTarget) %.>% 
    summarise(., 
              y17 = get(var1) %.>% .[[1]], 
              y18 = get(var1) %.>% .[[2]], 
              dff = y18 - y17, 
              n_y17 = n %.>% .[[1]], 
              n_y18 = n %.>% .[[2]]
    )
  #
}
draw_hist <- function(df, w_tType, tick_max){
  
  df %.>% dplyr::filter(., tType == w_tType) -> ttl1
  
  ggplot(ttl1) +
    aes(x=factor(tTarget), y=dff) %.>% 
    geom_point(., color = "red", alpha = 0.8) + 
    geom_col(., fill = "grey", alhpha = 0.6) + 
    scale_x_discrete(breaks = seq(0,tick_max,5)) +
    labs(x="Percentile block", y="Difference in Income (2018-2017)") + 
    ggtitle(str_glue("Income difference of Korean Sample Households by rank method {w_tType}")) + theme_bw() -> g1 
  
  if(length(w_tType)>1){ g1 + facet_wrap(~tType, nrow=2, scales="free_y")} else {g1}
#
}