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

### Define funcs ----

gen_tincome <- function(var, df){
#  
  df %.>% 
    dplyr::mutate(., target = get(var)) %.>% 
     group_by(., V1) %.>% 
     mutate(.,
            tTarget = ntile(target, 100)) %.>% 
     select(., V1, tTarget, V99, target) %.>% 
     group_by(., V1, tTarget) %.>% 
     summarise(., 
               tincome_mn = mean(target),
               tincome_md = median(target), 
               tincome_wtn = weighted.mean(target, V99),
               tincome_wtd = weighted.median(target, V99),
               n = n()
     ) %.>% 
     arrange(., tTarget, V1) 
#  
}

cal_dff <- function(var1, df){
  
  df %.>% 
    group_by(., tTarget) %.>% 
    summarise(., 
              y17 = get(var1) %.>% .[[1]], 
              y18 = get(var1) %.>% .[[2]], 
              dff = y18 - y17, 
              n_y17 = n %.>% .[[1]], 
              n_y18 = n %.>% .[[2]]
    )
}
