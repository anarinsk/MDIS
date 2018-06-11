### Initialize ----

source("./code_header.R")
readRDS("./data/tbl.rds") -> tbl 

### Filter and GDP deflator ----

deflate_income <- function(){

  factor <- 111.8 / 110.9
  this_vars <- paste0("V", 100:133) 

  tbl %.>% 
    dplyr::filter(., V2 == 14) %.>% 
    dplyr::filter(., V1 == 2017) -> ttl1

  tbl %.>% 
    dplyr::filter(.,
      V2 == 14) %.>% 
    dplyr::filter(., V1 == 2018) %.>% 
    dplyr::mutate_at(., this_vars, funs(.*factor)) -> ttl2 

  ttl1 %.>% 
    bind_rows(., ttl2) %.>% 
    select(., -V2) %.>% 
    arrange(., V1) }
deflate_income() -> tbl 
saveRDS(tbl, "./data/tbl_gdpd.rds")

### Calculate percent block ----

assign_percentile1 <- function(var, df, bin = 100){
  #  
  var_exp <- enquo(var)
  df %.>% 
    dplyr::mutate(., target = !!var_exp) %.>% 
    group_by(., V1) %.>% 
    mutate(.,
           tTarget = ntile(-target, bin), 
           tTarget2 = percent_rank(-target),
           tType = 1) %.>% 
    select(., V1, V3, V99, target, tTarget, tTarget2, tType)
}
assign_percentile2 <- function(var, df, bin = 100){
  #  
  var_exp <- enquo(var)
  df %.>% 
    dplyr::mutate(., target = !!var_exp) %.>% 
    select(., V1, V3, V99, target) -> ttl1 
  
  list(x1=ttl1$V1, x2=ttl1$V3, y=ttl1$target, z=ttl1$V99) %.>% 
    pmap_df(., function(x1,x2,y,z){
      tibble(V1 = rep(x1,z), V3 = rep(x2,z), target = rep(y,z))}) -> ttl2
  
  ttl2 %.>% 
    group_by(., V1) %.>% 
    mutate(., 
           tTarget = ntile(-target, bin),
           tTarget2 = percent_rank(-target),
           V99 = 1, 
           tType = 2) %.>% 
    distinct(., V1, V3, tTarget, .keep_all=T) %.>% 
    select(., V1, V3, V99, target, tTarget2, tTarget, tType) 
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
assign_PCT <- function(var, df, bin=100){
  var_exp <- enquo(var)
  df %.>% assign_percentile1(!!var_exp, ., bin = bin) -> ttl1 
  df %.>% assign_percentile2(!!var_exp, ., bin = bin) -> ttl2
  #df %.>% assign_percentile3(!!var_exp, .) -> ttl3
  bind_rows(ttl1, ttl2) 
}
cal_dff <- function(var1, df){
#  
  var1_exp <- enquo(var1)
  
  df %.>% 
    group_by(., tType, tTarget) %.>% 
    summarise(., 
              y17 = !!var1_exp %.>% .[[1]], 
              y18 = !!var1_exp %.>% .[[2]], 
              dff = y18 - y17, 
              n_y17 = n %.>% .[[1]], 
              n_y18 = n %.>% .[[2]]
    )
#
}
draw_hist <- function(df, w_tType){
  
  df %.>% dplyr::filter(., tType %in% w_tType) -> ttl1
  
  ggplot(ttl1) +
    aes(x=tTarget, y=dff) %.>% 
    geom_point(., color = "red", alpha = 0.5) + 
    geom_col(., color = "grey") + 
    labs(x="Percentile block", y="Difference in Income (2018-2017)") + 
    ggtitle("My Graph") + theme_hc() -> g1 
  
  if(length(w_tType)>1) {g1 + facet_wrap(~tType, nrow = 2, scales = "free_y")} else {g1}
}

### Draw results ----

tbl %.>% 
  dplyr::filter(.,
    V4 %in% c(1)
    #V5 %in% 1
  ) %.>% 
  assign_PCT(V100, ., 100) %.>% 
  gen_tincome(.) %.>% 
  cal_dff(tincome_wtd, .) -> vdf

vdf %.>% draw_hist(., c(1,2))

### Monte Carlo-like? ----

vars_basic <- c("V100", "V101", "V102", "V103")

pick_x <- function(year, x, df, what_V4 = c(1,2)){
  df %.>% 
    dplyr::filter(., V1 == year, V4 %in% what_V4) %.>% 
    sample_n(., size = x, replace = T, weight = V99)
}
sim_ndraw <- function(n_draw, what_V4, vars_sel = vars_basic, df=tbl){
#  
  list(year = c(rep(2017,n_draw), rep(2018,n_draw)),  round=c(seq(1:n_draw), seq(1:n_draw))) -> itr_sch
  
  itr_sch %.>% 
    pmap_df(., 
      function(year, round) {
        pick_x(year, 100, df, what_V4) %.>% 
        mutate(., rd = unlist(round)[1]) 
      } 
    ) %.>% 
    dplyr::select(., V1, V3, rd, one_of(vars_sel))
#
}
sim_summary <- function(var, funs, df){
  
  var_exp <- enquo(var)
  
  df %.>% 
    arrange(., V1, rd, -!!var_exp) %.>% 
    group_by(., V1, rd) %.>% 
    mutate(., rank_sample = seq(1:100)) %.>% 
    group_by(., V1, rank_sample) %.>% 
    summarise(., val = funs(!!var_exp)) %.>% 
    group_by(., rank_sample) %.>%  
    mutate(., V1 = factor(V1)) %.>% 
    spread(., key = V1, value = val) %.>% 
    rename(., 
      Y17 = `2017`,
      Y18 = `2018`
      )
}

sim_ndraw(10000, c(1,2), c(vars_basic, "V106")) -> ttl1
sim_ndraw(10000, c(1), c(vars_basic, "V106"))   -> ttl2

ttl1 %.>% 
  sim_summary(V101, mean, .) %.>% 
  mutate(., 
    group = "all" 
  )-> ttl1_1

ttl2 %.>% 
  sim_summary(V101, mean, .)  %.>% 
  mutate(., 
    group = "worker" 
  )-> ttl2_1

ttl1_1 %.>% bind_rows(., ttl2_1) -> tbl_smr 

tbl_smr %.>% ggplot(.) + 
  aes(x = rank_sample, y = Y18-Y17) + 
  geom_col(color = "black") + 
  facet_grid(.~group, scales = "free_y") + 
  ggtitle("Sampling Income Mean") + 
  xlab("rank(descending order)") + 
  ylab("Quarterly income") +
  theme_economist()
  
tbl_smr %.>% ggplot(.) + 
  aes(x = rank_sample, y = (Y18-Y17)/Y17) + 
  geom_col(color = "black") + 
  facet_wrap(~group, ncol=2) + 
  ggtitle("Sampling Income Change") + 
  xlab("rank(descending order)") + 
  ylab("Change(%)") +
  theme_economist() 

tbl_smr %.>% dplyr::filter(., Y18 - Y17 > 0) -> vdf 
