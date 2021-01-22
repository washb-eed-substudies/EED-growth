rm(list=ls())

source(here::here("0-config.R"))
devtools::install_github("washb-eed-substudies/washbgam", force=T)

d <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Caitlin/bangladesh-dm-ee-ee-growth-stool-urine-lab-covariates-anthro.csv"))

wealth <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Caitlin/real_ids_hhwealth_quart.csv"))
d <- left_join(d, wealth, by = c("dataid", "clusterid", "block"))

#Set list of adjustment variables
#Make vectors of adjustment variable names
Wvars<-c("sex","birthord", "momage","momheight","momedu", 
         "hfiacat", "Nlt18","Ncomp", "watmin", "walls", "floor", "roof", "HHwealth",
         "tr", "life_viol_any_t3")

Wvars[!(Wvars %in% colnames(d))]

#Add in time varying covariates:
covariates <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Caitlin/EED-Growth Covariates - Bangladesh.csv"))
covariates <- covariates[c(1:40),c(1,7:18)]
covariates <- unique(covariates)
covariates <- covariates %>% na_if("")

cov.list <- vector('list', nrow(covariates))
for(i in 1:length(cov.list)){
  vec <- as.character(covariates[i,2:13])
  vec <- append(vec, Wvars)
  vec <- vec[!is.na(vec)]
  cov.list[[i]] <- vec
}
names(cov.list) <- paste0('adjset', seq_along(cov.list))

urine.t1 <- c("ln_L_conc_t1", "ln_M_conc_t1")
stool.t1 <- c("ln_mpo1", "ln_aat1", "ln_neo1")
urine.t2 <- c("ln_L_conc_t2", "ln_M_conc_t2")
stool.t2 <- c("ln_mpo2", "ln_aat2", "ln_neo2", "ln_reg1b2")
urine.t3 <- c("ln_L_conc_t3", "ln_M_conc_t3")
stool.t3 <- c("ln_mpo3", "ln_aat3", "ln_neo3")

Y.vars <- c("laz_t1", "waz_t1", "hcz_t1", "whz_t1")

H1_adj_models <- NULL

for(i in urine.t1){
  for(j in Y.vars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset1"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

for(i in stool.t1){
  for(j in Y.vars){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset1"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}


H1_adj_res <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1_adj_models$fit[i][[1]], d=H1_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1_adj_res <-  bind_rows(H1_adj_res, preds$res)
}


