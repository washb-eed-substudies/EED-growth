rm(list=ls())

source(here::here("1-data cleaning.R"))
#remotes::install_github('washb-eed-substudies/washbgam')
library(washbgam)

#Make vectors of adjustment variable names
covariates <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Caitlin/EED-Growth Covariates - Bangladesh.csv"))

#baseline covariates
Wvars <- covariates[c(43:57),2]
Wvars <- Wvars[-12]

#timevarying covariates
timecov <- covariates[c(1:40),c(2,8:19)]

#obtain unique adjustment sets
timecov <- unique(timecov)

#make blank cells NAs
timecov <- timecov %>% na_if("")

#create adjustment set vectors
cov.list <- vector('list', nrow(timecov))
for(i in 1:length(cov.list)){
  vec <- as.character(timecov[i,2:13]) # create vector using row i in columns with variable names
  vec <- append(vec, Wvars) #add baseline characteristics
  vec <- vec[!is.na(vec)] #remove any NAs (from blank cells)
  vec <- sub("z_t1", "z_t1_cov", vec)
  vec <- sub("z_t2", "z_t2_cov", vec)
  vec <- sub("z_t3", "z_t3_cov", vec)
  cov.list[[i]] <- vec #insert into list item i
}
names(cov.list) <- paste0('adjset', seq_along(cov.list)) #rename 



urine.t1 <- c("ln_L_conc_t1", "ln_M_conc_t1")
stool.t1 <- c("ln_mpo1", "ln_aat1", "ln_neo1")
urine.t2 <- c("ln_L_conc_t2", "ln_M_conc_t2")
stool.t2 <- c("ln_mpo2", "ln_aat2", "ln_neo2", "ln_reg2")
urine.t3 <- c("ln_L_conc_t3", "ln_M_conc_t3")
stool.t3 <- c("ln_mpo3", "ln_aat3", "ln_neo3")

all.growth.t1 <- c("laz_t1", "waz_t1", "hcz_t1", "whz_t1")
laz.waz.t1 <- c("laz_t1", "waz_t1")
hcz.t1 <- "hcz_t1"
whz.t1 <- "whz_t1"

laz.waz.t2 <- c("laz_t2", "waz_t2")
hcz.t2 <- "hcz_t2"
whz.t2 <- "whz_t2"

laz.waz.t3 <- c("laz_t3", "waz_t3")
hcz.t3 <- "hcz_t3"
whz.t3 <- "whz_t3"

velo.t1.t2 <- c("len_velocity_t1_t2", "wei_velocity_t1_t2", "hc_velocity_t1_t2")
velo.t1.t3 <- c("len_velocity_t1_t3", "wei_velocity_t1_t3", "hc_velocity_t1_t3")
velo.t2.t3 <- c("len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3")

###### Analysis
#hypothesis 1
H1_adj_models <- NULL

#analysis 1
for(i in urine.t1){
  for(j in all.growth.t1){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset1"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#analysis 2
for(i in stool.t1){
  for(j in all.growth.t1){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset2"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#analysis 3
for(i in stool.t2){
  for(j in laz.waz.t2){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset3"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#analysis 4
for(i in urine.t2){
  for(j in laz.waz.t2){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset4"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#analysis 5
for(i in stool.t2){
  for(j in hcz.t2){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset5"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#analysis 6
for(i in urine.t2){
  for(j in hcz.t2){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset6"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#analysis 7
for(i in stool.t2){
  for(j in whz.t2){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset7"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#analysis 8
for(i in urine.t2){
  for(j in whz.t2){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset8"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#analysis 9
for(i in stool.t3){
  for(j in laz.waz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset9"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#analysis 10
for(i in urine.t3){
  for(j in laz.waz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset10"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}
#analysis 11
for(i in stool.t3){
  for(j in hcz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset11"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#analysis 12
for(i in urine.t3){
  for(j in hcz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset12"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}
#analysis 13
for(i in stool.t3){
  for(j in whz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset13"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#analysis 14
for(i in urine.t3){
  for(j in whz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset14"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H1_adj_models <- bind_rows(H1_adj_models, res)
  }
}

#bind all together
H1_adj_res <- NULL
for(i in 1:nrow(H1_adj_models)){
  res <- data.frame(X=H1_adj_models$X[i], Y=H1_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H1_adj_models$fit[i][[1]], d=H1_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H1_adj_res <-  bind_rows(H1_adj_res, preds$res)
}

#####Hypothesis 2
H2_adj_models <- NULL

#analysis 15
for(i in stool.t1){
  for(j in laz.waz.t2){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset15"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#analysis 16
for(i in urine.t1){
  for(j in laz.waz.t2){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset16"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#analysis 17
for(i in stool.t1){
  for(j in hcz.t2){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset17"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#analysis 18
for(i in urine.t1){
  for(j in hcz.t2){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset18"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#analysis 19
for(i in stool.t1){
  for(j in whz.t1){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset19"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#analysis 20
for(i in urine.t1){
  for(j in whz.t1){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset20"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#analysis 21
for(i in stool.t1){
  for(j in laz.waz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset21"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#analysis 22
for(i in urine.t1){
  for(j in laz.waz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset22"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#analysis 23
for(i in stool.t1){
  for(j in hcz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset23"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#analysis 24
for(i in urine.t1){
  for(j in hcz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset24"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#analysis 25
for(i in stool.t1){
  for(j in whz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset25"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#analysis 26
for(i in urine.t1){
  for(j in whz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset26"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H2_adj_models <- bind_rows(H2_adj_models, res)
  }
}

#bind all together
H2_adj_res <- NULL
for(i in 1:nrow(H2_adj_models)){
  res <- data.frame(X=H2_adj_models$X[i], Y=H2_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H2_adj_models$fit[i][[1]], d=H2_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H2_adj_res <-  bind_rows(H2_adj_res, preds$res)
}

##### Hypothesis 3
H3_adj_models <- NULL

#analysis 27
for(i in stool.t2){
  for(j in laz.waz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset27"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_adj_models <- bind_rows(H3_adj_models, res)
  }
}

#analysis 28
for(i in urine.t2){
  for(j in laz.waz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset28"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_adj_models <- bind_rows(H3_adj_models, res)
  }
}

#analysis 29
for(i in stool.t2){
  for(j in hcz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset29"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_adj_models <- bind_rows(H3_adj_models, res)
  }
}

#analysis 30
for(i in urine.t2){
  for(j in hcz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset30"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_adj_models <- bind_rows(H3_adj_models, res)
  }
}

#analysis 31
for(i in stool.t2){
  for(j in whz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset31"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_adj_models <- bind_rows(H3_adj_models, res)
  }
}

#analysis 32
for(i in urine.t2){
  for(j in whz.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset32"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H3_adj_models <- bind_rows(H3_adj_models, res)
  }
}

#bind all together
H3_adj_res <- NULL
for(i in 1:nrow(H3_adj_models)){
  res <- data.frame(X=H3_adj_models$X[i], Y=H3_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H3_adj_models$fit[i][[1]], d=H3_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H3_adj_res <-  bind_rows(H3_adj_res, preds$res)
}


##### Hypothesis 4
H4_adj_models <- NULL

#analysis 33
for(i in stool.t1){
  for(j in velo.t1.t2){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset19"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_adj_models <- bind_rows(H4_adj_models, res)
  }
}

#analysis 34
for(i in urine.t1){
  for(j in velo.t1.t2){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset20"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_adj_models <- bind_rows(H4_adj_models, res)
  }
}

#analysis 35
for(i in stool.t1){
  for(j in velo.t1.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset25"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_adj_models <- bind_rows(H4_adj_models, res)
  }
}

#analysis 36
for(i in urine.t1){
  for(j in velo.t1.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset26"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_adj_models <- bind_rows(H4_adj_models, res)
  }
}

#analysis 37
for(i in stool.t1){
  for(j in velo.t2.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset33"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_adj_models <- bind_rows(H4_adj_models, res)
  }
}

#analysis 38
for(i in urine.t1){
  for(j in velo.t2.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset34"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H4_adj_models <- bind_rows(H4_adj_models, res)
  }
}

#bind all together
H4_adj_res <- NULL
for(i in 1:nrow(H4_adj_models)){
  res <- data.frame(X=H4_adj_models$X[i], Y=H4_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H4_adj_models$fit[i][[1]], d=H4_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H4_adj_res <-  bind_rows(H4_adj_res, preds$res)
}

##### Hypothesis 5
H5_adj_models <- NULL

#analysis 37
for(i in stool.t2){
  for(j in velo.t2.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset31"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H5_adj_models <- bind_rows(H5_adj_models, res)
  }
}

#analysis 38
for(i in urine.t2){
  for(j in velo.t2.t3){
    print(i)
    print(j)
    res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset32"]])
    res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
    H5_adj_models <- bind_rows(H5_adj_models, res)
  }
}

#bind all together
H5_adj_res <- NULL
for(i in 1:nrow(H5_adj_models)){
  res <- data.frame(X=H5_adj_models$X[i], Y=H5_adj_models$Y[i])
  preds <- predict_gam_diff(fit=H5_adj_models$fit[i][[1]], d=H5_adj_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  H5_adj_res <-  bind_rows(H5_adj_res, preds$res)
}


#BH procedure
H1_adj_res_BH <- H1_adj_res %>% group_by(Y) %>% 
  mutate(corrected.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

H2.H3_adj_res <- rbind(H2_adj_res, H3_adj_res)

H2.H3_adj_res_BH <- H2.H3_adj_res %>% group_by(Y) %>% 
  mutate(corrected.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

H4.H5_adj_res <- rbind(H4_adj_res, H5_adj_res)

H4.H5_adj_res_BH <- H4.H5_adj_res %>% group_by(Y) %>% 
  mutate(corrected.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

H2_adj_res_BH <- H2.H3_adj_res_BH[1:nrow(H2_adj_res),]
H3_adj_res_BH <- H2.H3_adj_res_BH[(nrow(H2_adj_res)+1):nrow(H2.H3_adj_res_BH),]

H4_adj_res_BH <- H4.H5_adj_res_BH[1:nrow(H4_adj_res),]
H5_adj_res_BH <- H4.H5_adj_res_BH[(nrow(H4_adj_res)+1):nrow(H4.H5_adj_res_BH),]
