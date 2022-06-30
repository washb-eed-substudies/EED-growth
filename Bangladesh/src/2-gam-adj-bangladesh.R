rm(list=ls())

source(here::here("Bangladesh/1-data cleaning-Bangladesh.R"))
#remotes::install_github('washb-eed-substudies/washbgam', force = TRUE)
library(washbgam)

#Make vectors of adjustment variable names
try(covariates <- read.csv(file = paste0(dropboxDir, "WBB-EE-analysis/Data/Cleaned/Caitlin/EED-Growth Covariates - Bangladesh.csv")))
try(covariates <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Caitlin/EED-Growth Covariates - Bangladesh.csv")))

#baseline covariates
w.vars <- covariates[c(44:58),1]
w.vars <- w.vars[-12] #remove roof which has no variation

#timevarying covariates
time.cov <- covariates[c(1:40),c(2,8:19)]

#obtain unique adjustment sets
time.cov <- unique(time.cov)

#make blank cells NAs
time.cov <- time.cov %>% na_if("")

#create adjustment set vectors
cov.list <- vector('list', nrow(time.cov))
for(i in 1:length(cov.list)){
  vec <- as.character(time.cov[i,2:13]) # create vector using row i in columns with variable names
  vec <- append(vec, w.vars) #add baseline characteristics
  vec <- vec[!is.na(vec)] #remove any NAs (from blank cells)
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
gam.analysis <- function(Xvar = NULL, Yvar = NULL, data = d, Wvar = NULL){
  for(i in Xvar){
    for(j in Yvar){
      print(i)
      print(j)
      res_adj <- fit_RE_gam(d=data, X=i, Y=j,  W=Wvar)
      res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
      all_models <- bind_rows(all_models, res)
    }
  }
  return(all_models)
}

all_models <- NULL
#analysis numbers are from covariates spreadsheet
all_models <- gam.analysis(Xvar = stool.t1, Yvar = all.growth.t1, W = cov.list[["adjset1"]]) #1
all_models <- gam.analysis(Xvar = urine.t1, Yvar = all.growth.t1, W = cov.list[["adjset2"]]) #2
all_models <- gam.analysis(Xvar = stool.t2, Yvar = laz.waz.t2, W = cov.list[["adjset3"]]) #3
all_models <- gam.analysis(Xvar = urine.t2, Yvar = laz.waz.t2, W = cov.list[["adjset4"]]) #4
all_models <- gam.analysis(Xvar = stool.t2, Yvar = hcz.t2, W = cov.list[["adjset5"]]) #5
all_models <- gam.analysis(Xvar = urine.t2, Yvar = hcz.t2, W = cov.list[["adjset6"]]) #6
all_models <- gam.analysis(Xvar = stool.t2, Yvar = whz.t2, W = cov.list[["adjset7"]]) #7
all_models <- gam.analysis(Xvar = urine.t2, Yvar = whz.t2, W = cov.list[["adjset8"]]) #8
all_models <- gam.analysis(Xvar = stool.t3, Yvar = laz.waz.t3, W = cov.list[["adjset9"]]) #9
all_models <- gam.analysis(Xvar = urine.t3, Yvar = laz.waz.t3, W = cov.list[["adjset10"]]) #10
all_models <- gam.analysis(Xvar = stool.t3, Yvar = hcz.t3, W = cov.list[["adjset11"]]) #11
all_models <- gam.analysis(Xvar = urine.t3, Yvar = hcz.t3, W = cov.list[["adjset12"]]) #12
all_models <- gam.analysis(Xvar = stool.t3, Yvar = whz.t3, W = cov.list[["adjset13"]]) #13
all_models <- gam.analysis(Xvar = urine.t3, Yvar = whz.t3, W = cov.list[["adjset14"]]) #14
all_models <- gam.analysis(Xvar = stool.t1, Yvar = laz.waz.t2, W = cov.list[["adjset15"]]) #15
all_models <- gam.analysis(Xvar = urine.t1, Yvar = laz.waz.t2, W = cov.list[["adjset16"]]) #16
all_models <- gam.analysis(Xvar = stool.t1, Yvar = hcz.t2, W = cov.list[["adjset17"]]) #17
all_models <- gam.analysis(Xvar = urine.t1, Yvar = hcz.t2, W = cov.list[["adjset18"]]) #18
all_models <- gam.analysis(Xvar = stool.t1, Yvar = whz.t2, W = cov.list[["adjset19"]]) #19
all_models <- gam.analysis(Xvar = urine.t1, Yvar = whz.t2, W = cov.list[["adjset20"]]) #20
all_models <- gam.analysis(Xvar = stool.t1, Yvar = laz.waz.t3, W = cov.list[["adjset21"]]) #21
all_models <- gam.analysis(Xvar = urine.t1, Yvar = laz.waz.t3, W = cov.list[["adjset22"]]) #22
all_models <- gam.analysis(Xvar = stool.t1, Yvar = hcz.t3, W = cov.list[["adjset23"]]) #23
all_models <- gam.analysis(Xvar = urine.t1, Yvar = hcz.t3, W = cov.list[["adjset24"]]) #24
all_models <- gam.analysis(Xvar = stool.t1, Yvar = whz.t3, W = cov.list[["adjset25"]]) #25
all_models <- gam.analysis(Xvar = urine.t1, Yvar = whz.t3, W = cov.list[["adjset26"]]) #26
all_models <- gam.analysis(Xvar = stool.t2, Yvar = laz.waz.t3, W = cov.list[["adjset27"]]) #27
all_models <- gam.analysis(Xvar = urine.t2, Yvar = laz.waz.t3, W = cov.list[["adjset28"]]) #28
all_models <- gam.analysis(Xvar = stool.t2, Yvar = hcz.t3, W = cov.list[["adjset29"]]) #29
all_models <- gam.analysis(Xvar = urine.t2, Yvar = hcz.t3, W = cov.list[["adjset30"]]) #30
all_models <- gam.analysis(Xvar = stool.t2, Yvar = whz.t3, W = cov.list[["adjset31"]]) #31
all_models <- gam.analysis(Xvar = urine.t2, Yvar = whz.t3, W = cov.list[["adjset32"]]) #32
all_models <- gam.analysis(Xvar = stool.t1, Yvar = velo.t1.t2, W = cov.list[["adjset19"]]) #33
all_models <- gam.analysis(Xvar = urine.t1, Yvar = velo.t1.t2, W = cov.list[["adjset20"]]) #34
all_models <- gam.analysis(Xvar = stool.t1, Yvar = velo.t1.t3, W = cov.list[["adjset25"]]) #35
all_models <- gam.analysis(Xvar = urine.t1, Yvar = velo.t1.t3, W = cov.list[["adjset26"]]) #36
all_models <- gam.analysis(Xvar = stool.t1, Yvar = velo.t2.t3, W = cov.list[["adjset33"]]) #37
all_models <- gam.analysis(Xvar = urine.t1, Yvar = velo.t2.t3, W = cov.list[["adjset34"]]) #38
all_models <- gam.analysis(Xvar = stool.t2, Yvar = velo.t2.t3, W = cov.list[["adjset31"]]) #39
all_models <- gam.analysis(Xvar = urine.t2, Yvar = velo.t2.t3, W = cov.list[["adjset32"]]) #40


#bind all together
all_models_res <- NULL
for(i in 1:nrow(all_models)){
  res <- data.frame(X=all_models$X[i], Y=all_models$Y[i])
  preds <- predict_gam_diff(fit=all_models$fit[i][[1]], d=all_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  gamm_diff_res <- preds$res
  all_models_res <-  bind_rows(all_models_res, gamm_diff_res)
}

#BH procedure
all.splits <- list(NA)
all.splits[[1]] <- all_models_res[1:20,]
all.splits[[2]] <- all_models_res[21:44,]
all.splits[[3]] <- all_models_res[45:64,]
all.splits[[4]] <- all_models_res[65:104,]
all.splits[[5]] <- all_models_res[105:128,]
all.splits[[6]] <- all_models_res[129:173,]
all.splits[[7]] <- all_models_res[174:nrow(all_models_res),]

total <- NA
for (i in 1:length(all.splits)) {
  all.splits[[i]] <- all.splits[[i]] %>%
    mutate(corrected.Pval=p.adjust(Pval, method="BH")) %>%
    as.data.frame()
  
  total <- rbind(total, all.splits[[i]])
}
total <- total[-1,]

H1_adj_res_BH <- total[1:64,]
H2_adj_res_BH <- total[65:104,]
H3_adj_res_BH <- total[105:128,]
H4_adj_res_BH <- total[129:173,]
H5_adj_res_BH <- total[174:nrow(total),]

saveRDS(total, here("Bangladesh/results/adjusted/all_results.RDS"))
saveRDS(H1_adj_res_BH, here("Bangladesh/results/adjusted/H1_adj_res.RDS"))
saveRDS(H2_adj_res_BH, here("Bangladesh/results/adjusted/H2_adj_res.RDS"))
saveRDS(H3_adj_res_BH, here("Bangladesh/results/adjusted/H3_adj_res.RDS"))
saveRDS(H4_adj_res_BH, here("Bangladesh/results/adjusted/H4_adj_res.RDS"))
saveRDS(H5_adj_res_BH, here("Bangladesh/results/adjusted/H5_adj_res.RDS"))





###### Effect Modification Analysis
V.set.t1 <- c("life_viol_any_t3")
V.set.t2 <- c("cesd_sum_t2", "life_viol_any_t3")
V.set.t3 <- c("pss_sum_mom_t3", "cesd_sum_ee_t3", "life_viol_any_t3")
V.set.t1.t2 <- c("cesd_sum_t2", "life_viol_any_t3")
V.set.t2.t3 <- c("pss_sum_mom_t3","cesd_sum_t2", "cesd_sum_ee_t3", "life_viol_any_t3")
V.set.t1.t3 <- c("pss_sum_mom_t3", "cesd_sum_t2", "cesd_sum_ee_t3", "life_viol_any_t3")

EMM_models <- NULL

#analysis 1
for(i in urine.t1){
  for(j in all.growth.t1){
    for (k in V.set.t1){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset1"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V =res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }
  }
}

#analysis 2
for(i in stool.t1){
  for(j in all.growth.t1){
    for (k in V.set.t1){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset2"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }
  }
}

#analysis 3
for(i in stool.t2){
  for(j in laz.waz.t2){
    for (k in V.set.t2){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset3"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }
  }
}

#analysis 4
for(i in urine.t2){
  for(j in laz.waz.t2){
    for (k in V.set.t2){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset4"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }
  }
}

#analysis 5
for(i in stool.t2){
  for(j in hcz.t2){
    for (k in V.set.t2){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset5"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
      }  
    }
}

#analysis 6
for(i in urine.t2){
  for(j in hcz.t2){
    for (k in V.set.t2){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset6"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 7
for(i in stool.t2){
  for(j in whz.t2){
    for (k in V.set.t2){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset7"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 8
for(i in urine.t2){
  for(j in whz.t2){
    for (k in V.set.t2){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset8"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 9
for(i in stool.t3){
  for(j in laz.waz.t3){
    for (k in V.set.t3){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset9"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 10
for(i in urine.t3){
  for(j in laz.waz.t3){
    for (k in V.set.t3){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset10"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 11
for(i in stool.t3){
  for(j in hcz.t3){
    for (k in V.set.t3){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset11"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 12
for(i in urine.t3){
  for(j in hcz.t3){
    for (k in V.set.t3){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset12"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 13
for(i in stool.t3){
  for(j in whz.t3){
    for (k in V.set.t3){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset13"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 14
for(i in urine.t3){
  for(j in whz.t3){
    for (k in V.set.t3){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset14"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

all.splits.EMM <- list(NA)
all.splits.EMM[[1]] <- EMM_models[1:20,]
all.splits.EMM[[2]] <- EMM_models[21:68,]
all.splits.EMM[[3]] <- EMM_models[69:128,]

total.EMM <- NA
for (i in 1:length(all.splits.EMM)) {
  all.splits.EMM[[i]] <- all.splits.EMM[[i]] %>%
    mutate(corrected.Pval=p.adjust(V.1, method="BH")) %>%
    as.data.frame()
  
  total.EMM <- rbind(total.EMM, all.splits.EMM[[i]])
}
total.EMM <- total.EMM[-1,]
total.EMM <- total.EMM %>% arrange(corrected.Pval)
