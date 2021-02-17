rm(list=ls())

source(here::here("Bangladesh/1-data cleaning-bangladesh.R"))
#remotes::install_github('washb-eed-substudies/washbgam', force = TRUE)
library(washbgam)

#Make vectors of adjustment variable names
covariates <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Caitlin/EED-Growth Covariates - Bangladesh.csv"))

#baseline covariates
w.vars <- covariates[c(55:69),1]
w.vars <- w.vars[-12]

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
      res <- data.frame(X=i, Y=j,V=k, int.p =res_adj$int.p, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
      EMM_models <- bind_rows(EMM_models, res)
    }
  }
}

for(i in urine.t1){
  for(j in all.growth.t1){
    for (k in V.set.t1){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset1"]], V = k)
      res <- data.frame(X=i, Y=j,V=k, int.p =res_adj$int.p, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
      EMM_models <- bind_rows(EMM_models, res)
    }
  }
}


all_models_res <- NULL
for(i in 1:nrow(EMM_models)){
  preds <- predict_gam_emm(fit=EMM_models$fit[i][[1]], d=EMM_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=EMM_models$X[i], Yvar=EMM_models$Y[i])
  gamm_diff_res <- data.frame(V=EMM_models$V[i] , preds$res)
  all_models_res <-  bind_rows(all_models_res, gamm_diff_res)
}



  # 
  # #analysis 2
  # for(i in stool.t1){
  #   for(j in all.growth.t1){
  #     for (k in V.set.t1){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset2"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }
  #   }
  # }
  # 
  # #analysis 3
  # for(i in stool.t2){
  #   for(j in laz.waz.t2){
  #     for (k in V.set.t2){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset3"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }
  #   }
  # }
  # 
  # #analysis 4
  # for(i in urine.t2){
  #   for(j in laz.waz.t2){
  #     for (k in V.set.t2){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset4"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }
  #   }
  # }
  # 
  # #analysis 5
  # for(i in stool.t2){
  #   for(j in hcz.t2){
  #     for (k in V.set.t2){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset5"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }  
  #   }
  # }
  # 
  # #analysis 6
  # for(i in urine.t2){
  #   for(j in hcz.t2){
  #     for (k in V.set.t2){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset6"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }  
  #   }
  # }
  # 
  # #analysis 7
  # for(i in stool.t2){
  #   for(j in whz.t2){
  #     for (k in V.set.t2){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset7"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }  
  #   }
  # }
  # 
  # #analysis 8
  # for(i in urine.t2){
  #   for(j in whz.t2){
  #     for (k in V.set.t2){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset8"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }  
  #   }
  # }
  # 
  # #analysis 9
  # for(i in stool.t3){
  #   for(j in laz.waz.t3){
  #     for (k in V.set.t3){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset9"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }  
  #   }
  # }
  # 
  # #analysis 10
  # for(i in urine.t3){
  #   for(j in laz.waz.t3){
  #     for (k in V.set.t3){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset10"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }  
  #   }
  # }
  # 
  # #analysis 11
  # for(i in stool.t3){
  #   for(j in hcz.t3){
  #     for (k in V.set.t3){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset11"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }  
  #   }
  # }
  # 
  # #analysis 12
  # for(i in urine.t3){
  #   for(j in hcz.t3){
  #     for (k in V.set.t3){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset12"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }  
  #   }
  # }
  # 
  # #analysis 13
  # for(i in stool.t3){
  #   for(j in whz.t3){
  #     for (k in V.set.t3){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset13"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }  
  #   }
  # }
  # 
  # #analysis 14
  # for(i in urine.t3){
  #   for(j in whz.t3){
  #     for (k in V.set.t3){
  #       print(i)
  #       print(j)
  #       print(k)
  #       res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset14"]], V = k)
  #       res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
  #       EMM_models <- bind_rows(EMM_models, res)
  #     }  
  #   }
  # }
  # 
  # all.splits.EMM <- list(NA)
  # all.splits.EMM[[1]] <- EMM_models[1:20,]
  # all.splits.EMM[[2]] <- EMM_models[21:68,]
  # all.splits.EMM[[3]] <- EMM_models[69:128,]
  # 
  # total.EMM <- NA
  # for (i in 1:length(all.splits.EMM)) {
  #   all.splits.EMM[[i]] <- all.splits.EMM[[i]] %>%
  #     mutate(corrected.Pval=p.adjust(V.1, method="BH")) %>%
  #     as.data.frame()
  #   
  #   total.EMM <- rbind(total.EMM, all.splits.EMM[[i]])
  # }
  # total.EMM <- total.EMM[-1,]
  # total.EMM <- total.EMM %>% arrange(corrected.Pval)
