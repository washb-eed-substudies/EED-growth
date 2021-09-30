rm(list=ls())

source(here::here("Kenya/1-data cleaning-kenya.R"))
library(washbgam)

outliers <- function(j, data){
  if (j %in% c("laz_t1", "laz_t2", "laz_t3", "len_velocity_t1_t2", 
               "len_velocity_t1_t3", "len_velocity_t2_t3")){
    dfunc <- data %>% filter(lenflag != 1)
  } else if (j %in% c("waz_t1", "waz_t2", "waz_t3",  "wei_velocity_t1_t2", 
                      "wei_velocity_t1_t3", "wei_velocity_t2_t3")){
    dfunc <- data %>% filter(weiflag != 1)
  } else if (j %in% c("hcz_t1", "hcz_t2", "hcz_t3",  "hc_velocity_t1_t2", 
                      "hc_velocity_t1_t3", "hc_velocity_t2_t3")){
    dfunc <- data %>% filter(hcflag != 1)
  } else if (j %in% c("whz_t1", "whz_t2", "whz_t3")){
    dfunc <- data %>% filter(whflag != 1)
  } 
  return(dfunc)
}

gam.analysis <- function(Xvar = NULL, Yvar = NULL, Wvar = NULL){
  for(i in Xvar){
    for(j in Yvar){
      print(i)
      print(j)
      dfunc <- outliers(j, d)
      res_adj <- fit_RE_gam(d=dfunc, X=i, Y=j,  W=Wvar)
      res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
      all_models <- bind_rows(all_models, res)
    }
  }
  return(all_models)
}

exp.t1 <- c("ln_lact1", "ln_mann1", "ln_mpo1", "ln_aat1", "ln_neo1")
exp.t2 <- c("ln_lact2", "ln_mann2", "ln_mpo2", "ln_aat2", "ln_neo2")
exp.t3 <- c("ln_lact3", "ln_mann3","ln_mpo3", "ln_aat3", "ln_neo3")
growth.t1 <- c("laz_t1", "waz_t1", "whz_t1" ,"hcz_t1")
growth.t2 <- c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2")
growth.t3 <- c("laz_t3", "waz_t3", "whz_t3" ,"hcz_t3")
velocity <- c(c(names(select(d, grep("velocity", names(d), value = T)))))
velo.t2.t3 <- names(select(d, grep("velocity_t2_t3", names(d), value = T)))

all_models <- NULL

#H1
#t1
Xvars <- c(exp.t1)            
Yvars <- c(growth.t1) 
all_models <- gam.analysis(Xvar = Xvars, Yvar = Yvars)

#t2
Xvars <- c(exp.t2)            
Yvars <- c(growth.t2) 
all_models <- gam.analysis(Xvar = Xvars, Yvar = Yvars)

#t3
Xvars <- c(exp.t3)            
Yvars <- c(growth.t3) 
all_models <- gam.analysis(Xvar = Xvars, Yvar = Yvars)

#H2: EED biomarkers at 3 months will be negatively associated with subsequent 
#LAZ, WAZ, WLZ, and head-circumference for age Z-score at 14 months and 28 months.
Xvars <- c(exp.t1)           
Yvars <- c(growth.t2,  growth.t3) 
all_models <- gam.analysis(Xvar = Xvars, Yvar = Yvars)

#H3: EED biomarkers at 14 months will be negatively associated with subsequent 
#LAZ, WAZ, WLZ, and head-circumference for age Z-score at 28 months.
Xvars <- c(exp.t2)           
Yvars <- c(growth.t3) 
all_models <- gam.analysis(Xvar = Xvars, Yvar = Yvars)

#H4: EED biomarkers at 3 months will be negatively associated with growth velocity 
#between 3-14 months, 14-28 months, and 3-28 months
Xvars <- c(exp.t1)           
Yvars <- c(velocity) 
all_models <- gam.analysis(Xvar = Xvars, Yvar = Yvars)

#H5: EED biomarkers at 14 months will be negatively associated with growth velocity 
#between 14-28 months
Xvars <- c(exp.t2)           
Yvars <- c(velo.t2.t3) 
all_models <- gam.analysis(Xvar = Xvars, Yvar = Yvars)

#Get primary contrasts
all_models_res <- NULL
for(i in 1:nrow(all_models)){
  res <- data.frame(X=all_models$X[i], Y=all_models$Y[i])
  preds <- predict_gam_diff(fit=all_models$fit[i][[1]], d=all_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  all_models_res <-  bind_rows(all_models_res, preds$res)
}

#BH procedure
all.splits <- list(NA)
all.splits[[1]] <- all_models_res[1:20,]
all.splits[[2]] <- all_models_res[21:40,]
all.splits[[3]] <- all_models_res[41:60,]
all.splits[[4]] <- all_models_res[61:100,]
all.splits[[5]] <- all_models_res[101:120,]
all.splits[[6]] <- all_models_res[121:165,]
all.splits[[7]] <- all_models_res[166:nrow(all_models_res),]

total <- NA
for (i in 1:length(all.splits)) {
  all.splits[[i]] <- all.splits[[i]] %>%
    mutate(corrected.Pval=p.adjust(Pval, method="BH")) %>%
    as.data.frame()
  
  total <- rbind(total, all.splits[[i]])
}
total <- total[-1,]

H1_unadj_res_BH <- total[1:60,]
H2_unadj_res_BH <- total[61:100,]
H3_unadj_res_BH <- total[101:120,]
H4_unadj_res_BH <- total[121:165,]
H5_unadj_res_BH <- total[166:nrow(total),]


saveRDS(total, here("Kenya/results/unadjusted/all_results.RDS"))
saveRDS(H1_unadj_res_BH, here("Kenya/results/unadjusted/H1_res.RDS"))
saveRDS(H2_unadj_res_BH, here("Kenya/results/unadjusted/H2_res.RDS"))
saveRDS(H3_unadj_res_BH, here("Kenya/results/unadjusted/H3_res.RDS"))
saveRDS(H4_unadj_res_BH, here("Kenya/results/unadjusted/H4_res.RDS"))
saveRDS(H5_unadj_res_BH, here("Kenya/results/unadjusted/H5_res.RDS"))


#### Plots

#Make list of plots
H1_plot_list <- NULL
H1_plot_data <- NULL
for(i in 1:nrow(H1_models)){
  res <- data.frame(X=H1_models$X[i], Y=H1_models$Y[i])
  simul_plot <- gam_simul_CI(H1_models$fit[i][[1]], H1_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H1_plot_list[[i]] <-  simul_plot$p
  H1_plot_data <-  rbind(H1_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}

#Save plots
saveRDS(H1_plot_list, here("plots/H1_unadj_splines.RDS"))
saveRDS(H1_plot_data, here("plots/H1_unadj_spline_data.RDS"))

#Make list of plots
H2_plot_list <- NULL
H2_plot_data <- NULL
for(i in 1:nrow(H2_models)){
  res <- data.frame(X=H2_models$X[i], Y=H2_models$Y[i])
  simul_plot <- gam_simul_CI(H2_models$fit[i][[1]], H2_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H2_plot_list[[i]] <-  simul_plot$p
  H2_plot_data <-  rbind(H2_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}

#Make list of plots
H3_plot_list <- NULL
H3_plot_data <- NULL
for(i in 1:nrow(H3_models)){
  res <- data.frame(X=H3_models$X[i], Y=H3_models$Y[i])
  simul_plot <- gam_simul_CI(H3_models$fit[i][[1]], H3_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H3_plot_list[[i]] <-  simul_plot$p
  H3_plot_data <-  rbind(H3_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}

#Make list of plots
H4_plot_list <- NULL
H4_plot_data <- NULL
for(i in 1:nrow(H4_models)){
  res <- data.frame(X=H4_models$X[i], Y=H4_models$Y[i])
  simul_plot <- gam_simul_CI(H4_models$fit[i][[1]], H4_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H4_plot_list[[i]] <-  simul_plot$p
  H4_plot_data <-  rbind(H4_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}


#Make list of plots
H5_plot_list <- NULL
H5_plot_data <- NULL
for(i in 1:nrow(H5_models)){
  res <- data.frame(X=H5_models$X[i], Y=H5_models$Y[i])
  simul_plot <- gam_simul_CI(H5_models$fit[i][[1]], H5_models$dat[i][[1]], xlab=res$X, ylab=res$Y, title="")
  H5_plot_list[[i]] <-  simul_plot$p
  H5_plot_data <-  rbind(H5_plot_data, data.frame(Xvar=res$X, Yvar=res$Y, adj=0, simul_plot$pred))
}

