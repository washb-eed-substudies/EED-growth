rm(list=ls())

source(here::here("Bangladesh/1-data cleaning-bangladesh.R"))
#remotes::install_github('washb-eed-substudies/washbgam', force = TRUE)
library(washbgam)

stress<-readRDS(paste0(dropboxDir, "Data/Cleaned/Andrew/stress_growth_data.RDS"))
stress<- stress %>% select(childid, grep("t2_f2", names(stress)), t2_iso_pca, grep("cort", names(stress)))
d<-d %>% left_join(stress, by="childid")

maternal<-read.csv(paste0(dropboxDir, "Data/Cleaned/Audrie/washb-bd-pregnancy-serum-micronutrient-immun-cortisol-covariates.csv")) %>%
  select(dataid, ln_preg_cort)
d<-d %>% left_join(maternal, by="dataid")

# exclude missing category from life_viol_any_t3 
d$life_viol_any_t3 <- factor(d$life_viol_any_t3, exclude = "Missing")
d$life_viol_any_t3 <- as.numeric(levels(d$life_viol_any_t3))[d$life_viol_any_t3]

d$pss_sum_dad_t3 <- as.numeric(d$pss_sum_dad_t3)

#Make vectors of adjustment variable names
covariates <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Caitlin/EED-Growth Covariates - Bangladesh.csv"))

#baseline covariates
w.vars <- covariates[c(44:58),1]
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

velo.t1.t2 <- c("len_velocity_t1_t2", "wei_velocity_t1_t2", "hc_velocity_t1_t2")
velo.t1.t3 <- c("len_velocity_t1_t3", "wei_velocity_t1_t3", "hc_velocity_t1_t3")
velo.t2.t3 <- c("len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3")

V.set.t1 <- c("ln_preg_cort", "life_viol_any_t3")
V.set.t2 <- c("t2_iso_pca", "cesd_sum_t2", "life_viol_any_t3")
V.set.t3 <- c("pss_sum_mom_t3_cont", "pss_sum_dad_t3", 
              "cesd_sum_ee_t3_cont", "life_viol_any_t3", 
              "t3_cort_slope", "t3_cort_z01", "t3_cort_z03")
V.set.t1.t2 <- c("ln_preg_cort", "t2_iso_pca", "cesd_sum_t2", "life_viol_any_t3")
V.set.t2.t3 <- c("t2_iso_pca", "pss_sum_mom_t3_cont","pss_sum_dad_t3",
                 "cesd_sum_t2", "cesd_sum_ee_t3_cont", "life_viol_any_t3",
                 "t3_cort_slope", "t3_cort_z01", "t3_cort_z03")
V.set.t1.t3 <- c("ln_preg_cort", "t2_iso_pca", "pss_sum_mom_t3_cont", "pss_sum_dad_t3",
                 "cesd_sum_t2", "cesd_sum_ee_t3_cont", "life_viol_any_t3",
                 "t3_cort_slope", "t3_cort_z01", "t3_cort_z03")

###### Analysis
gam.analysis <- function(save, Xvar = NULL, Yvar = NULL, data = d, Wvar = NULL, Vvar = NULL){
  for(i in Xvar){
    for(j in Yvar){
      for(k in Vvar){
        print(i)
        print(j)
        print(k)
        res_adj <- fit_RE_gam(d=data, X=i, Y=j,  W=Wvar, V=k)
        res <- data.frame(X=i, Y=j,V=k, int.p =res_adj$int.p, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
        save <- bind_rows(save, res)
      }
    }
  }
  return(save)
}

EMM_models_t1C <- NULL
EMM_models_t2C <- NULL
EMM_models_t3C <- NULL
EMM_models_t1S <- NULL
EMM_models_t2S <- NULL
EMM_models_t1v <- NULL
EMM_models_t2v <- NULL

#analysis 1
EMM_models_t1C <- gam.analysis(EMM_models_t1C, urine.t1, all.growth.t1, d, cov.list[["adjset1"]], V.set.t1)
EMM_models_t1C <- gam.analysis(EMM_models_t1C, stool.t1, all.growth.t1, d, cov.list[["adjset2"]], V.set.t1)

EMM_models_t2C <- gam.analysis(EMM_models_t2C, stool.t2, laz.waz.t2, d, cov.list[["adjset3"]], V.set.t2)
EMM_models_t2C <- gam.analysis(EMM_models_t2C, urine.t2, laz.waz.t2, d, cov.list[["adjset4"]], V.set.t2)
EMM_models_t2C <- gam.analysis(EMM_models_t2C, stool.t2, hcz.t2, d, cov.list[["adjset5"]], V.set.t2)
EMM_models_t2C <- gam.analysis(EMM_models_t2C, urine.t2, hcz.t2, d, cov.list[["adjset6"]], V.set.t2)
EMM_models_t2C <- gam.analysis(EMM_models_t2C, stool.t2, whz.t2, d, cov.list[["adjset7"]], V.set.t2)
EMM_models_t2C <- gam.analysis(EMM_models_t2C, urine.t2, whz.t2, d, cov.list[["adjset8"]], V.set.t2)

EMM_models_t3C <- gam.analysis(EMM_models_t3C, stool.t3, laz.waz.t3, d, cov.list[["adjset9"]], V.set.t3)
EMM_models_t3C <- gam.analysis(EMM_models_t3C, urine.t3, laz.waz.t3, d, cov.list[["adjset10"]], V.set.t3)
EMM_models_t3C <- gam.analysis(EMM_models_t3C, stool.t3, hcz.t3, d, cov.list[["adjset11"]], V.set.t3)
EMM_models_t3C <- gam.analysis(EMM_models_t3C, urine.t3, hcz.t3, d, cov.list[["adjset12"]], V.set.t3)
EMM_models_t3C <- gam.analysis(EMM_models_t3C, stool.t3, whz.t3, d, cov.list[["adjset13"]], V.set.t3)
EMM_models_t3C <- gam.analysis(EMM_models_t3C, urine.t3, whz.t3, d, cov.list[["adjset14"]], V.set.t3)

EMM_models_t1S <- gam.analysis(EMM_models_t1S, stool.t1, laz.waz.t2, d, cov.list[["adjset15"]], V.set.t1.t2) #15
EMM_models_t1S <- gam.analysis(EMM_models_t1S, urine.t1, laz.waz.t2, d, cov.list[["adjset16"]], V.set.t1.t2) #16
EMM_models_t1S <- gam.analysis(EMM_models_t1S, stool.t1, hcz.t2, d, cov.list[["adjset17"]], V.set.t1.t2) #17
EMM_models_t1S <- gam.analysis(EMM_models_t1S, urine.t1, hcz.t2, d, cov.list[["adjset18"]], V.set.t1.t2) #18
EMM_models_t1S <- gam.analysis(EMM_models_t1S, stool.t1, whz.t2, d, cov.list[["adjset19"]], V.set.t1.t2) #19
EMM_models_t1S <- gam.analysis(EMM_models_t1S, urine.t1, whz.t2, d, cov.list[["adjset20"]], V.set.t1.t2) #20
EMM_models_t1S <- gam.analysis(EMM_models_t1S, stool.t1, laz.waz.t3, d, cov.list[["adjset21"]], V.set.t1.t3) #21
EMM_models_t1S <- gam.analysis(EMM_models_t1S, urine.t1, laz.waz.t3, d, cov.list[["adjset22"]], V.set.t1.t3) #22
EMM_models_t1S <- gam.analysis(EMM_models_t1S, stool.t1, hcz.t3, d, cov.list[["adjset23"]], V.set.t1.t3) #23
EMM_models_t1S <- gam.analysis(EMM_models_t1S, urine.t1, hcz.t3, d, cov.list[["adjset24"]], V.set.t1.t3) #24
EMM_models_t1S <- gam.analysis(EMM_models_t1S, stool.t1, whz.t3, d, cov.list[["adjset25"]], V.set.t1.t3) #25
EMM_models_t1S <- gam.analysis(EMM_models_t1S, urine.t1, whz.t3, d, cov.list[["adjset26"]], V.set.t1.t3) #26

EMM_models_t2S <- gam.analysis(EMM_models_t2S, stool.t2, laz.waz.t3, d, cov.list[["adjset27"]], V.set.t2.t3) #27
EMM_models_t2S <- gam.analysis(EMM_models_t2S, urine.t2, laz.waz.t3, d, cov.list[["adjset28"]], V.set.t2.t3) #28
EMM_models_t2S <- gam.analysis(EMM_models_t2S, stool.t2, hcz.t3, d, cov.list[["adjset29"]], V.set.t2.t3) #29
EMM_models_t2S <- gam.analysis(EMM_models_t2S, urine.t2, hcz.t3, d, cov.list[["adjset30"]], V.set.t2.t3) #30
EMM_models_t2S <- gam.analysis(EMM_models_t2S, stool.t2, whz.t3, d, cov.list[["adjset31"]], V.set.t2.t3) #31
EMM_models_t2S <- gam.analysis(EMM_models_t2S, urine.t2, whz.t3, d, cov.list[["adjset32"]], V.set.t2.t3) #32

EMM_models_t1v <- gam.analysis(EMM_models_t1v, stool.t1, velo.t1.t2, d, cov.list[["adjset19"]], V.set.t1.t2) #33
EMM_models_t1v <- gam.analysis(EMM_models_t1v, urine.t1, velo.t1.t2, d, cov.list[["adjset20"]], V.set.t1.t2) #34
EMM_models_t1v <- gam.analysis(EMM_models_t1v, stool.t1, velo.t1.t3, d, cov.list[["adjset25"]], V.set.t1.t3) #35
EMM_models_t1v <- gam.analysis(EMM_models_t1v, urine.t1, velo.t1.t3, d, cov.list[["adjset26"]], V.set.t1.t3) #36
EMM_models_t1v <- gam.analysis(EMM_models_t1v, stool.t1, velo.t2.t3, d, cov.list[["adjset33"]], V.set.t1.t3) #37
EMM_models_t1v <- gam.analysis(EMM_models_t1v, urine.t1, velo.t2.t3, d, cov.list[["adjset34"]], V.set.t1.t3) #38

EMM_models_t2v <- gam.analysis(EMM_models_t2v, stool.t2, velo.t2.t3, d,cov.list[["adjset31"]], V.set.t2.t3) #39
EMM_models_t2v <- gam.analysis(EMM_models_t2v, urine.t2, velo.t2.t3, d, cov.list[["adjset32"]], V.set.t2.t3) #40


gam.results <- function(models, save){
  for(i in 1:nrow(models)){
    preds <- predict_gam_emm(fit=models$fit[i][[1]], d=models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=models$X[i], Yvar=models$Y[i])
    gamm_diff_res <- data.frame(V=models$V[i] , preds$res)
    save <-  bind_rows(save, gamm_diff_res)
  }
  save
}

res_t1C <- NULL
res_t2C <- NULL
res_t3C <- NULL
res_t1S <- NULL
res_t2S <- NULL
res_t1v <- NULL
res_t2v <- NULL

res_t1C <- gam.results(EMM_models_t1C, res_t1C) %>% mutate(G = 1)
res_t2C <- gam.results(EMM_models_t2C, res_t2C) %>% mutate(G = 2)
res_t3C <- gam.results(EMM_models_t3C, res_t3C) %>% mutate(G = 3)
res_t1S <- gam.results(EMM_models_t1S, res_t1S) %>% mutate(G = 4)
res_t2S <- gam.results(EMM_models_t2S, res_t2S) %>% mutate(G = 5)
res_t1v <- gam.results(EMM_models_t1v, res_t1v) %>% mutate(G = 6)
res_t2v <- gam.results(EMM_models_t2v, res_t2v) %>% mutate(G = 7)

total <- rbind(res_t1C, res_t2C, res_t3C, res_t1S, res_t2S, res_t1v, res_t2v)
total <- total %>% group_by(G) %>% 
  mutate(BH.Pval=p.adjust(Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

res_t1C <- res_t1C %>% mutate(BH.Pval=p.adjust(Pval, method="BH"))
res_t2C <- res_t2C %>% mutate(BH.Pval=p.adjust(Pval, method="BH"))
res_t3C <- res_t3C %>% mutate(BH.Pval=p.adjust(Pval, method="BH"))
res_t1S <- res_t1S %>% mutate(BH.Pval=p.adjust(Pval, method="BH"))
res_t2S <- res_t2S %>% mutate(BH.Pval=p.adjust(Pval, method="BH"))
res_t1v <- res_t1v %>% mutate(BH.Pval=p.adjust(Pval, method="BH"))
res_t2v <- res_t2v %>% mutate(BH.Pval=p.adjust(Pval, method="BH"))

saveRDS(total, here("Bangladesh/results/EMM/all_results.RDS"))
saveRDS(res_t1C, here("Bangladesh/results/EMM/res_t1C.RDS"))
saveRDS(res_t2C, here("Bangladesh/results/EMM/res_t2C.RDS"))
saveRDS(res_t3C, here("Bangladesh/results/EMM/res_t3C.RDS"))
saveRDS(res_t1S, here("Bangladesh/results/EMM/res_t1S.RDS"))
saveRDS(res_t2S, here("Bangladesh/results/EMM/res_t2S.RDS"))
saveRDS(res_t1v, here("Bangladesh/results/EMM/res_t1v.RDS"))
saveRDS(res_t2v, here("Bangladesh/results/EMM/res_t2v.RDS"))

