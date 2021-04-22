rm(list=ls())

source(here::here("0-config.R"))

#source(here::here("Kenya/1-data cleaning-Kenya.R"))

d<-read.csv("C:/Users/Sophia/Documents/WASH/EED-Growth/kenya data2.csv")
d<-d%>%select(-X)
#remotes::install_github('washb-eed-substudies/washbgam', force = TRUE)
library(washbgam)

#Make vectors of adjustment variable names
covariates <- read.csv(file = paste0(dropboxDir, "WBK-EE-analysis/Data/Cleaned/Caitlin/EED-Growth Covariates - Kenya.csv"))

#baseline covariates
w.vars <- covariates[c(44:56),1]
w.vars <- w.vars[-10] #remove floor which has no variation

#timevarying covariates
time.cov <- covariates[c(1:40),c(2,8:16)]

#obtain unique adjustment sets
time.cov <- unique(time.cov)

#make blank cells NAs
time.cov <- time.cov %>% na_if("")

#create adjustment set vectors
cov.list <- vector('list', nrow(time.cov))
for(i in 1:length(cov.list)){
  vec <- as.character(time.cov[i,2:10]) # create vector using row i in columns with variable names
  vec <- append(vec, w.vars) #add baseline characteristics
  vec <- vec[!is.na(vec)] #remove any NAs (from blank cells)
  cov.list[[i]] <- vec #insert into list item i
}
names(cov.list) <- paste0('adjset', seq_along(cov.list)) #rename 

urine.t1 <- c("ln_lact1", "ln_mann1")
stool.t1 <- c("ln_mpo1", "ln_aat1", "ln_neo1")
urine.t2 <- c("ln_lact2", "ln_mann2")
stool.t2 <- c("ln_mpo2", "ln_aat2", "ln_neo2")
urine.t3 <- c("ln_lact3", "ln_mann3")
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

V <- c("pss_score", "phq_score_t3")
d$pss_score <- as.numeric(d$pss_score)
d$phq_score_t3 <- as.numeric(d$phq_score_t3)

###### Analysis
gam.analysis <- function(save, Xvar = NULL, Yvar = NULL, W = NULL){
  for(i in Xvar){
    for(j in Yvar){
      for(k in V){
        print(i)
        print(j)
        print(k)
        res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W = W, V=k)
        res <- data.frame(X=i, Y=j,V=k, int.p =res_adj$int.p, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
        save <- bind_rows(save, res)
      }
    }
  }
  return(save)
}

EMM_models_t3C <- NULL
EMM_models_t1S <- NULL
EMM_models_t2S <- NULL
EMM_models_t1v <- NULL
EMM_models_t2v <- NULL

#analysis numbers are from covariates spreadsheet
EMM_models_t3C <- gam.analysis(EMM_models_t3C, Xvar = stool.t3, Yvar = laz.waz.t3, W = cov.list[["adjset9"]]) #9
EMM_models_t3C <- gam.analysis(EMM_models_t3C, Xvar = urine.t3, Yvar = laz.waz.t3, W = cov.list[["adjset10"]]) #10
EMM_models_t3C <- gam.analysis(EMM_models_t3C, Xvar = stool.t3, Yvar = hcz.t3, W = cov.list[["adjset11"]]) #11
EMM_models_t3C <- gam.analysis(EMM_models_t3C, Xvar = urine.t3, Yvar = hcz.t3, W = cov.list[["adjset12"]]) #12
EMM_models_t3C <- gam.analysis(EMM_models_t3C, Xvar = stool.t3, Yvar = whz.t3, W = cov.list[["adjset13"]]) #13
EMM_models_t3C <- gam.analysis(EMM_models_t3C, Xvar = urine.t3, Yvar = whz.t3, W = cov.list[["adjset14"]]) #14

EMM_models_t1S <- gam.analysis(EMM_models_t1S, Xvar = stool.t1, Yvar = laz.waz.t3, W = cov.list[["adjset21"]]) #21
EMM_models_t1S <- gam.analysis(EMM_models_t1S, Xvar = urine.t1, Yvar = laz.waz.t3, W = cov.list[["adjset22"]]) #22
EMM_models_t1S <- gam.analysis(EMM_models_t1S, Xvar = stool.t1, Yvar = hcz.t3, W = cov.list[["adjset23"]]) #23
EMM_models_t1S <- gam.analysis(EMM_models_t1S, Xvar = urine.t1, Yvar = hcz.t3, W = cov.list[["adjset24"]]) #24
EMM_models_t1S <- gam.analysis(EMM_models_t1S, Xvar = stool.t1, Yvar = whz.t3, W = cov.list[["adjset25"]]) #25
EMM_models_t1S <- gam.analysis(EMM_models_t1S, Xvar = urine.t1, Yvar = whz.t3, W = cov.list[["adjset26"]]) #26

EMM_models_t2S <- gam.analysis(EMM_models_t2S, Xvar = stool.t2, Yvar = laz.waz.t3, W = cov.list[["adjset27"]]) #27
EMM_models_t2S <- gam.analysis(EMM_models_t2S, Xvar = urine.t2, Yvar = laz.waz.t3, W = cov.list[["adjset28"]]) #28
EMM_models_t2S <- gam.analysis(EMM_models_t2S, Xvar = stool.t2, Yvar = hcz.t3, W = cov.list[["adjset29"]]) #29
EMM_models_t2S <- gam.analysis(EMM_models_t2S, Xvar = urine.t2, Yvar = hcz.t3, W = cov.list[["adjset30"]]) #30
EMM_models_t2S <- gam.analysis(EMM_models_t2S, Xvar = stool.t2, Yvar = whz.t3, W = cov.list[["adjset31"]]) #31
EMM_models_t2S <- gam.analysis(EMM_models_t2S, Xvar = urine.t2, Yvar = whz.t3, W = cov.list[["adjset32"]]) #32

EMM_models_t1v <- gam.analysis(EMM_models_t1v, Xvar = stool.t1, Yvar = velo.t1.t3, W = cov.list[["adjset25"]]) #35
EMM_models_t1v <- gam.analysis(EMM_models_t1v, Xvar = urine.t1, Yvar = velo.t1.t3, W = cov.list[["adjset26"]]) #36
EMM_models_t1v <- gam.analysis(EMM_models_t1v, Xvar = stool.t1, Yvar = velo.t2.t3, W = cov.list[["adjset33"]]) #37
EMM_models_t1v <- gam.analysis(EMM_models_t1v, Xvar = urine.t1, Yvar = velo.t2.t3, W = cov.list[["adjset34"]]) #38

EMM_models_t2v <- gam.analysis(EMM_models_t2v, Xvar = stool.t2, Yvar = velo.t2.t3, W = cov.list[["adjset31"]]) #39
EMM_models_t2v <- gam.analysis(EMM_models_t2v, Xvar = urine.t2, Yvar = velo.t2.t3, W = cov.list[["adjset32"]]) #40

gam.results <- function(models, save){
  for(i in 1:nrow(models)){
    preds <- predict_gam_emm(fit=models$fit[i][[1]], d=models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=models$X[i], Yvar=models$Y[i])
    gamm_diff_res <- data.frame(V=models$V[i] , preds$res) %>% mutate(int.Pval = c(NA, models$int.p[[i]]))
    save <-  bind_rows(save, gamm_diff_res)
  }
  save
}

res_t3C <- NULL
res_t1S <- NULL
res_t2S <- NULL
res_t1v <- NULL
res_t2v <- NULL

res_t3C <- gam.results(EMM_models_t3C, res_t3C) %>% mutate(G = 1)
res_t1S <- gam.results(EMM_models_t1S, res_t1S) %>% mutate(G = 2)
res_t2S <- gam.results(EMM_models_t2S, res_t2S) %>% mutate(G = 3)
res_t1v <- gam.results(EMM_models_t1v, res_t1v) %>% mutate(G = 4)
res_t2v <- gam.results(EMM_models_t2v, res_t2v) %>% mutate(G = 5)

total <- rbind(res_t3C, res_t1S, res_t2S, res_t1v, res_t2v)
total <- total %>% group_by(G) %>% 
  mutate(BH.Pval=p.adjust(Pval, method="BH")) %>%
  mutate(BH.int.Pval=p.adjust(int.Pval, method="BH")) %>%
  ungroup() %>%
  as.data.frame()

res_t3C <- filter(total, G==1)
res_t1S <- filter(total, G==2)
res_t2S <- filter(total, G==3)
res_t1v <- filter(total, G==4)
res_t2v <- filter(total, G==5)

saveRDS(total, here("Kenya/results/EMM/all_results.RDS"))
saveRDS(res_t3C, here("Kenya/results/EMM/res_t3C.RDS"))
saveRDS(res_t1S, here("Kenya/results/EMM/res_t1S.RDS"))
saveRDS(res_t2S, here("Kenya/results/EMM/res_t2S.RDS"))
saveRDS(res_t1v, here("Kenya/results/EMM/res_t1v.RDS"))
saveRDS(res_t2v, here("Kenya/results/EMM/res_t2v.RDS"))

saveRDS(EMM_models_t3C, "C:/Users/Sophia/Documents/WASH/EED-Growth/Kenya models/EEM_models_t3C.RDS")
saveRDS(EMM_models_t1S, "C:/Users/Sophia/Documents/WASH/EED-Growth/Kenya models/EEM_models_t1S.RDS")
saveRDS(EMM_models_t2S, "C:/Users/Sophia/Documents/WASH/EED-Growth/Kenya models/EEM_models_t2S.RDS")
saveRDS(EMM_models_t1v, "C:/Users/Sophia/Documents/WASH/EED-Growth/Kenya models/EEM_models_t1v.RDS")
saveRDS(EMM_models_t2v, "C:/Users/Sophia/Documents/WASH/EED-Growth/Kenya models/EEM_models_t2v.RDS")
