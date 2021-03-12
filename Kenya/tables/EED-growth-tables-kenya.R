rm(list=ls())

source(here::here("0-config.R"))
source(here::here("table-functions.R"))

# load enrollment characteristics and results
#d <- read.csv(paste0(dropboxDir, ""))
all_unadj <- readRDS(here('Kenya/results/unadjusted/all_results.RDS'))
all_adj <- readRDS(here('Kenya/results/adjusted/all_results.RDS'))


#### Table 2  ####
tbls <- function(exp.var = NULL, out.var = NULL, exp.names = NULL, out.names = NULL, tbl.name = NULL){
  exposure <- exp.var
  outcome <- out.var
  expo_var <- exp.names
  out_var <- out.names
  
  comb.table <- growth_tbl(tbl.name, expo_var, out_var, exposure, outcome, all_unadj, all_adj, T)
  return(comb.table)
}

exp.t1 <- c("ln_lact1", "ln_mann1", "ln_mpo1", "ln_aat1", "ln_neo1")
exp.t2 <- c("ln_lact2", "ln_mann2", "ln_mpo2", "ln_aat2", "ln_neo2")
exp.t3 <- c("ln_lact3", "ln_mann3","ln_mpo3", "ln_aat3", "ln_neo3")
exp.t1.names <- c("Lactulose 6 months", "Mannitol 6 months", "Myeloperoxidase 6 months", 
                  "Alpha-1-antitrypsin 6 months", "Neopterin 6 months")
exp.t2.names <- c("Lactulose 17 months", "Mannitol 17 months", "Myeloperoxidase 17 months", 
                  "Alpha-1-antitrypsin 17 months", "Neopterin 17 months")
exp.t3.names <- c("Lactulose 22 months", "Mannitol 22 months", "Myeloperoxidase 22 months", 
                  "Alpha-1-antitrypsin 22 months", "Neopterin 22 months")
out.t1 <- c("laz_t1", "waz_t1", "hcz_t1", "whz_t1")
out.t2 <- c("laz_t2", "waz_t2", "hcz_t2", "whz_t2")
out.t3 <- c("laz_t3", "waz_t3", "hcz_t3", "whz_t3")
out.t1.names <- c("LAZ 6 months", "WAZ 6 months", "HCZ 6 months", "WHZ 6 months")
out.t2.names <- c("LAZ 17 months", "WAZ 17 months", "HCZ 17 months", "WHZ 17 months")
out.t3.names <- c("LAZ 22 months", "WAZ 22 months", "HCZ 22 months", "WHZ 22 months")
velo.t1.t2 <- c("len_velocity_t1_t2", "wei_velocity_t1_t2", "hc_velocity_t1_t2")
velo.t1.t3 <- c("len_velocity_t1_t3", "wei_velocity_t1_t3", "hc_velocity_t1_t3")
velo.t2.t3 <- c("len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3")
velo.t1.t2.names <- c("Length velocity 6-17 months", "Weight velocity 6-17 months", "HC velocity 6-17 months")
velo.t1.t3.names <- c("Length velocity 6-22 months", "Weight velocity 6-22 months", "HC velocity 6-22 months")
velo.t2.t3.names <- c("Length velocity 17-22 months", "Weight velocity 17-22 months", "HC velocity 17-22 months")

tbl2 <- NULL
tbl2 <- tbls(exp.var = exp.t1, exp.names = exp.t1.names,
             out.var = c(out.t2, out.t3), out.names = c(out.t2.names, out.t3.names), 
             tbl.name = "EED markers at 6 months and Subsequent LAZ")

tbl2 <- rbind(tbl2, tbls(exp.var = exp.t1, exp.names = exp.t1.names,
                         out.var =  c(velo.t1.t2, velo.t1.t3, velo.t2.t3),
                         out.names = c(velo.t1.t2.names, velo.t1.t3.names, velo.t2.t3.names), 
                         tbl.name = "EED markers at 6 months and Length velocity"))

tbl2 <- rbind(tbl2, tbls(exp.var = exp.t2, exp.names = exp.t2.names,
                         out.var = out.t3, out.names = out.t3.names, 
                         tbl.name = "EED markers at 17 months and Subsequent LAZ"))

tbl2 <- rbind(tbl2, tbls(exp.var = exp.t2, exp.names = exp.t2.names,
                         out.var = c("len_velocity_t2_t3"), out.names = c("Length velocity 17-22 months"), 
                         tbl.name = "EED markers at 17 months and Length velocity"))

tbl2 <- rbind(tbl2, tbls(exp.var = exp.t1, exp.names = exp.t1.names,
                         out.var = out.t1, out.names = out.t1.names, 
                         tbl.name = "EED markers and concurrent growth (6 months)"))

tbl2 <- rbind(tbl2, tbls(exp.var = exp.t2, exp.names = exp.t2.names,
                         out.var = out.t2, out.names = out.t2.names, 
                         tbl.name = "EED markers and concurrent growth (17 months)"))

tbl2 <- rbind(tbl2, tbls(exp.var = exp.t3, exp.names = exp.t3.names,
                         out.var = out.t3, out.names = out.t3.names, 
                         tbl.name = "EED markers and concurrent growth (22 months)"))


#### SAVE TABLES ####
write.csv(tbl2, file="~/Documents/WASH Benefits/Secondary analysis papers/EED and growth/Tables/EED-Growth tables-Kenya.csv")
