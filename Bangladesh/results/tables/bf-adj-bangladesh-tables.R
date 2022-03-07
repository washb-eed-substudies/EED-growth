rm(list=ls())

source(here::here("0-config.R"))
source(here::here("table-functions_r 4 23.R"))

# load enrollment characteristics and results
#d <- read.csv(paste0(dropboxDir, ""))
all_adj <- readRDS(here('Bangladesh/results/adjusted/bf/all_results_sens_bf.RDS'))


#### Table 2  ####
tbls <- function(exp.var = NULL, out.var = NULL, exp.names = NULL, out.names = NULL, tbl.name = NULL){
  exposure <- exp.var
  outcome <- out.var
  expo_var <- exp.names
  out_var <- out.names
  
  comb.table <- growth_tbl(tbl.name, expo_var, out_var, exposure, outcome, all_unadj, all_adj, T)
  return(comb.table)
}

exp.t1 <- c("ln_L_conc_t1", "ln_M_conc_t1", "ln_mpo1", "ln_aat1", "ln_neo1")
exp.t2 <- c("ln_L_conc_t2", "ln_M_conc_t2", "ln_mpo2", "ln_aat2", "ln_neo2", "ln_reg2")
exp.t3 <- c("ln_L_conc_t3", "ln_M_conc_t3","ln_mpo3", "ln_aat3", "ln_neo3")
exp.t1.names <- c("Lactulose 3 months", "Mannitol 3 months", "Myeloperoxidase 3 months", 
                  "Alpha-1-antitrypsin 3 months", "Neopterin 3 months")
exp.t2.names <- c("Lactulose 14 months", "Mannitol 14 months", "Myeloperoxidase 14 months", 
                  "Alpha-1-antitrypsin 14 months", "Neopterin 14 months", "Reg-1b 14 months")
exp.t3.names <- c("Lactulose 28 months", "Mannitol 28 months", "Myeloperoxidase 28 months", 
                  "Alpha-1-antitrypsin 28 months", "Neopterin 28 months")
out.t1 <- c("laz_t1", "waz_t1", "hcz_t1", "whz_t1")
out.t2 <- c("laz_t2", "waz_t2", "hcz_t2", "whz_t2")
out.t3 <- c("laz_t3", "waz_t3", "hcz_t3", "whz_t3")
out.t1.names <- c("LAZ 3 months", "WAZ 3 months", "HCZ 3 months", "WHZ 3 months")
out.t2.names <- c("LAZ 14 months", "WAZ 14 months", "HCZ 14 months", "WHZ 14 months")
out.t3.names <- c("LAZ 28 months", "WAZ 28 months", "HCZ 28 months", "WHZ 28 months")
velo.t1.t2 <- c("len_velocity_t1_t2", "wei_velocity_t1_t2", "hc_velocity_t1_t2")
velo.t1.t3 <- c("len_velocity_t1_t3", "wei_velocity_t1_t3", "hc_velocity_t1_t3")
velo.t2.t3 <- c("len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3")
velo.t1.t2.names <- c("Length velocity 3-14 months", "Weight velocity 3-14 months", "HC velocity 3-14 months")
velo.t1.t3.names <- c("Length velocity 3-28 months", "Weight velocity 3-28 months", "HC velocity 3-28 months")
velo.t2.t3.names <- c("Length velocity 14-28 months", "Weight velocity 14-28 months", "HC velocity 14-28 months")

tbl2 <- NULL
tbl2 <- tbls(exp.var = exp.t1, exp.names = exp.t1.names,
             out.var = c(out.t2, out.t3), out.names = c(out.t2.names, out.t3.names), 
             tbl.name = "EED markers at 3 months and Subsequent LAZ")

tbl2 <- rbind(tbl2, tbls(exp.var = exp.t1, exp.names = exp.t1.names,
                         out.var =  c(velo.t1.t2, velo.t1.t3, velo.t2.t3),
                         out.names = c(velo.t1.t2.names, velo.t1.t3.names, velo.t2.t3.names), 
                         tbl.name = "EED markers at 3 months and Length velocity"))

tbl2 <- rbind(tbl2, tbls(exp.var = exp.t2, exp.names = exp.t2.names,
                         out.var = out.t3, out.names = out.t3.names, 
                         tbl.name = "EED markers at 14 months and Subsequent LAZ"))

tbl2 <- rbind(tbl2, tbls(exp.var = exp.t2, exp.names = exp.t2.names,
                         out.var = c("len_velocity_t2_t3"), out.names = c("Length velocity 14-28 months"), 
                         tbl.name = "EED markers at 14 months and Length velocity"))

tbl2 <- rbind(tbl2, tbls(exp.var = exp.t1, exp.names = exp.t1.names,
                         out.var = out.t1, out.names = out.t1.names, 
                         tbl.name = "EED markers and concurrent growth"))

tbl2 <- rbind(tbl2, tbls(exp.var = exp.t2, exp.names = exp.t2.names,
                         out.var = out.t2, out.names = out.t2.names, 
                         tbl.name = "EED markers and concurrent growth"))

tbl2 <- rbind(tbl2, tbls(exp.var = exp.t3, exp.names = exp.t3.names,
                         out.var = out.t3, out.names = out.t3.names, 
                         tbl.name = "EED markers and concurrent growth"))


#### SAVE TABLES ####
#write.csv(tbl2, file="~/Documents/WASH Benefits/Secondary analysis papers/EED and growth/Tables/EED-Growth tables.csv")

##word doc
tbls.word <- function(exp.var = NULL, out.var = NULL, exp.names = NULL, out.names = NULL, tbl.name = NULL){
  exposure <- exp.var
  outcome <- out.var
  expo_var <- exp.names
  out_var <- out.names
  
  comb.table <- growth_tbl_flex(tbl.name, expo_var, out_var, exposure, outcome, 
                                all_unadj, all_adj, T)
  return(comb.table)
}

tbl1 <- tbls.word(exp.var = exp.t1, exp.names = exp.t1.names,
                  out.var = c(out.t2, out.t3), out.names = c(out.t2.names, out.t3.names), 
                  tbl.name = "")

tbl2 <- tbls.word(exp.var = exp.t1, exp.names = exp.t1.names,
                  out.var =  c(velo.t1.t2, velo.t1.t3, velo.t2.t3),
                  out.names = c(velo.t1.t2.names, velo.t1.t3.names, velo.t2.t3.names), 
                  tbl.name = "")

tbl3 <- tbls.word(exp.var = exp.t2, exp.names = exp.t2.names,
                  out.var = out.t3, out.names = out.t3.names, 
                  tbl.name = "")

tbl4 <- tbls.word(exp.var = exp.t2, exp.names = exp.t2.names,
                  out.var = velo.t2.t3, out.names = velo.t2.t3.names, 
                  tbl.name = "")

tbl5 <- tbls.word(exp.var = exp.t1, exp.names = exp.t1.names,
                  out.var = out.t1, out.names = out.t1.names, 
                  tbl.name = "")

tbl6 <- tbls.word(exp.var = exp.t2, exp.names = exp.t2.names,
                  out.var = out.t2, out.names = out.t2.names, 
                  tbl.name = "")

tbl7 <- tbls.word(exp.var = exp.t3, exp.names = exp.t3.names,
                  out.var = out.t3, out.names = out.t3.names, 
                  tbl.name = "")

save_as_docx("EED markers at 3 months and subsequent growth" = tbl1, 
             "EED markers at 3 months and growth velocity" = tbl2, 
             "EED markers at 14 months and subsequent growth" = tbl3, 
             "EED markers at 14 months and growth velocity" = tbl4, 
             "EED markers and concurrent growth at 3 months" = tbl5, 
             "EED markers and concurrent growth at 14 months" = tbl6, 
             "EED markers and concurrent growth at 28 months" = tbl7, 
             path="/Users/sophiatan/Documents/WASH/EED-growth-bd-bf-sensitivity-tables.docx",
             # path = "~/Documents/WASH Benefits/Secondary analysis papers/EED and growth/Tables/EED-Growth tables 9 30.docx",
             pr_section = sect_properties)
