rm(list=ls())

source(here::here("0-config.R"))
source(here::here("table-functions_R 4 23.R"))

# load enrollment characteristics and results
#d <- read.csv(paste0(dropboxDir, ""))
t3C <- readRDS(here('Kenya/results/EMM/res_t3C.RDS'))
t1S <- readRDS(here('Kenya/results/EMM/res_t1S.RDS'))
t2S <- readRDS(here('Kenya/results/EMM/res_t2S.RDS'))
t1v <- readRDS(here('Kenya/results/EMM/res_t1v.RDS'))
t2v <- readRDS(here('Kenya/results/EMM/res_t2v.RDS'))

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

sub <- c("pss_quartile_t3", "phq_score_t3")
sub.names <- c("Maternal stress", "Maternal depression")


tbl1 <- subgroup_tbl("EED 6 months", exp.t1.names, c(velo.t2.t3.names, velo.t1.t3.names), sub.names, exp.t1, c(velo.t2.t3, velo.t1.t3), sub, t1v)

tbl2 <- subgroup_tbl("EED 17 months", exp.t2.names, out.t3.names, sub.names, exp.t2, out.t3, sub, t2S)
tbl3 <- subgroup_tbl("EED 17 months", exp.t2.names, velo.t2.t3.names, sub.names, exp.t2, velo.t2.t3, sub, t2v)

tbl4 <- subgroup_tbl("EED 22 months", exp.t3.names, out.t3.names, sub.names, exp.t3, out.t3, sub, t3C)

save_as_docx("Table 1: Effect modification of EED at 6 months and growth velocity" = tbl1,
             "Table 2: Effect modification of EED at 17 months and growth at 22 months" = tbl2,
             "Table 3: Effect modification of EED at 17 months and growth velocity" = tbl3,
             "Table 4: Effect modification of EED and growth at 22 months" = tbl4,
             path = "C:/Users/Sophia/Documents/WASH/EED-Growth/kenya-emm-tables v2.docx",
             pr_section = sect_properties)
