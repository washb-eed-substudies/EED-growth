rm(list=ls())

source(here::here("0-config.R"))
source(here::here("table-functions.R"))

# load enrollment characteristics and results
#d <- read.csv(paste0(dropboxDir, ""))
t1C <- readRDS(here('Bangladesh/results/EMM/res_t1C.RDS'))
t2C <- readRDS(here('Bangladesh/results/EMM/res_t2C.RDS'))
t3C <- readRDS(here('Bangladesh/results/EMM/res_t3C.RDS'))
t1S <- readRDS(here('Bangladesh/results/EMM/res_t1S.RDS'))
t2S <- readRDS(here('Bangladesh/results/EMM/res_t2S.RDS'))
t1v <- readRDS(here('Bangladesh/results/EMM/res_t1v.RDS'))
t2v <- readRDS(here('Bangladesh/results/EMM/res_t2v.RDS'))

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

sub.t1 <- c("ln_preg_cort", "life_viol_any_t3")
sub.t1.names <- c("Maternal cortisol", "Maternal exposure to IPV")
sub.t2.child <- c("t2_iso_pca")
sub.t2.child.names <- c("Child combined F2-isoprostanes")
sub.t2.mat <- c("cesd_sum_t2", "life_viol_any_t3")
sub.t2.mat.names <- c("Maternal depression Year 1", "Maternal exposure to IPV")
sub.t3.child <- c("t3_cort_z01", "t3_cort_z03", "t3_cort_slope")
sub.t3.child.names <- c("Child pre-stressor cortisol", "Child post-stressor cortisol",
                        "Child cortisol slope")
sub.t3.mat <- c("pss_sum_mom_t3_cont", "pss_sum_dad_t3", 
                "cesd_sum_ee_t3_cont", "life_viol_any_t3")
sub.t3.mat.names <- c("Maternal Stress", "Paternal Stress", 
                      "Maternal depression Year 2", "Maternal exposure to IPV")
sub.t1sv.child <- c("t2_iso_pca", "t3_cort_z01", "t3_cort_z03", "t3_cort_slope")
sub.t1sv.child.names <- c("Child combined F2-isoprostanes", "Child pre-stressor cortisol", "Child post-stressor cortisol",
                           "Child cortisol slope")
sub.t1sv.mat <- c("ln_preg_cort", "cesd_sum_t2", "pss_sum_mom_t3_cont", "pss_sum_dad_t3", 
                  "cesd_sum_ee_t3_cont", "life_viol_any_t3")
sub.t1sv.mat.names <- c("Maternal cortisol", "Maternal depression Year 1", "Maternal Stress", "Paternal Stress", 
                        "Maternal depression Year 2", "Maternal exposure to IPV")
sub.t2sv.child <- c("t2_iso_pca", "t3_cort_z01", "t3_cort_z03", "t3_cort_slope")
sub.t2sv.child.names <- c("Child combined F2-isoprostanes", "Child pre-stressor cortisol", "Child post-stressor cortisol",
                          "Child cortisol slope")
sub.t2sv.mat <- c("cesd_sum_t2", "pss_sum_mom_t3_cont", "pss_sum_dad_t3", 
                  "cesd_sum_ee_t3_cont", "life_viol_any_t3")
sub.t2sv.mat.names <- c("Maternal depression Year 1", "Maternal Stress", "Paternal Stress", 
                        "Maternal depression Year 2", "Maternal exposure to IPV")

tbl1 <- subgroup_tbl("EED 3 months", exp.t1.names, out.t1.names, sub.t1.names, exp.t1, out.t1, sub.t1, t1C)
tbl2 <- subgroup_tbl("EED 3 months", exp.t1.names, c(out.t2.names, out.t3.names), sub.t1sv.child.names, exp.t1, c(out.t2, out.t3), sub.t1sv.child, t1S)
tbl3 <- subgroup_tbl("EED 3 months", exp.t1.names, c(out.t2.names, out.t3.names), sub.t1sv.mat.names, exp.t1, c(out.t2, out.t3), sub.t1sv.mat, t1S)
tbl4 <- subgroup_tbl("EED 3 months", exp.t1.names, c(velo.t1.t2.names, velo.t2.t3.names, velo.t1.t3.names), sub.t1sv.child.names, exp.t1, c(velo.t1.t2, velo.t2.t3, velo.t1.t3), sub.t1sv.child, t1v)
tbl5 <- subgroup_tbl("EED 3 months", exp.t1.names, c(velo.t1.t2.names, velo.t2.t3.names, velo.t1.t3.names), sub.t1sv.mat.names, exp.t1, c(velo.t1.t2, velo.t2.t3, velo.t1.t3), sub.t1sv.mat, t1v)

tbl6 <- subgroup_tbl("EED 14 months", exp.t2.names, out.t2.names, sub.t2.child.names, exp.t2, out.t2, sub.t2.child, t2C)
tbl7 <- subgroup_tbl("EED 14 months", exp.t2.names, out.t2.names, sub.t2.mat.names, exp.t2, out.t2, sub.t2.mat, t2C)
tbl8 <- subgroup_tbl("EED 14 months", exp.t2.names, out.t3.names, sub.t2sv.child.names, exp.t2, out.t3, sub.t2sv.child, t2S)
tbl9 <- subgroup_tbl("EED 14 months", exp.t2.names, out.t3.names, sub.t2sv.mat.names, exp.t2, out.t3, sub.t2sv.mat, t2S)
tbl10 <- subgroup_tbl("EED 14 months", exp.t2.names, velo.t2.t3.names, sub.t2sv.child.names, exp.t2, velo.t2.t3, sub.t2sv.child, t2v)
tbl11 <- subgroup_tbl("EED 14 months", exp.t2.names, velo.t2.t3.names, sub.t2sv.mat.names, exp.t2, velo.t2.t3, sub.t2sv.mat, t2v)

tbl12 <- subgroup_tbl("EED 28 months", exp.t3.names, out.t3.names, sub.t3.child.names, exp.t3, out.t3, sub.t3.child, t3C)
tbl13 <- subgroup_tbl("EED 28 months", exp.t3.names, out.t3.names, sub.t3.mat.names, exp.t3, out.t3, sub.t3.mat, t3C)

save_as_docx("Table 1: Effect modification of EED and child growth at 3 months" = tbl1,
             "Table 2: Effect modification of EED at 3 months and growth at 14 and 28 months with child biomarkers" = tbl2,
             "Table 3: Effect modification of EED at 3 months and growth at 14 and 28 months with maternal measurements" = tbl3,
             "Table 4: Effect modification of EED at 3 months and growth velocity with child biomarkers" = tbl4,
             "Table 5: Effect modification of EED at 3 months and growth velocity with maternal measurements" = tbl5,
             "Table 6: Effect modification of EED and child growth at 14 months with child biomarkers" = tbl6,
             "Table 7: Effect modification of EED and child growth at 14 months with maternal measurements" = tbl7,
             "Table 8: Effect modification of EED at 14 months and growth at 28 months with child biomarkers" = tbl8,
             "Table 9: Effect modification of EED at 14 months and growth at 28 months with maternal measurements" = tbl9,
             "Table 10: Effect modification of EED at 14 months and growth velocity with child biomarkers" = tbl10,
             "Table 11: Effect modification of EED at 14 months and growth velocity with maternal measurements" = tbl11,
             "Table 12: Effect modification of EED and child growth at 28 months with child biomarkers" = tbl12,
             "Table 13: Effect modification of EED and child growth at 28 months with maternal measurements" = tbl13,
             path = "C:/Users/Sophia/Documents/WASH/EED-Growth/bangladesh-emm-tablesv2.docx")
