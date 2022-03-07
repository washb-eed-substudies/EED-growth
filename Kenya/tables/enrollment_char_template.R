source(here::here("0-config.R"))
library(tidyverse)
library(flextable)
library(officer)

d<-read.csv("/Users/sophiatan/Documents/WASH/kenya data2.csv") %>% select(!X)

filtering <- function(row){
  any(!is.na(row))
}

# OTHER ANALYSES WITH T2 AND T3 EXPOSURES AND OUTCOMES UNCOMMENT AND FILL IN THE CODE BELOW (UNCOMMENT WITH CTRL+SHIFT+C ON PC)
exposures_m3 <- c("ln_lact1", "ln_mann1", "ln_mpo1", "ln_aat1", "ln_neo1")
outcomes_m3 <- c("laz_t1", "waz_t1", "whz_t1" ,"hcz_t1", "len_velocity_t1_t2", "wei_velocity_t1_t2", "hc_velocity_t1_t2", 
                 "len_velocity_t1_t3", "wei_velocity_t1_t3", "hc_velocity_t1_t3") 
exposures_y1 <- c("ln_lact2", "ln_mann2", "ln_mpo2", "ln_aat2", "ln_neo2")
outcomes_y1 <- c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2", "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3") 
exposures_y2 <- c("ln_lact3", "ln_mann3","ln_mpo3", "ln_aat3", "ln_neo3")
outcomes_y2 <- c("laz_t3", "waz_t3", "whz_t3" ,"hcz_t3") 

t1 <- d[apply(select(d, all_of(exposures_m3)), 1, filtering),]
t1 <- t1[apply(select(t1, all_of(c(outcomes_m3, outcomes_y1, outcomes_y2))), 1, filtering),] # only has rows where we have both some exposure data and some outcome data
t2 <- d[apply(select(d, all_of(exposures_y1)), 1, filtering),] # only has rows where we have exposure data at t2
t2 <- t2[apply(select(t2, all_of(c(outcomes_y1, outcomes_y2))), 1, filtering),] # only has rows where we have both some exposure data and some outcome data
t3 <- d[apply(select(d, all_of(exposures_y2)), 1, filtering),] # only has rows where we have exposure data at t3
t3 <- t3[apply(select(t3, all_of(outcomes_y2)), 1, filtering),] # only has rows where we have both some exposure data and some outcome data
d <- t1 %>% full_join(t2, by=names(d)) %>% full_join(t3, by=names(d)) # has all children included in this analysis

# YOU SHOULDN'T HAVE TO CHANGE ANYTHING BELOW THIS EXCEPT THE PATH TO SAVE THE TABLE 
# UNLESS YOU WANT TO ADD ADDITIONAL CHARACTERISTICS TO THE ENROLLMENT TABLE
# TO ADD ADDITIONAL CHILD CHARACTERISTICS, PASS IN child_char = c(vector with variable names in the table) 
# and child_char_names = c(vector with names for the characteristics you want to appear in the table) 
# AS ADDITIONAL ARGUMENTS IN LINE 94 AND/OR mom_char and mom_char_names FOR ADDITIONAL MATERNAL CHARACTERISTICS
characteristics <- function(d, child_char = NULL, child_char_names = NULL, mom_char = NULL, mom_char_names = NULL) {
  nperc <- function(vector){
    n <- sum(vector==1, na.rm=T)
    perc <- round(n/sum(!is.na(vector))*100)
    paste(n, " (", perc, "%)", sep="")
  }
  
  mediqr <- function(vector){
    quantiles <- round(quantile(vector, na.rm=T), 2)
    paste(quantiles[3], " (", quantiles[2], ", ", quantiles[4], ")", sep="")
  }
  
  child <- c('sex',child_char,'laz_t1','waz_t1','whz_t1','hcz_t1',
             'laz_t2','waz_t2','whz_t2','hcz_t2',
             'laz_t3','waz_t3','whz_t3','hcz_t3')
  
  mom <- c('momage', mom_char, 'pss_score')
  
  n_med_col <- NULL
  for (var in c(child, mom)) {
    if (var %in% c('sex', 'diar7d_t2', 'diar7d_t3', 'life_viol_any_t3') | is.factor(d[[var]])) {
      if (var == 'sex') {
        n <- sum(d$sex==1, na.rm=T)
        perc <- round(n/sum(!is.na(d$sex))*100)
        n_med_col <- c(n_med_col, paste(n, " (", perc, "%)", sep=""))
      }else {
        d[[var]] <- na_if(d[[var]], "Missing")
        n_med_col <- c(n_med_col, nperc(d[[var]]))
      }
    }else {
      n_med_col <- c(n_med_col, mediqr(d[[var]]))
    }
  }
  
  tbl1 <- data.table("C1" = c("Child", rep("", length(child)-1),"Mother", rep("",length(mom)-1)),
                     "C2" = c("", rep("", length(child_char)), "Anthropometry (3 months, Year 1)","","","",
                              "Anthropometry (14 months, Year 1)","","","",
                              "Anthropometry (28 months, Year 2)","","","",
                              "Anthropometry at enrollment", rep("", length(mom_char)), "Perceived stress at Year 2"),
                     "C3" = c("Female", child_char_names,
                              "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
                              "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
                              "Length-for-age Z score", "Weight-for-age Z score", "Weight-for-length Z score", "Head circumference-for-age Z score",
                              "Age (years)", 
                              mom_char_names, "Perceived Stress Scale score"),
                     "C4" = n_med_col)
  
  tbl1flex <- flextable(tbl1, col_keys=names(tbl1))
  tbl1flex <- set_header_labels(tbl1flex,
                                values = list("C1" = "", "C2" = "", "C3" = "", "C4" = "n (%) or median (IQR)"))
  tbl1flex <- hline_top(tbl1flex, part="header", border=fp_border(color="black", width = 1))
  tbl1flex <- hline_bottom(tbl1flex, part="all", border=fp_border(color="black", width = 1))
  tbl1flex <- autofit(tbl1flex, part = "all")
  tbl1flex <- align(tbl1flex, j = c(1, 2, 3), align = "left", part="all")
  tbl1flex <- align(tbl1flex, j = 4, align = "center", part="all")
  tbl1flex <- fit_to_width(tbl1flex, max_width=8)
  tbl1flex
}

enroll <- characteristics(d)
sect_properties <- prop_section(
  page_size = page_size(orient = "portrait", width=8.5, height=11),
  page_margins = page_mar(bottom=.3, top=.3, right=.3, left=.3, gutter = 0)
)
save_as_docx("Table 1" = enroll, path=paste0(here(), "/Kenya/tables/enrollment_tbl_kenya.docx"), 
             pr_section = sect_properties) 
