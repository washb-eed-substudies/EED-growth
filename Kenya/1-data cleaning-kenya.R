rm(list=ls())
source(here::here("0-config.R"))

#install packages as needed
#install.packages("forcats")
#install.packages("SmartEDA")
#install.packages("ggpubr")
#install.packages("dplyr")
#install.packages("ggplot2")
#install.packages("haven")

#load libraries
library(forcats)
library(SmartEDA)
library(ggpubr)
library(dplyr)
library(ggplot2)
library(haven)

#read in datasets
#primary dataset
allkids <- readRDS(file=paste0(dropboxDir, "WBK-EE-analysis/Data/Cleaned/Andrew/WBK-EE-covariates.RDS"))
allkids <- read.csv(file = paste0(dropboxDir, "WASHB-Kenya-Data/1-primary-outcome-datasets/washb-kenya-enrol.csv"))
allkids$sex[allkids$sex == "Male"] <- "1"
allkids$sex <- as.numeric(allkids$sex)

#wealth data
wealth <- read.csv(file = paste0(dropboxDir, "WBK-EE-analysis/Data/Cleaned/Caitlin/Kenya-wealth-index.csv"))
wealth <- wealth %>% rename(HHwealth = HHwealth.PC1) %>% select(childid, clusterid, HHwealth)
d <- left_join(allkids, wealth, by = c("childid", "clusterid"))

#anthro data
anthro <- read.csv(file = paste0(dropboxDir, "WBK-EE-analysis/Data/Cleaned/Audrie/kenya-dm-ee-anthro-ee.csv"))
anthro <- anthro %>% select(-sex) %>% rename(month_at3 = month_t3)
d <- left_join(d, anthro, by = "childid")

#nulliparous
nulliparous <- read.csv(file = paste0(dropboxDir, "WBK-EE-analysis/Data/Cleaned/Caitlin/washb-kenya-allkids-ee-med-age-no-dob.csv"))
nulliparous <- nulliparous %>%
  mutate(birthord = ifelse(nulliparous == "yes", "First Born",
                    ifelse(nulliparous == "no", "Second Born or Greater", NA))) %>%
  select(childid, birthord)
d <- left_join(d, nulliparous, by = "childid")
  
#stool
stool <- read.csv(file = paste0(dropboxDir, "WBK-EE-analysis/Data/Cleaned/Andrew/washb-kenya-eed-stool.csv"))
stool <- stool %>%
  mutate(ln_aat1 = log(aat1), ln_aat2 = log(aat2), ln_aat3 = log(aat3),
         ln_mpo1 = log(mpo1), ln_mpo2 = log(mpo2), ln_mpo3 = log(mpo3),
         ln_neo1 = log(neo1), ln_neo2 = log(neo2), ln_neo3 = log(neo3)) %>%
  select(childid, aged1, aged2, aged3, month1, month2, month3, ln_aat1, ln_aat2, ln_aat3, 
         ln_mpo1, ln_mpo2, ln_mpo3, ln_neo1, ln_neo2, ln_neo3) %>%
  rename(ageday_st1 = aged1, ageday_st2 = aged2, ageday_st3 = aged3,
         month_st1 = month1, month_st2 = month2, month_st3 = month3)
d <- left_join(d, stool, by = "childid")

#urine
urine <- read.csv(file = paste0(dropboxDir, "WBK-EE-analysis/Data/Cleaned/Andrew/washb-kenya-eed-urine.csv"))
urine <- urine %>%
  mutate(ln_lact1 = log(Lact1), ln_lact2 = log(Lact2), ln_lact3 = log(Lact3), 
         ln_mann1 = log(Mann1), ln_mann2 = log(Mann2), ln_mann3 = log(Mann3)) %>%
  select(childid, aged1, aged2, aged3, month1, month2, month3, ln_lact1, ln_lact2, ln_lact3, 
         ln_mann1, ln_mann2, ln_mann3) %>%
  rename(ageday_ut1 = aged1, ageday_ut2 = aged2, ageday_ut3 = aged3,
         month_ut1 = month1, month_ut2 = month2, month_ut3 = month3)
d <- left_join(d, urine, by = "childid")

#select only kids who were sampled at least once
age.var <- c(grep(c("ageday_u"), names(d), value = TRUE), grep(c("ageday_s"), names(d), value = TRUE))
ages <- d %>% select(childid, all_of(age.var)) 
ages <- ages %>%
  mutate(missing = rowSums(is.na(ages))) %>%
  filter(missing != 6) %>%
  select(childid)

d <- d %>%
  filter(childid %in% ages$childid)

k_phq_pss <- read_csv(
  file = paste0(dropboxDir, "WBK-EE-analysis/Data/Cleaned/Caitlin/washk_phq_pss_20210203.csv"),
  col_types = list(phq_comments = 'c',
                   pss_comment = 'c',
                   phq1 = 'f', phq2 = 'f',
                   phq3 = 'f', phq4 = 'f', 
                   phq5 = 'f', phq6 = 'f', 
                   phq7 = 'f', phq8 = 'f', 
                   phq9 = 'f', 
                   pss_1 = 'i', pss_2 = 'i', 
                   pss_3 = 'i', pss_4 = 'i', 
                   pss_5 = 'i', pss_6 = 'i', 
                   pss_7 = 'i', pss_8 = 'i', 
                   pss_9 = 'i', pss_10 = 'i')) 

k_phq <- k_phq_pss %>% 
  select(childid, starts_with('phq'),
         -phq_comments) %>% 
  # only EED children
  filter(childid %in% d$childid) %>% 
  # create factored vars
  mutate(across(.cols = num_range('phq', 1:8), 
                .fns = ~ factor(., levels = c('0-1 day', '2-6 days', '7-11 days', '12-14 days'), 
                                ordered = TRUE)), 
         phq9 = factor(phq9, levels = c('Not difficult at all', 'Somewhat difficult', 
                                        'Very difficult', 'Extremely difficult'),
                       ordered = TRUE), 
         # to match PHQ-9 scale 
         across(.cols = starts_with('phq'), 
                .fns = ~ as.integer(.) - 1))

k_phq$sum_na <- apply(k_phq, 1, function(x) sum(is.na(x)))

missing <- cbind(childid = k_phq$childid, sum_na = k_phq$sum_na)

k_phq <- k_phq %>%
  filter(sum_na <= 1) %>%
  select(-sum_na)

k_phq <- cbind(k_phq[,1], t(apply(k_phq[,-1], 1, function(x) { x[is.na(x)] <- mean(x, na.rm=TRUE)
return(x)})))
# find quartiles 
# cut at each point
k_phq <- k_phq %>%
  mutate(phq_score_t3 = rowSums(k_phq[,-1]),
         phq_quartile_t3 = cut(phq_score_t3, 
                               breaks = quantile(phq_score_t3, probs = seq(0, 1, 0.25)),
                               include.lowest = TRUE,
                               labels = FALSE)) %>%
  select(childid, phq_quartile_t3, phq_score_t3)

missing <- as.data.frame(missing) %>%
  filter(as.numeric(sum_na) > 1) %>%
  mutate(phq_quartile_t3 = "Missing", phq_score_t3 = NA) %>%
  select(-sum_na)

k_phq <- rbind(k_phq, missing)
d <- left_join(d, k_phq, by = c("childid"))
# ----
# Perceived Stress Score
k_pss <- k_phq_pss %>% 
  select(childid, starts_with('pss'),
         -pss_comment, -pss_note1) %>% 
  filter(childid %in% d$childid) %>%
  mutate(across(.cols = starts_with('pss'), 
                .fns = ~ replace(., . > 4, NA_integer_)),
         across(.cols = c(pss_4, pss_5, pss_7, pss_8), 
                .funs = ~ abs(. - 4))) %>% 
  pivot_longer(cols = starts_with('pss')) %>% 
  group_by(childid) %>% 
  #filter(all(!is.na(value))) %>% # remove children without any answers
  summarize(pss_score = sum(value)) 

# bin scores
k_pss <- k_pss %>%
  mutate(pss_quartile_t3 = cut(k_pss$pss_score, 
                               breaks = quantile(k_pss$pss_score, probs = seq(0, 1, 0.25), na.rm = TRUE)),
         pss_quartile_t3 = ifelse(is.na(pss_quartile_t3), "Missing", pss_quartile_t3))

d <- left_join(d, k_pss, by = c("childid"))

#### covariate cleaning
covariates <- read.csv(file = paste0(dropboxDir, "WBK-EE-analysis/Data/Cleaned/Caitlin/EED-Growth Covariates - Kenya.csv"))
w.vars <- covariates[c(44:56),1]
time.cov <- covariates[,8:18]
time.cov <- as.vector(as.matrix(time.cov))
time.cov <- unique(time.cov)
time.cov <- time.cov[time.cov != ""]
w.vars <- c(w.vars, time.cov)

#create table of missingness and class of all variables
d2 <- d %>% select(any_of(w.vars))                 
miss <- data.frame(name = names(d2), missing = colSums(is.na(d2)), row.names = c(1:ncol(d2)))
for (i in 1:nrow(miss)) {
  miss$class[i] <- class(d2[,which(colnames(d2) == miss[i, 1])])
}

#visualize and inspect distributions
#ExpCatViz(d2)
#ExpNumViz(d2)

#remove/modify variables with no variation
w.vars <- w.vars[-which(w.vars == "floor")]
d <- d %>% select(-floor) %>%
  mutate(HHS = as.factor(HHS), 
         HHS = ifelse(HHS %in% c("2","3"), "=>2", ifelse(HHS == "9", NA, HHS)))

#keep age variables, birth order, and Nlt18 as numeric
num.var <- c("Nlt18", "dminwat", "Ncomp", grep("age", names(d2), value = TRUE))
for (i in num.var){
  d[[i]] <- as.numeric(d[[i]])
}

#collapse upper categories of birth order and Nlt18
d <- d %>%
  mutate(Nlt18 = ifelse(Nlt18 > 4 & !is.na(Nlt18), ">4", Nlt18))

#Z-score momheight and turn into a factor variable
d <- d %>%
  mutate(momheight = scale(momheight, center = TRUE, scale = TRUE),
         momheight = cut(momheight, 
                         c(min(momheight, na.rm = T), -2, -1, 0, 1, 2, max(momheight, na.rm = T)),
                         right = FALSE,
                         include.lowest = TRUE))

#redo missing table to get new classes
d2 <- d %>% select(any_of(w.vars))   
miss <- data.frame(name = names(d2), missing = colSums(is.na(d2)), row.names = c(1:ncol(d2)))
for (i in 1:nrow(miss)) {
  miss$class[i] <- class(d2[,which(colnames(d2) == miss[i, 1])])
}

#turn all integer and character variables into factors and create missing category
factor.var <- miss[which(miss$class %in% c("character","integer","factor")),1]
factor.var <- append(factor.var, grep("month", names(d2), value = TRUE))

for (i in factor.var) {
  d[[i]] <- as.factor(d[[i]])
}

#remove factor vars with small number of missingness to avoid data sparsity
factor.var <- factor.var[!factor.var %in% c("birthord", "HHS", "Nlt18")]
for (i in factor.var) {
  d[[i]] <- fct_explicit_na(d[[i]], "Missing")
}

#check new distributions
#ExpCatViz(d[,which(colnames(d) %in% colnames(d2))])

####growth vars
growth.var <- c("laz_t1", "laz_t2","laz_t3",
                "waz_t1","waz_t2","waz_t3",
                "hcz_t1","hcz_t2","hcz_t3")

#check distributions
#plots <- vector("list")
#for (i in growth.var) {
#  x <- d2[[i]]
#  name <- i
#  plots[[i]] <- ggplot(data = d2, aes(x = x)) +
#    geom_density() +
#    labs(x = paste0(name))
#}
#ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]])

#create separate growth covariate variables to categorize (need to do if your outcome is growth)
for (i in growth.var){
  d[paste(i,"_cat", sep="")] <- d[i] 
}
growth.cat <- grep("_cat", names(d), value = "TRUE")

#use meaningful cut points and create factor variable with missingness level
for (i in growth.cat) {
  cutpoints <- c(-2, -1, 0, 1)   #<-- change this row if needed
  cuts <- c(min(d[[i]], na.rm = T), cutpoints, max(d[[i]], na.rm = T))
  d[[i]] <- cut(d[[i]], 
                cuts,
                right = FALSE,
                include.lowest = TRUE)
  d[[i]] <- as.factor(d[[i]])
  d[[i]] <- fct_explicit_na(d[[i]], "Missing")
  d[[i]] <- factor(d[[i]], levels = levels(d[[i]]))
}

#recheck distribution of plots
#plots <- vector("list")
#for (i in growth.cat) {
#  x <- d[[i]]
#  name <- i
#  plots[[i]] <- ggplot(data = d, aes(x = x)) +
#    geom_bar() +
#    labs(x = paste0(name))
#}
#ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]])

#redo missing table to check missingness and classes
d2 <- d %>% select(all_of(names(d2)), all_of(growth.cat)) 
miss <- data.frame(name = names(d2), missing = colSums(is.na(d2)), row.names = c(1:ncol(d2)))
for (i in 1:nrow(miss)) {
  miss$class[i] <- class(d2[,which(colnames(d2) == miss[i, 1])])
}

d <- d %>%
  mutate(hcflag = ifelse(
childid %in% c(
201401201,
202104101,
202106101,
202212101,
210803101,
223504201,
223507101,
233601101,
234002101,
234007101,
234207101,
234512101,
235111101,
250104101,
253405101,
255006101,
258803101,
337503101,
337512101,
337802101,
337913101,
338605101,
340002101,
341612101,
341617101,
348912101,
349802101,
350204101,
350901301,
354306101,
354802101,
355205101,
355309101,
355402101,
355403101,
355408101,
355409101,
355410101,
355507101,
356307101,
356702101,
356704101,
356708101,
356709101,
357504101,
357507101,
357512101,
357513101,
357605101,
357801101,
357802101,
357803101,
357806101,
357809101,
357811101,
358803101,
360210101,
362205101,
362304101,
363804101,
364702101,
365207101,
365301101,
365302101,
366007101,
710905101,
728602101,
729302101,
729402101,
729601101,
729607101,
757002101,
757102101,
768104101,
780504101,
784004101,
784601101,
784604101,
784606101,
784807102,
784901101,
784908101,
785206101,
785208101,
785210101,
787301101,
335703101,
350203101,
350203102,
708103101,
757010201), 1, 0),
lenflag = ifelse(
  childid %in% c(
223501101,
234201101,
337008101,
340105101,
707203101,
763208101,
766703101,
766803101), 1, 0),
weiflag = ifelse(
  childid %in% c(
202601101,
202603101,
209301101,
210803101,
211906101,
223504201,
223506101,
228905101,
234205101,
235004101,
235108101,
235111101,
235114101,
236407101,
238503101,
239109101,
239114101,
250810101,
253402101,
337509101,
338605101,
341208101,
345511101,
347805101,
349802101,
355002101,
355009101,
355302101,
355409101,
355410101,
356708101,
357803101,
357809101,
359905101,
359905102,
361101101,
364101101,
763818101,
766603101,
766803101,
782103101,
783903101,
784601101,
784807102,
784908101,
787501101,
788404101,
788705101,
258604101,
753701101,
774506101,
775303101), 1, 0),
whflag = ifelse(lenflag == 1 | weiflag == 1, 1, 0))
