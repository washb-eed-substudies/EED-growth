rm(list=ls())
source(here::here("0-config.R"))

#install packages as needed
#install.packages("forcats")
#install.packages("SmartEDA")
#install.packages("ggpubr")
#install.packages("dplyr")
#install.packages("ggplot2")

#load libraries
library(forcats)
library(SmartEDA)
library(ggpubr)
library(dplyr)
library(ggplot2)

#read in dataset
d <- read.csv(file = paste0(dropboxDir, "WBB-EE-analysis/Data/Cleaned/Caitlin/bangladesh-dm-ee-ee-growth-stool-urine-lab-covariates-anthro.csv"))

#join in wealth data (if necessary)
wealth <- read.csv(file = paste0(dropboxDir, "WBB-EE-analysis/Data/Cleaned/Audrie/hhwealth.csv"))
d <- left_join(d, wealth, by = c("dataid", "clusterid", "block"))

#create vector of names of adjustment variables - I am using the original table I made and distilling it down to a vector of unique names, but you can also manually type in all of the covariates that you need
covariates <- read.csv(file = paste0(dropboxDir, "WBB-EE-analysis/Data/Cleaned/Caitlin/EED-Growth Covariates - Bangladesh.csv"), na.strings=c(""))

baseline.cov <- covariates[c(44:58),1]
baseline.cov <- baseline.cov[!is.na(baseline.cov)]

time.cov <- covariates[,8:19]
time.cov <- as.vector(as.matrix(time.cov))
time.cov <- unique(time.cov)
time.cov <- time.cov[!is.na(time.cov)]

w.vars <- c(baseline.cov, time.cov)
w.vars[!(w.vars %in% colnames(d))]

w.vars <- w.vars[!w.vars %in% (grep("cat", w.vars, value = TRUE))]

#create table of missingness and class of all variables
d2 <- d %>% select(any_of(w.vars))                 
miss <- data.frame(name = names(d2), missing = colSums(is.na(d2)), row.names = c(1:ncol(d2)))
for (i in 1:nrow(miss)) {
  miss$class[i] <- class(d2[,which(colnames(d2) == miss[i, 1])])
}

#visualize and inspect distributions
#ExpCatViz(d2)
#ExpNumViz(d2)

#keep age variables, birth order, and Nlt18 as numeric
num.var <- c("birthord", "Nlt18", grep("age", names(d2), value = TRUE), 
             "cesd_sum_ee_t3", "cesd_sum_t2", "pss_sum_mom_t3")
for (i in num.var){
  d[[i]] <- as.numeric(d[[i]])
}

#collapse upper categories of birth order and Nlt18
d <- d %>%
  mutate(Nlt18 = ifelse(Nlt18 > 4 & !is.na(Nlt18), ">4", Nlt18))

d <- d %>%
  mutate(birthord = ifelse(birthord == 1 & !is.na(birthord), "First Born", 
                           ifelse(birthord > 1 & !is.na(birthord), "Secord Born or Higher", 
                                  birthord)))

#Z-score momheight and turn into a factor variable
d <- d %>%
  mutate(momheight = scale(momheight, center = TRUE, scale = TRUE),
         momheight = cut(momheight, 
                         c(min(momheight, na.rm = T), -2, -1, 0, 1, 2, max(momheight, na.rm = T)),
                        right = FALSE,
                        include.lowest = TRUE))

#convert pss and cesd at t3 to quantiles
pss.quartile <- quantile(d$pss_sum_mom_t3, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
d$pss_sum_mom_t3 <- as.character(cut(d$pss_sum_mom_t3, breaks = pss.quartile))

cesd.quartile <- quantile(d$cesd_sum_ee_t3, probs = c(0, 0.25, 0.5, 0.75, 1), na.rm = TRUE)
d$cesd_sum_ee_t3 <- as.character(cut(d$cesd_sum_ee_t3, breaks = cesd.quartile))

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
  d[[i]] <- fct_explicit_na(d[[i]], "Missing")
}


#check new distributions
ExpCatViz(d[,which(colnames(d) %in% colnames(d2))])

#remove variables with no variation
d <- d %>% select(-roof)
w.vars <- w.vars[-which(w.vars == "roof")]

####growth vars
growth.var <- grep("z_", names(d2), value = "TRUE")

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
growth.cat <- growth.cat[-1]

#use meaningful cut points and create factor variable with missingness level
for (i in growth.cat) {
  cutpoints <- c(-3, -2, -1, 0)   #<-- change this row if needed
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
#  x <- d2[[i]]
#  name <- i
#  plots[[i]] <- ggplot(data = d2, aes(x = x)) +
#    geom_bar() +
#    labs(x = paste0(name))
#}
#ggarrange(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]])

#redo missing table to check missingness and classes
d2 <- d %>% select(all_of(w.vars), all_of(growth.cat)) 
miss <- data.frame(name = names(d2), missing = colSums(is.na(d2)), row.names = c(1:ncol(d2)))
for (i in 1:nrow(miss)) {
  miss$class[i] <- class(d2[,which(colnames(d2) == miss[i, 1])])
}

