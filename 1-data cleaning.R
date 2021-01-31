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
d <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Caitlin/bangladesh-dm-ee-ee-growth-stool-urine-lab-covariates-anthro.csv"))

#join in wealth data (if necessary)
wealth <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Caitlin/real_ids_hhwealth_quart.csv"))
d <- left_join(d, wealth, by = c("dataid", "clusterid", "block"))

#create vector of names of adjustment variables - I am using the original table I made and distilling it down to a vector of unique names, but you can also manually type in all of the covariates that you need
covariates <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Caitlin/EED-Growth Covariates - Bangladesh.csv"))
w.vars <- covariates[43:57, 2]
time.cov <- covariates[,8:19]
time.cov <- as.vector(as.matrix(time.cov))
time.cov <- unique(time.cov)
time.cov <- time.cov[time.cov != ""]
w.vars <- c(w.vars, time.cov)

#create table of missingness and class of all variables
d2 <- d %>% select(all_of(w.vars))                       
miss <- data.frame(name = names(d2), missing = colSums(is.na(d2)), row.names = c(1:ncol(d2)))
for (i in 1:nrow(miss)) {
  miss$class[i] <- class(d2[,which(colnames(d2) == miss[i, 1])])
}

#visualize and inspect distributions
#ExpCatViz(d2)
#ExpNumViz(d2)

#keep age variables, birth order, and Nlt18 as numeric
age.var <- c("birthord", "Nlt18", grep("age", names(d2), value = TRUE))
for (i in age.var){
  d[[i]] <- as.numeric(d[[i]])
}

#collapse upper categories of birth order and Nlt18
d <- d %>%
  mutate(Nlt18 = ifelse(Nlt18 > 4 & !is.na(Nlt18), ">4", Nlt18))

d <- d %>%
  mutate(birthord = ifelse(birthord > 4 & !is.na(Nlt18), ">4", birthord))

#Z-score momheight and turn into a factor variable
d <- d %>%
  mutate(momheight = scale(momheight, center = TRUE, scale = TRUE),
         momheight = cut(momheight, 
                         c(min(momheight, na.rm = T), -2, -1, 0, 1, 2, max(momheight, na.rm = T)),
                        right = FALSE,
                        include.lowest = TRUE))

#redo missing table to get new classes
d2 <- d %>% select(all_of(w.vars))   
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
#ExpCatViz(d[,which(colnames(d) %in% colnames(d2))])

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
  d[paste(i,"_cov", sep="")] <- d[i] 
}
growth.cat <- grep("_cov", names(d), value = "TRUE")

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

