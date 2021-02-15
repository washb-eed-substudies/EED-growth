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
d <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Caitlin/kenya-dm-ee-anthro-ee.csv"))

colnames(d)
stool <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Andrew/washb-kenya-eed-stool.csv"))
d <- left_join(d, wealth, by = c("dataid", "clusterid", "block"))