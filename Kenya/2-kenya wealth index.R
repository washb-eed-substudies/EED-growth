#PCA for Kenya data
rm(list=ls())
source(here::here("0-config.R"))
library(dplyr)

kenya <- read.csv(file = paste0(dropboxDir, "WASHB-Kenya-Data/1-primary-outcome-datasets/washb-kenya-enrol.csv"))
  
assets <- kenya %>%
  select(childid, hhid, clusterid, cow, goat, chicken, dog,
         floor, roof, wall,
         electricity, radio, television, mobile, clock, bicycle, motorcycle,
         stove, gascook, car)

summary(assets)

#replace "Don't Know" with NA
for (i in 8:ncol(assets)) {
  assets[,i] = ifelse(assets[,i] == 9, NA, assets[,i])
}

#remove rows with mostly missing data
table(rowSums(is.na(assets)))
assets <- assets %>%
  mutate(missing = rowSums(is.na(assets))) %>%
  filter(missing < 14) %>%
  select(-missing)
table(rowSums(is.na(assets)))

#impute remaining missing values using median
assets.impute <- assets
for(i in 4:ncol(assets.impute)){
  assets.impute[is.na(assets.impute[,i]), i] <- median(assets.impute[,i], na.rm = TRUE)
}

#perform pca
pca <- prcomp(assets.impute[,4:ncol(assets.impute)], center = TRUE, scale = TRUE)
summary(pca)
pca$rotation[,1]

#visualize PC1 and PC2
#biplot(pca)

#predict HHwealth using 1st PC
assets.impute$HHwealth <- predict(pca)

wealth <- assets.impute %>%
  select(childid, hhid, clusterid, HHwealth)

#export data
write.csv(wealth, file = paste0(dropboxDir, "WBB-EE-analysis/Data/Cleaned/Caitlin/Kenya-wealth-index.csv"))
