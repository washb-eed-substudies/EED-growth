rm(list=ls())

source(here::here("1-data cleaning.R"))
#remotes::install_github('washb-eed-substudies/washbgam', force = TRUE)
library(washbgam)

#Make vectors of adjustment variable names
covariates <- read.csv(file = paste0(dropboxDir, "Data/Cleaned/Caitlin/EED-Growth Covariates - Bangladesh.csv"))

#baseline covariates
w.vars <- covariates[c(55:69),1]
w.vars <- w.vars[-12]

#timevarying covariates
time.cov <- covariates[c(1:40),c(2,8:19)]

#obtain unique adjustment sets
time.cov <- unique(time.cov)

#make blank cells NAs
time.cov <- time.cov %>% na_if("")

#create adjustment set vectors
cov.list <- vector('list', nrow(time.cov))
for(i in 1:length(cov.list)){
  vec <- as.character(time.cov[i,2:13]) # create vector using row i in columns with variable names
  vec <- append(vec, w.vars) #add baseline characteristics
  vec <- vec[!is.na(vec)] #remove any NAs (from blank cells)
  cov.list[[i]] <- vec #insert into list item i
}
names(cov.list) <- paste0('adjset', seq_along(cov.list)) #rename 

urine.t1 <- c("ln_L_conc_t1", "ln_M_conc_t1")
stool.t1 <- c("ln_mpo1", "ln_aat1", "ln_neo1")
urine.t2 <- c("ln_L_conc_t2", "ln_M_conc_t2")
stool.t2 <- c("ln_mpo2", "ln_aat2", "ln_neo2", "ln_reg2")
urine.t3 <- c("ln_L_conc_t3", "ln_M_conc_t3")
stool.t3 <- c("ln_mpo3", "ln_aat3", "ln_neo3")

all.growth.t1 <- c("laz_t1", "waz_t1", "hcz_t1", "whz_t1")
laz.waz.t1 <- c("laz_t1", "waz_t1")
hcz.t1 <- "hcz_t1"
whz.t1 <- "whz_t1"

laz.waz.t2 <- c("laz_t2", "waz_t2")
hcz.t2 <- "hcz_t2"
whz.t2 <- "whz_t2"

laz.waz.t3 <- c("laz_t3", "waz_t3")
hcz.t3 <- "hcz_t3"
whz.t3 <- "whz_t3"



V.set.t1 <- c("life_viol_any_t3")
V.set.t2 <- c("cesd_sum_t2", "life_viol_any_t3")
V.set.t3 <- c("pss_sum_mom_t3", "cesd_sum_ee_t3", "life_viol_any_t3")
V.set.t1.t2 <- c("cesd_sum_t2", "life_viol_any_t3")
V.set.t2.t3 <- c("pss_sum_mom_t3","cesd_sum_t2", "cesd_sum_ee_t3", "life_viol_any_t3")
V.set.t1.t3 <- c("pss_sum_mom_t3", "cesd_sum_t2", "cesd_sum_ee_t3", "life_viol_any_t3")

EMM_models <- NULL

#analysis 1
for(i in urine.t1){
  for(j in all.growth.t1){
    for (k in V.set.t1){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset1"]], V = k)
      res <- data.frame(X=i, Y=j,V=k, int.p =res_adj$int.p, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
      EMM_models <- bind_rows(EMM_models, res)
    }
  }
}



X=i
Y=j
W=cov.list[["adjset1"]]
V = k
forcedW=NULL
V=NULL
id="clusterid"
family = "gaussian"
pval = 0.2
print=TRUE

model <- res$fit[[1]]
ressum <- summary(model)

#make subgroup function like predict_gam_diff

fit <- res$fit[[1]]
d <- res$dat[[1]]
quantile_diff = c(0.25, 0.75)
Xvar = i
Yvar = j
Vvar = k 
binaryX = FALSE 




##Why is there no variation in V in the dataframe?

  set.seed(12345)
  require(mgcv)
  require(dplyr)
  d$dummy <- 0
  Wvars <- colnames(d)[!(colnames(d) %in% c("Y", "X", 
                                            "id", "dummy"))]
  for (i in Wvars) {
    if (class(d[, i]) == "character" | class(d[, i]) == 
        "factor") {
      d[, i] <- Mode(d[, i])
    }
    else {
      d[, i] <- median(d[, i])
    }
  }
  d <- d[order(d$X), ]
  
  if (binaryX == F) {
    q1 <- unname(quantile(d$X, quantile_diff[1]))
    q3 <- unname(quantile(d$X, quantile_diff[2]))
    q1_pos <- which(abs(d$X - q1) == min(abs(d$X - q1)))[1]
    q3_pos <- which(abs(d$X - q3) == min(abs(d$X - q3)))[1]
    d$X[q1_pos] <- q1
    d$X[q3_pos] <- q3
  }
  if (binaryX == T) {
    q1 <- min(d$X)
    q3 <- max(d$X)
    q1_pos <- 1
    q3_pos <- nrow(d)
    d$X[q1_pos] <- min(d$X)
    d$X[q3_pos] <- max(d$X)
  }
  preds <- predict(fit, newdata = d, type = "response")
  Xp <- predict(fit, newdata = d, type = "lpmatrix")
  Xp <- Xp[order(d$X), ]
  diff <- t(apply(Xp, 1, function(x) x - Xp[q1_pos, ]))
  point.diff <- diff %*% coef(fit)
  se.diff <- sqrt(diag(diff %*% vcov(fit) %*% t(diff)))
  lb.diff <- point.diff - 1.96 * se.diff
  ub.diff <- point.diff + 1.96 * se.diff
  Zval <- abs(point.diff/se.diff)
  Pval <- exp(-0.717 * Zval - 0.416 * Zval^2)
  plotdf <- data.frame(Y = Yvar, X = Xvar, N = nrow(d), q1 = d$X[q1_pos], 
                       q3 = d$X[q3_pos], pred.q1 = preds[q1_pos], pred.q3 = preds[q3_pos], 
                       point.diff, lb.diff = lb.diff, ub.diff = ub.diff, Pval = Pval)
  if (binaryX == T) {
    res <- plotdf[nrow(plotdf), ]
  }
  else {
    res <- plotdf[round(nrow(d) * quantile_diff[2], 0), ]
  }
  return(list(res = res, plotdf = plotdf))
}

#analysis 2
for(i in stool.t1){
  for(j in all.growth.t1){
    for (k in V.set.t1){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset2"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }
  }
}

#analysis 3
for(i in stool.t2){
  for(j in laz.waz.t2){
    for (k in V.set.t2){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset3"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }
  }
}

#analysis 4
for(i in urine.t2){
  for(j in laz.waz.t2){
    for (k in V.set.t2){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset4"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }
  }
}

#analysis 5
for(i in stool.t2){
  for(j in hcz.t2){
    for (k in V.set.t2){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset5"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 6
for(i in urine.t2){
  for(j in hcz.t2){
    for (k in V.set.t2){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset6"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 7
for(i in stool.t2){
  for(j in whz.t2){
    for (k in V.set.t2){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset7"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 8
for(i in urine.t2){
  for(j in whz.t2){
    for (k in V.set.t2){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset8"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 9
for(i in stool.t3){
  for(j in laz.waz.t3){
    for (k in V.set.t3){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset9"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 10
for(i in urine.t3){
  for(j in laz.waz.t3){
    for (k in V.set.t3){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset10"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 11
for(i in stool.t3){
  for(j in hcz.t3){
    for (k in V.set.t3){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset11"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 12
for(i in urine.t3){
  for(j in hcz.t3){
    for (k in V.set.t3){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset12"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 13
for(i in stool.t3){
  for(j in whz.t3){
    for (k in V.set.t3){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset13"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

#analysis 14
for(i in urine.t3){
  for(j in whz.t3){
    for (k in V.set.t3){
      print(i)
      print(j)
      print(k)
      res_adj <- fit_RE_gam(d=d, X=i, Y=j,  W=cov.list[["adjset14"]], V = k)
      res <- data.frame(X=i, Y=j, V=k, V = res_adj$int.p)
      EMM_models <- bind_rows(EMM_models, res)
    }  
  }
}

all.splits.EMM <- list(NA)
all.splits.EMM[[1]] <- EMM_models[1:20,]
all.splits.EMM[[2]] <- EMM_models[21:68,]
all.splits.EMM[[3]] <- EMM_models[69:128,]

total.EMM <- NA
for (i in 1:length(all.splits.EMM)) {
  all.splits.EMM[[i]] <- all.splits.EMM[[i]] %>%
    mutate(corrected.Pval=p.adjust(V.1, method="BH")) %>%
    as.data.frame()
  
  total.EMM <- rbind(total.EMM, all.splits.EMM[[i]])
}
total.EMM <- total.EMM[-1,]
total.EMM <- total.EMM %>% arrange(corrected.Pval)
