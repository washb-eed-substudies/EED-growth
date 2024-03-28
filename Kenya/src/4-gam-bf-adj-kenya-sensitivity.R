
source(here::here("0-config.R"))

d<-read.csv("/Users/sophiatan/Documents/WASH/kenya data2.csv") %>% select(!X)
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

library(readr)
bl <- read_dta("/Users/sophiatan/Downloads/infant feeding modules/ee_bl_append_ID_clean_infant_20180221.dta")
ml <- read_dta("/Users/sophiatan/Downloads/infant feeding modules/ee_ml_append_ID_clean_infant_20180221.dta")
bf_full <- bl %>% select(childid, c_605) %>%
  full_join(ml %>% select(childid, c_605), by="childid") %>% 
  rename("bf_t1" = "c_605.x", "bf_t2" = "c_605.y")

d <- d %>% left_join(bf_full, "childid")

d <- d %>% mutate(bf_t1 = factor(bf_t1, labels = c("Breastfeeding", "Weaned")),
                  bf_t2 = factor(bf_t2, labels=c("Breastfeeding", "Weaned")))

library(washbgam)

add_bf <- function(x, y, W){
  if(grepl("1", x)|grepl("1",y)) {
    W <- c(W, "bf_t1")
  }
  if(grepl("2",x)|grepl("2",y)) {
    W <- c(W, "bf_t2")
  }
  W
}

outliers <- function(j, data){
  if (j %in% c("laz_t1", "laz_t2", "laz_t3", "len_velocity_t1_t2", 
               "len_velocity_t1_t3", "len_velocity_t2_t3")){
    dfunc <- data %>% filter(lenflag != 1)
  } else if (j %in% c("waz_t1", "waz_t2", "waz_t3",  "wei_velocity_t1_t2", 
                      "wei_velocity_t1_t3", "wei_velocity_t2_t3")){
    dfunc <- data %>% filter(weiflag != 1)
  } else if (j %in% c("hcz_t1", "hcz_t2", "hcz_t3",  "hc_velocity_t1_t2", 
                      "hc_velocity_t1_t3", "hc_velocity_t2_t3")){
    dfunc <- data %>% filter(hcflag != 1)
  } else if (j %in% c("whz_t1", "whz_t2", "whz_t3")){
    dfunc <- data %>% filter(whflag != 1)
  } 
  return(dfunc)
}

#Make vectors of adjustment variable names
covariates <- read.csv(file = paste0(dropboxDir, "WBK-EE-analysis/Data/Cleaned/Caitlin/EED-Growth Covariates - Kenya.csv"))

#baseline covariates
w.vars <- covariates[c(44:56),1]
w.vars <- w.vars[-10] #remove floor which has no variation

#timevarying covariates
time.cov <- covariates[c(1:40),c(2,8:18)]

#obtain unique adjustment sets
time.cov <- unique(time.cov)

#make blank cells NAs
time.cov <- time.cov %>% na_if("")

#create adjustment set vectors
cov.list <- vector('list', nrow(time.cov))
for(i in 1:length(cov.list)){
  vec <- as.character(time.cov[i,2:12]) # create vector using row i in columns with variable names
  vec <- append(vec, w.vars) #add baseline characteristics
  vec <- vec[!is.na(vec)] #remove any NAs (from blank cells)
  cov.list[[i]] <- vec #insert into list item i
}
names(cov.list) <- paste0('adjset', seq_along(cov.list)) #rename 

urine.t1 <- c("ln_lact1", "ln_mann1")
stool.t1 <- c("ln_mpo1", "ln_aat1", "ln_neo1")
urine.t2 <- c("ln_lact2", "ln_mann2")
stool.t2 <- c("ln_mpo2", "ln_aat2", "ln_neo2")
urine.t3 <- c("ln_lact3", "ln_mann3")
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

velo.t1.t2 <- c("len_velocity_t1_t2", "wei_velocity_t1_t2", "hc_velocity_t1_t2")
velo.t1.t3 <- c("len_velocity_t1_t3", "wei_velocity_t1_t3", "hc_velocity_t1_t3")
velo.t2.t3 <- c("len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3")

###### Analysis
gam.analysis <- function(Xvar = NULL, Yvar = NULL, Wvar = NULL){
  for(i in Xvar){
    for(j in Yvar){
      print(i)
      print(j)
      dfunc <- outliers(j, d)
      res_adj <- fit_RE_gam(d=dfunc, X=i, Y=j,  W=add_bf(i,j,Wvar))
      res <- data.frame(X=i, Y=j, fit=I(list(res_adj$fit)), dat=I(list(res_adj$dat)))
      all_models <- bind_rows(all_models, res)
    }
  }
  return(all_models)
}

all_models <- NULL
#analysis numbers are from covariates spreadsheet
all_models <- gam.analysis(Xvar = stool.t1, Yvar = all.growth.t1, W = cov.list[["adjset1"]]) #1
all_models <- gam.analysis(Xvar = urine.t1, Yvar = all.growth.t1, W = cov.list[["adjset2"]]) #2
all_models <- gam.analysis(Xvar = stool.t2, Yvar = laz.waz.t2, W = cov.list[["adjset3"]]) #3
all_models <- gam.analysis(Xvar = urine.t2, Yvar = laz.waz.t2, W = cov.list[["adjset4"]]) #4
all_models <- gam.analysis(Xvar = stool.t2, Yvar = hcz.t2, W = cov.list[["adjset5"]]) #5
all_models <- gam.analysis(Xvar = urine.t2, Yvar = hcz.t2, W = cov.list[["adjset6"]]) #6
all_models <- gam.analysis(Xvar = stool.t2, Yvar = whz.t2, W = cov.list[["adjset7"]]) #7
all_models <- gam.analysis(Xvar = urine.t2, Yvar = whz.t2, W = cov.list[["adjset8"]]) #8
all_models <- gam.analysis(Xvar = stool.t3, Yvar = laz.waz.t3, W = cov.list[["adjset9"]]) #9
all_models <- gam.analysis(Xvar = urine.t3, Yvar = laz.waz.t3, W = cov.list[["adjset10"]]) #10
all_models <- gam.analysis(Xvar = stool.t3, Yvar = hcz.t3, W = cov.list[["adjset11"]]) #11
all_models <- gam.analysis(Xvar = urine.t3, Yvar = hcz.t3, W = cov.list[["adjset12"]]) #12
all_models <- gam.analysis(Xvar = stool.t3, Yvar = whz.t3, W = cov.list[["adjset13"]]) #13
all_models <- gam.analysis(Xvar = urine.t3, Yvar = whz.t3, W = cov.list[["adjset14"]]) #14
all_models <- gam.analysis(Xvar = stool.t1, Yvar = laz.waz.t2, W = cov.list[["adjset15"]]) #15
all_models <- gam.analysis(Xvar = urine.t1, Yvar = laz.waz.t2, W = cov.list[["adjset16"]]) #16
all_models <- gam.analysis(Xvar = stool.t1, Yvar = hcz.t2, W = cov.list[["adjset17"]]) #17
all_models <- gam.analysis(Xvar = urine.t1, Yvar = hcz.t2, W = cov.list[["adjset18"]]) #18
all_models <- gam.analysis(Xvar = stool.t1, Yvar = whz.t2, W = cov.list[["adjset19"]]) #19
all_models <- gam.analysis(Xvar = urine.t1, Yvar = whz.t2, W = cov.list[["adjset20"]]) #20
all_models <- gam.analysis(Xvar = stool.t1, Yvar = laz.waz.t3, W = cov.list[["adjset21"]]) #21
all_models <- gam.analysis(Xvar = urine.t1, Yvar = laz.waz.t3, W = cov.list[["adjset22"]]) #22
all_models <- gam.analysis(Xvar = stool.t1, Yvar = hcz.t3, W = cov.list[["adjset23"]]) #23
all_models <- gam.analysis(Xvar = urine.t1, Yvar = hcz.t3, W = cov.list[["adjset24"]]) #24
all_models <- gam.analysis(Xvar = stool.t1, Yvar = whz.t3, W = cov.list[["adjset25"]]) #25
all_models <- gam.analysis(Xvar = urine.t1, Yvar = whz.t3, W = cov.list[["adjset26"]]) #26
all_models <- gam.analysis(Xvar = stool.t2, Yvar = laz.waz.t3, W = cov.list[["adjset27"]]) #27
all_models <- gam.analysis(Xvar = urine.t2, Yvar = laz.waz.t3, W = cov.list[["adjset28"]]) #28
all_models <- gam.analysis(Xvar = stool.t2, Yvar = hcz.t3, W = cov.list[["adjset29"]]) #29
all_models <- gam.analysis(Xvar = urine.t2, Yvar = hcz.t3, W = cov.list[["adjset30"]]) #30
all_models <- gam.analysis(Xvar = stool.t2, Yvar = whz.t3, W = cov.list[["adjset31"]]) #31
all_models <- gam.analysis(Xvar = urine.t2, Yvar = whz.t3, W = cov.list[["adjset32"]]) #32
all_models <- gam.analysis(Xvar = stool.t1, Yvar = velo.t1.t2, W = cov.list[["adjset19"]]) #33
all_models <- gam.analysis(Xvar = urine.t1, Yvar = velo.t1.t2, W = cov.list[["adjset20"]]) #34
all_models <- gam.analysis(Xvar = stool.t1, Yvar = velo.t1.t3, W = cov.list[["adjset25"]]) #35
all_models <- gam.analysis(Xvar = urine.t1, Yvar = velo.t1.t3, W = cov.list[["adjset26"]]) #36
all_models <- gam.analysis(Xvar = stool.t1, Yvar = velo.t2.t3, W = cov.list[["adjset33"]]) #37
all_models <- gam.analysis(Xvar = urine.t1, Yvar = velo.t2.t3, W = cov.list[["adjset34"]]) #38
all_models <- gam.analysis(Xvar = stool.t2, Yvar = velo.t2.t3, W = cov.list[["adjset31"]]) #39
all_models <- gam.analysis(Xvar = urine.t2, Yvar = velo.t2.t3, W = cov.list[["adjset32"]]) #40


#bind all together
all_models_res <- NULL
for(i in 1:nrow(all_models)){
  res <- data.frame(X=all_models$X[i], Y=all_models$Y[i])
  preds <- predict_gam_diff(fit=all_models$fit[i][[1]], d=all_models$dat[i][[1]], quantile_diff=c(0.25,0.75), Xvar=res$X, Yvar=res$Y)
  gamm_diff_res <- preds$res
  all_models_res <-  bind_rows(all_models_res, gamm_diff_res)
}

#BH procedure
all.splits <- list(NA)
all.splits[[1]] <- all_models_res[1:20,]
all.splits[[2]] <- all_models_res[21:40,]
all.splits[[3]] <- all_models_res[41:60,]
all.splits[[4]] <- all_models_res[61:100,]
all.splits[[5]] <- all_models_res[101:120,]
all.splits[[6]] <- all_models_res[121:165,]
all.splits[[7]] <- all_models_res[166:nrow(all_models_res),]

total <- NA
for (i in 1:length(all.splits)) {
  all.splits[[i]] <- all.splits[[i]] %>%
    mutate(corrected.Pval=p.adjust(Pval, method="BH")) %>%
    as.data.frame()
  
  total <- rbind(total, all.splits[[i]])
}
total <- total[-1,]

H1_adj_res_BH <- total[1:60,]
H2_adj_res_BH <- total[61:100,]
H3_adj_res_BH <- total[101:120,]
H4_adj_res_BH <- total[121:165,]
H5_adj_res_BH <- total[166:nrow(total),]


saveRDS(total, here("Kenya/results/adjusted/bf/all_results.RDS"))
saveRDS(H1_adj_res_BH, here("Kenya/results/adjusted/bf/H1_res.RDS"))
saveRDS(H2_adj_res_BH, here("Kenya/results/adjusted/bf/H2_res.RDS"))
saveRDS(H3_adj_res_BH, here("Kenya/results/adjusted/bf/H3_res.RDS"))
saveRDS(H4_adj_res_BH, here("Kenya/results/adjusted/bf/H4_res.RDS"))
saveRDS(H5_adj_res_BH, here("Kenya/results/adjusted/bf/H5_res.RDS"))
