rm(list=ls())
source(here::here("0-config.R"))
library(tibble)

source(here::here("Bangladesh/1-data cleaning-bangladesh.R"))
exposures_m3 <- c("ln_L_conc_t1", "ln_M_conc_t1", "ln_mpo1", "ln_aat1", "ln_neo1")
outcomes_m3 <- c("laz_t1", "waz_t1", "whz_t1" ,"hcz_t1", "len_velocity_t1_t2", "wei_velocity_t1_t2", "hc_velocity_t1_t2", 
                 "len_velocity_t1_t3", "wei_velocity_t1_t3", "hc_velocity_t1_t3") 
exposures_y1 <- c("ln_L_conc_t2", "ln_M_conc_t2", "ln_mpo2", "ln_aat2", "ln_neo2", "ln_reg2")
outcomes_y1 <- c("laz_t2", "waz_t2", "whz_t2" ,"hcz_t2", "len_velocity_t2_t3", "wei_velocity_t2_t3", "hc_velocity_t2_t3") 
exposures_y2 <- c("ln_L_conc_t3", "ln_M_conc_t3","ln_mpo3", "ln_aat3", "ln_neo3")
outcomes_y2 <- c("laz_t3", "waz_t3", "whz_t3" ,"hcz_t3") 

#function for filtering for only participants with at least one outcome
filtering <- function(row){
  any(!is.na(row))}

m3_has_exposures<-d[apply(select(d, all_of(exposures_m3)), 1, filtering),]
m3_has_both<-m3_has_exposures[apply(select(m3_has_exposures, all_of(c(outcomes_m3, outcomes_y1, outcomes_y2))), 1, filtering),]
m3_clusters<-length(unique(m3_has_both$clusterid))
m3_n<-nrow(m3_has_both)

y1_has_exposures<-d[apply(select(d, all_of(exposures_y1)), 1, filtering),]
y1_has_both<-y1_has_exposures[apply(select(y1_has_exposures, all_of(c(outcomes_y1, outcomes_y2))), 1, filtering),]
y1_clusters<-length(unique(y1_has_both$clusterid))
y1_n<-nrow(y1_has_both)

y2_has_exposures <- d[apply(select(d, all_of(exposures_y2)), 1, filtering),]
y2_has_both<-y2_has_exposures[apply(select(y2_has_exposures, all_of(outcomes_y2)), 1, filtering),]
y2_clusters<-length(unique(y2_has_both$clusterid))
y2_n<-nrow(y2_has_both)

data <- tibble(x = -10:100, y= -10:100)
head(data)

data %>% 
  ggplot(aes(x, y)) +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(-10, 100, 10)) +
  theme_void() ->
  p

p +
  geom_rect(xmin = 25, xmax=75, ymin=96, ymax=100, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=98,label= '13,279 compounds assessed for eligibility', size=3) ->
  p

p +
  geom_rect(xmin = 54, xmax=92, ymin=84, ymax=94, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 73, y=89, label= 'Excluded: 7,728 compounds \n 7,429 compounds excluded to create buffer zones\n 219 compounds did not meet enrollment criteria\n 80 compounds declined to participate', size=3) +
  annotate('text', x= 10, y=89,label= 'Enrollment', size=4) +
  
  
  geom_rect(xmin = 20, xmax=80, ymin=76, ymax=82, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=79,label= '720 clusters created and randomly allocated across 7 arms \n 5,551 compounds randomly allocated across 7 arms \n 2 of 7 arms selected into substudy', size=3)  +
  annotate('text', x= 10, y=79,label= 'Allocation', size=4) +
  
  geom_rect(xmin = 27, xmax=73, ymin=64, ymax=74, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=69,label= "paste(bold('                   Control arm, Nutrition arm, \n     Water + Sanitation + Handwashing arm and\nNutrition + Water + Sanitation + Handwashing arm'))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=68.5,label= paste('\n\n', 180+90+90+90, ' clusters \n', 1382+702+699+686, ' households', sep=""), size=3) +
  annotate('text', x= 10, y=63,label= 'Subsample Target', size=4) +
  
  
  geom_rect(xmin = 76, xmax=96, ymin=57, ymax=69, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 86, y=65.8,label= "paste(bold('Number of clusters not \n selected into substudy'))", parse=TRUE, size=3) + 
  annotate('text', x= 86, y=64.4,label= "paste(bold('Month 3'))", parse=TRUE, size=3) + 
  annotate('text', x= 86, y=63.1,label= paste(180-54+90-55+90-54+90-54, ' clusters', sep=""), size=2.5) + 
  annotate('text', x= 86, y=61.9,label= "paste(bold('Month 14'))", parse=TRUE, size=3) + 
  annotate('text', x= 86, y=60.7,label= paste(180-68+90-64+90-63+90-63, ' clusters', sep=""), size=2.5) + 
  annotate('text', x= 86, y=59.5,label= "paste(bold('Month 28'))", parse=TRUE, size=3) + 
  annotate('text', x= 86, y=58.3,label= paste(180-68+90-66+90-66+90-67, ' clusters', sep=""), size=2.5) +
  
  geom_rect(xmin = 42, xmax=58, ymin=45, ymax=62, color='black',
            fill='white', size=0.25) +
  annotate('text', x=50, y=61,label= "paste(bold('Month 3'))", parse=TRUE, size=3) + 
  annotate('text', x=50, y=60,label= paste('\n\n', 54+55+54+54, ' clusters \n', 406+425+408+406, ' children', sep=""), size=3) +
  annotate('text', x=50, y=55.5,label= "paste(bold('Month 14'))", parse=TRUE, size=3) + 
  annotate('text', x=50, y=54.5,label= paste('\n\n', 68+64+63+63, ' clusters \n', 516+496+486+480, ' children', sep=""), size=3) +
  annotate('text', x=50, y=50,label= "paste(bold('Month 28'))", parse=TRUE, size=3) + 
  annotate('text', x=50, y=49,label= paste('\n\n', 68+66+66+67, ' clusters \n', 516+514+514+505, ' children', sep=""), size=3) +
  
  
  geom_rect(xmin = 37, xmax=63, ymin=3, ymax=43, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=41.9,label= "paste(bold('Month 3'))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=38,label= paste('\n\n',120+135+131+127,' children lost to follow-up \n',4+2+4,' moved \n',63+93+81+89,' absent \n',16+1+3+1,' withdrew \n',22+23+27+32,' no live birth \n',15+16+16+5,' child death ', sep=""), size=2.5) + 
  annotate('text', x= 50, y=30,label= "paste(bold('Month 14'))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=24.3,label= paste('\n',110+71+78+74,' new children measured \n',140+101+107+100,' children lost to follow-up \n',14+13+17+9,' moved \n',16+29+17+14,' absent \n',62+17+23+14,' withdrew \n',29+30+32+37,' no live birth \n',19+24+21+11,' child death ', sep=""), size=2.5) + 
  annotate('text', x= 50, y=16.3,label= "paste(bold('Month 28'))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=10.6,label= paste('\n',25+18+28,' new children measured \n',158+104+115+142,' children lost to follow-up \n',35+22+36+28,' moved \n',3+4+9+2,' absent \n',72+30+42+18,' withdrew \n',29+32+35+38,' no live birth \n',19+27+20+18,' child death ', sep=""), size=2.5) + 
  annotate('text', x= 10, y=22.5,label= 'Follow-up', size=4) +
  
  geom_rect(xmin = 42, xmax=58, ymin=-15, ymax=1, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=-.2,label= "paste(bold('Month 3'))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=-.8,label= paste('\n\n',54+55+54+54,' clusters \n',286+290+277+279,' children', sep=""), size=3) + 
  annotate('text', x= 50, y=-5.1,label= "paste(bold('Month 14'))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=-5.7,label= paste('\n\n',68+64+63+63,' clusters \n',376+395+379+380,' children', sep=""), size=3) + 
  annotate('text', x= 50, y=-10,label= "paste(bold('Month 28'))", parse=TRUE, size=3) +
  annotate('text', x= 50, y=-10.8,label= paste('\n\n',68+66+66+67,' clusters \n',358+399+372+401,' children', sep=""), size=3) + 
  annotate('text', x= 10, y=-6,label= 'Subsample Enrollment', size=4) +
  
  
  geom_rect(xmin = 36, xmax=64, ymin=-28, ymax=-17, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=-18.2,label= "paste(bold('Month 3'))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=-18.8,label= paste("\n", 286+290+277+279-m3_n, ' missing exposure or outcome', sep=""), size=3) + 
  annotate('text', x= 50, y=-21.4,label= "paste(bold('Month 14'))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=-22,label= paste("\n", 376+395+379+380-y1_n, ' missing exposure or outcome', sep=""), size=3) + 
  annotate('text', x= 50, y=-24.6,label= "paste(bold('Month 28'))", parse=TRUE, size=3) + 
  annotate('text', x= 50, y=-25.2,label= paste("\n", 358+399+372+401-y2_n, ' missing exposure or outcome', sep=""), size=3) + 
  annotate('text', x= 10, y=-19.5,label= 'Specimen Collection', size=4) +
  
  
  geom_rect(xmin = 42, xmax=58, ymin=-47, ymax=-30, color='black',
            fill='white', size=0.25) +
  annotate('text', x= 50, y=-31.2,label= "paste(bold('Month 3'), sep='')", parse=T, size=3) +
  annotate('text', x= 50, y=-31.9,label= paste("\n\n", m3_clusters, ' clusters \n ', m3_n, ' children', sep=''), size=3) +
  annotate('text', x= 50, y=-36.5,label= "paste(bold('Month 14'), sep='')", parse=T, size=3) +
  annotate('text', x= 50, y=-37.2,label= paste("\n\n", y1_clusters, ' clusters \n ', y1_n, ' children', sep=''), size=3) +
  annotate('text', x= 50, y=-41.7,label= "paste(bold('Month 28'), sep='')", parse=T, size=3) +
  annotate('text', x= 50, y=-42.4,label= paste("\n\n", y2_clusters, ' clusters \n ', y2_n, ' children', sep=''), size=3) +
  annotate('text', x= 10, y=-38.5,label= 'Analysis', size=4) ->
  p

p +
  geom_segment(
    x=50, xend=50, y=96, yend=82, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=54, y=89, yend=89, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=76, yend=74, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=64, yend=62, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=76, y=63, yend=63, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=45, yend=43, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=3, yend=1, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=-15, yend=-17, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(
    x=50, xend=50, y=-28, yend=-30, 
    size=0.15, linejoin = "mitre", lineend = "butt",
    arrow = arrow(length = unit(1, "mm"), type= "closed")) ->
  p

# YOU MAY NEED TO CHANGE THE FILE PATHS HERE
ggsave(p, file = here("Bangladesh/plots/enrollment_figure1.jpg"), height=14, width=9)


