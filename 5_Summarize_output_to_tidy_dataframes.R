
# table output and generation ---------------------------------------------

############# consider if this is necessary. since everything is graphed.
############# figure out which tables should be produced and in what format
library(jagsUI)
library(tidyverse)
library(ggmcmc)
library(tidybayes)
library(ggrepel)
library(ggforce)

### caste level summaries
### full summaries
### harvest, activity, total group-level and species-level
### age-sex summaries
### age-sex raw data for website - 

Y <- 2021
FY = 1976
years <- FY:Y

names(years) <- paste(years)

prov_zone <- read.csv("data/Province and zone table.csv")
alt_regs <- prov_zone %>% 
  select(prov,zone,region) 

#load("national_provincial_summaries.RData")


# Simple estimates effort and groups --------------------------------------


load("national_provincial_summaries1.RData")

tmp_sim <- tmp_sim %>% 
  left_join(.,alt_regs,
            by = c("prov","zone"))
  

zone_sums_b <- tmp_sim %>%
  group_by(var,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,prov,zone,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = quantile(sum,0.025,names = FALSE,na.rm = T),
            uci = quantile(sum,0.975,names = FALSE,na.rm = T))


reg_sums_b <- tmp_sim %>%
  filter(region != "") %>% 
  group_by(var,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,region,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = quantile(sum,0.025,names = FALSE,na.rm = T),
            uci = quantile(sum,0.975,names = FALSE,na.rm = T))



prov_sums_b <- tmp_sim %>%
  group_by(var,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = quantile(sum,0.025,names = FALSE,na.rm = T),
            uci = quantile(sum,0.975,names = FALSE,na.rm = T))


nat_sums_b <- tmp_sim %>%
  group_by(var,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = quantile(sum,0.025,names = FALSE,na.rm = T),
            uci = quantile(sum,0.975,names = FALSE,na.rm = T))


save(list = c("nat_sums_b",
              "reg_sums_b",
              "prov_sums_b",
              "zone_sums_b"),
     file = "data/Posterior_summaries1.RData")



rm(list = c("tmp_sim",
            "nat_sums_b",
   "reg_sums_b",
   "prov_sums_b",
   "zone_sums_b"))





# Species summaries -------------------------------------------------------


load("national_provincial_summaries2.RData")

tmp_sp <- tmp_sp %>% 
  left_join(.,alt_regs,
            by = c("prov","zone"))


zone_sums_a <- tmp_sp %>%
  group_by(AOU,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,prov,zone,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

prov_sums_a <- tmp_sp %>%
  group_by(AOU,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

reg_sums_a <- tmp_sp %>%
  filter(region != "") %>% 
  group_by(AOU,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,region,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))


nat_sums_a <- tmp_sp %>%
  group_by(AOU,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))




save(list = c("nat_sums_a",
              "prov_sums_a",
              "reg_sums_a",
              "zone_sums_a"),
     file = "data/Posterior_summaries2.RData")
rm(list = c("tmp_sp",
            "nat_sums_a",
            "prov_sums_a",
            "reg_sums_a",
            "zone_sums_a"))




# age ratios --------------------------------------------------------------


load(paste0("national_provincial_summaries3.RData"))


nat_sums_c <- tmp_sp_demo %>%
  group_by(AOU,BAGE,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,year,.draw) %>%
  summarise(rati = I/A) %>% 
  group_by(AOU,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = quantile(rati,0.025,names = FALSE),
            uci = quantile(rati,0.975,names = FALSE))

nat_sums_cF <- tmp_sp_demo %>%
  filter(BSEX %in% c("F")) %>% 
  group_by(AOU,BAGE,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,year,.draw) %>%
  summarise(rati = I/A) %>% 
  group_by(AOU,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = quantile(rati,0.025,names = FALSE),
            uci = quantile(rati,0.975,names = FALSE))


nat_sums_c2 <- tmp_sp_demo %>%
  filter(BSEX %in% c("F","M")) %>% 
  group_by(AOU,BSEX,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,year,.draw) %>%
  pivot_wider(names_from = BSEX,
              values_from = sum) %>% 
  group_by(AOU,year,.draw) %>%
  summarise(rati = F/M) %>% 
  group_by(AOU,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = quantile(rati,0.025,names = FALSE),
            uci = quantile(rati,0.975,names = FALSE))

nat_sums_asxy <- tmp_sp_demo %>%
  group_by(AOU,BSEX,BAGE,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,BAGE,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))
nat_sums_ag <- tmp_sp_demo %>%
  group_by(AOU,BAGE,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BAGE,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

nat_sums_sx <- tmp_sp_demo %>%
  group_by(AOU,BSEX,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

rm("tmp_sp_demo")



prov_sums_cF <- NULL
reg_sums_cF <- NULL
zone_sums_cF <- NULL
prov_sums_c <- NULL
reg_sums_c <- NULL
zone_sums_c <- NULL
prov_sums_c2 <- NULL
reg_sums_c2 <- NULL
prov_sums_asxy <- NULL
reg_sums_asxy <- NULL
zone_sums_asxy <- NULL
prov_sums_ag <- NULL
reg_sums_ag <- NULL
zone_sums_ag <- NULL
prov_sums_sx <- NULL
reg_sums_sx <- NULL
zone_sums_sx <- NULL

for(suf in c("a","b","c")){
  
load(paste0("national_provincial_summaries3_",suf,".RData"))


tmp_sp_demo <- tmp_sp_demo1 %>% 
  left_join(.,alt_regs,
            by = c("prov","zone"))

rm("tmp_sp_demo1")

zone_sums_c_tt <- tmp_sp_demo %>%
  group_by(AOU,BAGE,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,prov,zone,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,prov,zone,year,.draw) %>%
  summarise(rati = I/A) %>% 
  group_by(AOU,prov,zone,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = quantile(rati,0.025,names = FALSE),
            uci = quantile(rati,0.975,names = FALSE))



prov_sums_c_tt <- tmp_sp_demo %>%
  group_by(AOU,BAGE,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,prov,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,prov,year,.draw) %>%
  summarise(rati = I/A) %>% 
  group_by(AOU,prov,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = quantile(rati,0.025,names = FALSE),
            uci = quantile(rati,0.975,names = FALSE))


if(suf != "b"){
  
reg_sums_c_tt <- tmp_sp_demo %>%
  filter(region != "") %>%
  group_by(AOU,BAGE,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,region,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,region,year,.draw) %>%
  summarise(rati = I/A) %>% 
  group_by(AOU,region,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = quantile(rati,0.025,names = FALSE),
            uci = quantile(rati,0.975,names = FALSE))
}






# age ratios Females Only--------------------------------------------------------------



zone_sums_cF_tt <- tmp_sp_demo %>%
  filter(BSEX %in% c("F")) %>% 
  group_by(AOU,BAGE,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,prov,zone,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,prov,zone,year,.draw) %>%
  summarise(rati = I/A) %>% 
  group_by(AOU,prov,zone,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = quantile(rati,0.025,names = FALSE),
            uci = quantile(rati,0.975,names = FALSE))



prov_sums_cF_tt <- tmp_sp_demo %>%
  filter(BSEX %in% c("F")) %>% 
  group_by(AOU,BAGE,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,prov,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,prov,year,.draw) %>%
  summarise(rati = I/A) %>% 
  group_by(AOU,prov,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = quantile(rati,0.025,names = FALSE),
            uci = quantile(rati,0.975,names = FALSE))

if(suf != "b"){
  
reg_sums_cF_tt <- tmp_sp_demo%>%
  filter(region != "") %>%
  filter(BSEX %in% c("F")) %>% 
  group_by(AOU,BAGE,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,region,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,region,year,.draw) %>%
  summarise(rati = I/A) %>% 
  group_by(AOU,region,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = quantile(rati,0.025,names = FALSE),
            uci = quantile(rati,0.975,names = FALSE))

}



# sex ratios --------------------------------------------------------------



zone_sums_c2_tt <- tmp_sp_demo %>%
  filter(BSEX %in% c("F","M")) %>% 
  group_by(AOU,BSEX,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,prov,zone,year,.draw) %>%
  pivot_wider(names_from = BSEX,
              values_from = sum) %>% 
  group_by(AOU,prov,zone,year,.draw) %>%
  summarise(rati = F/M) %>% 
  group_by(AOU,prov,zone,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = quantile(rati,0.025,names = FALSE),
            uci = quantile(rati,0.975,names = FALSE))



prov_sums_c2_tt <- tmp_sp_demo %>%
  filter(BSEX %in% c("F","M")) %>% 
  group_by(AOU,BSEX,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,prov,year,.draw) %>%
  pivot_wider(names_from = BSEX,
              values_from = sum) %>% 
  group_by(AOU,prov,year,.draw) %>%
  summarise(rati = F/M) %>% 
  group_by(AOU,prov,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = quantile(rati,0.025,names = FALSE),
            uci = quantile(rati,0.975,names = FALSE))


if(suf != "b"){
  
reg_sums_c2_tt <- tmp_sp_demo %>%
  filter(region != "") %>%
  filter(BSEX %in% c("F","M")) %>% 
  group_by(AOU,BSEX,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,region,year,.draw) %>%
  pivot_wider(names_from = BSEX,
              values_from = sum) %>% 
  group_by(AOU,region,year,.draw) %>%
  summarise(rati = F/M) %>% 
  group_by(AOU,region,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = quantile(rati,0.025,names = FALSE),
            uci = quantile(rati,0.975,names = FALSE))
}






# age and sex harvest --------------------------------------------------------------
## should be the same as above but grouped by BAGE and BSEX, but with an additional tmp file that combines the demo proportions with the kill_ys values
## this above combining step needs to happen in the previous script

# Age and Sex specific harvests -------------------------------------------



zone_sums_asxy_tt <- tmp_sp_demo %>%
  group_by(AOU,BSEX,BAGE,prov,zone,year) %>%
  summarise(mean = mean(.value),
            median = quantile(.value,0.5,names = FALSE),
            lci = quantile(.value,0.025,names = FALSE),
            uci = quantile(.value,0.975,names = FALSE))

prov_sums_asxy_tt <- tmp_sp_demo %>%
  group_by(AOU,BSEX,BAGE,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,BAGE,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))
if(suf != "b"){
  
reg_sums_asxy_tt <- tmp_sp_demo %>%
  filter(region != "") %>%
  group_by(AOU,BSEX,BAGE,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,BAGE,region,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))
}





# Age specific harvests -------------------------------------------



zone_sums_ag_tt <- tmp_sp_demo %>%
  group_by(AOU,BAGE,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BAGE,prov,zone,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

prov_sums_ag_tt <- tmp_sp_demo %>%
  group_by(AOU,BAGE,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BAGE,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

if(suf != "b"){
  
reg_sums_ag_tt <- tmp_sp_demo %>%
  filter(region != "") %>%
  group_by(AOU,BAGE,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BAGE,region,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

}


# sex specific harvests -------------------------------------------



zone_sums_sx_tt <- tmp_sp_demo %>%
  group_by(AOU,BSEX,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,prov,zone,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

prov_sums_sx_tt <- tmp_sp_demo %>%
  group_by(AOU,BSEX,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

if(suf != "b"){
  
reg_sums_sx_tt <- tmp_sp_demo %>%
  filter(region != "") %>%
  group_by(AOU,BSEX,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,region,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

}

rm("tmp_sp_demo")



prov_sums_cF <- bind_rows(prov_sums_cF,prov_sums_cF_tt)
zone_sums_cF <- bind_rows(zone_sums_cF,zone_sums_cF_tt)
prov_sums_c <- bind_rows(prov_sums_c,prov_sums_c_tt)
zone_sums_c <- bind_rows(zone_sums_c,zone_sums_c_tt)
prov_sums_c2 <- bind_rows(prov_sums_c2,prov_sums_c2_tt)
prov_sums_asxy <- bind_rows(prov_sums_asxy,prov_sums_asxy_tt)
zone_sums_asxy <- bind_rows(zone_sums_asxy,zone_sums_asxy_tt)
prov_sums_ag <- bind_rows(prov_sums_ag,prov_sums_ag_tt)
zone_sums_ag <- bind_rows(zone_sums_ag,zone_sums_ag_tt)
prov_sums_sx <- bind_rows(prov_sums_sx,prov_sums_sx_tt)
zone_sums_sx <- bind_rows(zone_sums_sx,zone_sums_sx_tt)

if(suf != "b"){
  reg_sums_sx <- bind_rows(reg_sums_sx,reg_sums_sx_tt)
  reg_sums_ag <- bind_rows(reg_sums_ag,reg_sums_ag_tt)
  reg_sums_asxy <- bind_rows(reg_sums_asxy,reg_sums_asxy_tt)
  reg_sums_c2 <- bind_rows(reg_sums_c2,reg_sums_c2_tt)
  reg_sums_c <- bind_rows(reg_sums_c,reg_sums_c_tt)
  reg_sums_cF <- bind_rows(reg_sums_cF,reg_sums_cF_tt)
  
}


}




# Export to files ---------------------------------------------------------


save(list = c("nat_sums_cF",
              "prov_sums_cF",
              "reg_sums_cF",
              "zone_sums_cF",
              "nat_sums_c",
              "prov_sums_c",
              "reg_sums_c",
              "zone_sums_c",
              "nat_sums_c2",
              "prov_sums_c2",
              "reg_sums_c2",
              "nat_sums_asxy",
              "prov_sums_asxy",
              "reg_sums_asxy",
              "zone_sums_asxy",
              "nat_sums_ag",
              "prov_sums_ag",
              "reg_sums_ag",
              "zone_sums_ag",
              "nat_sums_sx",
              "prov_sums_sx",
              "reg_sums_sx",
              "zone_sums_sx"),
     file = "data/Posterior_summaries3.RData")

















