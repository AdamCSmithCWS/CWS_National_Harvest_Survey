
# table output and generation ---------------------------------------------

############# consider if this is necessary. since everything is graphed.
############# figure out which tables should be produced and in what format
library(jagsUI)
library(tidyverse)
#library(ggmcmc)
library(tidybayes)
library(ggrepel)
library(ggforce)
library(HDInterval)
source("functions/utility_functions.R")

### caste level summaries
### full summaries
### harvest, activity, total group-level and species-level
### age-sex summaries
### age-sex raw data for website - 

Y <- 2023
FY = 1976
years <- FY:Y

names(years) <- paste(years)

prov_zone <- read.csv("data/Province and zone table.csv")
alt_regs <- prov_zone %>% 
  select(prov,zone,region) 

#load("national_provincial_summaries.RData")

# load the convergence information and track parameters that have failed below
# For rhat convergence failures, consider removing the zonal estimates from the summaries
# problem with removing them is that the estimated proportions for remaining species
# will be affected...also 
# alternative is to flag estimates that

converge_sum <- readRDS(paste0("output/all_parameter_convergence_summary_",Y,".rds")) 
# Simple estimates effort and groups --------------------------------------

load("national_provincial_summaries1.RData")

tmp_sim <- tmp_sim %>% 
  left_join(.,alt_regs,
            by = c("prov","zone"))
  
zone_sums_b <- tmp_sim %>%
  filter(is.na(residence)) %>% 
  group_by(var,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,prov,zone,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))


reg_sums_b <- tmp_sim %>%
  filter(region != "") %>% 
  filter(is.na(residence)) %>% 
  group_by(var,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,region,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))



prov_sums_b <- tmp_sim %>%
  filter(is.na(residence)) %>% 
  group_by(var,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))


nat_sums_b <- tmp_sim %>%
  filter(is.na(residence)) %>% 
  group_by(var,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))


### same as above by residency
### 
zone_sums_b_res <- tmp_sim %>%
  filter(!is.na(residence)) %>% 
  group_by(var,residence,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,residence,prov,zone,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))


reg_sums_b_res <- tmp_sim %>%
  filter(!is.na(residence)) %>% 
  filter(region != "") %>% 
  group_by(var,residence,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,residence,region,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))



prov_sums_b_res <- tmp_sim %>%
  filter(!is.na(residence)) %>% 
  group_by(var,residence,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,residence,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))


nat_sums_b_res <- tmp_sim %>%
  filter(!is.na(residence)) %>% 
  group_by(var,residence,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,residence,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE,na.rm = T),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))



save(list = c("nat_sums_b",
              "reg_sums_b",
              "prov_sums_b",
              "zone_sums_b",
              "nat_sums_b_res",
              "reg_sums_b_res",
              "prov_sums_b_res",
              "zone_sums_b_res"),
     file = "data/Posterior_summaries1.RData")




rm(list = c("tmp_sim",
            "nat_sums_b",
   "reg_sums_b",
   "prov_sums_b",
   "zone_sums_b",
   "nat_sums_b_res",
   "reg_sums_b_res",
   "prov_sums_b_res",
   "zone_sums_b_res"))





# Species summaries -------------------------------------------------------


load("national_provincial_summaries2.RData")

tmp_sp <- tmp_sp %>% 
  left_join(.,alt_regs,
            by = c("prov","zone"))


# Temporary Removal of Harlequin Duck harvest in East ---------------------
## Must be integrated into the full model or the data-prep process
## in 2024-2025 analysis

harl <- 1550

tst <- tmp_sp %>% 
  filter(!(AOU == harl & prov %in% c("NF",
                                     "PE",
                                     "NS",
                                     "NB") & year > 1988),
         !(AOU == harl & prov %in% c("PQ",
                                     "ON") & year > 1989))

tst2 <- tst %>% filter(AOU == harl)

tmp_sp <- tmp_sp %>% 
  filter(!(AOU == harl & prov %in% c("NF",
                                     "PE",
                                     "NS",
                                     "NB") & year > 1988),
         !(AOU == harl & prov %in% c("PQ",
                                     "ON") & year > 1989))

# Snow Goose combined blue- and white-phases
sngo_aou <- c(1692,1693) # white- and blue-phase sngo

sngo_obs <- tmp_sp %>% # extract only sngo records
  filter(AOU %in% sngo_aou) %>% 
  group_by(prov,zone,year,.draw) %>%
  summarise(.value = sum(.value)) %>% # sum draw values for each prov, zone, and year 
  mutate(AOU = 1695) # add new AOU value

tmp_sp <- bind_rows(tmp_sp,sngo_obs) #append sngo full species to rest of estimates

zone_sums_a <- tmp_sp %>%
  group_by(AOU,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,prov,zone,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

prov_sums_a <- tmp_sp %>%
  group_by(AOU,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

# reg_sums_a <- tmp_sp %>%
#   filter(region != "") %>% 
#   group_by(AOU,region,year,.draw) %>%
#   summarise(sum = sum(.value)) %>%
#   group_by(AOU,region,year) %>%
#   summarise(mean = mean(sum),
#             median = quantile(sum,0.5,names = FALSE),
#             lci = as.numeric(hdi(sum,0.95)[1]),
#             uci = as.numeric(hdi(sum,0.95)[2]))


nat_sums_a <- tmp_sp %>%
  group_by(AOU,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))




save(list = c("nat_sums_a",
              "prov_sums_a",
              #"reg_sums_a",
              "zone_sums_a"),
     file = "data/Posterior_summaries2.RData")
rm(list = c("tmp_sp",
            "nat_sums_a",
            "prov_sums_a",
            #"reg_sums_a",
            "zone_sums_a"))




# age ratios --------------------------------------------------------------

# load demographic convergence summary
demog_summary <- readRDS("output/demog_summary.rds")
# this identifies species, years, and regions where the demographic parameters
# failed to converge. 
# these species x region (maybe years) combinations should be removed from the demographic 
# summaries below

## species for which we don't report age-ratios or demographic summaries
## these species sexes are not treated equally in both ages (e.g., unidentifiable when immature)
## 
sp_demog_never <- c(1300,
                    1310,
                    1540,
                    1630,
                    1650,
                    1660,
                    1670,
                    1360,
                    1520,
                    1550,
                    1620,
                    1730)
# sp_demog_keep <- demog_summary %>% 
#   group_by(.,prov,zone,model,AOU) %>% 
#   summarise(keep = ifelse(any(rhat_fail),FALSE,TRUE),
#             min_ess = min(ess_bulk),
#             max_rhat = max(rhat)) %>% 
#   filter(keep)

sp_demog_keep <- demog_summary %>% 
  group_by(.,prov,zone,model,AOU) %>% 
  summarise(keep = ifelse(any(rhat_fail),FALSE,TRUE)) %>% 
  filter(!AOU %in% sp_demog_never) %>% 
  filter(keep)



sp_demog_drop <- demog_summary %>% 
  group_by(.,prov,zone,model,AOU) %>% 
  summarise(keep = ifelse(any(rhat_fail),TRUE,FALSE)) %>% 
  filter(keep)


load(paste0("national_provincial_summaries3.RData"))

tmp_sp_demo <- tmp_sp_demo %>%
  inner_join(.,sp_demog_keep,
             by = c("prov","zone","AOU"))

nat_sums_c <- tmp_sp_demo %>%
  group_by(AOU,BAGE,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,year,.draw) %>%
  summarise(rati = ceiling(I)/ceiling(A)) %>% 
  group_by(AOU,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = as.numeric(hdi(rati,0.9)[1]),
            uci = as.numeric(hdi(rati,0.9)[2]))

nat_sums_cF <- tmp_sp_demo %>%
  filter(BSEX %in% c("F")) %>% 
  group_by(AOU,BAGE,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,year,.draw) %>%
  summarise(rati = ceiling(I)/ceiling(A)) %>% 
  group_by(AOU,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = as.numeric(hdi(rati,0.9)[1]),
            uci = as.numeric(hdi(rati,0.9)[2]))


nat_sums_c2 <- tmp_sp_demo %>%
  filter(BSEX %in% c("F","M")) %>% 
  group_by(AOU,BSEX,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,year,.draw) %>%
  pivot_wider(names_from = BSEX,
              values_from = sum) %>% 
  group_by(AOU,year,.draw) %>%
  summarise(rati = ceiling(F)/ceiling(M)) %>% 
  group_by(AOU,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = as.numeric(hdi(rati,0.9)[1]),
            uci = as.numeric(hdi(rati,0.9)[2]))

nat_sums_asxy <- tmp_sp_demo %>%
  group_by(AOU,BSEX,BAGE,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,BAGE,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))
nat_sums_ag <- tmp_sp_demo %>%
  group_by(AOU,BAGE,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BAGE,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

nat_sums_sx <- tmp_sp_demo %>%
  group_by(AOU,BSEX,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

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
            by = c("prov","zone")) %>%
  inner_join(.,sp_demog_keep,
             by = c("prov","zone","AOU"))

rm("tmp_sp_demo1")

zone_sums_c_tt <- tmp_sp_demo %>%
  group_by(AOU,BAGE,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,prov,zone,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,prov,zone,year,.draw) %>%
  summarise(rati = ceiling(I)/ceiling(A)) %>% 
  group_by(AOU,prov,zone,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = as.numeric(hdi(rati,0.9)[1]),
            uci = as.numeric(hdi(rati,0.9)[2]))



prov_sums_c_tt <- tmp_sp_demo %>%
  group_by(AOU,BAGE,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,prov,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,prov,year,.draw) %>%
  summarise(rati = ceiling(I)/ceiling(A)) %>% 
  group_by(AOU,prov,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = as.numeric(hdi(rati,0.9)[1]),
            uci = as.numeric(hdi(rati,0.9)[2]))


if(suf != "b"){
  
reg_sums_c_tt <- tmp_sp_demo %>%
  filter(region != "") %>%
  group_by(AOU,BAGE,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,region,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,region,year,.draw) %>%
  summarise(rati = ceiling(I)/ceiling(A)) %>% 
  group_by(AOU,region,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = as.numeric(hdi(rati,0.9)[1]),
            uci = as.numeric(hdi(rati,0.9)[2]))
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
  summarise(rati = ceiling(I)/ceiling(A)) %>% 
  group_by(AOU,prov,zone,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = as.numeric(hdi(rati,0.9)[1]),
            uci = as.numeric(hdi(rati,0.9)[2]))



prov_sums_cF_tt <- tmp_sp_demo %>%
  filter(BSEX %in% c("F")) %>% 
  group_by(AOU,BAGE,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,prov,year,.draw) %>%
  pivot_wider(names_from = BAGE,
              values_from = sum) %>% 
  group_by(AOU,prov,year,.draw) %>%
  summarise(rati = ceiling(I)/ceiling(A)) %>% 
  group_by(AOU,prov,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = as.numeric(hdi(rati,0.9)[1]),
            uci = as.numeric(hdi(rati,0.9)[2]))

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
  summarise(rati = ceiling(I)/ceiling(A)) %>% 
  group_by(AOU,region,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = as.numeric(hdi(rati,0.9)[1]),
            uci = as.numeric(hdi(rati,0.9)[2]))

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
  summarise(rati = ceiling(F)/ceiling(M)) %>% 
  group_by(AOU,prov,zone,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = as.numeric(hdi(rati,0.9)[1]),
            uci = as.numeric(hdi(rati,0.9)[2]))



prov_sums_c2_tt <- tmp_sp_demo %>%
  filter(BSEX %in% c("F","M")) %>% 
  group_by(AOU,BSEX,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%  
  group_by(AOU,prov,year,.draw) %>%
  pivot_wider(names_from = BSEX,
              values_from = sum) %>% 
  group_by(AOU,prov,year,.draw) %>%
  summarise(rati = ceiling(F)/ceiling(M)) %>% 
  group_by(AOU,prov,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = as.numeric(hdi(rati,0.9)[1]),
            uci = as.numeric(hdi(rati,0.9)[2]))


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
  summarise(rati = ceiling(F)/ceiling(M)) %>% 
  group_by(AOU,region,year) %>%
  summarise(mean = mean(rati),
            median = quantile(rati,0.5,names = FALSE),
            lci = as.numeric(hdi(rati,0.9)[1]),
            uci = as.numeric(hdi(rati,0.9)[2]))
}






# age and sex harvest --------------------------------------------------------------
## should be the same as above but grouped by BAGE and BSEX, but with an additional tmp file that combines the demo proportions with the kill_ys values
## this above combining step needs to happen in the previous script

# Age and Sex specific harvests -------------------------------------------



zone_sums_asxy_tt <- tmp_sp_demo %>%
  group_by(AOU,BSEX,BAGE,prov,zone,year) %>%
  summarise(mean = mean(.value),
            median = quantile(.value,0.5,names = FALSE),
            lci = as.numeric(hdi(.value,0.95)[1]),
            uci = as.numeric(hdi(.value,0.95)[2]))

prov_sums_asxy_tt <- tmp_sp_demo %>%
  group_by(AOU,BSEX,BAGE,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,BAGE,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))
if(suf != "b"){
  
reg_sums_asxy_tt <- tmp_sp_demo %>%
  filter(region != "") %>%
  group_by(AOU,BSEX,BAGE,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,BAGE,region,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))
}





# Age specific harvests -------------------------------------------



zone_sums_ag_tt <- tmp_sp_demo %>%
  group_by(AOU,BAGE,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BAGE,prov,zone,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

prov_sums_ag_tt <- tmp_sp_demo %>%
  group_by(AOU,BAGE,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BAGE,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

if(suf != "b"){
  
reg_sums_ag_tt <- tmp_sp_demo %>%
  filter(region != "") %>%
  group_by(AOU,BAGE,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BAGE,region,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

}


# sex specific harvests -------------------------------------------



zone_sums_sx_tt <- tmp_sp_demo %>%
  group_by(AOU,BSEX,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,prov,zone,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

prov_sums_sx_tt <- tmp_sp_demo %>%
  group_by(AOU,BSEX,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

if(suf != "b"){
  
reg_sums_sx_tt <- tmp_sp_demo %>%
  filter(region != "") %>%
  group_by(AOU,BSEX,region,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,region,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

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







# period specific harvest -------------------------------------------------



load("national_provincial_summaries5.RData")

tmp_sp_period_all <- tmp_sp_period_all %>% 
  left_join(.,alt_regs,
            by = c("prov","zone"))


# Temporary Removal of Harlequin Duck harvest in East ---------------------
## Must be integrated into the full model or the data-prep process
## in 2024-2025 analysis

harl <- 1550

# tst <- tmp_sp_period_all %>% 
#   filter(!(AOU == harl & prov %in% c("NF",
#                                      "PE",
#                                      "NS",
#                                      "NB") & year > 1988),
#          !(AOU == harl & prov %in% c("PQ",
#                                      "ON") & year > 1989))
# 
# tst2 <- tst %>% filter(AOU == harl)

tmp_sp_period_all <- tmp_sp_period_all %>% 
  filter(!(AOU == harl & prov %in% c("NF",
                                     "PE",
                                     "NS",
                                     "NB") & year > 1988),
         !(AOU == harl & prov %in% c("PQ",
                                     "ON") & year > 1989))

# Snow Goose combined blue- and white-phases
sngo_aou <- c(1692,1693) # white- and blue-phase sngo

sngo_obs <- tmp_sp_period_all %>% # extract only sngo records
  filter(AOU %in% sngo_aou) %>% 
  group_by(prov,zone,period,year,.draw) %>%
  summarise(.value = sum(.value)) %>% # sum draw values for each prov, zone, and year 
  mutate(AOU = 1695) # add new AOU value

tmp_sp_period_all <- bind_rows(tmp_sp_period_all,sngo_obs) #append sngo full species to rest of estimates

aous <- unique(tmp_sp_period_all$AOU)

nat_sums_per <- NULL
prov_sums_per <- NULL
zone_sums_per <- NULL

for(aou in aous){
  if(is.na(aou)){next}
tmp_sp_per_sel <- tmp_sp_period_all %>%
  filter(AOU %in% aou)


zone_sums_per_t <- tmp_sp_per_sel %>%
  group_by(AOU,prov,zone,period,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,prov,zone,period,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

zone_sums_per <- bind_rows(zone_sums_per,
                           zone_sums_per_t)



# Have to change this to weekly estimates period varies by zone -----------



prov_sums_per_t <- tmp_sp_per_sel %>%
  group_by(AOU,prov,period,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,prov,period,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

prov_sums_per <- bind_rows(prov_sums_per,
                           prov_sums_per_t)



# reg_sums_a <- tmp_sp %>%
#   filter(region != "") %>% 
#   group_by(AOU,region,year,.draw) %>%
#   summarise(sum = sum(.value)) %>%
#   group_by(AOU,region,year) %>%
#   summarise(mean = mean(sum),
#             median = quantile(sum,0.5,names = FALSE),
#             lci = as.numeric(hdi(sum,0.95)[1]),
#             uci = as.numeric(hdi(sum,0.95)[2]))


nat_sums_per_t <- tmp_sp_per_sel %>%
  group_by(AOU,period,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,period,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = as.numeric(hdi(sum,0.95)[1]),
            uci = as.numeric(hdi(sum,0.95)[2]))

nat_sums_per <- bind_rows(nat_sums_per,
                           nat_sums_per_t)


}


save(list = c("nat_sums_per",
              "prov_sums_per",
              #"reg_sums_a",
              "zone_sums_per"),
     file = "data/Posterior_summaries5.RData")
rm(list = c("nat_sums_per",
            "prov_sums_per",
            #"reg_sums_a",
            "zone_sums_per"))










