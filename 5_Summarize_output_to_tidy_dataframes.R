
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



load("national_provincial_summaries.RData")



zone_sums_b <- tmp_sim %>%
  group_by(var,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(var,prov,zone,year) %>%
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


nat_sums_a <- tmp_sp %>%
  group_by(AOU,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))





# age ratios --------------------------------------------------------------



zone_sums_c <- tmp_sp_demo %>%
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



prov_sums_c <- tmp_sp_demo %>%
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

# group_by(AOU,BAGE,prov,year,.draw) %>%
# summarise(sum = sum(.value)) %>%
# group_by(AOU,BAGE,prov,year) %>%
# summarise(mean = mean(sum),
#           median = quantile(sum,0.5,names = FALSE),
#           lci = quantile(sum,0.025,names = FALSE),
#           uci = quantile(sum,0.975,names = FALSE))


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


# summarise(sum = sum(.value)) %>%
# group_by(AOU,BAGE,year) %>%
# summarise(mean = mean(sum),
#           median = quantile(sum,0.5,names = FALSE),
#           lci = quantile(sum,0.025,names = FALSE),
#           uci = quantile(sum,0.975,names = FALSE))




# age ratios Females Only--------------------------------------------------------------



zone_sums_cF <- tmp_sp_demo %>%
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



prov_sums_cF <- tmp_sp_demo %>%
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

# group_by(AOU,BAGE,prov,year,.draw) %>%
# summarise(sum = sum(.value)) %>%
# group_by(AOU,BAGE,prov,year) %>%
# summarise(mean = mean(sum),
#           median = quantile(sum,0.5,names = FALSE),
#           lci = quantile(sum,0.025,names = FALSE),
#           uci = quantile(sum,0.975,names = FALSE))


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


# summarise(sum = sum(.value)) %>%
# group_by(AOU,BAGE,year) %>%
# summarise(mean = mean(sum),
#           median = quantile(sum,0.5,names = FALSE),
#           lci = quantile(sum,0.025,names = FALSE),
#           uci = quantile(sum,0.975,names = FALSE))




# sex ratios --------------------------------------------------------------



zone_sums_c2 <- tmp_sp_demo %>%
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



prov_sums_c2 <- tmp_sp_demo %>%
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






# age and sex harvest --------------------------------------------------------------
## should be the same as above but grouped by BAGE and BSEX, but with an additional tmp file that combines the demo proportions with the kill_ys values
## this above combining step needs to happen in the previous script

# Age and Sex specific harvests -------------------------------------------



zone_sums_asxy <- tmp_sp_demo %>%
  group_by(AOU,BSEX,BAGE,prov,zone,year) %>%
  summarise(mean = mean(.value),
            median = quantile(.value,0.5,names = FALSE),
            lci = quantile(.value,0.025,names = FALSE),
            uci = quantile(.value,0.975,names = FALSE))

prov_sums_asxy <- tmp_sp_demo %>%
  group_by(AOU,BSEX,BAGE,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,BAGE,prov,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))


nat_sums_asxy <- tmp_sp_demo %>%
  group_by(AOU,BSEX,BAGE,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,BAGE,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))



# Age specific harvests -------------------------------------------



zone_sums_ag <- tmp_sp_demo %>%
  group_by(AOU,BAGE,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BAGE,prov,zone,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

prov_sums_ag <- tmp_sp_demo %>%
  group_by(AOU,BAGE,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BAGE,prov,year) %>%
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



# sex specific harvests -------------------------------------------



zone_sums_sx <- tmp_sp_demo %>%
  group_by(AOU,BSEX,prov,zone,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,prov,zone,year) %>%
  summarise(mean = mean(sum),
            median = quantile(sum,0.5,names = FALSE),
            lci = quantile(sum,0.025,names = FALSE),
            uci = quantile(sum,0.975,names = FALSE))

prov_sums_sx <- tmp_sp_demo %>%
  group_by(AOU,BSEX,prov,year,.draw) %>%
  summarise(sum = sum(.value)) %>%
  group_by(AOU,BSEX,prov,year) %>%
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











# Export to files ---------------------------------------------------------


save(list = c("nat_sums_a",
              "prov_sums_a",
              "zone_sums_a",
              "nat_sums_b",
              "prov_sums_b",
              "zone_sums_b",
              "nat_sums_cF",
              "prov_sums_cF",
              "zone_sums_cF",
              "nat_sums_c",
              "prov_sums_c",
              "zone_sums_c",
              "nat_sums_c2",
              "prov_sums_c2",
              #"zone_sums_c2",
              "nat_sums_asxy",
              "prov_sums_asxy",
              "zone_sums_asxy",
              "nat_sums_ag",
              "prov_sums_ag",
              "zone_sums_ag",
              "nat_sums_sx",
              "prov_sums_sx",
              "zone_sums_sx"),
     file = "data/Posterior_summaries.RData")

















