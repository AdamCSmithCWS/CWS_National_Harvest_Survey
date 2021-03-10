
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

Y <- 2019
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
              "zone_sums_c2",
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


nat_sums_cF













# 
# 
# 
# # extra loop to store caste-level total harvest estimates -----------------
# 
# for(pr in provs){
#   
#   
#   
#   for(z in 1:3){
#     
#     
#     if(file.exists(paste("output/full harvest zip",pr,z,"duck","alt mod.RData"))){
#       load(paste("output/full harvest zip",pr,z,"duck","alt mod.RData"))
#       
#       ### using tidybayes package functions to compile posterior samples into dataframes
#       tmp_killd <- out2$samples %>% gather_draws(kill_cy[c,y]) 
#       
#       
#       tmp_killd$var <- "TODUK_Caste"
#       tmp_killd$name <- "Total ducks harvest by caste"
#       
#       
#       ys <- data.frame(y = 1:jdat$nyears,
#                        year = years) 
#       
#       cs <- data.frame(c = 1:jdat$ncastes,
#                        caste = factor(c("D","B","A","E")[1:jdat$ncastes],
#                                       ordered = T,
#                                       levels = c("D","B","A","E")))
#       
#       
#       tmp_killd <- full_join(tmp_killd,ys,by = "y")
#       tmp_killd <- full_join(tmp_killd,cs,by = "c")
#       
#       tmp_killd$prov <- pr
#       tmp_killd$zone <- z
#       
#       tmp_kill <- tmp_killd
#       rm(list = "out2")
#       
#       
#       
#       
#       # Goose -------------------------------------------------------------------
#       
#       
#       if(file.exists(paste("output/full harvest zip",pr,z,"goose","alt mod.RData"))){
#         load(paste("output/full harvest zip",pr,z,"goose","alt mod.RData"))
#         
#         ### using tidybayes package functions to compile posterior samples into dataframes
#         tmp_killg <- out2$samples %>% gather_draws(kill_cy[c,y]) 
#         
#         
#         tmp_killg$var <- "TOGOK_Caste"
#         tmp_killg$name <- "Total goose harvest by caste"
#         
#         
#         ys <- data.frame(y = 1:jdat$nyears,
#                          year = years) 
#         
#         cs <- data.frame(c = 1:jdat$ncastes,
#                          caste = factor(c("D","B","A","E")[1:jdat$ncastes],
#                                         ordered = T,
#                                         levels = c("D","B","A","E")))
#         
#         
#         tmp_killg <- full_join(tmp_killg,ys,by = "y")
#         tmp_killg <- full_join(tmp_killg,cs,by = "c")
#         
#         tmp_killg$prov <- pr
#         tmp_killg$zone <- z
#         
#         tmp_kill <- bind_rows(tmp_kill,tmp_killg)
#       }
#       
#       
#       
#       # Murres -------------------------------------------------------------------
#       
#       
#       if(file.exists(paste("output/full harvest zip",pr,z,"murre","alt mod.RData"))){
#         load(paste("output/full harvest zip",pr,z,"murre","alt mod.RData"))
#         
#         ### using tidybayes package functions to compile posterior samples into dataframes
#         tmp_killm <- out2$samples %>% gather_draws(kill_cy[c,y]) 
#         
#         
#         tmp_killm$var <- "MURRK_Caste"
#         tmp_killm$name <- "Total murre harvest by caste"
#         
#         
#         ys <- data.frame(y = 1:jdat$nyears,
#                          year = years[c((length(years)-(jdat$nyears-1)):length(years))]) 
#         
#         cs <- data.frame(c = 1:jdat$ncastes,
#                          caste = factor(c("D","B","A","E")[1:jdat$ncastes],
#                                         ordered = T,
#                                         levels = c("D","B","A","E")))
#         
#         
#         tmp_killm <- full_join(tmp_killm,ys,by = "y")
#         tmp_killm <- full_join(tmp_killm,cs,by = "c")
#         
#         tmp_killm$prov <- pr
#         tmp_killm$zone <- z
#         
#         tmp_kill <- bind_rows(tmp_kill,tmp_killm)
#         
#         
#       }
#       
#       if(pr == "AB" & z == 1){
#         kill_caste <- tmp_kill
#       }else{
#         kill_caste <- bind_rows(kill_caste,tmp_kill)
#       }
#     }
#   }#z
# }#pr
# 
# 
# #save(list = "kill_caste",file = "kill_caste.RData")
# #load("kill_caste.RData")
# 
# 
# 
# zone_kill_caste <- kill_caste %>%
#   group_by(var,prov,zone,year,caste,.draw) %>%
#   summarise(sum = sum(.value)) %>%
#   group_by(var,prov,zone,year,caste) %>%
#   summarise(mean = mean(sum),
#             median = quantile(sum,0.5,names = FALSE,na.rm = T),
#             lci = quantile(sum,0.025,names = FALSE,na.rm = T),
#             uci = quantile(sum,0.975,names = FALSE,na.rm = T))
# 
# 
# prov_kill_caste <- kill_caste %>%
#   group_by(var,prov,year,caste,.draw) %>%
#   summarise(sum = sum(.value)) %>%
#   group_by(var,prov,year,caste) %>%
#   summarise(mean = mean(sum),
#             median = quantile(sum,0.5,names = FALSE,na.rm = T),
#             lci = quantile(sum,0.025,names = FALSE,na.rm = T),
#             uci = quantile(sum,0.975,names = FALSE,na.rm = T))
# 
# 
# nat_kill_caste <- kill_caste %>%
#   group_by(var,year,caste,.draw) %>%
#   summarise(sum = sum(.value)) %>%
#   group_by(var,year,caste) %>%
#   summarise(mean = mean(sum),
#             median = quantile(sum,0.5,names = FALSE,na.rm = T),
#             lci = quantile(sum,0.025,names = FALSE,na.rm = T),
#             uci = quantile(sum,0.975,names = FALSE,na.rm = T))
# 
# 
# 
# save(list = c("zone_kill_caste",
#               "prov_kill_caste",
#               "nat_kill_caste"),file = "kill_caste_summary.RData")
# 




