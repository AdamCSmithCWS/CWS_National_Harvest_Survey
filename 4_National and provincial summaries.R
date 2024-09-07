
#############################3
## script to summarize the zone level estimates into the national and provincial estimates
library(jagsUI)
library(tidyverse)
#library(ggmcmc)
library(tidybayes)
library(ggrepel)
library(ggforce)
#source("functions/other_reg_setup.R")
source("functions/utility_functions.R")
source("functions/other_reg_setup.R") 
others = c("COOTK","WOODK","SNIPK","DOVEK","PIGEK","CRANK") #"RAILK" ,"MURRK"
#
# this function removes all SNIPK pre-1991, by setting the allowable limits to 0
### caste level summaries
### full summaries
### harvest, activity, total group-level and species-level
### age-sex summaries
### age-sex raw data for website - 
castes <- data.frame(c = c(1,2,3,4),
                     caste = c("D","B","A","E"),
                     residence = c("Resident","Resident","Resident","Non-Resident"))

Y <- 2023
FY = 1976
years <- FY:Y

names(years) <- paste(years)



# load website published estimates ----------------------------------------



# # write.csv(var_names_sim,"data/website_variable_names.csv",row.names = F)
# # 
# # write.csv(species_web_names,"data/website_species_variable_names.csv",row.names = F)
# 
# # load all output from species models and other models --------------------------

#load(paste0("data/parts and harvest survey info",Y,".RData"))

provzone <- read.csv("data/Province and zone table.csv")
provs = unique(provzone$prov)


# Load other harvest regulations ------------------------------------------

load("data/regs_other.RData")

non_res_combine = paste(rep(provs,each = 3),rep(c(1,2,3),times = length(provs)))
#this above just ensures all non-resident hunters are combined with resident hunters for these groups, separating out caste E is rarely feasible (even caste B is sketchy)
keep_E <- paste(rep(c("MB","NB","SK"),each = 3),rep(c(1,2,3),times = 3))
# province and zone loops -------------------------------------------------
non_res_combine <- non_res_combine[-which(non_res_combine %in% keep_E)]


# load website names --------------------------------------------------

sim_vars <- read.csv("data/website_variable_names_in.csv")
sp_vars <- read.csv("data/website_species_variable_names_in.csv")



gps <- c("duck",
           "goose",
           "other",
         "murre")


### need to add in the final variable names, species names, and year indicators to this file
### so that it can be added to final summaries in the following script (5_Summarize_output_to_tidy_dataframes....R)


# %>% 
#   mutate(var = stringr::str_extract(variable, "^\\w+"),
#          d1 = jags_dim_tidy(1,variable),
#          d2 = jags_dim_tidy(2,variable),
#          d3 = jags_dim_tidy(3,variable))


sum_convergence <- TRUE # change to FALSE to run convergence summary first

if(!sum_convergence){
parameter_summary <- NULL
sp_save <- NULL
for(pr in provs){
  for(z in 1:3){
    for(spgp in gps){
      
      # for(i in 1:nrow(problems)){
      #   pr = as.character(problems[i,"prov"])
      #   z = as.integer(problems[i,"zone"])
      #   spgp = as.character(problems[i,"model"])
        
      if(spgp == "other"){
       saved_file <- paste("output/other harvest zip",pr,z,"alt mod.RData")
      }else{
        saved_file <- paste("output/full harvest zip",pr,z,spgp,"alt mod.RData")        
      }
      if(!file.exists(saved_file)){next}
      load(saved_file)
      out2sum <- out2sum %>% 
        mutate(prov = pr,
               zone = z,
               model = spgp,
               var = stringr::str_extract(variable, "^\\w+"),
                        d1 = jags_dim_tidy(1,variable),
                        d2 = jags_dim_tidy(2,variable),
                        d3 = jags_dim_tidy(3,variable))
      parameter_summary <- bind_rows(parameter_summary,
                                     out2sum)
      
      if(spgp != "other"){
      sp.save.out <- sp.save.out %>% 
        mutate(prov = pr,
               zone = z,
               model = spgp)
      sp_save <- bind_rows(sp_save,
                           sp.save.out)
      }
      
      
    
  }
  }
 saveRDS(parameter_summary,
         paste0("output/all_parameter_convergence_summary_",Y,".rds")) 
}

rhat_lim <- 1.1
ess_lim <- 50

parameter_summary <- parameter_summary %>% 
  mutate(variable_type = stringr::str_extract(variable, "^\\w+"),
         rhat_fail = ifelse(rhat > rhat_lim,TRUE,FALSE),
         ess_fail = ifelse(ess_bulk < ess_lim,TRUE,FALSE))

simplified_summary <- parameter_summary %>% 
  group_by(prov,zone,model,variable_type) %>% 
  summarise(n_fail = sum(rhat_fail),
            max_rhat = max(rhat),
            n_ess_fail = sum(ess_fail))


## identify models that have non-converged parameters that need to be re-run
## excluding models that have convergence fails that can be flagged
problems <- simplified_summary %>% 
  filter(!grepl("ax",variable_type), #demographic parameters that need to be flagged when failed
         !grepl("adult",variable_type), #demographic parameters that need to be flagged when failed 
         !grepl("female",variable_type),  #demographic parameters that need to be flagged when failed
         !grepl("beta",variable_type),  #demographic parameters that need to be flagged when failed
         n_fail > 0,
         !grepl("psucc",variable_type), # other model usually uninformed parameters (group by year combinations that have no open season)
         !grepl("NSUCC_gcy",variable_type)) %>% # other model usually uninformed parameters (group by year combinations that have no open season) 
  group_by(prov,zone,model,variable_type) %>% 
  summarise(n = n(),
            n_sum = sum(n_fail),
            max_rhat = max(max_rhat),
            n_ess_fail = mean(n_ess_fail))



demog_summary <- parameter_summary %>% 
  filter(grepl("ax",variable_type),
         !grepl("sd",variable_type)) %>% 
  mutate(demog = d1,
         species = d2,
         y = d3) %>% 
  left_join(.,sp_save,
            by = c("prov",
                   "zone",
                   "model",
                   "species" = "spn"),
            relationship = "many-to-one")

saveRDS(demog_summary,"output/demog_summary.rds")


spkill_summary <- parameter_summary %>% 
  filter(variable_type == "kill_ys") %>% 
  mutate(species = d2,
         y = d1) %>% 
  left_join(.,sp_save,
            by = c("prov",
                   "zone",
                   "model",
                   "species" = "spn"),
            relationship = "many-to-one")
saveRDS(spkill_summary,"output/spkill_summary.rds")


other_summary <- parameter_summary %>% 
  filter(variable_type == "kill_yg" |
         variable_type == "days_yg") %>% 
  mutate(group = d2,
         y = d1) 
saveRDS(other_summary,"output/other_summary.rds")


}
sum_convergence <- TRUE





for(jj in 1:3){


### select which set of summaries to calculate
do_sim <- ifelse(jj == 1,TRUE,FALSE)  # ~ 10GB RAM
do_sp <- ifelse(jj == 2,TRUE,FALSE) # ~20GM RAM
do_sp_demo <- ifelse(jj == 3,TRUE,FALSE) # ~80GB RAM!
do_sp_props <- ifelse(jj == 4,TRUE,FALSE) # not currently implemented, calculated in 6_Output_webtables...R


  
if(do_sim){
  tmp_sim <- NULL
}
if(do_sp){
  tmp_sp <- NULL
}
if(do_sp_demo){
    tmp_sp_demo <- NULL
}
if(do_sp_props){
tmp_sp_props <- NULL
}



for(pr in provs){
  

# other data load ---------------------------------------------------------
# 
#   nyears = length(years)
#   minyr <- min(years)
#   

#   reg_mat <- as.matrix(regs[,grps]) #to enter model as data ensuring that group-level annual estimates are never > 0 in years with no season.
#   grps_f <- factor(grps,levels = grps,ordered = TRUE) #ensures consistent ordering of the harvested groups
#   
#   fyear = NA
#   for(g in 1:ngroups){
#     fyear[g] <- min(which(regs[,g+1] > 0))
#   }
#   

  # data set up -------------------------------------------------------

  
   for(z in 1:3){
     
     regf <- other_reg_setup()
     #   
     regs <- regf$regs
     ngroups <- regf$ngroups
     grps <- regf$grps
     
     wkill = grps
     wsucc = paste0("SU",gsub("K",replacement = "",x = grps)) 
     
     if(file.exists(paste("output/full harvest zip",pr,z,"duck","alt mod.RData"))){
       load(paste("output/full harvest zip",pr,z,"duck","alt mod.RData"))
       
       ys <- data.frame(y = 1:jdat$nyears,
                        year = years) 
       

     if(do_sim){

       ### using tidybayes package functions to compile posterior samples into dataframes
           tmp_duck <- out2$samples %>% gather_draws(NACTIVE_y[y],
                                                days_y[y],
                                                NSUCC_y[y],
                                                kill_y[y]) 
           
           vnm <- sim_vars[which(sim_vars$source == "duck"),]
           #vnm <- rename(vnm,group = var)
           
           tmp_duck <- left_join(tmp_duck,vnm,by = c(".variable" = "newvar"))
           
           
           # ys <- data.frame(y = 1:jdat$nyears,
           #                  year = years) 
           
           tmp_duck <- full_join(tmp_duck,ys,by = "y")
           tmp_duck$prov <- pr
           tmp_duck$zone <- z
           
           tmp_sim <- bind_rows(tmp_sim,tmp_duck)
           
           
           ## caste specific estimates
           ## In 2024 make sure to add this commented out version to captuer
           ## the additional monitored parameters NACTIVE_cy and days_cy
           ## 
            # tmp_duck <- out2$samples %>% gather_draws(NSUCC_cy[c,y],
            #                                           NACTIVE_cy[c,y],
            #                                           days_cy[c,y],
            #                                           kill_cy[c,y]) 
           tmp_duck <- out2$samples %>% gather_draws(kill_cy[c,y]) 
           
           vnm <- sim_vars[which(sim_vars$source == "duck"),]
           #vnm <- rename(vnm,group = var)
           
           tmp_duck <- left_join(tmp_duck,vnm,by = c(".variable" = "newvar")) %>% 
             left_join(.,castes,
                       by = c("c")) %>% 
             full_join(.,ys,by = "y")
           
           tmp_duck$prov <- pr
           tmp_duck$zone <- z
           
           tmp_sim <- bind_rows(tmp_sim,tmp_duck)
           
     }
       
           ## species harvests
       if(do_sp){    
           tmp_sp_duck <- out2$samples %>% gather_draws(kill_ys[y,s]) 
           vnm <- sp_vars[which(sp_vars$source == "duck"),c("sp","species","newvar")]
           spss <- sp.save.out[which(sp.save.out$PRHUNT == pr & sp.save.out$ZOHUNT == z),c("AOU","spfact","spn")]
           spss <- spss[order(spss$spn),]
           tmp_sp_duck <- left_join(tmp_sp_duck,spss,by = c("s" = "spn"))
           tmp_sp_duck <- left_join(tmp_sp_duck,vnm,by = c("AOU" = "sp"))
           tmp_sp_duck <- left_join(tmp_sp_duck,ys,by = "y")
           tmp_sp_duck$prov <- pr
           tmp_sp_duck$zone <- z
           
           tmp_sp <- bind_rows(tmp_sp,tmp_sp_duck)
           
       }
       
     
       
           ## species age-sex harvests
           
       if(do_sp_demo){
           demog = data.frame(BSEX = rep(c("F","M"),each = 2),
                              BAGE = rep(c("A","I"),times = 2),
                              d = 1:4,
                              stringsAsFactors = F)
           
           tmp_sp_duck_demo <- out2$samples %>% gather_draws(kill_ysax[d,s,y]) 
           vnm <- sp_vars[which(sp_vars$source == "duck"),c("sp","species")]
           spss <- sp.save.out[which(sp.save.out$PRHUNT == pr & sp.save.out$ZOHUNT == z),c("AOU","spfact","spn")]
           spss <- spss[order(spss$spn),]
           tmp_sp_duck_demo <- left_join(tmp_sp_duck_demo,spss,by = c("s" = "spn"))
           tmp_sp_duck_demo <- left_join(tmp_sp_duck_demo,vnm,by = c("AOU" = "sp"))
           tmp_sp_duck_demo <- left_join(tmp_sp_duck_demo,ys,by = "y")
           tmp_sp_duck_demo <- left_join(tmp_sp_duck_demo,demog,by = "d")
           tmp_sp_duck_demo$prov <- pr
           tmp_sp_duck_demo$zone <- z
         
           tmp_sp_demo <- bind_rows(tmp_sp_demo,tmp_sp_duck_demo)
           
       }     
           

             
           
           }
     
     if(file.exists(paste("output/full harvest zip",pr,z,"goose","alt mod.RData"))){
             load(paste("output/full harvest zip",pr,z,"goose","alt mod.RData"))
             
       ys <- data.frame(y = 1:jdat$nyears,
                        year = years) 
       
       if(do_sim){
           tmp_goose <- out2$samples %>% gather_draws(NSUCC_y[y],
                                                     kill_y[y]) 
           
           vnm <- sim_vars[which(sim_vars$source == "goose"),]
           #vnm <- rename(vnm,group = var)
           
           tmp_goose <- left_join(tmp_goose,vnm,by = c(".variable" = "newvar"))
           
           

           tmp_goose <- full_join(tmp_goose,ys,by = "y")
           tmp_goose$prov <- pr
           tmp_goose$zone <- z
           tmp_sim <- bind_rows(tmp_sim,tmp_goose)
           
           
           ## caste specific estimates
           ## 2024 uncomment this next line because NSUCC_cy should have been monitored
           # tmp_goose <- out2$samples %>% gather_draws(NSUCC_cy[c,y],
           #                                            kill_cy[c,y]) 
           # 
           tmp_goose <- out2$samples %>% gather_draws(kill_cy[c,y]) 
           
           vnm <- sim_vars[which(sim_vars$source == "goose"),]
           #vnm <- rename(vnm,group = var)
           
           tmp_goose <- left_join(tmp_goose,vnm,by = c(".variable" = "newvar")) %>% 
             left_join(.,castes,
                       by = c("c")) %>% 
             full_join(.,ys,by = "y")
           
           tmp_goose$prov <- pr
           tmp_goose$zone <- z
           tmp_sim <- bind_rows(tmp_sim,tmp_goose)
           
       }
           ## species harvests
           
       if(do_sp){
           tmp_sp_goose <- out2$samples %>% gather_draws(kill_ys[y,s]) 
           vnm <- sp_vars[which(sp_vars$source == "goose"),c("sp","species","newvar")]
           spss <- sp.save.out[which(sp.save.out$PRHUNT == pr & sp.save.out$ZOHUNT == z),c("AOU","spfact","spn")]
           spss <- spss[order(spss$spn),]
           tmp_sp_goose <- left_join(tmp_sp_goose,spss,by = c("s" = "spn"))
           tmp_sp_goose <- left_join(tmp_sp_goose,vnm,by = c("AOU" = "sp"))
           tmp_sp_goose <- left_join(tmp_sp_goose,ys,by = "y")
           tmp_sp_goose$prov <- pr
           tmp_sp_goose$zone <- z
           tmp_sp <- bind_rows(tmp_sp,tmp_sp_goose)
           
       }
           ## species age-sex harvests 
           
       if(do_sp_demo){
                      demog = data.frame(BSEX = rep(c("U","U"),each = 1),
                              BAGE = rep(c("A","I"),times = 1),
                              d = 1:2,
                              stringsAsFactors = F)
           
           tmp_sp_goose_demo <- out2$samples %>% gather_draws(kill_ysax[d,s,y]) 
           vnm <- sp_vars[which(sp_vars$source == "goose"),c("sp","species")]
           spss <- sp.save.out[which(sp.save.out$PRHUNT == pr & sp.save.out$ZOHUNT == z),c("AOU","spfact","spn")]
           spss <- spss[order(spss$spn),]
           tmp_sp_goose_demo <- left_join(tmp_sp_goose_demo,spss,by = c("s" = "spn"))
           tmp_sp_goose_demo <- left_join(tmp_sp_goose_demo,vnm,by = c("AOU" = "sp"))
           tmp_sp_goose_demo <- left_join(tmp_sp_goose_demo,ys,by = "y")
           tmp_sp_goose_demo <- left_join(tmp_sp_goose_demo,demog,by = "d")
           tmp_sp_goose_demo$prov <- pr
           tmp_sp_goose_demo$zone <- z
           tmp_sp_demo <- bind_rows(tmp_sp_demo,tmp_sp_goose_demo)
           
       }
           
     }
     
     if(file.exists(paste("output/full harvest zip",pr,z,"murre","alt mod.RData"))){
       load(paste("output/full harvest zip",pr,z,"murre","alt mod.RData"))

       ys <- data.frame(y = 1:jdat$nyears,
                        year = years[c(((length(years)-jdat$nyears)+1):length(years))]) 
       
       if(do_sim){
       tmp_murre <- out2$samples %>% gather_draws(NSUCC_y[y],
                                                  kill_y[y]) 
       
       vnm <- sim_vars[which(sim_vars$source == "murre"),]
       #vnm <- rename(vnm,group = var)
       
       tmp_murre <- left_join(tmp_murre,vnm,by = c(".variable" = "newvar"))
       
       

       tmp_murre <- full_join(tmp_murre,ys,by = "y")
       tmp_murre$prov <- pr
       tmp_murre$zone <- z
       tmp_sim <- bind_rows(tmp_sim,tmp_murre)
       }
       ## species harvests
       if(do_sp){
       tmp_sp_murre <- out2$samples %>% gather_draws(kill_ys[y,s]) 
       vnm <- sp_vars[which(sp_vars$source == "murre"),c("sp","species","newvar")]
       spss <- sp.save.out[which(sp.save.out$PRHUNT == pr & sp.save.out$ZOHUNT == z),c("AOU","spfact","spn")]
       spss <- spss[order(spss$spn),]
       tmp_sp_murre <- left_join(tmp_sp_murre,spss,by = c("s" = "spn"))
       tmp_sp_murre <- left_join(tmp_sp_murre,vnm,by = c("AOU" = "sp"))
       tmp_sp_murre <- left_join(tmp_sp_murre,ys,by = "y")
       tmp_sp_murre$prov <- pr
       tmp_sp_murre$zone <- z
       tmp_sp <- bind_rows(tmp_sp,tmp_sp_murre)
       }
       ## species age-sex harvests
       if(do_sp_demo){
       demog = data.frame(BSEX = rep(c("U","U"),each = 1),
                          BAGE = rep(c("A","I"),times = 1),
                          d = 1:2,
                          stringsAsFactors = F)
       
       tmp_sp_murre_demo <- out2$samples %>% gather_draws(kill_ysax[d,s,y]) 
       vnm <- sp_vars[which(sp_vars$source == "murre"),c("sp","species")]
       spss <- sp.save.out[which(sp.save.out$PRHUNT == pr & sp.save.out$ZOHUNT == z),c("AOU","spfact","spn")]
       spss <- spss[order(spss$spn),]
       tmp_sp_murre_demo <- left_join(tmp_sp_murre_demo,spss,by = c("s" = "spn"))
       tmp_sp_murre_demo <- left_join(tmp_sp_murre_demo,vnm,by = c("AOU" = "sp"))
       tmp_sp_murre_demo <- left_join(tmp_sp_murre_demo,ys,by = "y")
       tmp_sp_murre_demo <- left_join(tmp_sp_murre_demo,demog,by = "d")
       tmp_sp_murre_demo$prov <- pr
       tmp_sp_murre_demo$zone <- z
       tmp_sp_demo <- bind_rows(tmp_sp_demo,tmp_sp_murre_demo)
       }
     }
     
     
     
     
     
     if(file.exists(paste("output/other harvest zip",pr,z,"alt mod.RData"))){
      
       
       
       if(do_sim){
         load(paste("output/other harvest zip",pr,z,"alt mod.RData"))
       
       vnm <- sim_vars[which(sim_vars$source == "other" & 
                               !grepl(pattern = "cy",sim_vars$newvar)),]
       vnm1 <- sim_vars[which(sim_vars$source == "other" & 
                                grepl(pattern = "cy",sim_vars$newvar)),]
       #vnm <- rename(vnm,group = var)
       
       #harvests
       tmp_otherk <- out2$samples %>% gather_draws(NACTIVE_y[y],
                                                  days_y[y])
       
       tmp_otherk <- left_join(tmp_otherk,vnm,by = c(".variable" = "newvar"))
       
       # harvest by group
       tmp_otherkg <- out2$samples %>% gather_draws(kill_yg[y,g])
       
      gpk <- data.frame(g = 1:ngroups,
                       var = grps)
      tmp_otherkg <- full_join(tmp_otherkg,gpk,by = "g")
      tmp_otherkg <- left_join(tmp_otherkg,vnm,by = c("var"))
      
      
      # harvest by caste and group
      # 2024 - un comment this section because harvest by caste is now monitored
      # 
      # tmp_otherkgc <- out2$samples %>% gather_draws(kill_cyg[c,y,g])
      # 
      
      # gpk <- data.frame(g = 1:ngroups,
      #                   var = grps)
      # tmp_otherkgc <- full_join(tmp_otherkgc,gpk,by = "g")
      # tmp_otherkgc <- left_join(tmp_otherkgc,vnm1,by = c("var")) %>% 
      #   left_join(.,castes,
      #             by = c("c"))
      # 
      # 
      
  #days
      tmp_otherd <- out2$samples %>% gather_draws(days_yg[y,g])
      gpd <- data.frame(g = 1:ngroups,
                        var = paste0("DA",gsub(grps,pattern = "K",replacement = "")))
      
      tmp_otherd <- full_join(tmp_otherd,gpd,by = "g")
      tmp_otherd <- left_join(tmp_otherd,vnm,by = "var")
      
  #succ    
      tmp_others <- out2$samples %>% gather_draws(NSUCC_yg[y,g])
      gps <- data.frame(g = 1:ngroups,
                        var = paste0("SU",gsub(grps,pattern = "K",replacement = "")))
      
      tmp_others <- full_join(tmp_others,gps,by = "g")
      tmp_others <- left_join(tmp_others,vnm,by = "var")
      
    #succ by caste and group
      # 
      
      tmp_othersc <- out2$samples %>% gather_draws(NSUCC_gcy[g,c,y])
      
      sugrps <- paste0("SU",gsub("K","",grps))
      gpk <- data.frame(g = 1:ngroups,
                        var = sugrps)
      tmp_othersc <- full_join(tmp_othersc,gpk,by = "g")
      tmp_othersc <- left_join(tmp_othersc,vnm1,by = c("var")) %>% 
        left_join(.,castes,
                  by = c("c"))
      
      
      tmp_other <- bind_rows(tmp_otherk,
                             tmp_otherkg,
                             #tmp_otherkgc,  ## 2024 - be sure to uncomment this
                             tmp_otherd,
                             tmp_others,
                             tmp_othersc)
      
  
      # ys <- data.frame(y = 1:jdat$nyears,
      #                  year = years) 
      
      tmp_other <- full_join(tmp_other,ys,by = "y")
      tmp_other$prov <- pr
      tmp_other$zone <- z
      #### this tmp file can be added to the similar regional ones
      #### then the group and year variables will facilitate a full 
      #### tidy summary to generate the national and provincial estimates
      
      tmp_sim <- bind_rows(tmp_sim,tmp_other)
       }
      
      }
       
      
     #tmp <- bind_rows(tmp_other) 
       
    
   }
   print(pr)
}




if(do_sim){
save(list = c("tmp_sim"),
     file = "national_provincial_summaries1.RData")
  rm(list = "tmp_sim")
}
if(do_sp){
save(list = c("tmp_sp"),
     file = "national_provincial_summaries2.RData")
  rm(list = "tmp_sp")
  
}

if(do_sp_demo){
save(list = c("tmp_sp_demo"),
     file = "national_provincial_summaries3.RData")

### split tmp_sp_demo in 3 to reduce memory requirements in next script
tmp_sp_demo1 <- tmp_sp_demo %>% 
  filter(prov %in% c("NF","NB",
                   "NS","PE"))
save(list = c("tmp_sp_demo1"),
     file = "national_provincial_summaries3_a.RData")


tmp_sp_demo1 <- tmp_sp_demo %>% 
  filter(prov %in% c("ON","PQ"))
save(list = c("tmp_sp_demo1"),
     file = "national_provincial_summaries3_b.RData")


tmp_sp_demo1 <- tmp_sp_demo %>% 
  filter(prov %in% c("MB","SK",
                   "AB","BC",
                   "YT","NT",
                   "NU"))
save(list = c("tmp_sp_demo1"),
     file = "national_provincial_summaries3_c.RData")

}

if(do_sp_props){
  save(list = c("tmp_sp_props"),
       file = "national_provincial_summaries4.RData")
  }


}#jj


















