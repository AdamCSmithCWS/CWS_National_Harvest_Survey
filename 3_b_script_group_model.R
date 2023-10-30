#### why do we care where the permits were purchased?
## it's only because of the archaic sampling system.
## if we know where the hunters plan to hunt, that's a better way to stratify them
## and then we only need to generate estimates for a single stratification (hunter-type by hunt-strata)
## and avoids the need to poststratify after the analysis (e.g., generate estimates for hunters purchasing in SK who hunted in AB)
### complicates the extrapolation of harvest into the past when hunters were stratified by zone of sale
### will require re-calculation of population sizes and extrapolation factors using historical data
### or, an assumption that since almost all hunters hunt in the zone of sale, then it won't make a significant difference to the 
### time-series if we just change the definition of the strata going forward.
### although, should we consider out-of-prov hunters as non-residents? Do they share hunting behaviours more with US-res or CAN-res?


#### eventually, we should produce response-level and part-level predictions
#### so that we can generate spatial estimates of hunting activity and harvest rates

##### 
## generalize this so that it applies across groups (geese, murres, ducks, as well as single species)

## broaden the model to include all zones at once
## share information among neighbouring regions on the species composition in a given year
## share information between years using a random-walk structure where the means in a given year are a function of last year's means
## also include model for age and sex ratios


#############
## these next few lines can be run
## after the various local folders have been adjusted



###############################################
###############

## need to remove the scaled gamma and the glm module
## this may remove the tendency to get the inconsistent with parents errors

## also

## add the relevant sex and age ratios as explicit calculations
# AGE RATIO: IMMATURES/ADULT: 
#   The ratio corresponds to the number of immatures per adult bird in the sample. Ratios were calculated if the total sample equals or exceeds 20 parts.
# 
# SEX RATIO: MALES PER FEMALE: 
#   The ratio corresponds to the number of males per female bird in the sample. Ratios were calculated if the total sample equals or exceeds 20 parts.


Y <- 2022
FY = 1976
years <- FY:Y

names(years) <- paste(years)

library(jagsUI)
library(tidyverse)
library(rjags)
# library(ggmcmc)
# library(tidybayes)
# library(ggrepel)
# library(ggforce)
library(doParallel)
library(foreach)
library(posterior)

#load.module("glm") 

 

# load output from data_prep.R --------------------------------------------

setwd("C:/Users/SmithAC/Documents/GitHub/CWS_national_harvest_survey")
#load(paste0("data/parts and harvest survey info",Y,".RData"))

source("functions/get_final_values.R")
provzone <- read.csv("data/Province and zone table.csv")
provs = unique(provzone$prov)

# 
# load("data/allkill.RData")
# ### species lists
# 
# 
# 
# ### species lists
# 
# 
# others = c("COOTK","WOODK","SNIPK","DOVEK","PIGEK","CRANK") #"RAILK" ,"MURRK"
# #dropping Rails because the data need to be reconciled
# 
# #prov_otherk <- read.csv(stringsAsFactors = F,"data/OTHERK_by_Prov.csv")
# 
# 
# 
# # regulations compile -----------------------------------------------------
# 
# regs_other <- list()
# length(regs_other) <- length(others)
# names(regs_other) <- others
# 
# for(spgp in others){ 
#  tmp <- read.csv(file = paste0("data/reg_",spgp,".csv"))
# names(tmp)[which(names(tmp) == "QC")] <- "PQ"
# names(tmp)[which(names(tmp) == "YK")] <- "YT"
# regs_other[[spgp]] <- tmp
# }
# 
# 
# non_res_combine = paste(rep(provs,each = 3),rep(c(1,2,3),times = length(provs)))
# #this above just ensures all non-resident hunters are combined with resident hunters for these groups, separating out caste E is rarely feasible (even caste B is sketchy)
# keep_E <- paste(rep(c("MB","NB","SK"),each = 3),rep(c(1,2,3),times = 3))
# # province and zone loops -------------------------------------------------
# non_res_combine <- non_res_combine[-which(non_res_combine %in% keep_E)]

provs = provs[-which(provs %in% c("NF","NU"))]##removing NF because definition of other has changed over time (including then excluding murres)
all_zones <- expand.grid(prov = provs,
                        zone = 1:3)

# all_zones <- data.frame(prov = c("MB","NB","SK","ON"),
#                         zone = c(2,2,1,3))
# all_zones <- data.frame(prov = c("MB","AB","NT","PQ","YT"),
#                         zone = c(1,1,1,2,1))

# all_zones <- data.frame(prov = c("MB","NB","PQ","YT"),
#                         zone = c(2,2,2,1))

# MCMC loops --------------------------------------------------------------
rerun <- TRUE # set to true if the loop should force model fit for zones already completed

n_cores <- 3
cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


fullrun <- foreach(i = 1:nrow(all_zones),
                   .packages = c("jagsUI","tidyverse","posterior"),
                   .inorder = FALSE,
                   .errorhandling = "pass") %dopar%
  {

    pr <- all_zones[i,"prov"]
    z <- all_zones[i,"zone"]
    


    if(file.exists(paste("data/data",pr,z,"other_save.RData",sep = "_")) &
       (rerun | !file.exists(paste("output/other harvest zip",pr,z,"alt mod.RData")))
       ){
    load(paste("data/data",pr,z,"other_save.RData",sep = "_"))

      mod.file = "models/group_model.R" # 
      
      
parms = c("NACTIVE_y",
          "NSUCC_yg",
          "nu_day",
          "sdhunter_day",
           "mean_totkill_ycg",
          # "mean_totdays_ycg",
           "mean_totkill_ycg_alt",
          # "mean_totdays_ycg_alt",
          #"kill_cyg",
          "kill_yg",
          "days_yg",
          "days_y",
          "nu",
          "sdhunter",
          "cst",
          "cst_day",
          "group",
          "ann_day",
          "parrive",
          "pleave",
          "psi",
          "tau_group",
          "psucc",
          "pactive",
          "NACTIVE_cy",
          "pops_cor",
          "NSUCC_gcy")

parm_check <- c("NACTIVE_y",
  "NSUCC_yg",
  "kill_yg",
  "days_yg",
  "days_y")


#adaptSteps = 200              # Number of steps to "tune" the samplers.
burnInSteps = 50000           # Number of steps to "burn-in" the samplers.

nChains = 3                   # Number of chains to run.
numSavedSteps=1000          # Total number of steps in each chain to save.
thinSteps=100                   # Number of steps to "thin" (1=keep every step).


# if(pr %in% c("YT","NT","NB")){
#   burnInSteps = burnInSteps*2
#   thinSteps = thinSteps*2
# }


nIter = ceiling( ( (numSavedSteps * thinSteps )+burnInSteps)) # Steps per chain.




  
  out2 = try(jagsUI(data = jdat,
                    parameters.to.save = parms,
                    n.chains = 3,
                    n.burnin = burnInSteps,
                    n.thin = thinSteps,
                    n.iter = nIter,
                    parallel = T,
                    modules = "glm",
                    model.file = mod.file),silent = F)

  
  
if(class(out2) != "try-error"){
 
  
  out2sum <- posterior::as_draws_df(out2$samples) %>%
    summarise_draws() %>% 
    as.data.frame() %>% 
    filter(!is.na(rhat))

  
  attempts <- 0
  
  
  while(any(out2sum$rhat > 1.1) & attempts < 1){
    attempts <- attempts+1
    burnInSteps = 0
    thinSteps = thinSteps*2
  nIter = ceiling( ( (numSavedSteps * thinSteps )+burnInSteps)) # Steps per chain.
  
  #initls <- get_final_values(out2)
  
  initls <- vector(mode = "list",3)

  for(cc in 1:3){
    initls[[cc]] <- eval(parse(text = (paste0("as.list(out2$model$cluster",
                                              cc,
                                              "$state()[[1]])"))))

  }
  
  
  out2 = try(jagsUI(data = jdat,
                    parameters.to.save = parms,
                    n.chains = 3,
                    n.burnin = burnInSteps,
                    n.thin = thinSteps,
                    n.iter = nIter,
                    inits = initls,
                    parallel = T,
                    modules = "glm",
                    model.file = mod.file),silent = F)
  out2sum <- posterior::as_draws_df(out2$samples) %>%
    summarise_draws() %>% 
    as.data.frame() %>% 
    filter(!is.na(rhat))
  
  
    
  }
  
  #launch_shinystan(shinystan::as.shinystan(out2$samples, model_name = paste(pr,mod.file))) 
  
 # load(paste("output/other harvest zip",pr,z,"alt mod.RData"))
 # 
 # 
 #  mean_tot_kill_ycg <- out2$samples %>% gather_draws(mean_totkill_ycg[y,c,g]) 
 # 
 #  if(length(jdat$castes) == 3){
 #    labs = c("D","B","AandE")
 #  }
 #  if(length(jdat$castes) == 4){
 #    labs = c("D","B","A","E")
 #  }
 #  
 #   mean_tot_kill_crane <- mean_tot_kill_ycg %>% 
 #    filter(g == which(grps == "CRANK")) %>% 
 #    group_by(y,c) %>% 
 #    summarise(mean = mean(.value),
 #              median = median(.value),
 #              lci = quantile(.value,0.025),
 #              uci = quantile(.value,0.975)) %>% 
 #    mutate(year = y + 1975,
 #           caste = factor(c, labels = labs,ordered = TRUE))
 #  
 #  ann_plot <- ggplot(data = mean_tot_kill_crane,
 #                     aes(x = year, y = mean, colour = caste))+
 #    geom_errorbar(aes(ymin = lci,ymax = uci),width = 0,alpha = 0.5,
 #                  position = position_dodge(width = 0.4))+
 #    geom_point(position = position_dodge(width = 0.4))
 #  print(ann_plot)
 #  
 #  
 #  mean_tot_kill_ycg_alt <- out2$samples %>% gather_draws(mean_totkill_ycg_alt[y,c,g]) 
 #  
 #  mean_tot_kill_crane_alt <- mean_tot_kill_ycg_alt %>% 
 #    filter(g == which(grps == "CRANK")) %>% 
 #    group_by(y,c) %>% 
 #    summarise(mean = mean(.value),
 #              median = median(.value),
 #              lci = quantile(.value,0.025),
 #              uci = quantile(.value,0.975)) %>% 
 #    mutate(year = y + 1975,
 #           caste = factor(c, labels = labs,ordered = TRUE))
 #  
 #  ann_plot <- ggplot(data = mean_tot_kill_crane_alt,
 #                     aes(x = year, y = median, colour = caste))+
 #    geom_errorbar(aes(ymin = lci,ymax = uci),width = 0,alpha = 0.5,
 #                  position = position_dodge(width = 0.4))+
 #    geom_point(position = position_dodge(width = 0.4))
 #  print(ann_plot)
 #  
 #  
 #  NSUCC_yg <- out2$samples %>% gather_draws(NSUCC_yg[y,g])
 #  
 #  mean_NSUCC_yg <- NSUCC_yg %>% 
 #    filter(g == which(grps == "CRANK")) %>% 
 #    group_by(y) %>% 
 #    summarise(mean = mean(.value),
 #              median = median(.value),
 #              lci = quantile(.value,0.025),
 #              uci = quantile(.value,0.975)) %>% 
 #    mutate(year = y + 1975)
 #  
 #  ann_plot <- ggplot(data = mean_NSUCC_yg,
 #                     aes(x = year, y = median))+
 #    geom_errorbar(aes(ymin = lci,ymax = uci),width = 0,alpha = 0.5)+
 #    geom_point()
 #  print(ann_plot)
 #  
 #  
 #  
 #  psucc_gcy <- out2$samples %>% gather_draws(psucc[g,c,y])
 #  
 #  mean_psucc_crane <- psucc_gcy %>% 
 #    filter(g == which(grps == "CRANK"),
 #           y > 43) %>% 
 #    group_by(y,c) %>% 
 #    summarise(mean = mean(.value),
 #              median = median(.value),
 #              lci = quantile(.value,0.025),
 #              uci = quantile(.value,0.975)) %>% 
 #    mutate(year = y + 1975,
 #           caste = factor(c, labels = labs,ordered = TRUE))
 #  
 #  ann_plot <- ggplot(data = mean_psucc_crane,
 #                     aes(x = year, y = mean, colour = caste))+
 #    geom_errorbar(aes(ymin = lci,ymax = uci),width = 0,alpha = 0.5,
 #                  position = position_dodge(width = 0.4))+
 #    geom_point(position = position_dodge(width = 0.4))
 #  print(ann_plot)
 #  
 #  
 #  "psucc",
 #  "pactive",
 #  "NACTIVE_cy",
 #  "pops_cor",
 #  "NSUCC_gcy"
 #  
 #  
  
  save(list = c("out2","jdat","grps","out2sum","attempts"),
       file = paste("output/other harvest zip",pr,z,"alt mod.RData"))
  
rm(list = "out2")
}
}

  }#i

stopCluster(cl = cluster)



  

