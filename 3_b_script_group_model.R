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


Y <- 2020
FY = 1976
years <- FY:Y

names(years) <- paste(years)

library(jagsUI)
library(tidyverse)
# library(ggmcmc)
# library(tidybayes)
# library(ggrepel)
# library(ggforce)
library(doParallel)
library(foreach)
library(posterior)

#load.module("glm") 

 

# load output from data_prep.R --------------------------------------------


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


# MCMC loops --------------------------------------------------------------

n_cores <- length(provs)
cluster <- makeCluster(n_cores, type = "PSOCK")
registerDoParallel(cluster)


fullrun <- foreach(pr = provs,
                   .packages = c("jagsUI","tidyverse","posterior"),
                   .inorder = FALSE,
                   .errorhandling = "pass") %dopar%
  {

#for(pr in provs2){
# Set up parallel stuff


for(z in 1:3){
# fullrun <- foreach(z = zns,
#                    .packages = c("jagsUI","tidyverse"),
#                    .inorder = FALSE,
#                    .errorhandling = "pass") %dopar%
#   {
    
    
    mod.file = "models/group_model.R" # 
    
    if(file.exists(paste("data/data",pr,z,"other_save.RData",sep = "_"))){
    load(paste("data/data",pr,z,"other_save.RData",sep = "_"))

parms = c("NACTIVE_y",
          "NSUCC_yg",
          "nu_day",
          "sdhunter_day",
          # "mean_totkill_ycg",
          # "mean_totdays_ycg",
          # "mean_totkill_ycg_alt",
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
          "tau_group")

parm_check <- c("NACTIVE_y",
  "NSUCC_yg",
  "kill_yg",
  "days_yg",
  "days_y")


#adaptSteps = 200              # Number of steps to "tune" the samplers.
burnInSteps = 5000           # Number of steps to "burn-in" the samplers.

nChains = 3                   # Number of chains to run.
numSavedSteps=1000          # Total number of steps in each chain to save.
thinSteps=10                   # Number of steps to "thin" (1=keep every step).


if(pr %in% c("YT","NT")){
  burnInSteps = burnInSteps*5
  thinSteps = thinSteps*5
}


nIter = ceiling( ( (numSavedSteps * thinSteps )+burnInSteps)) # Steps per chain.




  
  out2 = try(jagsUI(data = jdat,
                    parameters.to.save = parms,
                    n.chains = 3,
                    n.burnin = burnInSteps,
                    n.thin = thinSteps,
                    n.iter = nIter,
                    parallel = T,
                    #modules = "glm",
                    model.file = mod.file),silent = F)

  
  
if(class(out2) != "try-error"){
 
  
  out2sum <- posterior::as_draws_df(out2$samples) %>%
    select(starts_with(parm_check)) %>% 
    summarise_draws() %>% 
    as.data.frame() %>% 
    filter(!is.na(rhat))
  
  attempts <- 0
  
  
  while(any(out2sum$rhat > 1.1) & attempts < 3){
    attempts <- attempts+1
    burnInSteps = 0
    thinSteps = thinSteps*2
  nIter = ceiling( ( (numSavedSteps * thinSteps )+burnInSteps)) # Steps per chain.
  
  initls <- get_final_values(out2)
  
  # initls <- vector(mode = "list",3)
  # 
  # for(cc in 1:3){
  #   initls[[cc]] <- eval(parse(text = (paste0("as.list(out2$model$cluster",
  #                                             cc,
  #                                             "$state()[[1]])"))))
  # 
  # }
  
  
  out2 = try(jagsUI(data = jdat,
                    parameters.to.save = parms,
                    n.chains = 3,
                    n.burnin = burnInSteps,
                    n.thin = thinSteps,
                    n.iter = nIter,
                    inits = initls,
                    parallel = T,
                    #modules = "glm",
                    model.file = mod.file),silent = F)
  out2sum <- posterior::as_draws_df(out2$samples) %>%
    select(starts_with(parm_check)) %>% 
    summarise_draws() %>% 
    as.data.frame() %>% 
    filter(!is.na(rhat))
  
  
    
  }
  
  #launch_shinystan(shinystan::as.shinystan(out2$samples, model_name = paste(pr,mod.file))) 
  
  save(list = c("out2","jdat","grps","out2sum","attempts"),
       file = paste("output/other harvest zip",pr,z,"alt mod.RData"))
  
rm(list = "out2")
}
}

  }#z

}#pr
stopCluster(cl = cluster)


# 
# 
# # # plotting comparisons to published estimates -----------------------------
# 
# source("functions/comparison_plotting_function_other.R")
# 
# source("functions/utility_functions.R")
# 
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
#   tmp <- read.csv(file = paste0("data/reg_",spgp,".csv"))
#   names(tmp)[which(names(tmp) == "QC")] <- "PQ"
#   regs_other[[spgp]] <- tmp
# }
# 
# 
# non_res_combine = paste(rep(provs,each = 3),rep(c(1,2,3),times = length(provs)))
# #this above just ensures all non-resident hunters are combined with resident hunters for these groups, separating out caste E is rarely feasible (even caste B is sketchy)
# keep_E <- paste(rep(c("MB","NB","SK"),each = 3),rep(c(1,2,3),times = 3))
# # province and zone loops -------------------------------------------------
# non_res_combine <- non_res_combine[-which(non_res_combine %in% keep_E)]
# 
# 
# 
# 
# 
# 
# jjsimcomp = 1
# simcomp_list <-  list() 
# 
# 
# 
# for(pr in provs){
#   
#   
# 
#   # 
#   nyears = length(years)
#   minyr <- min(years)
#   # 
#   for(spgp in others){ 
#     tmp <- regs_other[[spgp]][,c("YEAR",pr)]
#     tmp[which(tmp[,pr] > 0),pr] <- 1
#     names(tmp) <- c("YEAR",spgp)
#     if(spgp == others[[1]]){
#       regs <- tmp
#     }else{
#       regs <- merge(regs,tmp,by = "YEAR")
#     }
#     
#   }
#   regs <- regs[which(regs$YEAR >= FY),]
#   regs <- regs[,which(colSums(regs) > 0)]
#   grps <- names(regs)[-1] #the -1 removes the column called
#   ngroups <- length(grps) #to enter model as data
#   if("SNIPK" %in% grps){
#     regs[which(regs$YEAR < 1992), "SNIPK"] <- 0
#   }### remove Snipe hunt per 1991
#   reg_mat <- as.matrix(regs[,grps]) #to enter model as data ensuring that group-level annual estimates are never > 0 in years with no season.
#   grps_f <- factor(grps,levels = grps,ordered = TRUE) #ensures consistent ordering of the harvested groups
#   
#   fyear = NA
#   for(g in 1:ngroups){
#     fyear[g] <- min(which(regs[,g+1] > 0))
#   }
#   
#   # data set up -------------------------------------------------------
#   
#   allkill = allkill
#   phunt = "PRHUNT"
#   zhunt = "ZOHUNT"
#   wkill = grps
#   wact = "ACTIVEOT"
#   wsucc = paste0("SU",gsub("K",replacement = "",x = grps)) 
#   wday = "DAYOT"
#   
#   
#   
#   mod.file = "models/group_model_zip23.R" # 
#   
#   
#   
#   
#   zns <- as.integer(unique(allkill[which(allkill[,phunt] == pr),zhunt]))
#   zns <- zns[which(zns > 0)]
#   for(z in zns){
#     
#     
#   
#   
#   
#  
#              mod.saved = paste("output/other harvest zip",pr,z,"alt mod.RData")
# 
#       
#       #mod.saved = paste("output/full harvest time sdhunter",pr,z,spgp,"alt mod.RData") #paste("output/full harvest",pr,z,spgp,"alt mod.RData")
#         if(file.exists(mod.saved)){
#           load(mod.saved) #load(paste("output/full harvest",pr,z,spgp,"alt mod.RData"))#        load(paste("output/full harvest caste time",pr,z,spgp,"mod.RData"))
# 
#          
# 
#           
#           var_pair = list(new = list("NACTIVE_y",
#                               "NSUCC_yg",
#                               "kill_yg",
#                               "days_y",
#                               "days_yg"),
#                       old = list("ACTIOT",
#                                wsucc,
#                                wkill,
#                                "DAYOT",
#                                NA),
#                       newgrps = list(NA,
#                                      wsucc,
#                                      wkill,
#                                      NA,
#                                      gsub(grps,pattern = "K",replacement = "Days"))) 
# 
#          
# 
#           
#           
#           
#           
# 
#   
#   simcomp_list[[jjsimcomp]] <- comp_plot_simple_other(prov = pr,
#                                                       zone = z,
#                                                       M = out2)
#  
#   
#   jjsimcomp <- jjsimcomp + 1  
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# }#end if jags output exists 
# 
# 
# 
#  }#z
# 
# }#pr
# 
#   
# ########## add a time-series plot of hte sd hunter values across castes (maybe possible to have a single sdhunter for all but caste 4)
# # plotting hunter effects -------------------------------------------------
# 
# # 
# #  
# #   
# #   jjhunter = 1
# #   hunter_list <- list()
# #   
# #   
# #   for(pr in provs2){
# #     zns <- unique(period[which(period$pr == pr),"zo"])
# #     for(z in zns){
# #       #       if(file.exists(paste("output/full harvest",pr,z,spgp,"mod.RData"))){
# #       # load(paste("output/full harvest",pr,z,spgp,"mod.RData"))
# #         if(file.exists(paste("output/hunter_effects",pr,z,spgp,"alt mod.RData"))){
# #           
# #           load(paste("output/hunter_effects",pr,z,spgp,"alt mod.RData"))#        load(paste("output/full harvest caste time",pr,z,spgp,"mod.RData"))
# #           
# #           
# #           
# #           
# #           
# #           
# #           hunter_list[[jjhunter]] <- comp_plot_hunter(prov = pr,zone = z)
# #           
# #           jjhunter = 1+jjhunter      
# #         }
# #         
# #       rm(out3)   
# #     
# #     }
# #   }
# # 
# #   
# #   # plotting pdfs -----------------------------------------------------------
#   
#   
#   
#   asuf <- c("ZIP")
#   
# 
#   
#   pdf(paste("output/comparison graphs simple other",asuf," ",".pdf"),
#       width = 10,
#       height = 7.5)
#   
#   for(pp in 1:length(simcomp_list)){
#     plt = simcomp_list[[pp]]
#     for(j in 1:length(plt)){
#       print(plt[[j]])
#     }}
#   dev.off()
#   
#   
  
  
  

