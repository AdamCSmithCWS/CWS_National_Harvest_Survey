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

#setwd("C:/GitHub/CWS_national_harvest_survey")
setwd("C:/Users/SmithAC/Documents/GitHub/CWS_national_harvest_survey")

Y <- 2023
FY = 1976
years <- FY:Y

names(years) <- paste(years)


library(jagsUI)
library(tidyverse)
# library(ggmcmc)
 library(tidybayes)
# library(ggrepel)
# library(ggforce)
library(doParallel)
library(foreach)

#load.module("glm") 

 

# load output from data_prep.R --------------------------------------------



provzone <- read.csv("data/Province and zone table.csv")
provs = unique(provzone$prov)

nfmurre <- data.frame(prov = "NF",
                      zone = 1,
                      spgp = "murre")
fit_table <- provzone %>% 
  select(prov,zone) %>% 
  expand_grid(.,spgp = c("duck","goose")) %>% 
  bind_rows(.,nfmurre) %>% 
  arrange(spgp)


#fit_table <- fit_table %>% filter(spgp %in% c("goose","murre") | (spgp == "duck" & prov == "ON" & zone == 3))
#fit_table <- fit_table %>% filter(spgp %in% c("duck") | (spgp == "goose" & prov %in% c("NT","YT")))

#fit_table <- fit_table %>% filter(spgp %in% c("duck"))
#fit_table <- fit_table %>% filter(paste0(spgp,prov,zone) %in% c("duckNB2","duckMB1","duckYT1"))
#fit_table <- fit_table %>% filter(paste0(spgp,prov,zone) %in% c("duckON2","duckON3","duckPQ1","duckPQ2","gooseNT1"))
fit_table <- fit_table %>% filter((spgp == "goose" & prov == "NF" & zone == 1))


 overwrite <- TRUE # set to TRUE if attempting to overwrite earlier model runs
 
 
# Province and Zone loop --------------------------------------------------
  n_cores <- 1
  cluster <- makeCluster(n_cores, type = "PSOCK")
  registerDoParallel(cluster)



  fullrun <- foreach(i = rev(c(1:nrow(fit_table))),
                     .packages = c("jagsUI","tidyverse","posterior"),
                     .inorder = FALSE,
                     .errorhandling = "pass") %dopar%
    {

      spgp <- as.character(fit_table[i,"spgp"])
      pr <- as.character(fit_table[i,"prov"])
      z <- as.character(fit_table[i,"zone"])
      
      period = read.csv(paste0("data/period.",spgp,".csv"))
      
if(overwrite | !file.exists(paste("output/full harvest zip",pr,z,spgp,"alt mod.RData"))){
      
if(file.exists(paste("data/data",pr,z,spgp,"save.RData",sep = "_"))){
load(paste("data/data",pr,z,spgp,"save.RData",sep = "_"))

  if(paste0(spgp,pr,z) %in% c("goosePQ1","goosePQ2","gooseON3")){
  mod.file = "models/species_harvest_model_alt_goose.R" # version with nu fixed at 3
  }else{
    if(spgp == "duck" & pr %in% c("ON","PQ")){
    mod.file = "models/species_harvest_model_more_flexible.R" #
    }else{
      mod.file = "models/species_harvest_model.R" #
      
    }
  }

  if(length(jdat$species_sparse) == 0){ # if there are no species below the threshold
    # if above fails, it
    mod.file = "models/species_harvest_model_allrich.R" #
    jdat$species_sparse <- NULL
    if(paste0(spgp,pr,z) %in% c("goosePQ1","goosePQ2","gooseON3")){
      mod.file = "models/species_harvest_model_alt_goose_allrich.R" # version with nu fixed at 3
    }
    
    
  }
  
  if(length(jdat$species_rich) == 0){ # if there are no species below the threshold
    # if above fails, it
    mod.file = "models/species_harvest_model_allsparse.R" #
    jdat$species_rich <- NULL
    # if(paste0(spgp,pr,z) %in% c("goosePQ1","goosePQ2","gooseON3")){
    #   mod.file = "models/species_harvest_model_alt_goose_allrich.R" # version with nu fixed at 3
    # }
    
    
  }
  
  
  if(spgp == "murre"){
  mod.file = "models/species_harvest_model_murre_allrich.R" #
  jdat$species_sparse <- NULL
  
  }
  

parms = c("NACTIVE_y",
          "NSUCC_y",
          "nu_day",
          "sdhunter_day",
          "mean_totkill_yc",
          "mean_totdays_yc",
          "kill_cy",
          "kill_ys",
          "kill_y",
          "kill_ysax",
          "days_y",
          "nu",
          "psi",
          "sdhunter",
          "sdyear",
          "sdyear_day",
          "cst",
          "cst_day",
          "ann",
          "ann_day",
          "padult_sy",
          "pfemale_sy",
          "parrive",
          "pleave",
          "pkill_py",
          "alpha_s",
          "alpha_ps",
          "alpha_sy",
          "alpha_psy",
          "alpha_axsy",
          "alpha_axs",
          "sd_alpha_s",
          "sd_alpha_axsy",
          "sd_axsy",
          "sd_alpha_period_s",
          "sd_psy",
          "pcomp_psy",
          "pcomp_axsy",
          "beta_sy",
          "beta_ps",
          "sd_alpha_psy"
          )


#adaptSteps = 200              # Number of steps to "tune" the samplers.
burnInSteps = 30000*5            # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=1000          # Total number of steps in each chain to save.
thinSteps=60*5                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( (numSavedSteps * thinSteps )+burnInSteps)) # Steps per chain.

t1 = Sys.time()

   


# MCMC sampling -----------------------------------------------------------

  out2 = try(jagsUI(data = jdat,
                    parameters.to.save = parms,
                    n.chains = 3,
                    n.burnin = burnInSteps,
                    n.thin = thinSteps,
                    n.iter = nIter,
                    parallel = T,
                    #modules = NULL,#"glm",
                    model.file = mod.file
                    #model.file = "models/species_harvest_model_alt4.R"
  ),silent = F)


  

if(class(out2) != "try-error"){


  
  
  out2sum <- posterior::as_draws_df(out2$samples) %>%
    summarise_draws() %>% 
    as.data.frame() %>% 
    filter(!is.na(rhat)) #removes the parameters that are fixed 

  # pcomp_psy <- out2sum %>% filter(grepl("pcomp_psy[",variable,fixed = TRUE))
  # 
  #  sum_pcomp_psy <- out2sum %>% filter(grepl("pcomp_psy[",variable,fixed = TRUE))%>%
  #        mutate(year = rep(c(1:jdat$nyears),each = jdat$nspecies*jdat$ndemog),
  #              species = rep(rep(c(1:jdat$nspecies),each = jdat$ndemog),times = jdat$nyears)) %>%
  #    left_join(.,sp.save.out,by = c("species" = "spn"))
  # 

#   alpha_ps <- out2sum %>% filter(grepl("alpha_ps[",variable,fixed = TRUE))
#   alpha_sy <- out2sum %>% filter(grepl("alpha_sy[",variable,fixed = TRUE))
#   alpha_psy <- out2sum %>% filter(grepl("alpha_psy[",variable,fixed = TRUE))
#  
#   pcomp_axsy <- out2sum %>% filter(grepl("pcomp_axsy[",variable,fixed = TRUE))
#   alpha_axs <- out2sum %>% filter(grepl("alpha_axs[",variable,fixed = TRUE))
#   
#   sd_alpha_axsy <- out2sum %>% filter(grepl("sd_alpha_axsy[",variable,fixed = TRUE))
#   
#   
#   beta_ps <- out2sum %>% filter(grepl("beta_ps[",variable,fixed = TRUE))
#   beta_sy <- out2sum %>% filter(grepl("beta_sy[",variable,fixed = TRUE))
#   
#  sum_pcomp_axsy <- out2sum %>% filter(grepl("pcomp_axsy[",variable,fixed = TRUE))%>%
#        mutate(year = rep(c(1:jdat$nyears),each = jdat$nspecies*jdat$ndemog),
#              species = rep(rep(c(1:jdat$nspecies),each = jdat$ndemog),times = jdat$nyears),
#              demog = rep(c(1:jdat$ndemog),times = jdat$nyears*jdat$nspecies)) %>%
#    left_join(.,sp.save.out,by = c("species" = "spn"))
# 
#  fail_axsy <- sum_pcomp_axsy %>%
#    mutate(fail_rh = ifelse(rhat > 1.1,TRUE,FALSE)) %>%
#    group_by(species,fail_rh) %>%
#    summarise(n_fail = n())
# # 
# #  #sum_beta_axsy <- out2sum %>% filter(grepl("beta_axsy",variable))
# #   sum_sd <- out2sum %>% filter(grepl("sd",variable,fixed = TRUE))
# # 
# #   sum_alpha_s <- out2sum %>% filter(grepl("alpha_s[",variable,fixed = TRUE))
# #   sum_alpha_sy <- out2sum %>% filter(grepl("alpha_sy[",variable,fixed = TRUE))
# #   sum_alpha_ps <- out2sum %>% filter(grepl("alpha_ps[",variable,fixed = TRUE))
# # 
# #   sum_pcomp_s <- out2sum %>% filter(grepl("pcomp_s[",variable,fixed = TRUE))
# #   sum_pcomp_s %>% filter(rhat > 1.1) %>% summarise(n_fail = n()/nrow(sum_pcomp_s))
# # 
# #   sum_pkill_py <- out2sum %>% filter(grepl("pkill_py[",variable,fixed = TRUE))
# # 
# # 
# #   sum_pcomp_psy <- out2sum %>% filter(grepl("pcomp_psy[",variable,fixed = TRUE))
# #   sum_pcomp_psy %>% filter(rhat > 1.1) %>% summarise(n_fail = n()/nrow(sum_pcomp_psy))
# # 
  # sum_pcomp_sy <- out2sum %>% filter(grepl("pcomp_sy[",variable,fixed = TRUE)) %>%
  #   mutate(year = rep(c(1:jdat$nyears),each = jdat$nspecies),
  #          species = rep(c(1:jdat$nspecies),times = jdat$nyears))
  # 
  # tst = sum_pcomp_sy %>% filter(rhat > 1.1) %>% group_by(year) %>% summarise(n_fail = n()/jdat$nspecies)
  # wh_fail <- ggplot(data = sum_pcomp_sy,aes(x = year,y = rhat,group = species,colour = species))+
  #   geom_line()
# #   print(wh_fail)
# # 
# # 
#   sum_kill_ys <- out2sum %>% filter(grepl("kill_ys[",variable,fixed = TRUE))%>%
#     mutate(year = rep(c(1:jdat$nyears),times = jdat$nspecies),
#            species = rep(c(1:jdat$nspecies),each = jdat$nyears)) %>%
#     left_join(.,sp.save.out,by = c("species" = "spn"))
# pdf(paste0("temp_kill_plot_full",pr,"_",z,".pdf"),
#     width = 11,
#     height = 8.5)
#  kill_plot <- ggplot(data = sum_kill_ys,aes(x = year,y = median))+
#    #  geom_ribbon(aes(x = year,y = mean,ymin = q5,ymax = q95,fill = species),alpha = 0.2, inherit.aes = FALSE)+
#    # geom_line()+
#    geom_errorbar(aes(x = year,y = median,ymin = q5,ymax = q95),
#                  alpha = 0.2, width = 0)+
#    geom_point()+
#    theme_bw()+
#    facet_wrap(vars(AOU),scales = "free")
#   print(kill_plot)
#  dev.off()
#  
#  raw_demog <- NULL
#  for(d in 1:jdat$ndemog){
#    for(s in 1:jdat$nspecies){
#      
#   
#    tmp <- data.frame(demog = d,
#                      species = s,
#                      year = 1:jdat$nyears,
#                      parts = jdat$w_axsy[d,s,])
#    
#    raw_demog <- bind_rows(raw_demog,tmp)
#    }
#  }
#   raw_demog <- raw_demog %>% 
#     group_by(species,year) %>% 
#     mutate(pdemog = parts/sum(parts))%>%
#     left_join(.,sp.save.out,by = c("species" = "spn"))
#  
#  
#  pdf(paste0("temp_demog_plot_full",pr,"_",z,".pdf"),
#      width = 11,
#      height = 8.5)
#  kill_plot <- ggplot(data = sum_pcomp_axsy,aes(x = year,y = median,
#                                                colour = factor(demog)))+
#    geom_point(data = raw_demog,
#                aes(x = year,y = pdemog),
#               size = 0.75,
#               shape = 3)+
#    #  geom_ribbon(aes(x = year,y = mean,ymin = q5,ymax = q95,fill = fill),alpha = 0.2, inherit.aes = FALSE)+
#    # geom_line()+
#    geom_errorbar(aes(x = year,y = median,ymin = q5,ymax = q95,
#                      colour = factor(demog)),
#                  alpha = 0.2, width = 0)+
#    geom_point()+
#    theme_bw()+
#    facet_wrap(vars(AOU),scales = "free")
#  print(kill_plot)
#  dev.off()
# # 
# #  
#  sum_kill_y <- out2sum %>% filter(grepl("kill_y[",variable,fixed = TRUE))%>%
#    mutate(year = rep(c(1:jdat$nyears),times = 1))
#  kill_plot <- ggplot(data = sum_kill_y,aes(x = year,y = median))+
#    #  geom_ribbon(aes(x = year,y = mean,ymin = q5,ymax = q95,fill = species),alpha = 0.2, inherit.aes = FALSE)+
#    # geom_line()+
#    geom_errorbar(aes(x = year,y = median,ymin = q5,ymax = q95),
#                  alpha = 0.2, width = 0)+
#    geom_point()+
#    theme_bw()
#  print(kill_plot)
#  
 #
 attempts <- 0
 #
 #  shinystan::launch_shinystan(shinystan::as.shinystan(out2$samples))
 #
 # 
  # out2test <- out2sum %>% filter(!grepl("axs",variable),
  #                                !grepl("ax",variable),
  #                                !grepl("pfemale",variable),
  #                                !grepl("padult",variable))
  
  # restart with final values as initial values
  # and sample more if >0.5% of parameters have rhat > 1.1
  while(quantile(out2sum$rhat,0.995) > 1.1 & attempts < 1){
    attempts <- attempts+1
    burnInSteps = 0
    thinSteps = thinSteps*5
    nIter = ceiling( ( (numSavedSteps * thinSteps )+burnInSteps)) # Steps per chain.
    
   # initls <- get_final_values(out2)
    
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
                      #modules = "glm",
                      model.file = mod.file))
                      
    out2sum <- posterior::as_draws_df(out2$samples) %>%
      summarise_draws() %>% 
      as.data.frame() %>% 
      filter(!is.na(rhat))
    
    
    
  }
  

  save(list = c("out2","jdat","sp.save.out","out2sum", "attempts"),
       file = paste("output/full harvest zip",pr,z,spgp,"alt mod.RData"))
  


}
}
}#end overwrite conditional
      
    }#end parallel cluster run

  stopCluster(cl = cluster)
  




