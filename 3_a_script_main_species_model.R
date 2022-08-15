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


Y <- 2021
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
  bind_rows(.,nfmurre)


#fit_table <- fit_table %>% filter(spgp %in% c("goose","murre") | (spgp == "duck" & prov == "ON" & zone == 3))
fit_table <- fit_table %>% filter(spgp %in% c("goose","murre"))



# Province and Zone loop --------------------------------------------------
  n_cores <- 16
  cluster <- makeCluster(n_cores, type = "PSOCK")
  registerDoParallel(cluster)



  fullrun <- foreach(i = c(1:nrow(fit_table)),
                     .packages = c("jagsUI","tidyverse","posterior"),
                     .inorder = FALSE,
                     .errorhandling = "pass") %dopar%
    {

      spgp <- fit_table[i,"spgp"]
      pr <- fit_table[i,"prov"]
      z <- fit_table[i,"zone"]
      
      period = read.csv(paste0("data/period.",spgp,".csv"))
      

if(file.exists(paste("data/data",pr,z,spgp,"save.RData",sep = "_"))){
load(paste("data/data",pr,z,spgp,"save.RData",sep = "_"))

  
  mod.file = "models/species_harvest_model_alt.R" # 
  


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
          "cst",
          "cst_day",
          "ann",
          "ann_day",
          "padult_sy",
          "pfemale_sy",
          "parrive",
          "pleave",
          "pkill_py",
          "mut",
          "mu_ps",
          "mu_s",
          "alpha_s",
          "alpha_ps",
          "alpha_psy",
          "alpha_sy",
          "alpha_axsy",
          "alpha_py",
          #"alpha_psy1",
          #"tau_alpha_psy",
          "sd_alpha_psy",
          "sd_alpha_s",
          "sd_alpha_period_s",
          "sd_psy",
          "pcomp_sy",
          "pcomp_psy",
          "pcomp_axsy",
          "pcomp_s",
          "pcomp_ps",
          "mu_psy",
          "beta_sy",
          "beta_ps",
          "sd_beta_sy",
          "sd_beta_ps",
          "sd_beta_axsy",
          "beta_axsy"
          )


#adaptSteps = 200              # Number of steps to "tune" the samplers.
burnInSteps = 5000            # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=1000          # Total number of steps in each chain to save.
thinSteps=10                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( (numSavedSteps * thinSteps )+burnInSteps)) # Steps per chain.

t1 = Sys.time()

   


# MCMC sampling -----------------------------------------------------------

w_sy <- apply(jdat$w_psy,c(2,3),sum)
w_ps <- apply(jdat$w_psy,c(1,2),sum)
w_s <- apply(jdat$w_psy,c(2),sum)

w_axs <- apply(jdat$w_axsy,c(1,2),sum)
jdat[["w_axs"]] <- w_axs
jdat[["nparts_s"]] <- apply(jdat$w_axsy,2,sum)  

jdat[["w_s"]] <- w_s
jdat[["nparts"]] <- sum(jdat$nparts_py)  


jdat[["w_sy"]] <- w_sy
jdat[["nparts_y"]] <- apply(jdat$nparts_py,2,sum)  

jdat[["w_ps"]] <- w_ps
jdat[["nparts_p"]] <- apply(jdat$nparts_py,1,sum)  

jdat[["midperiod"]] <- as.integer(floor(jdat$nperiods/2))
jdat[["midyear"]] <- as.integer(floor(jdat$nyears/2))

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

  
  t2 = Sys.time()
if(class(out2) != "try-error"){


  
  
  out2sum <- posterior::as_draws_df(out2$samples) %>%
    summarise_draws() %>% 
    as.data.frame() %>% 
    filter(!is.na(rhat))

  # sum_pcomp_axsy <- out2sum %>% filter(grepl("pcomp_axsy[",variable,fixed = TRUE))%>% 
  #       mutate(year = rep(c(1:jdat$nyears),each = jdat$nspecies*jdat$ndemog),
  #             species = rep(rep(c(1:jdat$nspecies),each = jdat$ndemog),times = jdat$nyears),
  #             demog = rep(c(1:jdat$ndemog),times = jdat$nyears*jdat$nspecies))
  # 
  # fail_axsy <- sum_pcomp_axsy %>% 
  #   mutate(fail_rh = ifelse(rhat > 1.1,TRUE,FALSE)) %>% 
  #   group_by(species,fail_rh) %>% 
  #   summarise(n_fail = n())
  # 
  # sum_beta_axsy <- out2sum %>% filter(grepl("beta_axsy",variable))
 #  sum_sd <- out2sum %>% filter(grepl("sd",variable,fixed = TRUE))
 #  
 #  sum_alpha_s <- out2sum %>% filter(grepl("alpha_s[",variable,fixed = TRUE))
 #  sum_alpha_sy <- out2sum %>% filter(grepl("alpha_sy[",variable,fixed = TRUE))
 #  sum_alpha_ps <- out2sum %>% filter(grepl("alpha_ps[",variable,fixed = TRUE))
 #  
 #  sum_pcomp_s <- out2sum %>% filter(grepl("pcomp_s[",variable,fixed = TRUE))
 #  sum_pcomp_s %>% filter(rhat > 1.1) %>% summarise(n_fail = n()/nrow(sum_pcomp_s))
 #  
 #  sum_pkill_py <- out2sum %>% filter(grepl("pkill_py[",variable,fixed = TRUE))
 #  
 #  
 #  sum_pcomp_psy <- out2sum %>% filter(grepl("pcomp_psy[",variable,fixed = TRUE))
 #  sum_pcomp_psy %>% filter(rhat > 1.1) %>% summarise(n_fail = n()/nrow(sum_pcomp_psy))
 #  
 #  sum_pcomp_sy <- out2sum %>% filter(grepl("pcomp_sy[",variable,fixed = TRUE)) %>% 
 #    mutate(year = rep(c(1:jdat$nyears),each = jdat$nspecies),
 #           species = rep(c(1:jdat$nspecies),times = jdat$nyears))
 #  tst = sum_pcomp_sy %>% filter(rhat > 1.1) %>% group_by(year) %>% summarise(n_fail = n()/jdat$nspecies)
 #  wh_fail <- ggplot(data = sum_pcomp_sy,aes(x = year,y = rhat,group = species,colour = species))+
 #    geom_line()
 #  print(wh_fail)
 #  
   attempts <- 0
 #  
 #  shinystan::launch_shinystan(shinystan::as.shinystan(out2$samples))
 #  
 #  
 #  sum_kill_ys <- out2sum %>% filter(grepl("kill_ys[",variable,fixed = TRUE))%>% 
 #    mutate(year = rep(c(1:jdat$nyears),times = jdat$nspecies),
 #           species = rep(c(1:jdat$nspecies),each = jdat$nyears)) %>% 
 #    left_join(.,sp.save.out,by = c("species" = "spn"))
 #   
 # kill_plot <- ggplot(data = sum_kill_ys,aes(x = year,y = mean,group = species,colour = species))+
 #    geom_ribbon(aes(x = year,y = mean,ymin = q5,ymax = q95,fill = species),alpha = 0.2, inherit.aes = FALSE)+
 #   geom_line()+
 #   facet_wrap(vars(AOU),scales = "free")
 #  print(kill_plot)
 #   
    
  out2test <- out2sum %>% filter(!grepl("axs",variable))
  while(any(out2test$rhat > 1.1) & attempts < 1){
    attempts <- attempts+1
    burnInSteps = 0
    thinSteps = thinSteps*3
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
  

  save(list = c("out2","jdat","sp.save.out","out2sum"),
       file = paste("output/full harvest zip",pr,z,spgp,"alt mod.RData"))
  


}
}
      
    }#end parallele cluster run

  stopCluster(cl = cluster)
  




