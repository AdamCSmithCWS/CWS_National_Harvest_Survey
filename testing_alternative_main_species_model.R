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
# library(doParallel)
# library(foreach)

#load.module("glm") 

 

# load output from data_prep.R --------------------------------------------



provzone <- read.csv("data/Province and zone table.csv")
provs = unique(provzone$prov)



spgp <- "goose"
pr = "ON"
z = 2 ### goose zone with 3 species but almost all harvest is 1 species
# group data set up -------------------------------------------------------

  period = read.csv(paste0("data/period.",spgp,".csv"))
  #aou.spgp <- sps[which(sps$group == spgp),"AOU"]
  provs2 <- provs
  mod.file = "models/simple_proportions_no_dirich.R" # 

  
  load(paste("data/data",pr,z,spgp,"save.RData",sep = "_"))




parms = c("pkill_py",
          "mut",
          "mu_ps",
          "mu_s",
          "alpha_s",
          "alpha_ps",
          "alpha_psy",
          "alpha_sy",
          "alpha_axsy",
          "alpha_py",
          "sd_alpha_s",
          "sd_alpha_period_s",
          "pcomp_sy",
          "pcomp_psy"
          )


#adaptSteps = 200              # Number of steps to "tune" the samplers.
burnInSteps = 5000            # Number of steps to "burn-in" the samplers.
nChains = 3                   # Number of chains to run.
numSavedSteps=1000          # Total number of steps in each chain to save.
thinSteps=10                   # Number of steps to "thin" (1=keep every step).
nIter = ceiling( ( (numSavedSteps * thinSteps )+burnInSteps)) # Steps per chain.

t1 = Sys.time()

   



# MCMC sampling -----------------------------------------------------------
jdat_new = list(w_psy = jdat$w_psy,
                nparts_py = jdat$nparts_py,
                nspecies = jdat$nspecies,
                nperiods = jdat$nperiods,
                nyears = jdat$nyears)


w_sy <- apply(jdat$w_psy,c(2,3),sum)
w_ps <- apply(jdat$w_psy,c(1,2),sum)


jdat_new[["w_sy"]] <- w_sy
jdat_new[["nparts_y"]] <- apply(jdat$nparts_py,2,sum)  

jdat_new[["w_ps"]] <- w_ps
jdat_new[["nparts_p"]] <- apply(jdat$nparts_py,1,sum)  

jdat_new[["midperiod"]] <- as.integer(floor(jdat$nperiods/2))


jdat[["w_sy"]] <- w_sy
jdat[["nparts_y"]] <- apply(jdat$nparts_py,2,sum)  

jdat[["w_ps"]] <- w_ps
jdat[["nparts_p"]] <- apply(jdat$nparts_py,1,sum)  

jdat[["midperiod"]] <- as.integer(floor(jdat$nperiods/2))


  out2 = try(jagsUI(data = jdat,
                    parameters.to.save = parms,
                    n.chains = 3,
                    n.burnin = burnInSteps,
                    n.thin = thinSteps,
                    n.iter = nIter,
                    parallel = T,
                    modules = "glm",
                    #model.file = mod.file
                    model.file = "models/species_harvest_model_alt.R"
  ),silent = F)

  
   out2sum <- posterior::as_draws_df(out2$samples) %>%
    summarise_draws() %>% 
    as.data.frame() %>% 
    filter(!is.na(rhat))

   
   #library(shinystan)  
shinystan::launch_shinystan(shinystan::as.shinystan(out2$samples)) 
 






