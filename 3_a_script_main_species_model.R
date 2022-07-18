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




# load("data/allkill.RData")
# ### species lists
# 
# aou.ducks <- sps[which(sps$group == "duck"),"AOU"]
# aou.goose <- sps[which(sps$group == "goose"),"AOU"]
# aou.murre <- sps[which(sps$group == "murre"),"AOU"]
# 


for(spgp in c("duck","goose","murre")){
### begining of loop through provinces only engage this loop if running the full analysis
### for a single province and zone, skip the next 4 lines
### and enter something like the following (e.g., to run Ontario-zone 3)

# group data set up -------------------------------------------------------

  period = read.csv(paste0("data/period.",spgp,".csv"))
  #aou.spgp <- sps[which(sps$group == spgp),"AOU"]
  provs2 <- provs
  mod.file = "models/species_harvest_model.R" # 
  
  # 
  # if(spgp == "goose"){
  #   
  #  
  #   
  #   # cal.spgp = calg
  #   # allkill = allkill
  #   # phunt = "PRHUNTG"
  #   # zhunt = "ZOHUNTG"
  #   # wkill = "TOGOK"
  #   # wact = "ACTIVEWF"
  #   # wsucc = "SUTOGO"
  #   # wday = "DAYWF"
  #   # nyears = length(years)
  #   # demog = data.frame(BSEX = rep(c("U","U"),each = 1),
  #   #                    BAGE = rep(c("A","I"),times = 1),
  #   #                    stringsAsFactors = F)
  #   # minyr <- min(years)
  #   non_res_combine = c("NF 1","NF 2","PE 1","NS 1","NS 2","BC 2","NT 1","YT 1","NB 1")
  #   
  #   
  # }
  # 
  # if(spgp == "duck"){
  #   
  #   # Y <- 2019
  #   # FY = 1976
  #   # years <- FY:Y
  #   # 
  #   # names(years) <- paste(years)
  #   # aou.spgp = aou.ducks
  #   # period = period.duck
  #   # cal.spgp = cald
  #   # allkill = allkill
  #   # phunt = "PRHUNT"
  #   # zhunt = "ZOHUNT"
  #   # wkill = "TODUK"
  #   # wact = "ACTIVEWF"
  #   # wsucc = "SUTODU"
  #   # wday = "DAYWF"
  #   # 
  #   # nyears = length(years)
  #   # demog = data.frame(BSEX = rep(c("F","M"),each = 2),
  #   #                    BAGE = rep(c("A","I"),times = 2),
  #   #                    stringsAsFactors = F)
  #   # minyr <- min(years)
  #   # provs2 <- provs
  #   # mod.file = "models/species_harvest_model.R" #
  #   
  # }
  # 
  # 
  # 
   if(spgp == "murre"){
  #   
  #   FY = 2014#### previous years Murre harvest was calculated differently, pre 2013 only total MURRK, and in 2013 it was a mix of infor from DAYOT and calendars and species composition
  #   years <- FY:Y
  #   
  #   names(years) <- paste(years)
  #   
  #   # aou.spgp = aou.murre
  #   # period = period.murre
  #   # cal.spgp = calm
  #   # allkill = allkill
  #   # phunt = "PRHUNTM"
  #   # zhunt = "ZOHUNTM"
  #   # wkill = "MURRK"
  #   # wact = "ACTIVEM"
  #   # wsucc = "SUCCM"
  #   # wday = "DAYM" #?
  #   #  nyears = length(years)
  #   # demog = data.frame(BSEX = rep(c("U","U"),each = 1),
  #   #                    BAGE = rep(c("A","I"),times = 1),
  #   #                    stringsAsFactors = F)
  #   # minyr <- FY
     provs2 = "NF"
  #   
  #   
       }
  # 
  # 

# Province and Zone loop --------------------------------------------------
  n_cores <- length(provs2)
  cluster <- makeCluster(n_cores, type = "PSOCK")
  registerDoParallel(cluster)



  fullrun <- foreach(pr = provs2,
                     .packages = c("jagsUI","tidyverse"),
                     .inorder = FALSE,
                     .errorhandling = "pass") %dopar%
    {

  #for(pr in provs2){
  zns <- unique(period[which(period$pr == pr),"zo"])
  
  # Set up parallel stuff
   
  for(z in zns){

if(file.exists(paste("data/data",pr,z,spgp,"save.RData",sep = "_"))){
load(paste("data/data",pr,z,spgp,"save.RData",sep = "_"))




parms = c("NACTIVE_y",
          "NSUCC_y",
          "nu_day",
          "sdhunter_day",
          "mean_totkill_yc",
          "mean_totdays_yc",
          "kill_cy",
          "kill_ys",
          "kill_y",
          "days_y",
          "nu",
          "sdhunter",
          "cst",
          "cst_day",
          "ann",
          "axcomp_axsy",
          "ann_day",
          "padult_sy",
          "pfemale_sy",
          "kill_ysax",
          "pcomp_psy",
          "parrive",
          "pleave",
          "psi",
          "pkill_py",
          "mut",
          "mu_ps",
          "mu_s",
          "alpha_s",
          "alpha_ps",
          "alpha_psy",
          "alpha_sy",
          #"alpha_psy1",
          #"tau_alpha_psy",
          "sd_alpha_psy",
          "sd_alpha_s",
          "sdalpha_perod_s",
          "pcomp_sy"
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
jdat[["w_sy"]] <- w_sy
jdat[["nparts_y"]] <- apply(jdat$nparts_py,2,sum)  
  out2 = try(jagsUI(data = jdat,
                    parameters.to.save = parms,
                    n.chains = 3,
                    n.burnin = burnInSteps,
                    n.thin = thinSteps,
                    n.iter = nIter,
                    parallel = T,
                    modules = "glm",
                    # model.file = mod.file
                    model.file = "models/species_harvest_model_alt4.R"
  ),silent = F)

  
  t2 = Sys.time()
if(class(out2) != "try-error"){


  
  
  out2sum <- posterior::as_draws_df(out2$samples) %>%
    summarise_draws() %>% 
    as.data.frame() %>% 
    filter(!is.na(rhat))
  
  attempts <- 0
  
  
  while(any(out2sum$rhat > 1.1) & attempts < 3){
    attempts <- attempts+1
    burnInSteps = 0
    thinSteps = thinSteps*2
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
                      n.thin = thinSteps*3,
                      n.iter = nIter*3,
                      inits = initls,
                      parallel = T,
                      modules = "glm",
                      model.file = "models/species_harvest_model_alt3.R"),silent = F)
    
    out2sum <- posterior::as_draws_df(out2$samples) %>%
      summarise_draws() %>% 
      as.data.frame() %>% 
      filter(!is.na(rhat))
    
    
    
  }
  

  save(list = c("out2","jdat","sp.save.out"),
       file = paste("output/full harvest zip",pr,z,spgp,"alt mod.RData"))
  


}
}
  }#z

}#pr
  stopCluster(cl = cluster)
  


}#spgp
# plotting comparisons to published estimates -----------------------------




