############################### model
############### ZIP time-series version
### data required
# pops = pops, # pops[c.y] total populations of permits by caste and year used to rescale all perhunter estimates to totals 
# #component that estimates p_active etc. to generate totals of active and successful hunters by year
# nactive = nactive, # nactive[c,y] number of active hunters by caste and year
# npotential = npotential, # npotential[c,y] number of potential hunters (respondents who bought a permit this year) by caste and year
# nsucc = nsucc, # nsucc[c,y] number of successful hunters by caste and year (active hunters with harvest > 0)
# #spcies composition components
# w_psy = partsarray, # w_psy[nperiods,nspecies,nyears] wings by period species year
# nparts_py = nparts_py, # nparts_py[nperiods,nyears] sum parts across species by period and year
# nparts_sy = nparts_sy, # nparts_sy[nspecies,nyears] sum parts species and year that have age and sex info
# kill_pyh = periodkill, # kill_pyh[nperiods,nyears,max(nhunters[y])] hunter-level total harvest by period from the calendars(separate hunter id caste doesn't matter)
# nkill_yh = nkill_yh, # nkill_yh[nyears,max(nhunters[y])] hunter-level summed harvest from calendar (separate hunter id caste doesn't matter)
# # demographic data for age and sex component of the model
# w_axsy = agesexarray, # w_axsy[ndemog,nspecies,nyears] wings by age-sex species year
# #indicators
# ndemog = ndemog, # 2 if geese (A and I) 4 if ducks (AF, IF, AM, IM) number of demographic units (age-sex combinations)
# nspecies = nspecies, # integer length = 1 number of species
# nyears = nyears, #integer length = 1 number of years
# nperiods = nperiods, # integer length = 1 number of periods
# nhunter_y = nhunter_y, # nhunter_y[nyears] number active hunters by year
# nhunter_cy = nhunter_cy, # nhunter_cy[castes,nyears] number active hunters by caste and year
# castes = castes, # castes (numeric, 1:4)
# ncastes = length(castes), # number of castes
# nhs , # integer length = 1 number of active hunters over all years (nrow for sumkill_active)
# #main data for overall harvest estimates
# hunter = hunter_n_cy, # vector(length = nhs) unique numeric indicator for active hunters by caste and year 
# kill = kill, # vector(length = nhs), total group (ducks, geese, murres) harvest of nhs response
# year = year, # vector(length = nhs), year of response
# caste = caste, # vector(length = nhs), caste of response
# days = days# vector(length = nhs), number of days spent hunting
# n_arrive #matrix nrow = ncastes, ncol = nyears, number of permits to add to the local populations based on the permist sampled outside of this zone but hutning here
# leave_hunt_cf #martrix nrow = nyears, ncol = 2, column-1 = number of hunters sampled here that hunted in a different zone, column-2 = total number of hunters
# demof #integer - indicates which demog are female
# demoa #integer - indicates which demog are male
# w_axs #total number of parts for each demographic category by species, across all years
# nparts_s # total number of parts with demog info by species, in all years
# w_s # total number of parts for each species - all years
# nparts #total number of parts all years and species
# w_sy # total number of parts for each species in each year
# nparts_y # total number of parts each year for all species
# w_ps #number of parts by period and species, summed across years
# nparts_p #total number of parts in each period all species and years
# midperiod # sets the second year as the mid-point of the period-time-series structure (period-2 tends to have the most parts)
# midyear  #sets the mid-point of annual time-series structures to the middle of the available years
# nspecies_rich #species with sufficient parts to make estimating time-varying demographic proportions reasonble

###### see below for alternate w[p,s,y,h] - hunter-specific data
#### hunter specific data would be better because it would better account for the uncertainty among hunters in the species composition
#### currently, this assumes that the selection of hunters in a given zone and year is representative
#### this over-estimates the certainty of the annual estimates of composition, and downweights the influence of the normalizing prior


model {
  
  
  ### probability a potential hunter is active
  #nactive = data (number of HQS responses from active hunters in a given caste and year)
  #npotential = data (number of HQS responses from potential hunters in a given caste and year)
  for(y in 1:nyears){
    
    ## two correction factors to account for the inter-province hunting
    ## pleave = proportion of permit population that hunted somewhere else (i.e, the proportion of the pops that are hunting somewhere else and should be removed, factor by which pops should be reduced)
    leave_hunt_cf[y,1] ~ dbinom(pleave[y],leave_hunt_cf[y,2])
    ## n_arrive[c,y] = caste and year estimate of the number of permits that should be added to the total population
    ## currently not estimated as part of the model because the number of permits used to estimate this value is extremely large (all permits sampled in all other zones) and so the binomial error associated with it should be trivial
    
 
  for(c in 1:ncastes){
    
    pops_cor[c,y] <- pops[c,y]+(n_arrive[c,y])-(pops[c,y]*pleave[y])
    
    
    nactive[c,y] ~ dbinom(pactive[c,y],npotential[c,y])
    NACTIVE_cy[c,y] <- pactive[c,y]*pops_cor[c,y] #NACTIVE_cy used (called H sub_c sub_y in Smith et al.) to rescale all per-hunter values in the derived quantities
    
    nsucc[c,y] ~ dbinom(psucc[c,y],nactive[c,y])
    NSUCC_cy[c,y] <- psucc[c,y]*NACTIVE_cy[c,y]
    
  }
   NSUCC_y[y] <- sum(NSUCC_cy[castes,y]) 
   NACTIVE_y[y] <- sum(NACTIVE_cy[castes,y])  
  }
  
  ### all species harvest
  for(i in 1:nhs){
    
    
   ### active hunters, 
    ########### could try a model that only models the kill values for hunters with kill > 0, then moltiplies the derived parameters by the psucc
    ############ loop only includes active hunters because if days[i] = 0 (i.e., they're not active), then there is no uncertainty about kill[i] (kill must = 0)
    
    ### number harvested
    kill[i] ~ dpois(lambda[i]) #kill is data - each hunter's estimate of the total number of ducks killed 
    lambda[i] <- lambda1[i]*z[i] + 0.00001 ## hack required for JAGS -- otherwise 'incompatible'-error
    z[i] ~ dbern(psi[year[i]]) #psi = proportion of non-zeros for each year
    log(lambda1[i]) <- elambda1[i] #
    elambda1[i] <- ann[year[i]] + cst[caste[i],year[i]] + hntr[caste[i],year[i],hunter[i]] + elambda_day[i] #elambda_day[i] acts as an offset on effort
    
    ## ann[y] is a yearly intercept for all species kill
    ## cst[c] is a caste-specific intercept for all species kill
    ## hntr[c.y,h] is an individual random error term to account for the individual hunter skill and overdispersion, currently normal distribution

    

    
    
    ### number of days truncated Poisson, because only active hunters are included and therefore days != 0
    days[i] ~ dpois(lambda_day[i])T(1,) #days is data - each hunter's estimate of the total number of days spent hunting
    log(lambda_day[i]) <- elambda_day[i] 
    elambda_day[i] <- ann_day[year[i]] + cst_day[caste[i],year[i]] + hntr_day[caste[i],year[i],hunter[i]]
    
    ## ann_day[y] is a yearly intercept for all species activity
    ## cst_day[c] is a caste-specific intercept for all species activity
    ## hntr_day[c.y,h] is an individual random error term to account for the individual hunter behaviour and overdispersion, currently a t-distribution
    
    
  }#i
  
  ### zip priors 
  ### psi is the estimated annual proportion of waterfowl hunters that hunt the group of interest (e.g., ducks or geese)
#logistic random-walk time series model for psi
  

  tau_alpha_psi ~ dscaled.gamma(0.5,50) # assumption that the
  alpha_psi[1] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008) doi:10.1214/08-AOAS191
  
  logit(psi[1]) <- alpha_psi[1]
  
  
   tau_alpha_pleave ~ dscaled.gamma(0.5,50) # assumption that the
  alpha_pleave[1] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008) doi:10.1214/08-AOAS191
  logit(pleave[1]) <- alpha_pleave[1]
  
  
  for(y in 2:nyears){
    
      alpha_psi[y] ~ dnorm(alpha_psi[y-1],tau_alpha_psi) 
    logit(psi[y]) <- alpha_psi[y]
    
    alpha_pleave[y] ~ dnorm(alpha_pleave[y-1],tau_alpha_pleave) 
    logit(pleave[y]) <- alpha_pleave[y]
    
  }
  

  # 
  
 CCST[1] <- 0
 CCST_day[1] <- 0
  for(c in 2:ncastes){
  CCST[c] ~ dnorm(0,0.1)
  CCST_day[c] ~ dnorm(0,0.1)
  }

 for(y in 1:nyears){  ### for caste 1, caste effects fixed at 0
   
   cst[1,y] <- 0
   cst_day[1,y] <- 0
 }
 
 for(c in 2:ncastes){
      for(y in 1:nyears){  ### random effects for caste effects, allowing them to vary randomly by year
     
    cst[c,y] ~ dnorm(CCST[c],tau_cst[c])
    cst_day[c,y] ~ dnorm(CCST_day[c],tau_cst_day[c])
    }
    tau_cst[c] ~ dscaled.gamma(0.5,50) # assumption that the
    tau_cst_day[c] ~ dscaled.gamma(0.5,50)
  }
  
  
  for(c in 1:ncastes){
    ## harvest rate priors
    for(yp in 1:2){ ## adding a first-half vs last-half of time-series split on hunter variance
      retrans_hunter[c,yp] <- 0.5*(1/tauhunter[c,yp])/nu_ret[c,yp] 
      sdhunter[c,yp] <- 1/pow(tauhunter[c,yp],0.5)
      tauhunter[c,yp] ~ dscaled.gamma(0.5,50)
      nu[c,yp] ~ dgamma(2,0.2)
      nu_ret[c,yp] <- (1.422*nu[c,yp]^0.906)/(1+(1.422*nu[c,yp]^0.906)) #approximate retransformation to equate a t-distribution to a normal distribution - see appendix of Link et al. 2020 BBS model selection paper
    }
    
    #activity (days) variance priors
    retrans_hunter_day[c] <- 0.5*(1/tauhunter_day[c])/nu_day_ret[c]
    sdhunter_day[c] <- 1/pow(tauhunter_day[c],0.5)
    tauhunter_day[c] ~ dscaled.gamma(0.5,50)
    nu_day[c] ~ dgamma(2,0.2)
    nu_day_ret[c] <- (1.422*nu_day[c]^0.906)/(1+(1.422*nu_day[c]^0.906)) #approximate retransformation to equate a t-distribution to a normal distribution - see appendix of Link et al. 2020 BBS model selection paper
    

    
    #logistic regression time-series model for the proportion that are active and successful
     mmu_psucc[c,1] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008) doi:10.1214/08-AOAS191
    # phi_psucc[c] ~ dgamma(0.001,0.001)
     tau_mmu_psucc[c] ~ dscaled.gamma(0.5,50)#time-series variance
     # 
     mmu_pactive[c,1] ~ dt(0, 1/2.5^2, 1) # cauchy(0, 2.5) prior (Gelman et al., 2008) doi:10.1214/08-AOAS191
     #phi_pactive[c] ~ dscaled.gamma(0.5,50) #
     tau_mu_pactive[c] ~ dscaled.gamma(0.5,50) #time-series variance
     
     ##time series model for the proportion that are active
     for(y in 2:nyears){
       mmu_psucc[c,y] ~ dnorm(mmu_psucc[c,y-1],tau_mmu_psucc[c])
       mmu_pactive[c,y] ~ dnorm(mmu_pactive[c,y-1],tau_mu_pactive[c])
     }
     
    for(y in 1:nyears){
      
      logit(psucc[c,y]) <- mmu_psucc[c,y]
      logit(pactive[c,y]) <- mmu_pactive[c,y]
      
      
      
      ############# hunter-specific effects
      for(h in 1:nhunter_cy[c,y]){
        ## hntr[c,y,h] is an individual random error term to allow for overdispersion in the harvest,
        ##  normal-distribution used for overdispersion in harvest assuming that activity accounts for most of the heavy-tailed variation in harvest
        ## hntr_day[c,y,h] is an individual random error term to allow for overdispersion in the activity (days spent hunting),
        ##  t-distribution used for overdispersion in activity to allow for heavy-tailed 
        ## variances on both activity and harvest are caste-specific precision to allow hunter-level dispersion to vary among castes
        ## consider whether tauhunter should also vary among all years...
        wy[c,y,h] <- step(y-(nyears/2))+1 #trick to identify if this value is in the first of last half of the time-series
        hntr_day[c,y,h] ~ dt(0,tauhunter_day[c],nu_day[c])
        hntr[c,y,h] ~ dt(0,tauhunter[c,wy[c,y,h]],nu[c,wy[c,y,h]])
        #n days probably accounts for a bit of the hunting skill effect as well as the activity effect
        # but not for goose harvests in eastern Canada - large hunter level variation and changing variation over time
      }#h
    }#y
    
  }#c
  
  ### yearly intercepts of total kill by first-difference 
  ann[1] ~ dnorm(0,0.1) # fixed effects for year-1 annual harvest level
  ann_day[1] ~ dnorm(0,0.1) # fixed effect for year-1 annual activity level
  
  for(y in 2:nyears){
    ann[y] ~ dnorm(ann[y-1],tauyear)
    ann_day[y] ~ dnorm(ann_day[y-1],tauyear_day)
      }
  
  # first-difference harvest and activity variance priors
   sdyear <- 1/pow(tauyear,0.5)
 tauyear ~ dscaled.gamma(0.5,50)
 sdyear_day <- 1/pow(tauyear_day,0.5)
 tauyear_day ~ dscaled.gamma(0.5,50)
  # 
  
  
  ##################################
  ### derived estimates of mean harvest by hunter ###############
  
  for(c in 1:ncastes){
    for(y in 1:nyears){
      ## derived estimated means, which can then be multiplied by the extrapolation factors for each caste and year
      # estimate of the mean (per hunter) kill per caste, and year
      # these are alternate calculations of the mean total kill and days which
      # do not rely on the log-normal assumption of the transformation factors
      # unfortunately, they often result in extremely unstable estimates affected by some long tails in the
      # distribution of hunter effects, so they're not used. 
      #   for(h in 1:nhunter_cy[c,y]){
      # 
      #   #hunter-level predictions of mean kill
      #   totkill_hcy[y,c,h] <- exp(ann[y] + cst[c,y] + hntr[c,y,h] + ann_day[y] + cst_day[c,y] + hntr_day[c,y,h]) *psi[y]
      #   totdays_hcy[y,c,h] <- exp(ann_day[y] + cst_day[c,y] + hntr_day[c,y,h])
      #   }
      # #mean per-hunter kill and days by year and caste
      # mean_totkill_yc_alt[y,c] <- mean(totkill_hcy[y,c,1:nhunter_cy[c,y]]) #mean kill per active hunter
      # mean_totdays_yc_alt[y,c] <- mean(totdays_hcy[y,c,1:nhunter_cy[c,y]]) #mean days per active hunter

      #mean per-hunter kill and days by year and caste - alternative estimate
      wy2[y,c] <- step(y-(nyears/2))+1 #trick to identify if this value is in the first of last half of the time-series
      
      mean_totkill_yc[y,c] <- exp(ann[y] + cst[c,y] + ann_day[y] + cst_day[c,y] + retrans_hunter_day[c] + retrans_hunter[c,wy2[y,c]]) *psi[y]
      mean_totdays_yc[y,c] <- exp(ann_day[y] + cst_day[c,y] + retrans_hunter_day[c])
      
      for(p in 1:nperiods){
        ## estimate of the mean (per hunter) kill per period, caste, and year
        mean_kill_pcy[p,c,y] <-  pkill_py[p,y] * mean_totkill_yc[y,c]
        #mean_kill_pcy[p,c,y] <-  pkill_py[p,y] * mean_totkill_retrans_yc[y,c] #alternate harvest estimate using retransformation fators
        
        for(s in 1:nspecies){
          # estimate of the mean (per hunter) kill, by period, caste, year, and species
          # mean kill by period caste and year * proportional composition of each species in each period and year
          mean_kill_pcys[p,c,y,s] <- mean_kill_pcy[p,c,y] * pcomp_psy[p,s,y]
          kill_pcys[p,c,y,s] <- mean_kill_pcys[p,c,y,s] * NACTIVE_cy[c,y]
          #
        }#s
      }#p
    }#y
  }#c
  
 #total harvest estimate by species year and period
 for(p in 1:nperiods){
   for(y in 1:nyears){
     for(s in 1:nspecies){
       kill_pys[p,y,s] <- sum(kill_pcys[p,1:ncastes,y,s])
     }#p
   }#y
 }#s
 

  #total harvest estimate by species year and caste
  ## mean harvest and days * population sizes of each caste and year * estimated proportion of population that is active to generate final harvest estimates.
  for(y in 1:nyears){
   
    for(c in 1:ncastes){
      kill_cy[c,y] <- mean_totkill_yc[y,c]*NACTIVE_cy[c,y] #total kill by caste and year
      days_cy[c,y] <- mean_totdays_yc[y,c]*NACTIVE_cy[c,y] #total days by caste and year
      
      for(s in 1:nspecies){
  kill_cys[c,y,s] <- sum(kill_pcys[1:nperiods,c,y,s])
      }#s
    }#c
    
    

      
    ##### kill_ys = species level total estimated harvest by year
    for(s in 1:nspecies){
      kill_ys[y,s] <- sum(kill_cys[castes,y,s])
        for(d in 1:ndemog){
            ### kill_ysax = total harvest by age/sex species and year (e.g., number of adult female mallards killed)
          kill_ysax[d,s,y] <- pcomp_axsy[d,s,y]*kill_ys[y,s]
        }#d
      padult_sy[s,y] <- sum(pcomp_axsy[demoa,s,y])
      pfemale_sy[s,y] <- sum(pcomp_axsy[demof,s,y])
    }#s
    
    #### summed total harvest and activity (e.g., all ducks) across all species and castes in a given year
    kill_y[y] <- sum(kill_cy[castes,y])
    days_y[y] <- sum(days_cy[castes,y])
    
  }#y
  
  
 
 
 
 
 
 
 
 

  
  ###################################
  ######## Multinomial components - 3 separate parts
  ### 1 - proportional distribution of all harvest across periods
  ### 2 - proportional distribution of species harvest across periods
  ### 3 - proportional distribution of age and sex categories by species and year
  
  ### estimating the proportional-seasonal distribution of total harvest by periods
  ## kill_pyh[p,y,h] is data, the hunter-level total harvest by period from the calendars
  ## nkill_yh[y,h] is also data, hunter-level total estimate of their harvest
  ### assumes that seasonal distribution is the same across castes...questionable...
  
######### As of 2021 - removed the dirichlet priors and instead using
 #### a multinomial logistic regression approach that has much improved sampling efficiency
 #### over the approach in Smith et al. 2020 that involves the dirichlet priors
 #### should have done this from the start
 #### multinomial log-ratios, time-series model for the distribution of harvest across periods
 #### fixed effects for the mean harvest across periods in the middle year
 #### hyperparameter alpha_py[p] and tau_alpha_py[p], shrinking each period's estimate in a given year
 #### towards last years proportion of the hunt occurring within that period
 ### time-series first difference model through years on the period-specific parameters
 
 
#####################################################################
  #### proportional distribution of all birds killed across periods
  ### this component ignores caste specific variation in the seasonal spread of the hunt
  ### multinomial distribution across periods  
  ### kill_pyh and nkill_yh[y,h] are data
  for (y in 1:nyears) {
    for(h in 1:nhunter_y[y]){
      kill_pyh[1:nperiods,y,h] ~ dmulti(pkill_py[1:nperiods,y], nkill_yh[y,h])   # multinom distr vector responses
    }#h
  }#y
  
  ##### pkill_py[p,y] is the estimated proportion of the total duck harvest in year-y that occurred in period-p
  for (p in 1:nperiods){
    for (y in 1:nyears){
      pkill_py[p,y] <- delta_py[p,y] / sum(delta_py[1:nperiods,y])
      delta_py[p,y] <- exp(alpha_py[p,y])
      # 
       } #y
  }#p

  
  #### multinomial log-ratios, time-series model for the distribution of harvest across periods
  #### fixed effects for the mean harvest across periods in the middle year
  #### hyperparameter alpha_py[p] and tau_alpha_py[p], shrinking each period's estimate in a given year
  #### towards last years proportion of the hunt occurring within that period
  ### time-series first difference model through years on the period-specific parameters
  alpha_py[1,1] <- 0
  for(y in 2:nyears){
    alpha_py[1,y] <- 0#~ dnorm(alpha_py[1,y-1],tau_alpha_py[1])
  }
  tau_alpha_py[1] ~ dscaled.gamma(0.5,50)
  for(p in 2:nperiods){
    alpha_py[p,midyear] ~ dnorm(0,0.5) # fixed effect period-effects on total kill
    #alpha_py[p,1] <- alpha_p[p]
    for(y in (midyear+1):nyears){
      
       alpha_py[p,y] ~ dnorm(alpha_py[p,y-1],tau_alpha_py[p]) # first difference model through time
    }
    for(y in 1:(midyear-1)){
      alpha_py[p,y] ~ dnorm(alpha_py[p,y+1],tau_alpha_py[p]) # first difference model through time
    }
    
    tau_alpha_py[p] ~ dscaled.gamma(0.5,50)
  }#p
  
  


  
  
 
  ######## The composition model still doesn't use the variation among hunters in the species distribution
  ### but perhaps it should, commented-out section that follows suggests how it could work
  ##### species composition
  #     for (p in 1:nperiods){
  # 
  # for (y in 1:nyears) {
  #   for(h in 1:nhuntscs[y]){
  # w[p,1:nspecies,y,h] ~ dmulti(pcomp_psy[p,1:nspecies,y], nparts_py[p,y,h])   # multinom distr vector responses
  # 
  # nparts_py[p,y,h] <- sum(w[p,1:nspecies,y,h])# yearl and period sums of all parts
  #   }#h
  # }#y
  
##################################################################################
  ## species composition across periods and years
  ## modification of the model used in Smith et al. 2020. 
  ## this approach uses all-year and all-period summaries of the parts to estimate
  ## predictors on the yearly proportions of species in each period
  

  # this is a sub-model that estimates the proportion of each species in teh harvest in each year
  # summing across all of the periods
  # this estimate is useful to generate the alpha_sy[s,y] values that are used as a predictor in the 
  # full annual period and year specific proportions
 w_s[1:nspecies] ~ dmulti(pcomp_s[1:nspecies], nparts)   # w_s is the total number of parts for each species (across all years), nparts is the sum of the total parts
  
 
   for (s in 1:nspecies){
   pcomp_s[s] <- delta_s[s] / sum(delta_s[1:nspecies])
     delta_s[s] <- exp(alpha_s[s])
   }   
    
    
   for(y in 1:nyears){
     w_sy[1:nspecies,y] ~ dmulti(pcomp_sy[1:nspecies,y], nparts_y[y])   # w_sy is the annual number of parts for each species (across all periods), nparts_y is the sum of the total parts for each year
     
   }
  
  
  
  ### extra data of parts by species and year helps estimate the various proportions
  
   for (s in 1:nspecies){
  
     for (y in 1:nyears){
       pcomp_sy[s,y] <- delta_sy[s,y] / sum(delta_sy[1:nspecies,y])
       delta_sy[s,y] <- exp(alpha_sy[s,y])
       
     } #y
     
   }#s   
  
  
  
  #### multinomial log-ratios, time-series model for the species composition
  tau_alpha_s ~ dscaled.gamma(0.1,50) #time-series variance
  sd_alpha_s <- 1/sqrt(tau_alpha_s)
  
  alpha_s[1] <- 0
  
  for(s in 2:nspecies){
    alpha_s[s] ~ dnorm(0,0.5)
  }
  
  ### first species is the reference group in each year
  for(y in 1:nyears){
    alpha_sy[1,y] <- 0 #~ dnorm(alpha_sy[1,y-1],tau_alpha_s[s]) 
  }
  
  for(s in 2:nspecies){
    alpha_sy[s,midyear] <- alpha_s[s] # ~ dnorm(0,0.5) 
    for(y in (midyear+1):nyears){
      
      alpha_sy[s,y] ~ dnorm(alpha_sy[s,y-1],tau_alpha_s) 
    }
    for(y in 1:(midyear-1)){
      alpha_sy[s,y] ~ dnorm(alpha_sy[s,y+1],tau_alpha_s) 
    }
  }#s
  
  ###time series of species overall proportions in the parts
  
  
  
  # this is a sub-model that estimates the proportion of each species in teh harvest in each period
  # summing across all years
  # this estimate is useful to generate the alpha_ps[period,species] values that are also used as a predictor in the 
  # full annual period and year specific proportions
   for (p in 1:nperiods){
     
     w_ps[p,1:nspecies] ~ dmulti(pcomp_ps[p,1:nspecies], nparts_p[p])   # multinom distr vector responses
     
   }
  
   # period effects by species
   for (s in 1:nspecies){
   for (p in 1:nperiods){
       pcomp_ps[p,s] <- delta_ps[p,s] / sum(delta_ps[p,1:nspecies])
       delta_ps[p,s] <- exp(alpha_ps[p,s])
       
     } #p
     
   }#s
  

  
  
  taualpha_perod_s ~ dscaled.gamma(1,50) # period-series variance
  sd_alpha_period_s <- 1/sqrt(taualpha_perod_s)
  
  
  for(p in 1:nperiods){
    
    alpha_ps[p,1] <- 0 # sets first species to the reference group in each period
  }
  
  for(s in 2:nspecies){
    alpha_ps[midperiod,s] ~ dnorm(0,0.5) #fixed effect for species 2:nspecies in middle period
  }
  
  for(p in (midperiod+1):nperiods){
    
    for(s in 2:nspecies){
      alpha_ps[p,s] ~ dnorm(alpha_ps[p-1,s],taualpha_perod_s) #species/period-specific effect
      
    }
  }
  
  for(p in 1:(midperiod-1)){
    
    for(s in 2:nspecies){
      alpha_ps[p,s] ~ dnorm(alpha_ps[p+1,s],taualpha_perod_s) #species/period-specific effect
      
    }
  }
  
  
  ##### full period by species, by year estimates of composition
  ### using the alpha_sy and alpha_ps parameters above as predictors on the 
  ### period, year, and species specific proportions
  
  ## the following betas are the coefficients that control the contribution of
  ## the alpha_sy and alpha_ps to teh alpha_psy
  ## they are random effects that vary by year and period
  tau_beta_sy ~ dscaled.gamma(0.1,50) #extra variance
  sd_beta_sy <- 1/sqrt(tau_beta_sy)
  tau_beta_ps ~ dscaled.gamma(0.1,50) #extra variance
  sd_beta_ps <- 1/sqrt(tau_beta_ps)
  BETA_sy ~ dnorm(1,1) # relatively narrow and primarily positive prior
  for(p in 1:nperiods){
    beta_sy[p] ~ dnorm(BETA_sy,tau_beta_sy)# coefficient controlling the influence of the species by year component - varies by period
  }
  BETA_ps ~ dnorm(1,1)
  for(y in 1:nyears){
    beta_ps[y] ~ dnorm(BETA_ps,tau_beta_ps)# coefficient controlling the influence of the period-specific pattern - varies by year
  }
  
  for (p in 1:nperiods){
    for (y in 1:nyears) {
      w_psy[p,1:nspecies,y] ~ dmulti(pcomp_psy[p,1:nspecies,y], nparts_py[p,y])   # multinom distr vector responses
    }
    
    ##### pcomp_psy[p,s,y] is the estimated proportion of the species composition in year-y, period-p, that is species-s
    
    for (s in 1:nspecies){
      for (y in 1:nyears){
        pcomp_psy[p,s,y] <- delta_psy[p,s,y] / sum(delta_psy[p,1:nspecies,y]) # softmax regression 
        delta_psy[p,s,y] <- exp(alpha_psy[p,s,y])
        alpha_psy[p,s,y] <- beta_sy[p]*alpha_sy[s,y] + beta_ps[y]*alpha_ps[p,s] # combination of species effect, species-year effect, species period effect
      } #y
      
    }#s
    
  }#p
  
  
  
  
  
  
  
  
  
  
  
  
  #####################################################################
  #### proportional distribution of age and sex by species
  #### pcomp_axsy[1:ndemog,s,y] = proportional distribution of age and sex classes by species and year
  
  ##### demographic composition

  ### model only allows demographic composition to vary throught time, if the species
  ### has at least 300 parts - i.e., at least 6 parts per year on average
  ### given that there are 4 proportions/year and ~50 years, this demographic composition
  ### requires a lot of data to estimate so many parameters 
  
  for (s in 1:nspecies){
    
    for (y in 1:nyears) {
      w_axsy[1:ndemog,s,y] ~ dmulti(pcomp_axsy[1:ndemog,s,y], nparts_sy[s,y])   # multinom distr vector responses
      
    }
    
    ##### pcomp_axsy[d,s,y] is the estimated proportion of the species harvest that is category-d of age and sex in year-y
    ### for ducks d is 1 for AF, 2 for IF, 3 for AM, and 4 for IM 
    
    for (d in 1:ndemog){

      for (y in 1:nyears){
        pcomp_axsy[d,s,y] <- delta_axsy[d,s,y] / sum(delta_axsy[1:ndemog,s,y])
        delta_axsy[d,s,y] <- exp(alpha_axsy[d,s,y])
           } #y
    }#d
    }#s
  
  #### multinomial log-ratios, model for the age and sex (ducks) or age (geese) composition

  ## annual variation only for data-rich species
  for(s in species_rich){
    tau_alpha_axsy[s] ~ dscaled.gamma(0.5,50) #variance of year-effects for the demographic parameters by species
    sd_alpha_axsy[s] <- 1/sqrt(tau_alpha_axsy[s])

    alpha_axs[ndemog,s] <- 0 #final demographic category fixed at 0

    for(y in 1:nyears){
      alpha_axsy[ndemog,s,y] <- alpha_axs[ndemog,s]  #final demographic group is fixed at 0 in all years for each species
    }#y

    for(d in 1:(ndemog-1)){
      alpha_axs[d,s] ~ dnorm(0,0.1)#tau_alpha_ax)
      alpha_axsy[d,s,midyear] <- alpha_axs[d,s]
      for(y in (midyear+1):nyears){

        alpha_axsy[d,s,y]  ~ dnorm(alpha_axsy[d,s,y-1],tau_alpha_axsy[s])
      }
      for(y in 1:(midyear-1)){
        alpha_axsy[d,s,y] ~ dnorm(alpha_axsy[d,s,y+1],tau_alpha_axsy[s])

      }#y
    }#d

  }#s

  
  
  ### for the non-data rich species (those with less than ~300 parts total)
  ### the annual values of demographic proportions are fixed at the mean 
  ### value for the species - complete pooling across all years
  
  for(s in species_sparse){
    alpha_axs[ndemog,s] <- 0 #fixed first demographic category = 0
    
    for(y in 1:nyears){
      alpha_axsy[ndemog,s,y] <- alpha_axs[ndemog,s]  #first demographic group is fixed at 0 in all years for each species
    }#y
    
    for(d in 1:(ndemog-1)){
      alpha_axs[d,s] ~ dnorm(0,0.1)#tau_alpha_ax)
      for(y in 1:nyears){
        alpha_axsy[d,s,y] <- alpha_axs[d,s]
      }
    }#d
    
  }#s
  
  
}## end of model

