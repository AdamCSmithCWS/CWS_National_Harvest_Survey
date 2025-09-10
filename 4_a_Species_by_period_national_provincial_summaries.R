
#############################3
## script to combine the posterior draws for all species by period estimates
library(jagsUI)
library(tidyverse)
library(tidybayes)
library(ggrepel)
source("functions/utility_functions.R")

### age-sex raw data for website - 
castes <- data.frame(c = c(1,2,3,4),
                     caste = c("D","B","A","E"),
                     residence = c("Resident","Resident","Resident","Non-Resident"))

Y <- 2024
FY = 1976
years <- FY:Y

names(years) <- paste(years)


provzone <- read.csv("data/Province and zone table.csv")
provs = unique(provzone$prov)

period_duck = read.csv(paste0("data/period.duck.csv"))
period_goose = read.csv(paste0("data/period.goose.csv"))
period_murre = read.csv(paste0("data/period.murre.csv"))




# load website names --------------------------------------------------

sim_vars <- read.csv("data/website_variable_names_in.csv")
sp_vars <- read.csv("data/website_species_variable_names_in.csv")






 do_sp_period <- TRUE # harvest by species and period and year

 
  
  
  for(pr in provs){
    
    tmp_sp_period <- NULL
    
    
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
      

      if(file.exists(paste("output/full harvest zip",pr,z,"duck","alt mod.RData"))){
        load(paste("output/full harvest zip",pr,z,"duck","alt mod.RData"))
        
        ys <- data.frame(y = 1:jdat$nyears,
                         year = years) 
        

        ## species harvest by period and year
        if(do_sp_period){
          prv = pr
          period_tmp <- period_duck %>% 
            filter(pr == prv,
                   zo == z) %>% 
            select(period, startweek,endweek) %>% 
            mutate(p = period)
          
          tmp_sp_duck_period <- out2$samples %>% gather_draws(kill_pys[p,y,s]) 
          
          vnm <- sp_vars[which(sp_vars$source == "duck"),c("sp","species")]
          spss <- sp.save.out[which(sp.save.out$PRHUNT == pr & sp.save.out$ZOHUNT == z),c("AOU","spfact","spn")]
          spss <- spss[order(spss$spn),]
          tmp_sp_duck_period <- left_join(tmp_sp_duck_period,spss,by = c("s" = "spn"))
          tmp_sp_duck_period <- left_join(tmp_sp_duck_period,vnm,by = c("AOU" = "sp"))
          tmp_sp_duck_period <- left_join(tmp_sp_duck_period,ys,by = "y")
          tmp_sp_duck_period <- left_join(tmp_sp_duck_period,period_tmp,by = c("p"))
          tmp_sp_duck_period$prov <- pr
          tmp_sp_duck_period$zone <- z
          
          tmp_sp_period <- bind_rows(tmp_sp_period,tmp_sp_duck_period)
          
          
        }
        
        
      }
      
      if(file.exists(paste("output/full harvest zip",pr,z,"goose","alt mod.RData"))){
        load(paste("output/full harvest zip",pr,z,"goose","alt mod.RData"))
        
        ys <- data.frame(y = 1:jdat$nyears,
                         year = years) 
        

        ## species harvest by period and year
        if(do_sp_period){
          prv = pr
          period_tmp <- period_goose %>% 
            filter(pr == prv,
                   zo == z) %>% 
            select(period, startweek,endweek) %>% 
            mutate(p = period)
          
          tmp_sp_goose_period <- out2$samples %>% gather_draws(kill_pys[p,y,s]) 
          
          vnm <- sp_vars[which(sp_vars$source == "goose"),c("sp","species")]
          spss <- sp.save.out[which(sp.save.out$PRHUNT == pr & sp.save.out$ZOHUNT == z),c("AOU","spfact","spn")]
          spss <- spss[order(spss$spn),]
          tmp_sp_goose_period <- left_join(tmp_sp_goose_period,spss,by = c("s" = "spn"))
          tmp_sp_goose_period <- left_join(tmp_sp_goose_period,vnm,by = c("AOU" = "sp"))
          tmp_sp_goose_period <- left_join(tmp_sp_goose_period,ys,by = "y")
          tmp_sp_goose_period <- left_join(tmp_sp_goose_period,period_tmp,by = c("p"))
          tmp_sp_goose_period$prov <- pr
          tmp_sp_goose_period$zone <- z
          
          tmp_sp_period <- bind_rows(tmp_sp_period,tmp_sp_goose_period)
          
          
        }  
      }
      
      if(file.exists(paste("output/full harvest zip",pr,z,"murre","alt mod.RData"))){
        load(paste("output/full harvest zip",pr,z,"murre","alt mod.RData"))
        
        ys <- data.frame(y = 1:jdat$nyears,
                         year = years[c(((length(years)-jdat$nyears)+1):length(years))]) 
        

      ## species harvest by period and year
      if(do_sp_period){
        prv = pr
        period_tmp <- period_murre %>% 
          filter(pr == prv,
                 zo == z) %>% 
          select(period, startweek,endweek) %>% 
          mutate(p = period)
        
        tmp_sp_murre_period <- out2$samples %>% gather_draws(kill_pys[p,y,s]) 
        
        vnm <- sp_vars[which(sp_vars$source == "murre"),c("sp","species")]
        spss <- sp.save.out[which(sp.save.out$PRHUNT == pr & sp.save.out$ZOHUNT == z),c("AOU","spfact","spn")]
        spss <- spss[order(spss$spn),]
        tmp_sp_murre_period <- left_join(tmp_sp_murre_period,spss,by = c("s" = "spn"))
        tmp_sp_murre_period <- left_join(tmp_sp_murre_period,vnm,by = c("AOU" = "sp"))
        tmp_sp_murre_period <- left_join(tmp_sp_murre_period,ys,by = "y")
        tmp_sp_murre_period <- left_join(tmp_sp_murre_period,period_tmp,by = c("p"))
        tmp_sp_murre_period$prov <- pr
        tmp_sp_murre_period$zone <- z
        
        tmp_sp_period <- bind_rows(tmp_sp_period,tmp_sp_murre_period)
        
        
      }  
      
      
      

      
      }
    }#z
      save(list = c("tmp_sp_period"),
           file = paste0("output/national_provincial_summaries5",pr,".RData"))
    print(pr)
  }
  
 tmp_sp_period_all <- NULL
 
 for(pr in provs){
  
     load(paste0("output/national_provincial_summaries5",pr,".RData"))
   tmp_sp_period_all <- bind_rows(tmp_sp_period_all,tmp_sp_period)
   print(pr)
   rm("tmp_sp_period")
 }


 save(list = c("tmp_sp_period_all"), file = "national_provincial_summaries5.RData")















