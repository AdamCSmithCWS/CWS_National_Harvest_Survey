### comparing annual species harvests
library(tidyverse)
yy = 2021
ly = yy - 1

ddir <- "temparch/"

ests <- NULL
for(i in c(yy)){
dtall = read.csv(paste0(ddir,"Species_Harvest_Prises_par_Espece_comma_1976-",i,".csv")) %>% 
  mutate(year_est = as.character(i))

ests <- bind_rows(ests,dtall)

}


sp_z <- ests %>% 
  filter(!is.na(zone),
         is.na(Age),
         is.na(Sex))




# load published estimates by zone prov and national ----------------------------------------

for(rr in c("by_zone","by_province","canada-wide")){
  tmp = read.csv(paste0("data/enp_nhs_a_",rr,"_20200805.csv"),stringsAsFactors = F)
  tmp1 = read.csv(paste0("data/enp_nhs_b_",rr,"_20200805.csv"),stringsAsFactors = F)
  tmp2 = read.csv(paste0("data/enp_nhs_c_",rr,"_20200805.csv"),stringsAsFactors = F)
  # names(tmp) <- c("var","name","prov","zone","resid","year","mean","sd")
  if(rr == "by_zone"){
    pubEsts_simple_all <- tmp
    pubEsts_species_all <- tmp1
    pubEsts_age_sex_all <- tmp2
  }else{
      pubEsts_simple_all <- bind_rows(pubEsts_simple_all,tmp)
      pubEsts_species_all <- bind_rows(pubEsts_species_all,tmp1)
      pubEsts_age_sex_all <- bind_rows(pubEsts_age_sex_all,tmp2)
  }

}

  names(pubEsts_simple_all) <- c("var","name","prov","zone","resid","year","mean","sd")
pubEsts_simple_all$lci = ceiling(pubEsts_simple_all$mean-(1.96*pubEsts_simple_all$sd))
pubEsts_simple_all$uci = ceiling(pubEsts_simple_all$mean+(1.96*pubEsts_simple_all$sd))
pubEsts_simple_all[which(pubEsts_simple_all$lci < 0),"lci"] <- 0
pubEsts_simple_all[which(is.na(pubEsts_simple_all$prov)),"prov"] <- "Canada"

names(pubEsts_species_all) <- c("sp","species","prov","zone","year","mean","sd")
pubEsts_species_all$lci = ceiling(pubEsts_species_all$mean-(1.96*pubEsts_species_all$sd))
pubEsts_species_all$uci = ceiling(pubEsts_species_all$mean+(1.96*pubEsts_species_all$sd))
pubEsts_species_all[which(pubEsts_species_all$lci < 0),"lci"] <- 0
pubEsts_species_all[which(is.na(pubEsts_species_all$prov)),"prov"] <- "Canada"


names(pubEsts_age_sex_all) <- c("sp","species","prov","zone","year","age_ratio")
pubEsts_age_sex_all[which(is.na(pubEsts_age_sex_all$prov)),"prov"] <- "Canada"

pubEsts_age_sex_all <- pubEsts_age_sex_all[which(pubEsts_age_sex_all$year > 1975),]
pubEsts_species_all <- pubEsts_species_all[which(pubEsts_species_all$year > 1975),]
pubEsts_simple_all <- pubEsts_simple_all[which(pubEsts_simple_all$year > 1975),]


species_web_names = unique(pubEsts_species_all[,c("sp","species")])



original_estimates <- pubEsts_species_all %>% 
  rename(AOU = sp,
         year_an = year,
         region_En = prov,
         mean_moyen = mean,
         lci_2.5 = lci,
         uci_97.5 = uci) %>% 
  select(zone,AOU,year_an,region_En,
         species,
         mean_moyen,lci_2.5,uci_97.5) %>% 
  mutate(year_est = "2018")

# zone graphs -------------------------------------------------------------

pdf("figures/annual_comparison_species_zone.pdf",
    width = 11,
    height = 8.5)
provs <- unique(sp_z[,c("prov","zone")])

for(i in 1:nrow(provs)){
  pr = provs[i,"prov"]
  z = provs[i,"zone"]
  
  tmp <- sp_z %>% 
    filter(prov == pr,
           zone == z)
  tmp2 <- original_estimates %>% 
    filter(region_En == unique(tmp$region_En),
           zone == z)
  tmp <- bind_rows(tmp,tmp2)
  
  
  # load(paste("data/data",pr,z,"duck_save.RData",sep = "_"))
  # d1 <- sp.save.out[which(sp.save.out$spn == 1),"AOU"]
  # if(file.exists(paste("data/data",pr,z,"goose_save.RData",sep = "_"))){
  # load(paste("data/data",pr,z,"goose_save.RData",sep = "_"))
  # g1 <- sp.save.out[which(sp.save.out$spn == 1),"AOU"]
  # goose1 <- unique(tmp[which(tmp$AOU == g1),"species"])  
  # }else{goose1 <- "none"}
  # 
  # 
  # duck1 <- unique(tmp[which(tmp$AOU == d1),"species"])  
  # 
  compp <- ggplot(data = tmp,
                  aes(x = year_an,
                      y = mean_moyen,
                      colour = year_est))+
    geom_errorbar(aes(ymin = lci_2.5,ymax = uci_97.5),alpha = 0.5,width = 0,
                  linewidth = 1,
                  position = position_dodge(width = 0.75))+
    geom_point(position = position_dodge(width = 0.75),alpha = 0.7,size = 1)+
    facet_wrap(vars(species),ncol = 5,scales = "free_y")+
    labs(title = paste(pr,z))+
    xlab("")+
    ylab("")+
    theme_bw()+ 
    theme( 
      # axis.text = element_text( size = 14 ),
      #      axis.text.x = element_text( size = 20 ),
      #      axis.title = element_text( size = 16, face = "bold" ),
      #      legend.position="none",
           # The new stuff
           strip.text = element_text(size = 8))+
    scale_colour_viridis_d(begin = 0.2,end = 0.8,direction = -1)

  print(compp) 
  

  
}

dev.off()

pdf("figures/Quebec_Greater_Snow_Goose.pdf",
    width = 11,
    height = 8.5)
tmp <- sp_z %>% 
  filter(prov == "PQ",
         !is.na(zone),
         grepl("Greater Snow",species))
tmp2 <- original_estimates %>% 
  filter(region_En == unique(tmp$region_En),
         !is.na(zone),
         grepl("Greater Snow",species))
tmp <- bind_rows(tmp,tmp2)

compp <- ggplot(data = tmp,
                aes(x = year_an,
                    y = mean_moyen,
                    colour = year_est))+
  geom_errorbar(aes(ymin = lci_2.5,ymax = uci_97.5),alpha = 0.5,width = 0,
                linewidth = 1,
                position = position_dodge(width = 0.75))+
  geom_point(position = position_dodge(width = 0.75),alpha = 0.7,size = 1)+
  facet_wrap(vars(region_En,zone,species),ncol = 5,scales = "free_y")+
  #labs(title = paste(pr,z))+
  xlab("")+
  ylab("")+
  theme_bw()+ 
  theme( 
    # axis.text = element_text( size = 14 ),
    #      axis.text.x = element_text( size = 20 ),
    #      axis.title = element_text( size = 16, face = "bold" ),
    #      legend.position="none",
    # The new stuff
    strip.text = element_text(size = 8))+
  scale_colour_viridis_d(begin = 0.2,end = 0.8,direction = -1)


print(compp) 

dev.off()



pdf("figures/All_Snow_Goose.pdf",
    width = 17,
    height = 14)
tmp <- sp_z %>% 
  filter(!is.na(zone),
         grepl("Greater Snow",species))
tmp2 <- original_estimates %>% 
  filter(!is.na(zone),
         grepl("Greater Snow",species))
tmp <- bind_rows(tmp,tmp2)

compp <- ggplot(data = tmp,
                aes(x = year_an,
                    y = mean_moyen,
                    colour = year_est))+
  geom_errorbar(aes(ymin = lci_2.5,ymax = uci_97.5),alpha = 0.5,width = 0,
                linewidth = 1,
                position = position_dodge(width = 0.75))+
  geom_point(position = position_dodge(width = 0.75),alpha = 0.7,size = 1)+
  facet_wrap(vars(region_En,species),ncol = 5,scales = "free_y")+
  labs(title = "Greater Snow Goose")+
  xlab("")+
  ylab("")+
  theme_bw()+ 
  theme( 
    # axis.text = element_text( size = 14 ),
    #      axis.text.x = element_text( size = 20 ),
    #      axis.title = element_text( size = 16, face = "bold" ),
    #      legend.position="none",
    # The new stuff
    strip.text = element_text(size = 8))+
  scale_colour_viridis_d(begin = 0.2,end = 0.8,direction = -1)


print(compp) 

tmp <- sp_z %>% 
  filter(!is.na(zone),
         grepl("Lesser Snow",species))
tmp2 <- original_estimates %>% 
  filter(!is.na(zone),
         grepl("Lesser Snow",species))
tmp <- bind_rows(tmp,tmp2)

compp <- ggplot(data = tmp,
                aes(x = year_an,
                    y = mean_moyen,
                    colour = species))+
  geom_errorbar(aes(ymin = lci_2.5,ymax = uci_97.5),alpha = 0.5,width = 0,
                linewidth = 1,
                position = position_dodge(width = 0.75))+
  geom_point(position = position_dodge(width = 0.75),alpha = 0.7,size = 1)+
  facet_wrap(vars(region_En,zone),ncol = 5,scales = "free_y")+
  labs(title = "Lesser Snow Goose")+
  xlab("")+
  ylab("")+
  theme_bw()+ 
  theme( 
    # axis.text = element_text( size = 14 ),
    #      axis.text.x = element_text( size = 20 ),
    #      axis.title = element_text( size = 16, face = "bold" ),
    #      legend.position="none",
    # The new stuff
    strip.text = element_text(size = 8))+
  scale_colour_viridis_d(begin = 0.2,end = 0.8,direction = -1)


print(compp) 


dev.off()






pdf("figures/annual_comparison_species_zone_10yr.pdf",
    width = 11,
    height = 8.5)
provs <- unique(sp_z[,c("prov","zone")])

for(i in 1:nrow(provs)){
  pr = provs[i,"prov"]
  z = provs[i,"zone"]
  
  tmp <- sp_z %>% 
    filter(prov == pr,
           zone == z, 
           year_an > yy-11)
  
  tmp2 <- original_estimates %>% 
    filter(region_En == unique(tmp$region_En),
           zone == z, 
           year_an > yy-11)
  tmp <- bind_rows(tmp,tmp2)
  

  
  compp <- ggplot(data = tmp,
                  aes(x = year_an,
                      y = mean_moyen,
                      colour = year_est))+
    geom_errorbar(aes(ymin = lci_2.5,ymax = uci_97.5),alpha = 0.2,width = 0,
                  position = position_dodge(width = 0.33))+
    geom_point(position = position_dodge(width = 0.33))+
    facet_wrap(vars(species),ncol = 5,scales = "free_y")+
    labs(title = paste(pr,z))+
    xlab("")+
    ylab("")+
    theme_bw()+ 
    theme( 
      # axis.text = element_text( size = 14 ),
      #      axis.text.x = element_text( size = 20 ),
      #      axis.title = element_text( size = 16, face = "bold" ),
      #      legend.position="none",
      # The new stuff
      strip.text = element_text(size = 8))+
    scale_colour_viridis_d(begin = 0.2,end = 0.8,direction = -1)
  
  print(compp) 
  
}

dev.off()
















# general estimates -------------------------------------------------------

var_names_sim <- unique(pubEsts_simple_all[,c("var","name")])

ests <- NULL
for(i in c(yy)){
  dtall = read.csv(paste0(ddir,"General_Estimates_Donnees_generales_comma_1976-",i,".csv")) %>% 
    mutate(year_est = as.character(i))
  
  ests <- bind_rows(ests,dtall)
  
}


sp_z <- ests %>% 
  filter(!is.na(zone))





original_estimates <- pubEsts_simple_all %>% 
  rename(Description_En = name,
         year_an = year,
         region_En = prov,
         mean_moyen = mean,
         lci_2.5 = lci,
         uci_97.5 = uci) %>% 
  select(zone,Description_En,year_an,region_En,
         var,
         mean_moyen,lci_2.5,uci_97.5) %>% 
  mutate(year_est = "2018")

# zone graphs -------------------------------------------------------------

pdf("figures/annual_comparison_General_zone.pdf",
    width = 11,
    height = 8.5)
provs <- unique(sp_z[,c("prov","zone")])

for(i in 1:nrow(provs)){
  pr = provs[i,"prov"]
  z = provs[i,"zone"]
  
  tmp <- sp_z %>% 
    filter(prov == pr,
           zone == z)
  tmp2 <- original_estimates %>% 
    filter(region_En == unique(tmp$region_En),
           zone == z)
  tmp <- bind_rows(tmp,tmp2)
  
  

  compp <- ggplot(data = tmp,
                  aes(x = year_an,
                      y = mean_moyen,
                      colour = year_est))+
    geom_errorbar(aes(ymin = lci_2.5,ymax = uci_97.5),alpha = 0.2,width = 0,
                  position = position_dodge(width = 0.75))+
    geom_point(position = position_dodge(width = 0.75),alpha = 0.7)+
    facet_wrap(vars(Description_En),ncol = 5,scales = "free_y")+
    labs(title = paste(pr,z))+
    xlab("")+
    ylab("")+
    theme_bw()+ 
    theme( 
      # axis.text = element_text( size = 14 ),
      #      axis.text.x = element_text( size = 20 ),
      #      axis.title = element_text( size = 16, face = "bold" ),
      #      legend.position="none",
      # The new stuff
      strip.text = element_text(size = 8))+
    scale_colour_viridis_d(begin = 0.2,end = 0.8,direction = -1)
  
  print(compp) 
  
  
  
}

dev.off()


pdf("figures/annual_comparison_General_zone_10yr.pdf",
    width = 11,
    height = 8.5)
provs <- unique(sp_z[,c("prov","zone")])

for(i in 1:nrow(provs)){
  pr = provs[i,"prov"]
  z = provs[i,"zone"]
  
  tmp <- sp_z %>% 
    filter(prov == pr,
           zone == z, 
           year_an > yy-11)
  
  tmp2 <- original_estimates %>% 
    filter(region_En == unique(tmp$region_En),
           zone == z, 
           year_an > yy-11)
  tmp <- bind_rows(tmp,tmp2)
  
  
  
  compp <- ggplot(data = tmp,
                  aes(x = year_an,
                      y = mean_moyen,
                      colour = year_est))+
    geom_errorbar(aes(ymin = lci_2.5,ymax = uci_97.5),alpha = 0.2,width = 0,
                  position = position_dodge(width = 0.33))+
    geom_point(position = position_dodge(width = 0.33))+
    facet_wrap(vars(Description_En),ncol = 5,scales = "free_y")+
    labs(title = paste(pr,z))+
    xlab("")+
    ylab("")+
    theme_bw()+ 
    theme( 
      # axis.text = element_text( size = 14 ),
      #      axis.text.x = element_text( size = 20 ),
      #      axis.title = element_text( size = 16, face = "bold" ),
      #      legend.position="none",
      # The new stuff
      strip.text = element_text(size = 8))+
    scale_colour_viridis_d(begin = 0.2,end = 0.8,direction = -1)
  
  print(compp) 
  
}

dev.off()



