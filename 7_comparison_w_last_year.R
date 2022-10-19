### comparing annual species harvests
library(tidyverse)
yy = 2021
ly = yy - 1

ddir <- "temparch/"

ests <- NULL
for(i in c(ly,yy)){
dtall = read.csv(paste0(ddir,"Species_Harvest_Prises_par_Espece_comma_1976-",i,".csv")) %>% 
  mutate(year_est = as.character(i))

ests <- bind_rows(ests,dtall)

}


sp_z <- ests %>% 
  filter(!is.na(zone),
         is.na(Age),
         is.na(Sex))


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
  
  
  
  load(paste("data/data",pr,z,"duck_save.RData",sep = "_"))
  d1 <- sp.save.out[which(sp.save.out$spn == 1),"AOU"]
  if(file.exists(paste("data/data",pr,z,"goose_save.RData",sep = "_"))){
  load(paste("data/data",pr,z,"goose_save.RData",sep = "_"))
  g1 <- sp.save.out[which(sp.save.out$spn == 1),"AOU"]
  goose1 <- unique(tmp[which(tmp$AOU == g1),"species"])  
  }else{goose1 <- "none"}


  duck1 <- unique(tmp[which(tmp$AOU == d1),"species"])  
  
  compp <- ggplot(data = tmp,
                  aes(x = year_an,
                      y = mean_moyen,
                      colour = year_est))+
    geom_errorbar(aes(ymin = lci_2.5,ymax = uci_97.5),alpha = 0.2,width = 0,
                  position = position_dodge(width = 0.33))+
    geom_point(position = position_dodge(width = 0.33))+
    facet_wrap(vars(species),ncol = 5,scales = "free_y")+
    labs(title = paste(pr,z,goose1,"and", duck1))+
    theme_bw()

  print(compp)  
  
  rm(list = c("d1","g1","goose1","duck1"))
}

dev.off()




