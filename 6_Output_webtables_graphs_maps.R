
library(tidyverse)
library(ggrepel)
library(ggforce)
library(sf)
library(RColorBrewer)

## load the output tidy output files
source("functions/palettes.R")
source("functions/general_plot_function.R")

#directory structure to hold output
dir.create("GoogleDrive")
dir.create("GoogleDrive/Graphs")
dir.create("website")
dir.create("website/maps")


load("data/Posterior_summaries1.RData")
load("data/Posterior_summaries2.RData")
load("data/Posterior_summaries3.RData")

prov_trans <- read.csv("data/Province_names_EN_FR.csv")

Y <- 2021
FY = 1976
years <- FY:Y

names(years) <- paste(years)

prov_sort <- c("CAN","BC","YT","AB","SK","NT","MB","ON","PQ","NB","PE","NS","NF")

# General Harvest Estimates Table -----------------------------------------------------------------
mod_vars = unique(nat_sums_b$var)

## includes general harvest data from duck, goose, murre, and other

a_var = read.csv("website/A_variable_names.csv")
a_var = a_var[which(a_var$keep.in.new.database == TRUE),]
a_var = a_var[,-which(names(a_var) == "keep.in.new.database")]
avars = a_var$Variable_Code
nat_sums_b$prov <- "CAN"
a_tab <- bind_rows(nat_sums_b,prov_sums_b,zone_sums_b) %>% 
  filter(var %in% avars) %>% 
  left_join(.,a_var,by = c("var" = "Variable_Code")) %>% 
  select(-median) %>% 
  mutate(mean = as.integer(ceiling(mean)),
         lci = as.integer(ceiling(lci)),
         uci = as.integer(ceiling(uci)),
         prov = factor(prov,levels = prov_sort,ordered = TRUE))%>% 
  arrange(prov,zone,var,desc(year)) %>% 
  filter(var != "SNIPK" | (var== "SNIPK" & year > 1991)) %>% 
  filter(var != "SUSNIP" | (var== "SUSNIP" & year > 1991)) %>% 
  left_join(.,prov_trans[,c("prov","Prov_En","Prov_Fr")],by = "prov") %>% 
  relocate(var,prov,zone,year,
           mean,lci,uci,
           Description_En,
           Description_Fr,
           Prov_En,Prov_Fr)

miss_var = avars[-which(avars %in% unique(a_tab$var))]
print(paste("Data missing for variables",paste(miss_var,collapse = " ")))

write.csv(a_tab,"website/General_harvest_table.csv",row.names = FALSE)

a_tab1 <- a_tab %>% relocate(Description_Fr,
                  Description_En,
                  Prov_Fr,Prov_En,zone,year,
                  mean,lci,uci,
                  var,prov)


a_tab_out <- a_tab1 %>%  
  rename(region_En = Prov_En,
         region_Fr = Prov_Fr,
         year_an = year,
         mean_moyen = mean,
         lci_2.5 = lci,
         uci_97.5 = uci)


write.csv(a_tab_out,paste0("GoogleDrive/General_Estimates_Donnees_generales_comma_",FY,"-",Y,".csv"),row.names = FALSE)
write.csv2(a_tab_out,paste0("GoogleDrive/General_Estimates_Donnees_generales_point_virgule_",FY,"-",Y,".csv"),row.names = FALSE)





# Species Harvest Estimates Table -----------------------------------------------------------------
sp_harv_list = unique(nat_sums_a$AOU)

b_var = read.csv("website/B_species_names.csv")

bvars = b_var$Species_Code
nat_sums_a$prov <- "CAN"
b_tab <- bind_rows(nat_sums_a,prov_sums_a,zone_sums_a) %>% 
  filter(AOU %in% bvars)%>% 
  left_join(.,b_var,by = c("AOU" = "Species_Code")) %>% 
  select(-median) %>% 
  mutate(mean = as.integer(ceiling(mean)),
         lci = as.integer(ceiling(lci)),
         uci = as.integer(ceiling(uci)),
         prov = factor(prov,levels = prov_sort,ordered = TRUE))%>% 
  arrange(prov,zone,AOU,desc(year)) %>% 
  left_join(.,prov_trans[,c("prov","Prov_En","Prov_Fr")],by = "prov") 

# Add the species demographic harvest estimates to same file


nat_sums_ag$prov <- "CAN"
nat_sums_sx$prov <- "CAN"
nat_sums_asxy$prov <- "CAN"

asxy_tab <- bind_rows(nat_sums_ag,prov_sums_ag,zone_sums_ag,
                   nat_sums_sx,prov_sums_sx,zone_sums_sx,
                   nat_sums_asxy,prov_sums_asxy,zone_sums_asxy) %>% 
  filter(AOU %in% bvars)%>% 
  left_join(.,b_var,by = c("AOU" = "Species_Code")) %>% 
  select(-median) %>% 
  mutate(mean = as.integer(ceiling(mean)),
         lci = as.integer(ceiling(lci)),
         uci = as.integer(ceiling(uci)),
         prov = factor(prov,levels = prov_sort,ordered = TRUE)) %>% 
  arrange(prov,zone,AOU,BAGE,BSEX,desc(year)) %>% 
  left_join(.,prov_trans[,c("prov","Prov_En","Prov_Fr")],by = "prov")

b_tab <- bind_rows(b_tab,asxy_tab) %>% 
  relocate(AOU,BAGE,BSEX,prov,zone,year,
           mean,lci,uci,
           English_Name,French_Name_New,French_Name_Old,Scientific_Name,
           Prov_En,Prov_Fr) %>% 
  rename(Age = BAGE,
         Sex = BSEX)

write.csv(b_tab,"website/species_harvest_table_incl_age_sex.csv",row.names = FALSE)


b_tab1 <- b_tab %>% relocate(French_Name_New,
                            English_Name,
                            Scientific_Name,
                            Prov_Fr,Prov_En,
                            zone,year,
                            mean,lci,uci,
                            French_Name_Old) %>% 
  rename(espece = French_Name_New,
         species = English_Name,
         scientific = Scientific_Name,
         espece_alt = French_Name_Old)



b_tab_out <- b_tab1 %>%  
  rename(region_En = Prov_En,
         region_Fr = Prov_Fr,
         year_an = year,
         mean_moyen = mean,
         lci_2.5 = lci,
         uci_97.5 = uci)

write.csv(b_tab_out,paste0("GoogleDrive/Species_Harvest_Prises_par_Espece_comma_",FY,"-",Y,".csv"),row.names = FALSE)

write.csv2(b_tab_out,paste0("GoogleDrive/Species_Harvest_Prises_par_Espece_point_virgule_",FY,"-",Y,".csv"),row.names = FALSE)





# Age ratio tables (standard and female only) -----------------------------



nat_sums_c$prov <- "CAN"
c_tab <- bind_rows(nat_sums_c,prov_sums_c,zone_sums_c) %>% 
  filter(AOU %in% bvars)%>% 
  left_join(.,b_var,by = c("AOU" = "Species_Code")) %>% 
  select(-median) %>% 
  mutate(mean = round(mean,2),
         lci = round(lci,2),
         uci = round(uci,2),
         prov = factor(prov,levels = prov_sort,ordered = TRUE)) %>% 
  arrange(prov,zone,AOU,desc(year)) %>% 
  left_join(.,prov_trans[,c("prov","Prov_En","Prov_Fr")],by = "prov") %>% 
  relocate(AOU,prov,zone,year,
           mean,lci,uci,
           English_Name,French_Name_New,French_Name_Old,Scientific_Name,
           Prov_En,Prov_Fr)


write.csv(c_tab,"website/species_age_ratios.csv",row.names = FALSE)


c_tab1 <- c_tab %>% relocate(French_Name_New,
                                English_Name,
                                Scientific_Name,
                                Prov_Fr,Prov_En,
                                zone,year,
                                mean,lci,uci,
                                French_Name_Old) %>% 
  rename(espece = French_Name_New,
         species = English_Name,
         scientific = Scientific_Name,
         espece_alt = French_Name_Old)



c_tab_out <- c_tab1 %>%  
  rename(region_En = Prov_En,
         region_Fr = Prov_Fr,
         year_an = year,
         mean_moyen = mean,
         lci_2.5 = lci,
         uci_97.5 = uci)

write.csv(c_tab_out,paste0("GoogleDrive/Ratio_dAge_Espece_Species_Age_Ratios_comma_",FY,"-",Y,".csv"),row.names = FALSE)
write.csv2(c_tab_out,paste0("GoogleDrive/Ratio_dAge_Espece_Species_Age_Ratios_point_virgule_",FY,"-",Y,".csv"),row.names = FALSE)







# Graphing of each set of estimates by variable ---------------------------------------

## one pdf for each group of variables with all regions (page with all zones and page with all prov plus national)
## pdfs
## general estimates
## species harvest
## species harvest by age group
## species harvest by sex
## species harvest by sex-age group
## age ratios


### in English and French

### in English and French

# Full time-series and last 10-year graphs -----------------------------------------------------

for(sy in c(FY,Y-9)){

for(l in c("Fr","En")){
  
  tmpp <- general_plot_a(dat = a_tab1,
                         startYear = sy,
                         endYear = Y,
                         lang = l)

  if(l == "Fr"){
    pdf(paste0("GoogleDrive/Graphs/Donnees_generales_",sy,"-",Y,"_",l,".pdf"),
        width = 11,
        height = 8.5)
  }else{
  pdf(paste0("GoogleDrive/Graphs/General_Estimates_",sy,"-",Y,"_",l,".pdf"),
      width = 11,
      height = 8.5)
  }
  for(i in 1:length(tmpp)){
    print(tmpp[[i]])
  }

  dev.off()#end general harvest estimates


  for(tty in c("Full",
               "Age",
               "Sex",
               "Age_Sex")){

    tmpp <- general_plot_b(dat = b_tab1,
                           startYear = sy,
                           endYear = Y,
                           lang = l,
                           type = tty)

    if(l == "Fr"){
    if(tty == "Full"){
      pdf(paste0("GoogleDrive/Graphs/Prises_par_Espece_",sy,"-",Y,"_",l,".pdf"),
          width = 11,
          height = 8.5)
    }else{
      pdf(paste0("GoogleDrive/Graphs/Prises_par_Espece_par_",tty,"_",sy,"-",Y,"_",l,".pdf"),
          width = 11,
          height = 8.5)
    }
    }else{
      if(tty == "Full"){
        pdf(paste0("GoogleDrive/Graphs/Species_Harvest_",sy,"-",Y,"_",l,".pdf"),
            width = 11,
            height = 8.5)
      }else{
        pdf(paste0("GoogleDrive/Graphs/Species_Harvest_by_",tty,"_",sy,"-",Y,"_",l,".pdf"),
            width = 11,
            height = 8.5)
      } 
    }
    for(i in 1:length(tmpp)){
      print(tmpp[[i]])
    }

    dev.off()#end species harvest estimates
  }#end tty loop
  
  
  tmpp <- general_plot_c(dat = c_tab1,
                         startYear = sy,
                         endYear = Y,
                         lang = l)
 
  if(l == "Fr"){ 
  pdf(paste0("GoogleDrive/Graphs/Ratio_dAge_",sy,"-",Y,"_",l,".pdf"),
      width = 11,
      height = 8.5)
  }else{
    pdf(paste0("GoogleDrive/Graphs/Age_Ratios_",sy,"-",Y,"_",l,".pdf"),
        width = 11,
        height = 8.5) 
  }
  for(i in 1:length(tmpp)){
    print(tmpp[[i]])
  }
  
  dev.off()#end general harvest estimates
  
  
}# end language loop

}# end long-term and 10-year plot loops















# mapping of estimates ----------------------------------------------------

## load map
base_map = st_read(dsn = "input_map",
                   layer = "Harvest_Survey_Zones_2017")

plot(base_map)

#colours for ratios (split at 1.0)
colscale2 <- brewer.pal(11,"RdBu")[-c(5,7)] #removes the middle lightest colour leaves 4 red (1:5), 4 blue (6:10)


## colour palette (different than one used in 2017 version)
# colour for harvest estimates
colscale1 <- brewer.pal(9,"Blues")[-c(1,2)]
seqs <- seq(0,1,length = length(colscale1)+1)[-c(1,length(colscale1)+1)]



## loop through all variables to create maps and generate table linking maps to estimates

# General harvest estimates
var_maps_a = a_tab %>% distinct(var,year,Description_En,Description_Fr) %>% 
  mutate(map_file = paste0(var,"_",year,".png"))



for(i in 1:nrow(var_maps_a)){
  vv = as.character(var_maps_a[i,"var"])
  
  rng <- a_tab %>% filter(var == vv,
                         !is.na(zone)) %>%
    ungroup() %>% 
    select(mean)
  
  bbkst <- round(quantile(unlist(rng),
                          probs = c(seqs),
                          na.rm = T,
                          names = F))
  om <- min(nchar(bbkst))-1
  
  bbks <- round(bbkst/(10^om))*10^om
  ss <- 1
  
  while(any(duplicated(bbks)) & ss < 6){
    bbks <- round((bbkst/(10^(om-ss)))*10^(om-ss))
    ss = ss+1
  }
  if(any(duplicated(bbks)) | bbks[1] == 0){
    rng <- unlist(rng)
    bbkst <- round(quantile(rng[which(rng > 0)],
                            probs = c(seqs),
                            na.rm = T,
                            names = F))
    om <- min(nchar(bbkst))-1
    
    bbks <- round(bbkst/(10^om))*10^om
    bbks[1] <- 1
    
  }
  bks <- c(0,
           bbks,
           round(max(rng,na.rm = T)+1))
  

  
  yy = as.integer(var_maps_a[i,"year"])
  ft =  as.character(var_maps_a[i,"map_file"])
  
  tmp <- a_tab %>% filter(var == vv,
                          year == yy,
                          !is.na(zone)) %>% 
    mutate(plot_cat = cut(mean,breaks = bks))
  
  colscale1b = c(colscale1,"white")
  names(colscale1b) <- c(levels(tmp$plot_cat),NA)
  
  if(bks[2] == 1){
    labs = c(paste0("0"),
             paste(bks[2:(length(bks)-2)],
                   bks[3:(length(bks)-1)],
                   sep = " - "),
             paste0("> ",bks[length(bks)-1]),
             "no estimate / aucune estimation") 
  }else{
  labs = c(paste0("1 - ",bks[2]),
           paste(bks[2:(length(bks)-2)],
                 bks[3:(length(bks)-1)],
                 sep = " - "),
           paste0("> ",bks[length(bks)-1]),
           "no estimate / aucune estimation") 
  names(labs) <- c(levels(tmp$plot_cat),NA)
  }
  
  tmpm = left_join(base_map,tmp,by = c("PROV" = "prov",
                                       "ZONE" = "zone"))
  
  tmap = ggplot()+
    geom_sf(data = tmpm,aes(fill = plot_cat),colour = grey(0.25))+
    theme_no_axes()+
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12))+
    scale_fill_manual(values = colscale1b,
                      labels = labs,
                      name = "",
                      drop = FALSE)

  png(filename = paste0("website/maps/",ft),
      res = 300,
      height = 8,
      width = 8,
      units = "in")
  print(tmap)
  dev.off()
  
  } 



## species harvest estimates



sp_maps_b = b_tab %>% distinct(AOU,year,English_Name,French_Name_New,Scientific_Name) %>% 
  mutate(map_file = paste0(AOU,"_",year,".png"))



for(i in 1:nrow(sp_maps_b)){
  vv = as.character(sp_maps_b[i,"AOU"])
  
  rng <- b_tab %>% filter(AOU == vv,
                          !is.na(zone),
                          is.na(Age),
                          is.na(Sex)) %>%
    ungroup() %>% 
    select(mean)
  
  bbkst <- round(quantile(unlist(rng),
                          probs = c(seqs),
                          na.rm = T,
                          names = F))
  om <- min(nchar(bbkst))-1
  
  bbks <- round(bbkst/(10^om))*10^om
  ss <- 1
  
  while(any(duplicated(bbks))){
    bbks <- round((bbkst/(10^(om-ss)))*10^(om-ss))
    ss = ss+1
  }
  bks <- c(0,
           bbks,
           round(max(rng,na.rm = T)+1))
  
  
  
  yy = as.integer(sp_maps_b[i,"year"])
  ft =  as.character(sp_maps_b[i,"map_file"])
  
  tmp <- b_tab %>% filter(AOU == vv,
                          year == yy,
                          !is.na(zone),
                          is.na(Age),
                          is.na(Sex)) %>% 
    mutate(plot_cat = cut(mean,breaks = bks))
  
  colscale1b = c(colscale1,"white")
  names(colscale1b) <- c(levels(tmp$plot_cat),NA)
  
  labs = c(paste0("1 - ",bks[2]),
           paste(bks[2:(length(bks)-2)],
                 bks[3:(length(bks)-1)],
                 sep = " - "),
           paste0("> ",bks[length(bks)-1]),
           "no estimate / aucune estimation") 
  names(labs) <- c(levels(tmp$plot_cat),NA)
  
  tmpm = left_join(base_map,tmp,by = c("PROV" = "prov",
                                       "ZONE" = "zone"))
  
  tmap = ggplot()+
    geom_sf(data = tmpm,aes(fill = plot_cat),colour = grey(0.25))+
    theme_no_axes()+
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12))+
    scale_fill_manual(values = colscale1b,
                      labels = labs,
                      name = "",
                      drop = FALSE)
  
  png(filename = paste0("website/maps/",ft),
      res = 300,
      height = 8,
      width = 8,
      units = "in")
  print(tmap)
  dev.off()
  
} 






