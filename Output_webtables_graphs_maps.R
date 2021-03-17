
library(tidyverse)
library(ggrepel)
library(ggforce)
library(sf)
library(RColorBrewer)

## load the output tidy output files



load("data/Posterior_summaries.RData")
prov_trans <- read.csv("data/Province_names_EN_FR.csv")

Y <- 2019
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

a_tab <- a_tab %>% relocate(Description_Fr,
                  Description_En,
                  Prov_Fr,Prov_En,zone,year,
                  mean,lci,uci,
                  var,prov)

write.csv(a_tab,paste0("GoogleDrive/General_Estimates_",FY,"-",Y,".csv"),row.names = FALSE)


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


b_tab <- b_tab %>% relocate(French_Name_New,
                            English_Name,
                            Scientific_Name,
                            Prov_Fr,Prov_En,
                            zone,year,
                            mean,lci,uci,
                            French_Name_Old) %>% 
  rename(espece = French_Name_New,
         species = English_Name,
         Scientific = Scientific_Name,
         espece_alt = French_Name_Old)

write.csv(b_tab,paste0("GoogleDrive/Species_Harvest_Estimates_incl_age_sex_",FY,"-",Y,".csv"),row.names = FALSE)






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

c_tab <- c_tab %>% relocate(French_Name_New,
                            English_Name,
                            Scientific_Name,
                            Prov_Fr,Prov_En,
                            zone,year,
                            mean,lci,uci,
                            French_Name_Old) %>% 
  rename(espece = French_Name_New,
         species = English_Name,
         Scientific = Scientific_Name,
         espece_alt = French_Name_Old)

write.csv(c_tab,paste0("GoogleDrive/Species_Age_Ratios_",FY,"-",Y,".csv"),row.names = FALSE)




# Graphing of each set of estimates by region ---------------------------------------

source("functions/palettes.R")

## one pdf for each region with all estimates for that region

### in English and French


# Graphing of each set of estimates by variable ---------------------------------------

## one pdf for each group of variables with all regions (page with all zones and page with all prov plus national)
## pdfs
## general estimates
## species harvest
## species harvest by age group
## species harvest by sex
## species harvest by sex-age group
## age ratios


for(l in c("Fr","En")){
  
  tmpeng <- general_plot_a(dat = a_tab,
                 startYear = FY,
                 endYear = Y,
                 lang = l,
                 type = "A")
  
  pdf(paste0("GoogleDrive/Graphs/General_Estimates_",l,".pdf"),
      width = 11,
      height = 8.5)
  for(i in 1:length(tmpeng)){
    print(tmpeng[[i]])
  }
  
  dev.off()#end general harvest estimates
  
}# end language loop

### in English and French



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
  
  while(any(duplicated(bbks))){
    bbks <- round((bbkst/(10^(om-ss)))*10^(om-ss))
    ss = ss+1
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



## species harvest estimates


# General harvest estimates
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






