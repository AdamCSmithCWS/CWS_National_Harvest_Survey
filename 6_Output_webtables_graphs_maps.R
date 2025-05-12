
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


prov_trans <- read_csv("data/Province_names_EN_FR.csv",
                       locale = locale(encoding = "latin1"),
                       name_repair = make.names)

Y <- 2023
FY = 1976
years <- FY:Y

names(years) <- paste(years)

prov_sort <- c("CAN","BC","YT","AB","SK","NT","MB","ON","PQ","NB","PE","NS","NF")




load("data/Posterior_summaries1.RData")
load("data/Posterior_summaries2.RData")
load("data/Posterior_summaries3.RData")


# General Harvest Estimates Table -----------------------------------------------------------------
mod_vars = unique(nat_sums_b$var)

## includes general harvest data from duck, goose, murre, and other

a_var = read_csv("website/A_variable_names.csv",
                 locale = locale(encoding = "latin1"),
                 name_repair = make.names)
a_var = a_var[which(a_var$keep.in.new.database == TRUE),]
a_var = a_var[,-which(names(a_var) == "keep.in.new.database")]
avars = a_var$Variable_Code


nat_sums_b$prov <- "CAN"
nat_sums_b_res$prov <- "CAN"

a_tab <- bind_rows(nat_sums_b,prov_sums_b,zone_sums_b,
                   nat_sums_b_res, prov_sums_b_res, zone_sums_b_res) %>% 
  filter(var %in% avars) %>% 
  left_join(.,a_var,by = c("var" = "Variable_Code")) %>% 
  select(-median) %>% 
  mutate(mean = as.integer(round(mean)),
         lci = as.integer(round(lci)),
         uci = as.integer(round(uci)),
         prov = factor(prov,levels = prov_sort,ordered = TRUE))%>% 
  arrange(prov,zone,var,desc(year),residence) %>% 
  filter(var != "SNIPK" | (var== "SNIPK" & year > 1991)) %>% 
  filter(var != "SUSNIP" | (var== "SUSNIP" & year > 1991)) %>% 
  left_join(.,prov_trans[,c("prov","Prov_En","Prov_Fr")],by = "prov") %>% 
  relocate(var,prov,zone,residence,year,
           mean,lci,uci,
           Description_En,
           Description_Fr,
           Prov_En,Prov_Fr)

miss_var = avars[-which(avars %in% unique(a_tab$var))]
print(paste("Data missing for variables",paste(miss_var,collapse = " ")))

#
# a_tab1 <- a_tab %>% relocate(Description_Fr,
#                   Description_En,
#                   Prov_Fr,Prov_En,zone,year,
#                   mean,lci,uci,
#                   var,prov)
# 

# a_tab_out <- a_tab1 %>% 
#   relocate(var,prov,zone,year,mean,lci,uci,Description_En,Description_Fr,Prov_En,Prov_Fr) %>% 
#   rowwise() %>% 
#   mutate(Prov_Fr = ifelse(is.na(Prov_Fr),"0",Prov_Fr),
#          Prov_En = ifelse(is.na(Prov_En),"0",Prov_En),
#          zone = ifelse(is.na(zone),0,zone)) %>% 
#   rename(Variable_Code = var,
#          Province_ID = prov,
#          Zone_ID = zone,
#          Year = year,
#          Estimate = mean,
#          Province_Name = Prov_En,
#          Province_Nom = Prov_Fr)


# 
# # same as above but by residence ------------------------------------------
# 
# 
# a_tab <- bind_rows(nat_sums_b_res,prov_sums_b_res,zone_sums_b_res) %>%
#   filter(var %in% avars) %>%
#   left_join(.,a_var,by = c("var" = "Variable_Code")) %>%
#   select(-median) %>%
#   mutate(mean = as.integer(round(mean)),
#          lci = as.integer(round(lci)),
#          uci = as.integer(round(uci)),
#          prov = factor(prov,levels = prov_sort,ordered = TRUE))%>%
#   arrange(prov,zone,var,residence,desc(year)) %>%
#   filter(var != "SNIPK" | (var== "SNIPK" & year > 1991)) %>%
#   filter(var != "SUSNIP" | (var== "SUSNIP" & year > 1991)) %>%
#   left_join(.,prov_trans[,c("prov","Prov_En","Prov_Fr")],by = "prov") %>%
#   relocate(var,prov,zone,residence,year,
#            mean,lci,uci,
#            Description_En,
#            Description_Fr,
#            Prov_En,Prov_Fr)
# 
# miss_var = avars[-which(avars %in% unique(a_tab$var))]
# print(paste("Data missing for variables",paste(miss_var,collapse = " ")))
# 
# #
a_tab1 <- a_tab %>% relocate(Description_Fr,
                             Description_En,
                             Prov_Fr,Prov_En,zone,year,residence,
                             mean,lci,uci,
                             var,prov)


a_tab_out <- a_tab %>% 
  relocate(var,prov,zone,year,mean,residence,lci,uci,Description_En,Description_Fr,Prov_En,Prov_Fr) %>%
  rowwise() %>%
  mutate(Prov_Fr = ifelse(is.na(Prov_Fr),"0",Prov_Fr),
         Prov_En = ifelse(is.na(Prov_En),"0",Prov_En),
         zone = ifelse(is.na(zone),0,zone),
         residence = ifelse(is.na(residence),"0",residence)) %>%
  rename(Variable_Code = var,
         Province_ID = prov,
         Zone_ID = zone,
         Canadian_Residents = residence,
         Year = year,
         Estimate = mean,
         Province_Name = Prov_En,
         Province_Nom = Prov_Fr)


a_tab_out1 <- a_tab_out %>% 
  filter(Canadian_Residents != "0")

a_tab_out <- a_tab_out %>% 
  filter(Canadian_Residents == "0") %>% 
  select(-Canadian_Residents)

write_csv(a_tab_out,paste0("GoogleDrive/General_Estimates_Donnees_generales_comma_",FY,"-",Y,".csv"))
write_csv2(a_tab_out,paste0("GoogleDrive/General_Estimates_Donnees_generales_point_virgule_",FY,"-",Y,".csv"))
write_csv(a_tab_out,paste0("website/General_harvest_table",FY,"-",Y,".csv"))
#write_csv(a_tab_out,paste0("website/General_harvest_table.csv"))

write_csv(a_tab_out1,paste0("GoogleDrive/General_Estimates_Residence_Donnees_generales_comma_",FY,"-",Y,".csv"))
write_csv2(a_tab_out1,paste0("GoogleDrive/General_Estimates_Residence_Donnees_generales_point_virgule_",FY,"-",Y,".csv"))




# Species Harvest Estimates Table -----------------------------------------------------------------


sp_harv_list = unique(nat_sums_a$AOU)

b_var = read_csv("website/B_species_names.csv",
                 locale = locale(encoding = "latin1"),
                 name_repair = make.names)

bvars = b_var$Species_Code
nat_sums_a$prov <- "CAN"
b_tab <- bind_rows(nat_sums_a,prov_sums_a,zone_sums_a) %>% 
  filter(AOU %in% bvars)%>% 
  left_join(.,b_var,by = c("AOU" = "Species_Code")) %>% 
  select(-median) %>% 
  mutate(mean = as.integer(round(mean)),
         lci = as.integer(round(lci)),
         uci = as.integer(round(uci)),
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
  mutate(mean = as.integer(round(mean)),
         lci = as.integer(round(lci)),
         uci = as.integer(round(uci)),
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

#

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



b_tab_out <- b_tab %>% 
  rowwise() %>% 
  mutate(Age = ifelse(is.na(Age),"All",Age),
         Sex = ifelse(is.na(Sex),"All",Sex),
         zone = ifelse(is.na(zone),0,zone)) %>% 
  rename(Species_Code = AOU,
         Province_ID = prov,
         Zone_ID = zone,
         Year = year,
         Estimate = mean,
         Species_Name_English = English_Name,
         Species_Name_French = French_Name_New,
         Province_Name = Prov_En,
         Province_Nom = Prov_Fr) %>% 
  select(Species_Code,Age,Sex,Province_ID,Zone_ID,Year,Estimate,lci,uci,
         Species_Name_English,Species_Name_French,Scientific_Name,
         Province_Name,Province_Nom)

write_csv(b_tab_out,paste0("GoogleDrive/Species_Harvest_Prises_par_Espece_comma_",FY,"-",Y,".csv"))

write_csv2(b_tab_out,paste0("GoogleDrive/Species_Harvest_Prises_par_Espece_point_virgule_",FY,"-",Y,".csv"))
write_csv(b_tab_out,paste0("website/species_harvest_table_incl_age_sex",FY,"-",Y,".csv"))

#write_csv(b_tab_out,paste0("website/species_harvest_table_incl_age_sex.csv"))




# Raw parts data ----------------------------------------------------------

load("data/allkill.RData")

# nparts_species_year <- outscse %>% 
#   group_by(PRHUNT,ZOHUNT,AOU,YEAR) %>% 
#   summarise(n_parts = n())
# 
# nparts_species_year_sex <- outscse %>% 
#   group_by(PRHUNT,ZOHUNT,AOU,YEAR,BSEX) %>% 
#   summarise(n_parts = n())
# 

nparts_species_year_age_sex <- outscse %>%
  group_by(PRHUNT,ZOHUNT,AOU,YEAR,BAGE,BSEX) %>%
  summarise(n_parts = as.integer(n()),
            .groups = "drop") %>% 
  mutate(DEMOGRAPHIC = paste(BAGE,BSEX,sep = "-")) %>% 
  filter(AOU > 100,
         !(AOU == 1550 & PRHUNT %in% c("NF", #dropping Harlequin Duck parts accidentally submitted in regions with no HARL harvest
                                     "PE",
                                     "NS",
                                     "NB") & YEAR > 1988),
         !(AOU == 1550 & PRHUNT %in% c("PQ",
                                     "ON") & YEAR > 1989)) %>% 
  select(-c(BAGE,BSEX)) %>% 
  pivot_wider(.,names_from = DEMOGRAPHIC,
              values_from = n_parts,
              values_fill = 0,
              names_sep = "_",
              names_expand = TRUE) %>% 
  arrange(PRHUNT,ZOHUNT,AOU,YEAR)

write_csv(nparts_species_year_age_sex,
          file = paste0("GoogleDrive/n_parts_by_zone_year_aou_sex_age",FY,"-",Y,".csv"))


raw_age_ratios <- outscse %>%
  group_by(PRHUNT,ZOHUNT,AOU,YEAR,BAGE) %>%
  summarise(n_parts = as.integer(n()),
            .groups = "drop") %>% 
  mutate(DEMOGRAPHIC = paste(BAGE,sep = "_")) %>% 
  filter(AOU > 100) %>% 
  select(-c(BAGE)) %>% 
  pivot_wider(.,names_from = DEMOGRAPHIC,
              values_from = n_parts,
              values_fill = 0,
              names_sep = "_",
              names_expand = TRUE) %>% 
  arrange(PRHUNT,ZOHUNT,AOU,YEAR) %>% 
  mutate(n_parts_w_age = I+A,
        raw_age_ratio = I/A,
         raw_age_ratio = ifelse(is.finite(raw_age_ratio) & n_parts_w_age > 0,signif(raw_age_ratio,2),(I+1/1)),
         raw_age_ratio = ifelse(n_parts_w_age == 0,NA,raw_age_ratio)) %>% 
  rename(prov = PRHUNT,
         zone = ZOHUNT)






# Age ratio tables (standard and female only) -----------------------------

nparts_species_year_1 <- outscse %>% 
  group_by(PRHUNT,ZOHUNT,AOU,YEAR) %>% 
  summarise(n_parts = n())
nparts_species_year_2 <- outscse %>% 
  group_by(PRHUNT,AOU,YEAR) %>% 
  summarise(n_parts = n())
nparts_species_year_3 <- outscse %>% 
  group_by(AOU,YEAR) %>% 
  summarise(n_parts = n()) %>% 
  mutate(PRHUNT = "CAN")
# nparts_species_year <- bind_rows(nparts_species_year_1,
#                                      nparts_species_year_2,
#                                      nparts_species_year_3)


nat_sums_c$prov <- "CAN"
nat_sums_c <- nat_sums_c %>% 
  left_join(.,nparts_species_year_3,by = c("year" = "YEAR",
                                           "AOU",
                                           "prov" = "PRHUNT")) %>% 
mutate(n_parts = ifelse(!is.na(n_parts),n_parts,0))
prov_sums_c <- prov_sums_c %>% 
  left_join(.,nparts_species_year_2,by = c("year" = "YEAR",
                                           "AOU",
                                           "prov" = "PRHUNT")) %>% 
  mutate(n_parts = ifelse(!is.na(n_parts),n_parts,0))
zone_sums_c <- zone_sums_c %>% 
  left_join(.,nparts_species_year_1,by = c("year" = "YEAR",
                                           "AOU",
                                           "prov" = "PRHUNT",
                                           "zone" = "ZOHUNT")) %>% 
  mutate(n_parts = ifelse(!is.na(n_parts),n_parts,0))


c_tab <- bind_rows(nat_sums_c,prov_sums_c,zone_sums_c) %>% 
  filter(AOU %in% bvars)%>% 
  left_join(.,b_var,by = c("AOU" = "Species_Code")) %>% 
  mutate(mean = round(mean,2),
         lci = round(lci,2),
         uci = round(uci,2),
         prov = factor(prov,levels = prov_sort,ordered = TRUE)) %>% 
  select(-median) %>% 
  arrange(prov,zone,AOU,desc(year)) %>% 
  left_join(.,prov_trans[,c("prov","Prov_En","Prov_Fr")],by = "prov") %>% 
  relocate(AOU,prov,zone,year,
           mean,lci,uci,
           English_Name,French_Name_New,French_Name_Old,Scientific_Name,
           Prov_En,Prov_Fr)




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



c_tab_out <- c_tab %>% 
  rowwise() %>% 
  mutate(Prov_Fr = ifelse(is.na(Prov_Fr),"0",Prov_Fr),
         Prov_En = ifelse(is.na(Prov_En),"0",Prov_En),
         zone = ifelse(is.na(zone),0,zone)) %>%  
  rename(Province_Name = Prov_En,
         Province_Nom = Prov_Fr,
         Year = year,
         Estimate = mean,
         Species_Code = AOU,
         Province_ID = prov,
         Zone_ID = zone,
         Species_Name_English = English_Name,
         Species_Name_French = French_Name_New,
         Part_Count = n_parts) %>% 
  select(Species_Code, Province_ID, Zone_ID, Year, Estimate, lci, uci,
         Species_Name_English, Species_Name_French, Scientific_Name,
         Province_Name, Province_Nom, Part_Count)
  

write_csv(c_tab_out,paste0("GoogleDrive/Ratio_dAge_Espece_Species_Age_Ratios_comma_",FY,"-",Y,".csv"))
write_csv2(c_tab_out,paste0("GoogleDrive/Ratio_dAge_Espece_Species_Age_Ratios_point_virgule_",FY,"-",Y,".csv"))
write_csv(c_tab_out,paste0("website/species_age_ratios",FY,"-",Y,".csv"))
#write_csv(c_tab_out,paste0("website/species_age_ratios.csv"))


c_tab_plot <- c_tab1 %>% 
  left_join(.,raw_age_ratios,
            by = c("prov","zone","year" = "YEAR","AOU"))




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

for(l in c("En","Fr")){
  
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
  
  
  tmpp <- general_plot_c(dat = c_tab_plot,
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
colscale2 <- brewer.pal(11,"RdBu") #removes the middle lightest colour leaves 4 red (1:5), 4 blue (6:10)


## colour palette (different than one used in 2017 version)
# colour for harvest estimates
colscale1 <- brewer.pal(9,"Blues")[-c(1,2)]
seqs <- seq(0,1,length = length(colscale1)+1)[-c(1,length(colscale1)+1)]



## loop through all variables to create maps and generate table linking maps to estimates

# General harvest estimates folder A -----------------------------------------------


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

  png(filename = paste0("website/maps/A/",ft),
      res = 300,
      height = 8,
      width = 8,
      units = "in")
  print(tmap)
  dev.off()
  
  } 



# species harvest estimates - folder B ------------------------------------


## species harvest estimates - folder B



sp_maps_b = b_tab %>% 
  distinct(AOU,year,English_Name,French_Name_New,Scientific_Name) %>% 
  mutate(AOU_leading = ifelse(nchar(AOU) == 3,paste0("0",AOU),AOU),
         map_file = paste0(AOU_leading,"_",year,".png"))



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
  
  while(any(duplicated(bbks)) & ss < 10){
    bbks <- round((bbkst/(10^(om-ss)))*10^(om-ss))
    ss = ss+1
  }
  if(any(duplicated(bbks))){
    bbks <- unique(bbks)
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
  
  png(filename = paste0("website/maps/B/",ft),
      res = 300,
      height = 8,
      width = 8,
      units = "in")
  print(tmap)
  dev.off()
  
} 









# age ratio estimates - folder C ------------------------------------------


# age ratio estimates - folder C



sp_maps_c = c_tab %>% 
  distinct(AOU,year,English_Name,French_Name_New,Scientific_Name) %>% 
  mutate(map_file = paste0("Ratio_",AOU,"_",year,".png"))

nbks <- (length(colscale2)+1)/2

seqs <- seq(0,1,length = nbks)[-c(1,nbks)]


for(i in 1:nrow(sp_maps_c)){
  vv = as.character(sp_maps_c[i,"AOU"])
  
  rng <- c_tab %>% filter(AOU == vv,
                          !is.na(zone)) %>%
    ungroup() %>% 
    select(mean)
  
  rngn <- rng %>% filter(mean <= 1)
  rngp <- rng %>% filter(mean > 1)
  
  
  bbkstn <- round(quantile(unlist(rngn),
                          probs = c(seqs),
                          na.rm = T,
                          names = F),
                  2)
  if(any(is.na(bbkstn))){
    bbkstn <- c(0.2,0.4,0.6,0.8)
  }
  
  if(any(duplicated(bbkstn))){
    bbkstnt <- seq(min(bbkstn)*0.9,max(bbkstn)*0.95,length.out = length(bbkstn))
    bbkstn <- bbkstnt
  }
  
  if(any((bbkstn == 1))){
    bbkstnt <- bbkstn[-length(bbkstn)]  
    bbkstnt[length(bbkstn)] <- max(bbkstnt) + (1-max(bbkstnt))/2
    bbkstn <- bbkstnt
  }
  
  bbkstp <- round(quantile(unlist(rngp),
                          probs = c(seqs),
                          na.rm = T,
                          names = F),
                 2)
  if(any(is.na(bbkstp))){
    bbkstp <- c(0.2,0.4,0.6,0.8)
  }
  if(any(duplicated(bbkstp))){
    bbkstpt <- seq(min(bbkstp)*0.9,max(bbkstp),length.out = length(bbkstn))
    bbkstp <- bbkstpt
  }
  
  
  # om <- min(nchar(bbkst))-1
  # 
  # bbks <- bbkstn #round(bbkst/(10^om))*10^om
  # 
  # ss <- 1
  # 
  # while(any(duplicated(bbks))){
  #   bbks <- round((bbkstn/(10^(om-ss)))*10^(om-ss))
  #   ss = ss+1
  # }
  bksp <- round(c(bbkstp,
           max(rngp,na.rm = T)*1.1),2)
  bksn <- round(c(0,
                  bbkstn,
                  1),2)
  
  bks <- c(bksn,bksp)
  
  
  yy = as.integer(sp_maps_c[i,"year"])
  ft =  as.character(sp_maps_c[i,"map_file"])
  
  tmp <- c_tab %>% filter(AOU == vv,
                          year == yy,
                          !is.na(zone)) %>% 
    mutate(plot_cat = cut(mean,breaks = bks))
  
  colscale2b <- colscale2[-length(bksn)]
  colscale2b = c(colscale2b,"white")
  names(colscale2b) <- c(levels(tmp$plot_cat),NA)
  
  labs = c(paste0("< ",bks[2]),
           paste(bks[2:(length(bks)-2)],
                 bks[3:(length(bks)-1)],
                 sep = " - "),
           paste0("> ",bks[length(bks)-1]),
           "no estimate / aucune estimation") 
  labs <- str_replace(labs,"^1 - ",">1 - ")
  names(labs) <- c(levels(tmp$plot_cat),NA)
  
  tmpm = left_join(base_map,tmp,by = c("PROV" = "prov",
                                       "ZONE" = "zone"))
  
  tmap = ggplot()+
    geom_sf(data = tmpm,aes(fill = plot_cat),colour = grey(0.65),
            linewidth = 0.5)+
    theme_no_axes()+
    theme(legend.position = "bottom",
          legend.text = element_text(size = 12))+
    scale_fill_manual(values = colscale2b,
                      labels = labs,
                      name = "",
                      drop = FALSE)
  
  png(filename = paste0("website/maps/C/",ft),
      res = 300,
      height = 8,
      width = 8,
      units = "in")
  print(tmap)
  dev.off()
  
} 






# Species proportions comparison ------------------------------------------

converge_sum <- readRDS(paste0("output/all_parameter_convergence_summary_",Y,".rds")) 
b_var = read_csv("website/B_species_names.csv",
                 locale = locale(encoding = "latin1"),
                 name_repair = make.names)

prop_estimates <- converge_sum %>% 
  filter(grepl("pcomp_psy",variable)) %>% 
  ungroup()

raw_prop <- NULL
for(pr in prov_sort[-1]){
  for(z in 1:3){
    for(spgp in c("duck","goose","murre")){
 
      if(!file.exists(paste("data/data",pr,z,spgp,"save.RData",sep = "_"))){next}
      load(paste("data/data",pr,z,spgp,"save.RData",sep = "_"))
      
raw_psy <- jdat$w_psy
raw_py <- jdat$nparts_py
aou_link <- sp.save.out
raw_propt <- NULL
for(p in 1:dim(raw_py)[1]){
  for(y in 1:dim(raw_py)[2]){
    tmpdf <- data.frame(p = p,
                        s = 1:jdat$nspecies,
                        y = y,
                        raw_prop = raw_psy[p,,y]/raw_py[p,y],
                        raw_count = raw_psy[p,,y],
                        raw_part_py =raw_py[p,y],
                        prov = pr,
                        zone = z,
                        model = spgp)
    raw_propt <- bind_rows(raw_propt,tmpdf) 
  }
}

raw_propt <- raw_propt %>% 
  left_join(.,aou_link,
            by = c("s" = "spn"))
raw_prop <- bind_rows(raw_prop,raw_propt)
print(paste(pr,z,spgp))
    }
  }
}


prop_estimates <- inner_join(prop_estimates,
                             raw_prop,
                             by = c("prov","zone","model",
                                    "d1" = "p",
                                    "d2" = "s",
                                    "d3" = "y")) %>% 
  mutate(year = d3+1975,
         period = d1) %>% 
  inner_join(.,b_var,
             by = c("AOU" = "Species_Code"))

pdf("Figures/proportions of parts by species period year zone.pdf",
    width = 11,
    height = 8.5)
for(pr in prov_sort[-1]){
  for(z in c(1:3)){
  prop_estimatest <- prop_estimates %>% 
    filter(prov == pr, zone == z)
  if(nrow(prop_estimatest) == 0){next}
  for(sp in unique(prop_estimatest$AOU)){
    tmpt <- prop_estimatest %>% 
      filter(AOU == sp)
    sp_n <- b_var %>% 
      filter(Species_Code == sp) %>% 
      select(English_Name,French_Name_New) 
    sp_n <- paste(sp_n[1,1],sp_n[1,2],pr,z,sep = " - ")
    
    prop_plot <- ggplot(data = tmpt,
                        aes(x = year,y = mean))+
      geom_errorbar(aes(ymin = q5,ymax = q95),
                    alpha = 0.3, width = 0)+
      geom_point()+
      geom_point(aes(x = year, y = raw_prop, colour = raw_part_py+1),
                 alpha = 0.8)+
      geom_smooth(aes(x = year, y = raw_prop),
                  alpha = 0.3)+
      scale_colour_viridis_c(trans = "log10",
                             guide = guide_legend(title = "Total parts by \n period and year"))+
      facet_wrap(vars(period))+
      labs(title = sp_n,
           subtitle = "Proportions of all parts by period (facets) and year (black = estimated and coloured = obs)")+
      ylab("Species estimated and observed proportion of all parts")
    
    print(prop_plot)
  }
  
  }
  
}

dev.off()


# prop_plot <- ggplot(data = prop_estimatest,
#                     aes(x = year,y = mean))+
#   geom_errorbar(aes(ymin = q5,ymax = q95),
#                 alpha = 0.3, width = 0)+
#   geom_point()+
#   geom_point(aes(x = year, y = raw_prop, colour = raw_part_py+1),
#              alpha = 0.8)+
#   scale_colour_viridis_c(trans = "log10")+
#   facet_wrap(vars(d1))
# print(prop_plot)
# 
# 
# 



# Species harvest by period and zone --------------------------------------

load("data/posterior_summaries5.RData")





load("data/allkill.RData")
sps <- sps %>% 
  select(AOU,group)


sp_harv_list = unique(nat_sums_per$AOU)

b_var = read_csv("website/B_species_names.csv",
                 locale = locale(encoding = "latin1"),
                 name_repair = make.names) %>% 
  left_join(sps, by = c("Species_Code" = "AOU"))
per_tab <- NULL
for(spgp in c("duck","goose","murre")){
  b_var_tmp <- b_var %>% 
    filter(group == spgp)
  
  periods = read.csv(paste0("data/period.",spgp,".csv"))
  
bvars = b_var_tmp$Species_Code

per_tab_tmp <- zone_sums_per %>% 
  filter(AOU %in% bvars)%>% 
  left_join(.,b_var_tmp,by = c("AOU" = "Species_Code")) %>% 
  left_join(periods,by = c("period",
                           "prov" = "pr",
                           "zone" = "zo")) %>% 
  select(-median) %>% 
  mutate(mean = as.integer(round(mean)),
         lci = as.integer(round(lci)),
         uci = as.integer(round(uci)),
         prov = factor(prov,levels = prov_sort,ordered = TRUE))%>% 
  arrange(prov,zone,AOU,period,desc(year)) %>% 
  left_join(.,prov_trans[,c("prov","Prov_En","Prov_Fr")],by = "prov") 

per_tab <- bind_rows(per_tab,per_tab_tmp)

}
# Add the species demographic harvest estimates to same file



#

per_tab1 <- per_tab %>% relocate(French_Name_New,
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



per_tab_out <- per_tab %>% 
  rowwise() %>% 
  mutate(zone = ifelse(is.na(zone),0,zone),
         Number_of_weeks_in_period = 1+(endweek-startweek)) %>% 
  rename(Species_Code = AOU,
         Province_ID = prov,
         Zone_ID = zone,
         Year = year,
         Period = period,
         First_week_of_period_rel_sept_1 = startweek,
         Last_week_of_period = endweek,
         Estimate = mean,
         Species_Name_English = English_Name,
         Species_Name_French = French_Name_New,
         Province_Name = Prov_En,
         Province_Nom = Prov_Fr) %>% 
  select(Species_Code,Province_ID,Zone_ID,Year,Period,First_week_of_period_rel_sept_1,Last_week_of_period,Number_of_weeks_in_period,
         Estimate,lci,uci,
         Species_Name_English,Species_Name_French,Scientific_Name,
         Province_Name,Province_Nom)

write_csv(per_tab_out,paste0("GoogleDrive/Species_period_Harvest_Prises_par_period_Espece_comma_",FY,"-",Y,".csv"))

write_csv2(per_tab_out,paste0("GoogleDrive/Species_period_Harvest_Prises_par_period_Espece_point_virgule_",FY,"-",Y,".csv"))
write_csv(per_tab_out,paste0("website/species_period_harvest_table",FY,"-",Y,".csv"))




# check against species totals --------------------------------------------

period_sums <- per_tab_out %>% 
  group_by(Species_Name_English,Province_Name,Zone_ID,
           Year) %>% 
  summarise(summed_period_harvest = sum(Estimate))

species_harv <- read_csv(paste0("website/species_harvest_table_incl_age_sex",FY,"-",Y,".csv")) %>% 
  filter(Zone_ID != 0,
         Sex == "All",
         Age == "All") %>% 
  select(Species_Name_English,Province_Name,Zone_ID,
         Year,Estimate,lci,uci) %>% 
  inner_join(period_sums) %>% 
  mutate(zone = paste(Province_Name,Zone_ID,sep = "-"))

pdf("Figures/compare_species_harv_w_summed_periods.pdf",
    width = 11,
    height = 8.5)
for(z in unique(species_harv$zone)){
  
  tmp <- species_harv %>% 
    filter(zone == z)
  
  plot_tmp <- ggplot(data = tmp,
                     aes(x = Year,y = Estimate))+
    geom_pointrange(aes(ymin = lci,ymax = uci),
                    colour = grey(0.7))+
    geom_point(aes(x = Year,y = summed_period_harvest),
               inherit.aes = FALSE)+
    theme_bw()+
    labs(title = z)+
    facet_wrap(vars(Species_Name_English),
               scales = "free_y")
  print(plot_tmp)
}
dev.off()





period_plotting <- per_tab_out %>% 
  mutate(zone = paste(Province_Name,Zone_ID,sep = "-"))


pdf("Figures/Species_weekly_harv_by_period.pdf",
    width = 11,
    height = 8.5)
for(z in unique(period_plotting$Species_Name_English)){
  
  tmp <- period_plotting %>% 
    filter(Species_Name_English == z) %>% 
    mutate(Estimate = Estimate/Number_of_weeks_in_period)
  
  plot_tmp <- ggplot(data = tmp,
                     aes(x = Last_week_of_period,y = Estimate,
                         colour = Year,
                         group = Year))+
    # geom_pointrange(aes(ymin = lci,ymax = uci),
    #                 position = position_dodge(width = 0.3))+
    geom_point(alpha = 0.8)+
    geom_line(alpha = 0.5)+
    theme_bw()+
    scale_y_continuous(transform = "log10")+
    scale_colour_viridis()+
    labs(title = z)+
    ylab("Weekly harvest estimate")+
    facet_wrap(vars(zone),
               scales = "free")
  print(plot_tmp)
}
dev.off()




