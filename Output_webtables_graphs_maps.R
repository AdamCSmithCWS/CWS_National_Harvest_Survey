
library(tidyverse)
library(ggrepel)
library(ggforce)
library(sf)
library(RColorBrewer)

## load the output tidy output files


load("data/Posterior_summaries.RData")


Y <- 2019
FY = 1976
years <- FY:Y

names(years) <- paste(years)



# General Harvest Estimates Table -----------------------------------------------------------------
mod_vars = unique(nat_sums_b$var)

## includes general harvest data from duck, goose, murre, and other

a_var = read.csv("website/A_variable_names.csv")
a_var = a_var[which(a_var$keep.in.new.database == TRUE),]
a_var = a_var[,-which(names(a_var) == "keep.in.new.database")]
avars = a_var$Variable_Code
nat_sums_b$prov <- "Canada"
a_tab <- bind_rows(nat_sums_b,prov_sums_b,zone_sums_b) %>% 
  filter(var %in% avars) %>% 
  left_join(.,a_var,by = c("var" = "Variable_Code")) %>% 
  select(-median) %>% 
  mutate(mean = as.integer(ceiling(mean)),
         lci = as.integer(ceiling(lci)),
         uci = as.integer(ceiling(uci)))%>% 
  arrange(prov,zone,var,year) %>% 
  filter(var != "SNIPK" | (var== "SNIPK" & year > 1991)) %>% 
  filter(var != "SUSNIP" | (var== "SUSNIP" & year > 1991)) %>% 
  relocate(var,prov,zone,year,
           mean,lci,uci,
           Description_En,
           Description_Fr)

miss_var = avars[-which(avars %in% unique(a_tab$var))]
print(paste("Data missing for variables",paste(miss_var,collapse = " ")))

write.csv(a_tab,"website/General_harvest_table.csv",row.names = FALSE)

# Species Harvest Estimates Table -----------------------------------------------------------------
sp_harv_list = unique(nat_sums_a$AOU)

b_var = read.csv("website/B_species_names.csv")

bvars = b_var$Species_Code
nat_sums_a$prov <- "Canada"
b_tab <- bind_rows(nat_sums_a,prov_sums_a,zone_sums_a) %>% 
  filter(AOU %in% bvars)%>% 
  left_join(.,b_var,by = c("AOU" = "Species_Code")) %>% 
  select(-median) %>% 
  mutate(mean = as.integer(ceiling(mean)),
         lci = as.integer(ceiling(lci)),
         uci = as.integer(ceiling(uci))) %>% 
  arrange(prov,zone,AOU,year)

# Add the species demographic harvest estimates to same file


nat_sums_ag$prov <- "Canada"
nat_sums_sx$prov <- "Canada"
nat_sums_asxy$prov <- "Canada"

asxy_tab <- bind_rows(nat_sums_ag,prov_sums_ag,zone_sums_ag,
                   nat_sums_sx,prov_sums_sx,zone_sums_sx,
                   nat_sums_asxy,prov_sums_asxy,zone_sums_asxy) %>% 
  filter(AOU %in% bvars)%>% 
  left_join(.,b_var,by = c("AOU" = "Species_Code")) %>% 
  select(-median) %>% 
  mutate(mean = as.integer(ceiling(mean)),
         lci = as.integer(ceiling(lci)),
         uci = as.integer(ceiling(uci))) %>% 
  arrange(prov,zone,AOU,BAGE,BSEX,year)

b_tab <- bind_rows(b_tab,asxy_tab) %>% 
  relocate(AOU,BAGE,BSEX,prov,zone,year,
           mean,lci,uci,
           English_Name,French_Name_New,French_Name_Old,Scientific_Name) %>% 
  rename(Age = BAGE,
         Sex = BSEX)

write.csv(b_tab,"website/species_harvest_table_incl_age_sex.csv",row.names = FALSE)







# Age ratio tables (standard and female only) -----------------------------



nat_sums_c$prov <- "Canada"
c_tab <- bind_rows(nat_sums_c,prov_sums_c,zone_sums_c) %>% 
  filter(AOU %in% bvars)%>% 
  left_join(.,b_var,by = c("AOU" = "Species_Code")) %>% 
  select(-median) %>% 
  mutate(mean = round(mean,2),
         lci = round(lci,2),
         uci = round(uci,2)) %>% 
  arrange(prov,zone,AOU,year) %>% 
  relocate(AOU,prov,zone,year,
           mean,lci,uci,
           English_Name,French_Name_New,French_Name_Old,Scientific_Name)


write.csv(c_tab,"website/species_age_ratios.csv",row.names = FALSE)





# mapping of estimates ----------------------------------------------------

## load map
base_map = st_read(dsn = "input_map",
                   layer = "Harvest_Survey_Zones_2017")

plot(base_map)


## colour palette (different than one used in 2017 version)
# colour for harvest estimates
colscale1 <- brewer.pal(9,"Blues")
#colours for ratios (split at 1.0)
colscale2 <- brewer.pal(11,"RdBu")[-6] #removes the middle lightest colour leaves 5 red (1:5), 5 blue (6:10)



## loop through all variables to create maps and generate table linking maps to estimates

## need a unique map for each variable and year, using the zone based estimates
var_maps_a = a_tab %>% distinct(var,year,Description_En,Description_Fr) %>% 
  mutate(map_file = paste0(var,"_",year,".png"))

for(j in 1:nrow(var_maps_a)){
  vv = as.character(var_maps_a[i,"var"])
  yy = as.integer(var_maps_a[i,"year"])
  ft =  as.character(var_maps_a[i,"map_file"])
  
  tmp <- a_tab %>% filter(var == vv,
                          year == yy,
                          !is.na(zone))
  
  tmpm = left_join(base_map,tmp,by = c("PROV" = "prov",
                                       "ZONE" = "zone"))
  
  
} 








