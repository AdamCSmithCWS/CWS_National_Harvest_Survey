
library(tidyverse)
library(ggrepel)
library(ggforce)

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
  filter(var != "SUSNIP" | (var== "SUSNIP" & year > 1991))

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



write.csv(b_tab,"website/species_harvest_table.csv",row.names = FALSE)







# Age ratio tables (standard and female only) -----------------------------


