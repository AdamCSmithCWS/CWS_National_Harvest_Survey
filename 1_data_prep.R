
####### the following commented out lines ~ 400 lines
## represent the add-hoc processes required to load all the 
## historical harvest survey data
## the line :load("full data prep updated harvest model.RData")
## on about line-400 will load everything created below



### 

# species changes for final harvest survey estimates ----------------------

### recode all 1720 as 1722 (large race Canada Geese)
### recode all 1699 and 1690 as 1692 (white-phase snow goose)
### recode all 1691 as 1693 (blue-phase snow goose)
### Drop Black Brant - 1740
### drop Eurasian Green-winged Teal - 1380
### consider splitting eastern and western Harlequin ducks
# 1550 = western, 1551 = eastern?
### consider splitting eastern and western Barrow's Goldeneye
# 1520 = western, 1521 = eastern?



Y <- 2023
years <- 1976:Y

names(years) <- paste(years)
home.fold1 <- "C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/"
#home.fold1 <- "M:/My Documents/Harvest Survey A146/"
# C:\Users\smithac\OneDrive - EC-EC\Harvest Survey A146
home.fold <- getwd()
# setwd(home.fold)

#library(foreign)
#library(runjags)
library(rjags)
library(tidyverse)
library(haven)


#sashome <- "C:\\Program Files\\SASHome\\SASFoundation\\9.4"
provs = c("AB","BC","SK","MB","ON","PQ","NS","PE","NB","NF","NT","YT")#,"NU") #All prov
#ignoring territories above

sps <- read.csv(paste(home.fold,"/data/Bird_names_2023.csv", sep = ""))
aou_rec <- read.csv("data/Reconcile_AOU_codes_2023.csv")


species <- unique(sps[which(sps$group %in% c("duck","goose","murre")),"specieslevelenglish"])


species <- species[-which(species == "Hybrid Mallard/Northern Pintail")]
gnames <- unique(sps[which(sps$group == "goose"),"specieslevelenglish"])
dnames <- unique(sps[which(sps$group == "duck"),"specieslevelenglish"])
 dnames <- dnames[-which(dnames == "Hybrid Mallard/Northern Pintail")]
#
#
#
goose <- TRUE         ### change to false, if no goose species included (simplifies the PEF file extraction)
murre <- T      #change to fales if no murre species should be included
zone <- T           ### change to false, if provincial summaries are desired
# #
# #
# ############## extracting harvest survey data for all years
#
provzone = read.csv("data/province and zone table.csv",stringsAsFactors = F)
casteslist = read.csv("data/caste table.csv",stringsAsFactors = F)

# 
# 
# #
# #
harvw <- list()
length(harvw) <- length(years)
names(harvw) <- as.character(years)
cald = harvw
calg = cald
calm = cald

cls = c("PRHUNT",
        "ZOHUNT",
        "AOU",
        "MONH",
        "DAYH",
        "BAGE",
        "BSEX",
        "PAGE",
        "SAMPLE",
        "PERMIT",
        "YEAR",
        #"FOLYEAR",
        #"JDLWA",
        "YRHUNT",
        #"JDHUN",
       "WEEK")
#
# simple funtion to replace permit numbers with unique anonymous factors
permit_replace <- function(x,Y){
  y <- as.integer(factor(x))
  y <- paste0(Y,"_",y)
  return(y)
}


for (y in years){
  dir.yr <- paste0(home.fold1,y)

  ## this if, and the related if(y == Y) statements ensure that the 
  ## sas based files are only loaded for the current year.
  if(y == Y){
  fil.yr <- paste0("harv",substring(y,3,4),"w")

  tmpharv <- read_sas(paste0(dir.yr,"/",fil.yr,".sas7bdat")) %>%
    rename_with(.,str_to_upper) %>% 
    mutate(PERM = PERMIT + SELYEAR*1e6,
           PERMIT = permit_replace(PERM,y))
  
  perm_lookup_nhs <- tmpharv %>%
    select(PERM,PERMIT) %>%
    distinct()

  tmpharv <- tmpharv %>%
    select(-PERM)


  harvw[[as.character(y)]] <- tmpharv

  saveRDS(harvw[[as.character(y)]],paste0("data/harvw_",y,"_anon.rds"))

  }else{

    tmpharv <- readRDS(paste0("arch/harvw_",y,".rds")) %>%
      rename_with(.,str_to_upper) %>% 
      mutate(PERM = PERMIT + SELYEAR*1e6,
             PERMIT = permit_replace(PERM,y))

    perm_lookup_nhs <- tmpharv %>%
        select(PERM,PERMIT) %>%
        distinct()

      tmpharv <- tmpharv %>%
        select(-PERM)

    harvw[[as.character(y)]] <- tmpharv

    saveRDS(harvw[[as.character(y)]],paste0("data/harvw_",y,"_anon.rds"))

  }

  if(y == Y){
  fil.yr <- paste0("dcal",substring(y,3,4))
  tmp_cal <- read_sas(paste0(dir.yr,"/",fil.yr,".sas7bdat")) %>%
    mutate(PERM = PERMIT + SELYEAR*1e6) %>%
    select(-PERMIT) %>%
    left_join(.,perm_lookup_nhs,
              by = "PERM")%>% 
    select(-PERM)

  cald[[as.character(y)]] <- tmp_cal
  saveRDS(cald[[as.character(y)]],paste0("data/cald_",y,"_anon.rds"))

  }else{

  tmp_cal <- readRDS(paste0("arch/cald_",y,".rds")) %>%
    mutate(PERM = PERMIT + SELYEAR*1e6)  %>%
    select(-PERMIT) %>%
    left_join(.,perm_lookup_nhs,
              by = "PERM")%>% 
    select(-PERM)

  cald[[as.character(y)]] <- tmp_cal
  saveRDS(cald[[as.character(y)]],paste0("data/cald_",y,"_anon.rds"))

  }

  if(y == Y){
  fil.yr <- paste0("gcal",substring(y,3,4))
  tmp_calg <- read_sas(paste0(dir.yr,"/",fil.yr,".sas7bdat")) %>%
    mutate(PERM = PERMIT + SELYEAR*1e6)  %>%
    select(-PERMIT) %>%
    left_join(.,perm_lookup_nhs,
              by = "PERM")%>% 
    select(-PERM)

  calg[[as.character(y)]] <- tmp_calg
  saveRDS(calg[[as.character(y)]],paste0("data/calg_",y,"_anon.rds"))
}else{
  tmp_calg <- readRDS(paste0("arch/calg_",y,".rds"))%>%
    mutate(PERM = PERMIT + SELYEAR*1e6)  %>%
    select(-PERMIT) %>%
    left_join(.,perm_lookup_nhs,
              by = "PERM")%>% 
    select(-PERM)

  calg[[as.character(y)]] <- tmp_calg
  saveRDS(calg[[as.character(y)]],paste0("data/calg_",y,"_anon.rds"))

}
   if(y > 2012){
     if(y == Y){
   fil.yr <- paste0("mcal",substring(y,3,4))
   tmp_calm <- read_sas(paste0(dir.yr,"/",fil.yr,".sas7bdat"))%>%
     mutate(PERM = PERMIT + SELYEAR*1e6)  %>%
     select(-PERMIT) %>%
     left_join(.,perm_lookup_nhs,
               by = "PERM")%>% 
     select(-PERM)

   calm[[as.character(y)]] <- tmp_calm
   saveRDS(calm[[as.character(y)]],paste0("data/calm_",y,"_anon.rds"))

     }else{

       tmp_calm <- readRDS(paste0("arch/calm_",y,".rds"))%>%
         mutate(PERM = PERMIT + SELYEAR*1e6)  %>%
         select(-PERMIT) %>%
         left_join(.,perm_lookup_nhs,
                   by = "PERM") %>% 
         select(-PERM)

       calm[[as.character(y)]] <- tmp_calm

       saveRDS(calm[[as.character(y)]],paste0("data/calm_",y,"_anon.rds"))
     }
   }

  saveRDS(perm_lookup_nhs,paste0("data/permit_lookup_",y,".rds"))

  if(y == Y){
 fil.yr = paste0("persal",substring(y,3,4))
 tmpp <- read_sas(paste0(home.fold1,"/PermitSales/",fil.yr,".sas7bdat"))
 


 if(any(tmpp$YEAR > 50,na.rm = T) ){
   tmpp$YEAR = tmpp$YEAR+1900
 }else{
   tmpp$YEAR = tmpp$YEAR+2000

 }
 saveRDS(tmpp,paste0("data/persal_",y,".rds"))
  }else{
 tmpp <- readRDS(paste0("data/persal_",y,".rds"))
  }
  
  if(y == Y){
 fil.yr = paste0("popsiz",substring(y,3,4))
 tmppop <- read_sas(paste0(home.fold1,"/PopulationSize/",fil.yr,".sas7bdat"))
 
 saveRDS(tmppop,paste0("data/popsiz_",y,".rds"))
  }else{
 tmppop <- readRDS(paste0("data/popsiz_",y,".rds"))
}

 ### if desired to swap BAGE for PAGE (geese), then scsYY is required, instead of scsYYe
 ### but then additional changes are needed to align with old data
  if(y == Y){
  fil.yr <- paste0("scs",substring(y,3,4),"e")
  tmp <- read_sas(paste0(dir.yr,"/",fil.yr,".sas7bdat")) %>% 
    mutate(PERM = PERMIT + y*1e6,
           PERMIT = permit_replace(PERMIT,y))
  
  perm_lookup_scs <- tmp %>% 
    select(PERM,PERMIT) %>% 
    distinct()
  
  tmp <- tmp %>% 
    select(-PERM)
  
  if(is.null(tmp)){
    dir.alt <- "C:/Users/smithac/OneDrive - EC-EC/Harvest Survey A146/alt"
    tmp <- read_sas(paste0(dir.alt,"/",fil.yr,".sas7bdat"))%>% 
      mutate(PERM = PERMIT + y*1e6,
             PERMIT = permit_replace(PERMIT,y))
    
    perm_lookup_scs <- tmp %>% 
      select(PERM,PERMIT) %>% 
      distinct()

    
    tmp <- tmp %>% 
      select(-PERM)
  }
  # fil.yr <- paste0("scs",substring(y,3,4))
  # tmp2 <- read.ssd(libname = dir.yr,
  #                  sectionnames = fil.yr,
  #                  sascmd = file.path(sashome, "sas.exe"))
  #
  # tmp2u <- unique(tmp2[,c("PRHUNT","ZOHUNT","AOU","MONH","DAYH","BAGE","BSEX","PAGE","PERMIT")])

  tmp[which(tmp$PRHUNT == ""),"PRHUNT"] <- tmp[which(tmp$PRHUNT == ""),"PRSALE"]
  tmp[which(tmp$PRHUNT == ""),"ZOHUNT"] <- tmp[which(tmp$PRHUNT == ""),"ZOSALE"]

  ## fixing a handful of years in which column names varied and years were recorded as 2 digits
  if(c("YHUN") %in% names(tmp)){
    names(tmp)[which(names(tmp) == "YHUN")] <- "YRHUNT"
  }
  if(any(tmp$YEAR < min(years),na.rm = T)){
    tmp[which(tmp$YEAR < min(years)),"YEAR"] <- tmp[which(tmp$YEAR < min(years)),"YEAR"]+1900
  }
  if(any(tmp$YRHUNT < min(years),na.rm = T)){
    tmp[which(tmp$YRHUNT < min(years)),"YRHUNT"] <- tmp[which(tmp$YRHUNT < min(years)),"YRHUNT"]+1900
  }
  # if(any(tmp$JDHUN < 1000)){
  #   tmp[which(tmp$JDHUN < 1000),"JDHUN"] <- tmp[which(tmp$JDHUN < 1000),"JDHUN"]+((y-1900)*10)
  # }

  miscls = cls[-which(cls %in% names(tmp))]

  if(length(miscls) > 0){

  if(miscls == "PAGE"){
    tmp$PAGE <- ""
  }
  }

  tmp = tmp[,cls]
  
  saveRDS(tmp,paste0("data/scs_",y,"_anon.rds"))
 
}else{
  
  tmp <- readRDS(paste0("arch/scs_",y,".rds"))%>% 
    mutate(PERM = PERMIT + y*1e6,
           PERMIT = permit_replace(PERMIT,y))
  
  perm_lookup_scs <- tmp %>% 
    select(PERM,PERMIT) %>% 
    distinct()
  
  
  tmp <- tmp %>% 
    select(-PERM)
  
  saveRDS(tmp,paste0("data/scs_",y,"_anon.rds"))
  
}

  saveRDS(perm_lookup_scs,paste0("data/permit_lookup_scs_",y,"_anon.rds"))
  
  if(y == years[1]) {
    outscse <- tmp
    perms = tmpp
    popsiz = tmppop
  }else{
    outscse <- rbind(outscse,tmp)
    perms = rbind(perms,tmpp)
    popsiz = rbind(popsiz,tmppop)
    }
  #

print(y)

  }#y


#save.image(file = "data/stored_SAS_download.RData")

saveRDS(perms,paste0("data/perms.rds"))
saveRDS(popsiz,paste0("data/popsiz.rds"))

saveRDS(cald,paste0("data/cald.rds"))
saveRDS(harvw,paste0("data/harvw.rds"))
saveRDS(calg,paste0("data/calg.rds"))
saveRDS(calm,paste0("data/calm.rds"))



#if(any(calm[[as.character(2019)]]$YEAR != 2019)){stop("ERROR murre calendar info for 2019 is wrong")}
for(y in years){
  
if(any(cald[[as.character(y)]]$YEAR != y)){print(paste("Warning duck calendar info for",y," had to be corrected == ",unique(cald[[as.character(y)]]$YEAR)))
  cald[[as.character(y)]]$YEAR <- y}
  if(any(calm[[as.character(y)]]$YEAR != y)){print(paste("Warning murre calendar info for",y," had to be corrected == ",unique(calm[[as.character(y)]]$YEAR)))
    calm[[as.character(y)]]$YEAR <- y}
  if(any(calg[[as.character(y)]]$YEAR != y)){print(paste("Warning goose calendar info for",y," had to be corrected == ",unique(calg[[as.character(y)]]$YEAR)))
    calg[[as.character(y)]]$YEAR <- y}
  
}

save(list = c("calg","cald","calm"),
     file = "data/calendars.RData")
# Fix species AOU values incl Eiders, CANG, SNGO, etc. --------------------------------------

for(i in 1:nrow(aou_rec)){
  c1 = aou_rec[i,"AOUoriginal"]
  c2 = aou_rec[i,"AOU"]

  outscse[which(outscse$AOU == c1),"AOU"] <- c2 #cod for COEI changed in ~1990

  
}

outscse <- outscse[which(!is.na(outscse$AOU)),]

#fixing historical data with -weeks
tof <- which(outscse$WEEK < 1) #small % of parts have negative weeks because the dates indicate hunting in August (range from -5 to -1)
outscse[tof,"MONH"] <- 9 #this works because all tof have MONH == 8, it's just hunters getting the month wrong

tof2 = which(outscse$MONH == 8) #few remaining parts with harvest month = august
outscse[tof2,"MONH"] <- 9 #this works because all tof have MONH == 8, it's just hunters getting the month wrong

first_day <- "09-01" ### No hunting in August, so all week definitions begin on September 1



# setting all weeks based on days since Sept 1 ----------------------------

outscse$date = as.Date(paste(outscse$YRHUNT,
                          outscse$MONH,
                          outscse$DAYH,sep = "-"),
                    format = "%Y-%m-%d")

for(y in years){
min_day_y <- as.Date(paste(y,first_day,sep = "-"),format = "%Y-%m-%d")

wy = which(outscse$YEAR == y)

outscse[wy,"WEEK"] = as.integer(ceiling((outscse[wy,"date"]-(min_day_y-1))/7))

}
# outscse$WKdif = outscse$WEEK - outscse$WEEK2
# 
# 
tof <- which(outscse$WEEK < 1) #small % of parts have negative weeks because the dates indicate hunting in August (range from -5 to -1)
if(length(tof) > 0){stop("some parts have non-positive weeks = hunting pre September 1")}



#### replace BAGE with PAGE
outscse[which(outscse$PAGE != ""),"BAGE"] <- outscse[which(outscse$PAGE != ""),"PAGE"]
# outscse[which(outscse$BAGE %in% c("2")),"BAGE"] <- "I"
# outscse[which(outscse$BAGE %in% c("3")),"BAGE"] <- "U"
# outscse[which(outscse$BAGE %in% c("")),"BAGE"] <- "U"



outscse[which(outscse$BAGE %in% c("1","S","T")),"BAGE"] <- "A"
outscse[which(outscse$BAGE %in% c("2")),"BAGE"] <- "I"
outscse[which(outscse$BAGE %in% c("3")),"BAGE"] <- "U"
outscse[which(outscse$BAGE %in% c("")),"BAGE"] <- "U"

outscse[which(outscse$BSEX %in% c("1")),"BSEX"] <- "M"
outscse[which(outscse$BSEX %in% c("2")),"BSEX"] <- "F"
outscse[which(outscse$BSEX %in% c("3")),"BSEX"] <- "U"
outscse[which(outscse$BSEX %in% c("")),"BSEX"] <- "U"

#outscse$BAGE = factor(outscse$BAGE)
#round(prop.table(table(outscse$BAGE,outscse$AOU),2),2)

#outscse$BSEX = factor(outscse$BSEX)
#round(prop.table(table(outscse$BSEX,outscse$AOU),2),2)


saveRDS(outscse,paste0("data/outscse.rds"))



# Creating kill file ------------------------------------------------------




cls = c("PERMIT",
        "CASTE",
        "YEAR",
        "SELYEAR",
        "PRHUNT",
        "ZOHUNT",
        "LATD",
        "LOND",
        "TODUK",
        "TOGOK",
        "COOTK",
        "WOODK",
        "SNIPK",
        "DOVEK",
        "PIGEK",
        "CRANK",
        "RAILK",
        "MURRK",
        "RNDMURK",
        "DAYWF",
        "DAYOT",
        "DAYM",
        "PRHUNTG",
        "ZOHUNTG",
        "LATG",
        "LONG",
        "PRHUNTM",
        "ZOHUNTM",
        "LATM",
        "LONM",
        "SUCCWF",
        "SUTODU",
        "SUTOGO",
        "SUCCOT",
        "SUCCM",
        "ACTIVEOT",
        "ACTIVE",
        "ACTIVEWF",
        "ACTIVEM",
        "POTNTL",
        "PRSALE",
        "ZOSALE",
        "PRSAMP",
        "ZOSAMP")
allkill <- NULL

for(y in years){
  tmp1 <- harvw[[as.character(y)]]
  tmp <- tmp1[,which(names(tmp1) %in% cls)]
  #tmp = tmp1
  
  if(y == years[1]){
    allkill <- tmp
  }else{
    allkill <- bind_rows(allkill,tmp)
  }
}

trem = which(allkill$CASTE %in% c("C","F","H"))
if(length(trem)>0){
  allkill = allkill[-trem,]
}### removing the unused castes; there are permits that have this caste designation across all years


## Retaining only potential hunters
tkp = which(allkill$POTNTL == "Y")
# if(length(tkp)>0){
allkill = allkill[tkp,]
#}### removing the hunters sampled from last year's permit file who indicated they didn't buy a permit this year
### and are therefore not potential hunters




allkill$uniperm = allkill$PERMIT 
dupuni = allkill$uniperm[duplicated(allkill$uniperm)]
## there are no duplicates.
dupdf = allkill[which(allkill$uniperm %in% dupuni),]
dupdf = dupdf[order(dupdf$uniperm),]

paste("There are ",nrow(dupdf),"permit numbers duplicated")
paste("WARNING Removing these",nrow(dupdf),"duplicated permits")
allkill = allkill[which(!allkill$uniperm %in% dupuni),]



wmigoo <- which(allkill$PRHUNTG == "")
allkill$PRHUNTG = as.character(allkill$PRHUNTG)
allkill[wmigoo,"PRHUNTG"] <- as.character(allkill[wmigoo,"PRHUNT"])
allkill[wmigoo,"ZOHUNTG"] <- allkill[wmigoo,"ZOHUNT"]



wsud = which(allkill$TODUK > 0)
allkill$SUTODU <- "N"
allkill[wsud,"SUTODU"] <- "Y"


wsud = which(allkill$TOGOK > 0)
allkill$SUTOGO <- "N"
allkill[wsud,"SUTOGO"] <- "Y"



tkeepP = which(allkill$PRSAMP %in% provs) #keeps only permits sampled in primary provinces. drops NU

allkill = allkill[tkeepP,]



nrow(allkill) == length(unique(allkill$uniperm))
allkill$year = allkill$YEAR-(min(allkill$YEAR)-1)
allkill$caste = factor(allkill$CASTE,
                       ordered = T,
                       levels = c("D","B","A","E"))





######## sampling population sizes
popsiz_s = merge(popsiz,provzone[,c("prov","provn")],by.x = "PRSAMP",by.y = "provn",all.x = T)
popsiz_s = unique(popsiz_s)



#### total number of permits in each year

popsiz_perm = merge(perms,provzone[,c("prov","provn")],by.x = "PRSALE",by.y = "provn",all.x = T)
popsiz_perm = unique(popsiz_perm)


### total number of permits by zone and year

z_pops <- popsiz_perm %>%
  select(-PRSALE) %>% 
  rename(PRSAMP = prov,ZOSAMP = ZOSALE) %>% 
  group_by(PRSAMP,ZOSAMP,YEAR) %>% 
  summarise(TOTSALE = sum(TOTSALE))

# popsiz_perm$yr = str_sub(popsiz_perm$YEAR,start = 3,end = 4)
# tmp = left_join(popsiz_perm,popsiz_s[,c("zone","caste","TOTPERM","yr","prov")])


## one off removal of 99 permits that responded to an incorrectly
## mailed HQS form that was missing the murre page
## it was decided to remove responses from these permits when calculating
## the murre harvest estimates
## see email from M.Gendron June 11, 2024
## "Also, as discussed last week, HQS questionnaires from NF from batch A057 should be deleted only when generating the murre estimates. If you recall, these responses were received via the two-page HQS, instead of the 3-pager.  I’ve added in the folder a file with the list of records, if using permit number is easier than batch number."
##    

if(Y == 2023){
one_off_drop <- readxl::read_excel("data/NF response via two page HQS.xlsx")
names(one_off_drop) <- c("PERM","B")


perm_lookup_nhs_one_off <- readRDS(paste0("data/permit_lookup_2023.rds"))

perms_drop_murre_2023 <- perm_lookup_nhs_one_off %>%
  inner_join(one_off_drop, by = "PERM") %>%
  select(PERMIT)
saveRDS(perms_drop_murre_2023,"data/permits_drop_murre_2023.rds")

}

# correcting the age and sex indicators -----------------------------------

save(list = c("allkill",
              "outscse",
              "z_pops",
              "popsiz_s",
              "popsiz_perm",
              "sps"),
     file = "data/allkill.RData")



# exporting the parts data as a readable csv file -------------------------
parts_out <- outscse[,c("PRHUNT","ZOHUNT","AOU","YRHUNT","MONH","DAYH","BAGE","BSEX","WEEK")]
names(parts_out) <- c("Province of hunt",
                      "Zone of hunt",
                      "AOU",
                      "Year",
                      "Month",
                      "Day",
                      "Age",
                      "Sex",
                      "Week of season")
parts_out <- left_join(parts_out,sps,by = "AOU")
write.csv(parts_out,paste0("GoogleDrive/All_raw_parts_data_",Y,".csv"))
# tmp <- parts_out %>% filter(specieslevelenglish == "Mallard")
# write.csv(tmp,paste0("output/Mallard_parts_all_years_through_",Y,".csv"))


######################
# period define across all years -----------------------------------------


zones <- 1:3
pers <- 1:20
##### identify periods based on weeks with at least prop_period-% of the parts across all years
prop_period <- 0.05
period.duck <- expand.grid(pr = provs,zo = zones,period = pers,stringsAsFactors = F)

period.duck[,"startweek"] <- NA
period.duck[,"endweek"] <- NA

period.goose <- period.duck
period.murre <- period.duck

for(pr in provs){
  pzones <- unique(outscse[which(outscse$PRHUNT == pr),"ZOHUNT"])
  if(anyNA(pzones)){pzones <- pzones[-which(is.na(pzones))]}
  for(z in pzones){
    tmp <- outscse[which(outscse$PRHUNT == pr & outscse$ZOHUNT == z & outscse$AOU %in% sps[which(sps$group == "duck"),"AOU"]),]

    testm <- table(tmp[,c("AOU")],tmp[,c("WEEK")])

    wsums <- colSums(testm)
    wprops <- wsums/sum(wsums)

    
    per1 <- 1
    per2 <- NA
    p = 1
    mw <- F
    q <- 0

    for(w in 1:length(wprops)){
      if(mw){

        if(sum(wprops[per1[p]:(per1[p]+q)]) > prop_period){
          q <- 0
          per2[p] <- w
          p <- p+1
          mw <- F
        }else{
          q <- q+1
        }
      }else{

        if(wprops[w] > prop_period){
          per1[p] <- w
          per2[p] <- w
          p <- p+1
        }else{
          per1[p] <- w
          mw <- T
          q <- q+1
        }
      }

    }#w
    if(length(per1) > length(per2)){ #if TRUE it means that the end of the final period did not include 5% of parts
      per1 <- per1[-length(per1)]
    per2[p-1] <- length(wprops)}

    for(j in 1:length(per1)){
      rs <- which(period.duck$pr == pr & period.duck$zo == z & period.duck$period == j)
      period.duck[rs,"startweek"] <- per1[j]
      period.duck[rs,"endweek"] <- per2[j]

    }

  }#z

}#pr
period.duck <- period.duck[-which(is.na(period.duck$startweek)),]
period.duck <- period.duck[order(period.duck$pr,period.duck$zo),]
write.csv(period.duck,"data/period.duck.csv",row.names = F)


##### goose periods

for(pr in provs){
  pzones <- unique(outscse[which(outscse$PRHUNT == pr),"ZOHUNT"])
  if(anyNA(pzones)){pzones <- pzones[-which(is.na(pzones))]}
  for(z in pzones){
    tmp <- outscse[which(outscse$PRHUNT == pr & outscse$ZOHUNT == z & outscse$AOU %in% sps[which(sps$group == "goose"),"AOU"]),]

    testm <- table(tmp[,c("AOU")],tmp[,c("WEEK")])

    wsums <- colSums(testm)
    wprops <- wsums/sum(wsums)

    ##### identify periods based on weeks with at least 5% of the parts across all years
    per1 <- 1
    per2 <- NA
    p = 1
    mw <- F
    q <- 0

    for(w in 1:length(wprops)){
      if(mw){

        if(sum(wprops[per1[p]:(per1[p]+q)]) > prop_period){
          q <- 0
          per2[p] <- w
          p <- p+1
          mw <- F
        }else{
          q <- q+1
        }
      }else{

        if(wprops[w] > prop_period){
          per1[p] <- w
          per2[p] <- w
          p <- p+1
        }else{
          per1[p] <- w
          mw <- T
          q <- q+1
        }
      }

    }#w
    if(length(per1) > length(per2)){per1 <- per1[-length(per1)]
    per2[p-1] <- length(wprops)}

    for(j in 1:length(per1)){
      rs <- which(period.goose$pr == pr & period.goose$zo == z & period.goose$period == j)
      period.goose[rs,"startweek"] <- per1[j]
      period.goose[rs,"endweek"] <- per2[j]

    }

  }#z

}#pr
period.goose <- period.goose[-which(is.na(period.goose$startweek)),]
period.goose <- period.goose[order(period.goose$pr,period.goose$zo),]
write.csv(period.goose,"data/period.goose.csv",row.names = F)



##### murre periods

for(pr in "NF"){
  pzones <- unique(outscse[which(outscse$PRHUNT == pr),"ZOHUNT"])
  if(anyNA(pzones)){pzones <- pzones[-which(is.na(pzones))]}
  for(z in pzones){
    tmp <- outscse[which(outscse$PRHUNT == pr & outscse$ZOHUNT == z & outscse$AOU %in% sps[which(sps$group == "murre"),"AOU"]),]
    wkblank = data.frame(WEEK = as.integer(1:max(tmp$WEEK)),
                         AOU_bl = 300)
    tmp <- merge(tmp,wkblank,by = c("WEEK"),all.y = TRUE)
    #tmp[which(is.na(tmp$AOU)),"AOU"]
    testm <- table(tmp[,c("AOU")],tmp[,c("WEEK")])
    
    wsums <- colSums(testm)
    wprops <- wsums/sum(wsums)
    
    if(length(unique(names(testm[1,]))) != max(as.integer(names(testm[1,]))))
    ##### identify periods based on weeks with at least 5% of the parts across all years
    per1 <- 1
    per2 <- NA
    p = 1
    mw <- F
    q <- 0
    
    for(w in 1:length(wprops)){
      if(mw){
        
        if(sum(wprops[per1[p]:(per1[p]+q)]) > prop_period){
          q <- 0
          per2[p] <- w
          p <- p+1
          mw <- F
        }else{
          q <- q+1
        }
      }else{
        
        if(wprops[w] > prop_period){
          per1[p] <- w
          per2[p] <- w
          p <- p+1
        }else{
          per1[p] <- w
          mw <- T
          q <- q+1
        }
      }
      
    }#w
    if(length(per1) > length(per2)){per1 <- per1[-length(per1)]
    per2[p-1] <- length(wprops)}
    
    for(j in 1:length(per1)){
      rs <- which(period.murre$pr == pr & period.murre$zo == z & period.murre$period == j)
      period.murre[rs,"startweek"] <- per1[j]
      period.murre[rs,"endweek"] <- per2[j]
      
    }
    
  }#z
  
}#pr
period.murre <- period.murre[-which(is.na(period.murre$startweek)),]
period.murre <- period.murre[order(period.murre$pr,period.murre$zo),]
write.csv(period.murre,"data/period.murre.csv",row.names = F)






 
#  







