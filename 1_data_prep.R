
####### the following commented out lines ~ 400 lines
## represent the add-hoc processes required to load all the 
## historical harvest survey data
## the line :load("full data prep updated harvest model.RData")
## on about line-400 will load everything created below



### 

# species changes for final harvest survey estimates ----------------------

### recode all 1720 as 1722 (large race Canada Geese)
### recode all 1691 as 1690 (white-phase lesser snow goose - drop blue phase)
### Drop Black Brant - 1740
### drop Eurasian Green-winged Teal - 1380
### consider splitting eastern and western Harlequin ducks
# 1550 = western, 1551 = eastern?
### consider splitting eastern and western Barrow's Goldeneye
# 1520 = western, 1521 = eastern?



Y <- 2020
years <- 1976:Y

names(years) <- paste(years)
home.fold1 <- "m:/My Documents/Harvest Survey A146/"
# 
home.fold <- getwd()
# setwd(home.fold)

library(foreign)
library(runjags)
library(rjags)
library(tidyverse)

sashome <- "C:\\Program Files\\SASHome\\SASFoundation\\9.4"
provs = c("AB","BC","SK","MB","ON","PQ","NS","PE","NB","NF","NT","YT")#,"NU") #All prov
#ignoring territories above

sps <- read.csv(paste(home.fold,"/data/Bird_names_2019.csv", sep = ""))
aou_rec <- read.csv("data/Reconcile_AOU_codes_2019.csv")


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
for (y in years){
  dir.yr <- paste0(home.fold1,y)

  fil.yr <- paste0("harv",substring(y,3,4),"w")
  harvw[[as.character(y)]] <- read.ssd(libname = dir.yr,
                                       sectionnames = fil.yr,
                                       sascmd = file.path(sashome, "sas.exe"))
  fil.yr <- paste0("dcal",substring(y,3,4))
  cald[[as.character(y)]] <- read.ssd(libname = dir.yr,
                                        sectionnames = fil.yr,
                                        sascmd = file.path(sashome, "sas.exe"))
  fil.yr <- paste0("gcal",substring(y,3,4))
  calg[[as.character(y)]] <- read.ssd(libname = dir.yr,
                                        sectionnames = fil.yr,
                                        sascmd = file.path(sashome, "sas.exe"))

   if(y > 2012){
   fil.yr <- paste0("mcal",substring(y,3,4))
   calm[[as.character(y)]] <- read.ssd(libname = dir.yr,
                                       sectionnames = fil.yr,
                                       sascmd = file.path(sashome, "sas.exe"))
 }
 fil.yr = paste0("persal",substring(y,3,4))
 tmpp <- read.ssd(libname = paste0(home.fold1,"/PermitSales"),
                                     sectionnames = fil.yr,
                                     sascmd = file.path(sashome, "sas.exe"))
 if(any(tmpp$YEAR > 50,na.rm = T) ){
   tmpp$YEAR = tmpp$YEAR+1900
 }else{
   tmpp$YEAR = tmpp$YEAR+2000

 }


 fil.yr = paste0("popsiz",substring(y,3,4))
 tmppop <- read.ssd(libname = paste0(home.fold1,"/PopulationSize"),
                  sectionnames = fil.yr,
                  sascmd = file.path(sashome, "sas.exe"))


 ### if desired to swap BAGE for PAGE (geese), then scsYY is required, instead of scsYYe
 ### but then additional changes are needed to align with old data
  fil.yr <- paste0("scs",substring(y,3,4),"e")
  tmp <- read.ssd(libname = dir.yr,
                                      sectionnames = fil.yr,
                                      sascmd = file.path(sashome, "sas.exe"))
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


save.image(file = "data/stored_SAS_download.RData")


load("data/stored_SAS_download.RData")

#if(any(calm[[as.character(2019)]]$YEAR != 2019)){stop("ERROR murre calendar info for 2019 is wrong")}
for(y in years){
  
if(any(cald[[as.character(y)]]$YEAR != y)){print(paste("ERROR duck calendar info for",y," is wrong == ",unique(cald[[as.character(y)]]$YEAR)))
  cald[[as.character(y)]]$YEAR <- y}
  if(any(calm[[as.character(y)]]$YEAR != y)){print(paste("ERROR murre calendar info for",y," is wrong == ",unique(calm[[as.character(y)]]$YEAR)))
    calm[[as.character(y)]]$YEAR <- y}
  if(any(calg[[as.character(y)]]$YEAR != y)){print(paste("ERROR goose calendar info for",y," is wrong == ",unique(calg[[as.character(y)]]$YEAR)))
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


tkp = which(allkill$POTNTL == "Y")
# if(length(tkp)>0){
allkill = allkill[tkp,]
#}### removing the hunters sampled from last year's permit file who indicated they didn't buy a permit this year
### and are therefore not potential hunters


trem = which(allkill$PERMIT == 0)
if(length(trem)>0){
  allkill = allkill[-trem,]
}### removes a single permit from 1985 with no permit number



allkill$uniperm = allkill$PERMIT + allkill$SELYEAR*1000000 + allkill$YEAR*10000000000
dupuni = allkill$uniperm[duplicated(allkill$uniperm)]
## there are no duplicates.
# dupdf = allkill[which(allkill$uniperm %in% dupuni),]
# dupdf = dupdf[order(dupdf$uniperm),]

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









# correcting the age and sex indicators -----------------------------------

save(list = c("allkill",
              "outscse",
              "z_pops",
              "popsiz_s",
              "popsiz_perm",
              "sps"),
     file = "data/allkill.RData")


######################
#define periods across all years

zones <- 1:3
pers <- 1:20

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

    ##### identify periods based on weeks with at least 5% of the parts across all years
    per1 <- 1
    per2 <- NA
    p = 1
    mw <- F
    q <- 0

    for(w in 1:length(wprops)){
      if(mw){

        if(sum(wprops[per1[p]:(per1[p]+q)]) > 0.05){
          q <- 0
          per2[p] <- w
          p <- p+1
          mw <- F
        }else{
          q <- q+1
        }
      }else{

        if(wprops[w] > 0.05){
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

        if(sum(wprops[per1[p]:(per1[p]+q)]) > 0.05){
          q <- 0
          per2[p] <- w
          p <- p+1
          mw <- F
        }else{
          q <- q+1
        }
      }else{

        if(wprops[w] > 0.05){
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
        
        if(sum(wprops[per1[p]:(per1[p]+q)]) > 0.05){
          q <- 0
          per2[p] <- w
          p <- p+1
          mw <- F
        }else{
          q <- q+1
        }
      }else{
        
        if(wprops[w] > 0.05){
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



#save.image(file = paste0("data/parts and harvest survey info",Y,".RData"))



 
#  







