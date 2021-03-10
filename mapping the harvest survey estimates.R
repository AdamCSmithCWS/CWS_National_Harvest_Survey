### mapping the harvest survey estimates
library(rgdal)
library(RColorBrewer)
library(dplyr)
specieslevel <- T
wd = "M:/My Documents/Harvest Survey A146/maps for website"
#wd = "c:/nhstemp/maps for website"
hdrive = "M:/My Documents/Harvest Survey A146"
setwd(wd)
Y = 2018

##### apend the most recent harsum and parsum files to the compiled online version
##### then re-run this full script
##### the goal is to replace all map files, making sure hte colour levels remain the same across years
##### in case the range of values changes with the addition of hte most recent year

### also re-visit the goose age and sex definitions, to make sure I've used the improved tail data
### 
if (specieslevel == T) {
  sps <- read.csv(paste("input/Bird names 2010.csv", sep = ""),
                  stringsAsFactors = F)
species <- unique(sps[which(sps$group %in% c("duck","goose","murre")),"specieslevelenglish"])
species <- species[-which(species == "Hybrid Mallard/Northern Pintail")]
gnames <- unique(sps[which(sps$group == "goose"),"specieslevelenglish"])
dnames <- unique(sps[which(sps$group == "duck"),"specieslevelenglish"])
dnames <- dnames[-which(dnames == "Hybrid Mallard/Northern Pintail")]
}


base <- readOGR(dsn = paste0(wd,"/input"),
                layer = "Harvest_Survey_Zones_2017")


spdat <- read.csv("input/harsum species web.csv",stringsAsFactors = F)
harvdat <- read.csv("input/harsum full web.csv",stringsAsFactors = F)
pardat <- read.csv("input/parsum full web.csv",stringsAsFactors = F)

############# add in the pardat spdat and harvdat files from recent years (starting in 2017)
for(y in 2017:Y){
  yn = as.character(y)
  spd = read.fwf(paste0(hdrive,"/HARSUM/HARSUM",yn,".TXT"),
                 widths = c(4,2,-1,1,-1,6,7,7),
                 strip.white = T,
                 col.names = names(spdat),
                 as.is = T)
 
  for(j in c(1,3,5,6)){
    spd[,j] <- suppressWarnings(as.integer(spd[,j]))
  }
  kp = which(!is.na((spd$Species_Code)))
  spd = spd[kp,]
  spd$Species_Code = suppressWarnings(as.integer(spd$Species_Code))
  spdat = rbind(spdat,spd)
  
  hrs = read.fwf(paste0(hdrive,"/HARSUM/HARSUM",yn,".TXT"),
                 widths = c(4,2,-1,1,1,6,7,7),
                 strip.white = T,
                 col.names = names(harvdat),
                 as.is = T)
  kp = which(hrs$Variable_Code %in% unique(harvdat$Variable_Code))
  hrs = hrs[kp,]
  for(j in c(1,3,6,7)){
    hrs[,j] <- suppressWarnings(as.integer(hrs[,j]))
  }
  harvdat = rbind(harvdat,hrs)
  
  yn = substr(yn,start = 3,stop = 4)
  pard = read.fwf(paste0(hdrive,"/parsum/PARSUM",yn,".ASC"),
                 widths = c(2,2,1,4,4,4,4,4,4,4,4,4,4),
                 strip.white = T,
                 col.names = names(pardat),
                 as.is = T)
  for(j in c(1,3:13)){
    pard[,j] <- suppressWarnings(as.integer(pard[,j]))
  }
  pard$Year = pard$Year+2000
  pardat = rbind(pardat,pard)
  
  
  
  }


### keep the colour scale constant across years
seqs <- seq(0,1,length = 10)[-c(1,10)]
colscale <- brewer.pal(9,"YlOrBr")





### loop through each species and year, producing a png of 
### the harvest distribution by zone




maplist <- expand.grid(Species_Code = as.integer(unique(spdat$Species_Code)),
                       Year = as.integer(unique(spdat$Year)))
for(v in unique(spdat$Species_Code)[1:length(unique(spdat$Species_Code))]){
  sp <- sps[which(sps$AOU == v),"ENGLISH"]
  spf <- gsub(sp,pattern = "/",replacement = "-",fixed = T)
  spf <- gsub(spf,pattern = ":",replacement = "-",fixed = T)

  
  tmp <- spdat[which(spdat$Species_Code == v),]
  if(nrow(tmp)<10){next}
  bbkst <- round(quantile(tmp$Estimate,
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
    round(max(tmp$Estimate,na.rm = T)+1))
  tmp$plotcat <- cut(tmp$Estimate,
                     breaks = bks)
  tmp$plotcol <- colscale[as.integer(tmp$plotcat)]
  for(y in unique(tmp$Year)){
   tmp2 <- tmp[which(tmp$Y == y),]
   tmp2$pzone <- paste(tmp2$Province_ID,tmp2$Zone_ID,sep = "")
   #tmp3 <- base@data
   tmp3 <- merge(x = base,
                 y = tmp2,
                 by = "pzone",
                 all.x = T) 
   tmp3@data[which(is.na(tmp3@data$plotcol)),"plotcol"] <- grey(0.7)
   #tmp3@data[which(tmp3@data$Estimate == 0),"plotcol"] <- grey(0.8)
maplist[which(maplist$Species_Code == v &
                maplist$Year == y),"mapname"] <- paste0(spf," ",v," ",y,".png")   
   png(filename = paste0("maps/",spf," ",v," ",y,".png"),
       res = 300,
       height = 8,
       width = 8,
       units = "in")
   
   plot(tmp3,
        col = tmp3@data$plotcol,
        main = "")#paste("Estimated number of",sp,"harvested in",y))
   legend("topright",
          fill = c(grey(0.7),
                   #grey(0.8),
                   colscale),
          legend = c("no estimate / aucune estimation",
                     #0,
                     paste0("1 - ",bks[2]),
                     paste(bks[2:(length(bks)-2)],
                           bks[3:(length(bks)-1)],
                           sep = " - "),
                     paste0("> ",bks[9])),
          bty = "n")
   dev.off()
   
   
}#y
}#v

write.csv(maplist,
          "list of species harvest maps.csv",
          row.names = F)







########### general harvest estimates




########### general harvest estimates




########### general harvest estimates




########### general harvest estimates

### only use the all hunter caste data
## must compile the caste-level data in the harvdat file into all-caste data
## similar code for true species level above

# also need real names for the variable codes

harvdat2 <- filter(tbl_df(harvdat),Hunter_Caste != "X")
harvdat2 = group_by(harvdat2,Year,Variable_Code)

harvdatt <- unique(harvdat[,c("Year",
                               "Province_ID",
                               "Zone_ID",
                               "Variable_Code")])

varsum <- function(x){
  sqrt(sum(x^2,na.rm = T))
}

for(i in 1:nrow(harvdatt)){
  p <- harvdatt[i,"Province_ID"]
  y <- harvdatt[i,"Year"]
  z <- harvdatt[i,"Zone_ID"]
  v <- harvdatt[i,"Variable_Code"]
  
  harvdatt[i,"Estimate"] <- sum(harvdat[which(harvdat$Year == y &
                                                 harvdat$Province_ID == p &
                                                 harvdat$Zone_ID == z &
                                                 harvdat$Variable_Code == v),"Estimate"],na.rm = T)
  
  harvdatt[i,"SE"] <- varsum(harvdat[which(harvdat$Year == y &
                                                 harvdat$Province_ID == p &
                                                 harvdat$Zone_ID == z &
                                                 harvdat$Variable_Code == v),"Estimate"])
  
}



maplist <- expand.grid(Variable_Code = unique(harvdatt$Variable_Code),
                       Year = unique(harvdatt$Year))


for(v in unique(harvdatt$Variable_Code)[1:length(unique(harvdatt$Variable_Code))]){
  
  #sp <- sps[which(sps$AOU == v),"ENGLISH"]
  #spf <- gsub(sp,pattern = "/",replacement = "-",fixed = T)
  #spf <- gsub(spf,pattern = ":",replacement = "-",fixed = T)
  spf <- v
  
  tmp <- harvdatt[which(harvdatt$Variable_Code == v),]
  if(nrow(tmp)<10){next}
  if(v %in% c("SUTOWF","TOWFK")){
    tmp = tmp[-which(tmp$Year == 1972),]
    }
  bbkst <- round(quantile(tmp$Estimate,
                          probs = c(seqs),
                          na.rm = T,
                          names = F))
  om <- max(1,min(nchar(bbkst))-1)
  
  bbks <- round(bbkst/(10^om))*10^om
  ss <- 1
  ### below rounds off breaks to more significant digits if necessary
  while(any(duplicated(bbks)) & om != 1){
    bbks <- round((bbkst/(10^(om-ss)))*10^(om-ss))
    ss = ss+1
  }
  #if above loop fails to remove duplicates, following loop just drops duplicated values
  
  if(any(duplicated(bbks))){
    bbks = bbks[-which(duplicated(bbks))]
  }
  
  bks <- c(0,
           bbks,
           round(max(tmp$Estimate,na.rm = T)+1))
  if(any(duplicated(bks))){
    bks = unique(bks)
  }
  tmp$plotcat <- cut(tmp$Estimate,
                     breaks = bks)
  tmp$plotcol <- colscale[as.integer(tmp$plotcat)]
  
  for(y in unique(tmp$Year)){
    if(y == 1972 & (v %in% c("SUTOWF","TOWFK"))){next}
    tmp2 <- tmp[which(tmp$Y == y),]
    tmp2$pzone <- paste(tmp2$Province_ID,tmp2$Zone_ID,sep = "")
    #tmp3 <- base@data
    tmp3 <- merge(x = base,
                  y = tmp2,
                  by = "pzone",
                  all.x = T) 
    tmp3@data[which(is.na(tmp3@data$plotcol)),"plotcol"] <- grey(0.7)
    #tmp3@data[which(tmp3@data$Estimate == 0),"plotcol"] <- grey(0.8)
    maplist[which(maplist$Variable_Code == v &
                    maplist$Year == y),"mapname"] <- paste0(spf," ",v," ",y,".png")   
    png(filename = paste0("maps/",spf," ",v," ",y,".png"),
        res = 300,
        height = 8,
        width = 8,
        units = "in")
    
    plot(tmp3,
         col = tmp3@data$plotcol,
         main = "")#paste("Estimate of",spf,"in",y))
    legend("topright",
           fill = c(grey(0.7),
                    #grey(0.8),
                    colscale),
           legend = c("no estimate / aucune estimation",
                      #0,
                      paste0("1 - ",bks[2]),
                      paste(bks[2:(length(bks)-2)],
                            bks[3:(length(bks)-1)],
                            sep = " - "),
                      paste0("> ",bks[9])),
           bty = "n")
    dev.off()
    
    
  }#y
}#v

write.csv(maplist,
          "list of general harvest maps.csv",
          row.names = F)








########### parts estimates






########### parts estimates





########### parts estimates





########### parts estimates





########### parts estimates


### new diverging colour scale
colscale <- brewer.pal(11,"RdYlBu")
seqs <- seq(0,0.9,length = 6)[-c(1)]


### create a sequence that equally spreads above and below 1

maplist <- expand.grid(Species_Code = unique(pardat$Species_Code),
                       Year = unique(pardat$Year))

pardat$adult = pardat$AF + pardat$AM + pardat$AU
pardat$immature = pardat$IF_fld + pardat$IM + pardat$IU
pardat$ratio = pardat$immature/pardat$adult
pardat[which((pardat$adult == 0 | pardat$immature == 0) |
               pardat$adult + pardat$immature < 20),"ratio"] = NA

for(v in unique(pardat$Species_Code)[1:length(unique(pardat$Species_Code))]){
  sp <- sps[which(sps$AOU == v),"ENGLISH"]
  spf <- gsub(sp,pattern = "/",replacement = "-",fixed = T)
  spf <- gsub(spf,pattern = ":",replacement = "-",fixed = T)
  
  
  tmp <- pardat[which(pardat$Species_Code == v),]
  if(nrow(tmp)<10 | length(which(!is.na(tmp$ratio))) < 10){next}
  
  bbkst1 <- round(quantile(abs(log(tmp[,"ratio"])),
                          probs = c(seqs),
                          na.rm = T,
                          names = F),2)
  
  # om <- min(nchar(bbkst))-1
  # 
  
  bbks <- c(rev(bbkst1[-5]*-1),0,bbkst1)     #round(bbkst/(10^om))*10^om
  bbks = exp(bbks)
  ss <- 1
  
  # while(any(duplicated(bbks))){
  #   bbks <- round((bbkst/(10^(om-ss)))*10^(om-ss))
  #   ss = ss+1
  # }
  
  if(min(tmp$ratio,na.rm = T) < min(bbks)){
    lb = min(tmp$ratio,na.rm = T)-0.05}else{
      lb = min(bbks)-0.05
    }
  if(max(tmp$ratio,na.rm = T) > max(bbks)){
    ub = max(tmp$ratio,na.rm = T)+0.05}else{
      ub = max(bbks)+0.05
    }
  bks <- c(lb,
           bbks,
           ub)
  
  catlabs = signif(bks,2)
  tmp$plotcat <- cut((tmp$ratio),
                     breaks = bks)
  
  tmp$plotcol <- colscale[as.integer(tmp$plotcat)]
  for(y in unique(tmp$Year)){
    tmp2 <- tmp[which(tmp$Y == y),]
    tmp2$pzone <- paste(tmp2$Province_ID,tmp2$Zone_ID,sep = "")
    #tmp3 <- base@data
    tmp3 <- merge(x = base,
                  y = tmp2,
                  by = "pzone",
                  all.x = T) 
    tmp3@data[which(is.na(tmp3@data$plotcol)),"plotcol"] <- grey(0.7)
    #tmp3@data[which(tmp3@data$Estimate == 0),"plotcol"] <- grey(0.8)
    mpname = paste0("maps/",spf," age ratio of parts ",v," ",y,".png")
    maplist[which(maplist$Species_Code == v &
                    maplist$Year == y),"mapname"] <- mpname
    png(filename = mpname,
        res = 300,
        height = 8,
        width = 8,
        units = "in")
    par(xpd = T)
    plot(tmp3,
         col = tmp3@data$plotcol,
         main = "")#paste("Estimated number of",sp,"harvested in",y))
    legpl = tmp3@bbox[,2]#-((tmp3@bbox[,2]-tmp3@bbox[,1])*0.02) #grabs the maximum x and y coordinates of the map bounding box
    legpl[2] = legpl[2]+(tmp3@bbox[2,2]-tmp3@bbox[2,1])*0.20
    legpl[1] = legpl[1]+(tmp3@bbox[1,2]-tmp3@bbox[1,1])*0.05
    
    legend(#"topright",
           #inset = -0.05,
           xjust = 1,
            x = legpl["x"],
            y = legpl["y"],
           fill = c(grey(0.7),
                    #grey(0.8),
                    colscale),
           legend = c("no estimate / aucune estimation",
                      #0,
                      paste0("< ",catlabs[2]),
                      paste(catlabs[2:(length(catlabs)-2)],
                            catlabs[3:(length(catlabs)-1)],
                            sep = " - "),
                      paste0("> ",catlabs[length(catlabs)-1])),
           bty = "n")
    dev.off()
    
    
  }#y
}#v

write.csv(maplist,
          "list of parts ratio maps.csv",
          row.names = F)









