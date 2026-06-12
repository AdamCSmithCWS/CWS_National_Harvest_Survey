## summarising raw permit files
## 





# Summarising raw permits by residency ------------------------------------


library(tidyverse)
# possible one-off analysis for Frank Baldwin see Teams discussion
# March, 2026, 
# Hey all, one of our stakeholder groups has requested data on mig game bird permit sales by residency for MB, SK, AB, and also total youth permit sales by province.  I think we used to include this information on the national harvest survey website, but now we just use total permit sales (returned stubs).  Across the prairies there have been lot of changes recently, including implementation of provincial term licences for non-residents, and they want to have current information on the composition of permit holders by province. Would you be able to pull this data for me, up to 2025 if possible?
# Yes, residency of hunters who indicate SK, MB, AB as primary hunting location, say over the last 10 years. And, they didn't ask for this, but I think it would be useful to summarize total # permits sold to people from MB, SK, AB, maybe over the last 10 years? I realize not everyone fills out the stratification questions so this would give us an idea of potential issues associated with our assumption of how many permit holders actually hunt.
# 
provzone = read.csv("data/province and zone table.csv",stringsAsFactors = F)
casteslist = read.csv("data/caste table.csv",stringsAsFactors = F)

years <- c(2014:2024)
hqs_all <- NULL
for(y in years){
  if(y > 2021){
  dir_fil <- paste0("C:/Users/SmithAC/OneDrive - EC-EC/Harvest Survey A146/",y,"/Files for analysis/hqs.",y)
  }else{
    dir_fil <- paste0("C:/Users/SmithAC/OneDrive - EC-EC/Harvest Survey A146/",y,"/original data files/hqs.",y)
    
  }
  starts <- c(13,17,23,25,36,42,45,48,51,57,60,63,66,69,72,75,78,90,96,98,102,107,109,113,133,135,137,139,148,151,153,155,159)
  ends <- c(16,22,23,25,37,44,47,50,53,59,62,65,68,71,74,77,80,91,97,101,106,108,112,117,134,136,138,140,150,152,154,158,163)
  ends-starts
  clnm <- c("SELYEAR","PERMIT","CASTE","POTNTL","PRHUNT","DAYWF","DAYOT","TODUK","TOGOK","COOTK",
            "WOODK","SNIPK","DOVEK","PIGEK","CRANK","RAILK","MURRK","PRHUNTG","ZOHUNT","LATD","LOND",
            "ZOHUNTG","LATG","LONG","PRSALE","ZOSALE","PRSAMP","ZOSAMP","DAYM","PRHUNTM","ZOHUNTM","LATM","LONM")
  cltyp <- c(rep("i",2),
             "c",
             "c",
             rep("i",29))
  tmp <- read_fwf(dir_fil,
                  col_positions = fwf_positions(start = starts,
                                                end = ends,
                                                col_names = clnm),
                  col_types = cltyp)

  hqs_all <- bind_rows(hqs_all,tmp)  

}

table(hqs_all$CASTE)

