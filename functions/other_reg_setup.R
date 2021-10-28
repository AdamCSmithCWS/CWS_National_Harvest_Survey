other_reg_setup <- function(oth = others,regs_o = regs_other)
{for(spgp in oth){ 
  tmp <- regs_o[[spgp]][,c("YEAR",pr)]
  tmp[which(tmp[,pr] > 0),pr] <- 1
  names(tmp) <- c("YEAR",spgp)
  if(spgp == oth[[1]]){
    regs <- tmp
  }else{
    regs <- merge(regs,tmp,by = "YEAR")
  }
  
}
  
  if(max(regs$YEAR) < Y){
    for(YN in c((max(regs$YEAR)+1):Y)){
      regs[nrow(regs)+1,] <- regs[nrow(regs),]
      regs[nrow(regs),"YEAR"] <- YN
    }
  }
  
  regs <- regs[which(regs$YEAR >= FY),]
  regs <- regs[,which(colSums(regs) > 0)]
  grps <- names(regs)[-1] #the -1 removes the column called
  ngroups <- length(grps) #to enter model as data
  if("SNIPK" %in% grps){
    regs[which(regs$YEAR < 1992), "SNIPK"] <- 0
  }### remove Snipe hunt pre 1991
  if("MURRK" %in% grps){
    # regs[which(regs$YEAR >2012 | regs$YEAR < 2001), "MURRK"] <- 0
    
    regs[, "MURRK"] <- 0
  }### remove Murre hunts need to reconcile historical database
  ret = list(ngroups = ngroups,regs = regs,grps = grps)
  return(ret)
}
