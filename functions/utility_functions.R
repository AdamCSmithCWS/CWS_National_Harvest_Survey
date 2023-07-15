##3 utility functions

jags_dim <- function(dim = 1,
                     var = "",
                     cl = "Parameter",
                     dat = NULL){
  ##3 function to extract the indicator value of a multi-dimension jagsUI summary table
  require(stringr)
  

  pat = paste0("(?<=",var,"\\[")
  
  if(dim > 1){
  for(j in 1:(dim-1)){
      
    pat2 = paste0(pat,")[:digit:]+")
    cl2 = str_extract(dat[,cl],pattern = pat2)
    
    d = max(nchar(cl2))
  
    pat = paste0(pat,"[:digit:]{1,",d,"}[:punct:]")
  }
  }
 
  
  pat = paste0(pat,")[:digit:]+")
  dds = as.integer(str_extract(dat[,cl],pattern = pat))
  return(dds)
  
}





jags_dim_tidy <- function(dim = 1,
                          x = variable){
  ##3 function to extract the indicator value of a multi-dimension indicator variable name
  require(stringr)
  
 
  if(dim == 1){
    pat <- stringr::str_replace_all(stringr::str_extract(x, "^\\w+\\[[:digit:]{1,}[:punct:]"),
                                "^\\w+\\[|[[:punct:]]",
                                replacement = "")
  }
  if(dim == 2){
    pat <- stringr::str_replace_all(stringr::str_extract(x, "^\\w+\\[[:digit:]{1,}[:punct:][:digit:]{1,}[:punct:]"),
                                    "^\\w+\\[[:digit:]{1,}[:punct:]|[[:punct:]]",
                                    replacement = "")
  }
  if(dim == 3){
    pat <- stringr::str_replace_all(stringr::str_extract(x, "^\\w+\\[[:digit:]{1,}[:punct:][:digit:]{1,}[:punct:][:digit:]{1,}[:punct:]"),
                                    "^\\w+\\[[:digit:]{1,}[:punct:][:digit:]{1,}[:punct:]|[[:punct:]]",
                                    replacement = "")
  }
  
  pat <- as.integer(pat)
return(pat)
}

