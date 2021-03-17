### 


general_plot_a<- function(dat = a_tab,
                        startYear = FY,
                        endYear = Y,
                        lang = "En",
                        type = "A"){
  
  require(ggforce)
  
  
  if(type == "A"){
     dat <- filter(dat,
                  year >= startYear & year <= endYear) %>% 
       ungroup()
     if(lang == "En"){
       lvl = unique(dat$Prov_En)
       dat <- dat %>% mutate(V = Description_En,
                             reg = factor(Prov_En,ordered = TRUE, levels = lvl))
     }else{
       lvl = unique(dat$Prov_Fr)
       dat <- dat %>% mutate(V = Description_Fr,
                             reg = factor(Prov_Fr,ordered = TRUE, levels = lvl))

     }
       
  }else{
    dat <- filter(dat,
                  year >= startYear & year <= endYear) %>% 
      mutate(full_species = ifelse(is.na(Age) & is.na(Sex),TRUE,FALSE),
             Age_only = ifelse(!is.na(Age) & is.na(Sex),TRUE,FALSE),
             Sex_only = ifelse(is.na(Age) & !is.na(Sex),TRUE,FALSE),
             Age_Sex = ifelse(!is.na(Age) & !is.na(Sex),TRUE,FALSE))
    if(lang == "En"){
      dat <- dat %>% rename(V = species)
    }else{
      dat <- dat %>% rename(V = espece)
    }
  }
     
 
source("Functions/palettes.R")  

npg = length(unique(dat$V))*2


  outgg = vector("list",length = npg)
  i = 1
      for(vv in unique(dat$V)){
        tmp = dat %>% filter(V == vv,
                             is.na(zone))
        outgg[[i]] = ggplot(data = tmp,aes(x = year,y = mean))+
          geom_point(col = my_single_col,size = 1)+
        geom_errorbar(aes(ymin = lci,ymax = uci),alpha = 0.5,col = my_single_col,width = 0)+
        geom_line(alpha = 0.3,col = my_single_col)+
        ylab(vv)+
          xlab("")+
        labs(title = paste(vv,"Canada - Province"))+
        #geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
        scale_y_continuous(limits = c(0,NA),labels = scales::comma)+
        # geom_text_repel(data = nwing_lab,aes(x = year,y = mean,label = np),
        #                 inherit.aes = FALSE,nudge_y = max(dat$mean)*-0.03,nudge_x = xndg,
        #                 colour = grey(0.7),min.segment.length = 0,size = 3)+
        theme_bw()+
        # theme(legend.position = "right",
        #       title = element_text(size = 9))+
        coord_cartesian(xlim = c(startYear, endYear))+
          facet_wrap(~ reg,scales = "free",ncol = 4,nrow = 4)
        
        i = i+1
        
        tmp = dat %>% filter(V == vv,
                             !is.na(zone))
        outgg[[i]] = ggplot(data = tmp,aes(x = year,y = mean))+
          geom_point(col = my_single_col,size = 1)+
          geom_errorbar(aes(ymin = lci,ymax = uci),alpha = 0.5,col = my_single_col,width = 0)+
          geom_line(alpha = 0.3,col = my_single_col)+
          ylab(vv)+
          xlab("")+
          labs(title = paste(vv,"Zone"))+
          #geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
          scale_y_continuous(limits = c(0,NA),labels = scales::comma)+
          # geom_text_repel(data = nwing_lab,aes(x = year,y = mean,label = np),
          #                 inherit.aes = FALSE,nudge_y = max(dat$mean)*-0.03,nudge_x = xndg,
          #                 colour = grey(0.7),min.segment.length = 0,size = 3)+
          theme_bw()+
          # theme(legend.position = "right",
          #       title = element_text(size = 9))+
          coord_cartesian(xlim = c(startYear, endYear))+
          facet_wrap(~ reg+zone,scales = "free",ncol = 5,nrow = 5)
        
        i = i+1
      
      }

  
  return(outgg)
}


