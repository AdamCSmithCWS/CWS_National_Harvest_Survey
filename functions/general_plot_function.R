### 

library(scales)
general_plot_a<- function(dat = a_tab,
                        startYear = FY,
                        endYear = Y,
                        lang = "En"){
  
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
          scale_x_continuous(breaks = pretty_breaks())+
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
          scale_x_continuous(breaks = pretty_breaks())+
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








general_plot_b<- function(dat = b_tab,
                          startYear = FY,
                          endYear = Y,
                          lang = "En",
                          type = "Age_Sex",
                          add_points = FALSE){
  

  
  

if(type == "Full"){
  dat <- filter(dat,
                year >= startYear & year <= endYear,
                is.na(Age) & is.na(Sex)) %>% 
    mutate(gr = "Species")
  lg_pos <- "none"
  add_points <- TRUE
}


if(type == "Age"){
  dat <- filter(dat,
                year >= startYear & year <= endYear,
                !is.na(Age) & is.na(Sex)) %>% 
    mutate(gr = Age)
  lg_pos <- "right"
}

if(type == "Sex"){
  dat <- filter(dat,
                year >= startYear & year <= endYear,
                is.na(Age) & !is.na(Sex)) %>% 
    mutate(gr = Sex)
  lg_pos <- "right"
}

if(type == "Age_Sex"){
  dat <- filter(dat,
                year >= startYear & year <= endYear,
                !is.na(Age) & !is.na(Sex)) %>% 
    mutate(gr = paste(Age,Sex,sep = "-")) ## define a generic group to control colour and fill in the plots - factor with all possible grouping levels
  lg_pos <- "bottom"
  
  }

  
  

    if(lang == "En"){
      lvl = unique(dat$Prov_En)
      dat <- dat %>% mutate(V = species,
                            reg = factor(Prov_En,ordered = TRUE, levels = lvl))
      
      
    }else{
      lvl = unique(dat$Prov_Fr)
      dat <- dat %>% mutate(V = espece,
                            reg = factor(Prov_Fr,ordered = TRUE, levels = lvl))
      
    }
  
  
  
  #source("Functions/palettes.R")  
  
  npg = length(unique(dat$V))*2
  
  
  outgg = vector("list",length = npg)
  i = 1
  for(vv in unique(dat$V)){
    tmp = dat %>% filter(V == vv,
                         is.na(zone))
    if(add_points){
      pp <- geom_point(aes(colour = gr),size = 1)
      ppe <- geom_errorbar(aes(ymin = lci,ymax = uci,fill = gr),alpha = 0.5,width = 0)
      ppl <- geom_line(alpha = 0.3,aes(colour = gr))
    }else{
      pp <- geom_point(aes(colour = gr),size = 1,alpha = 0)
      ppe <- geom_ribbon(aes(ymin = lci,ymax = uci,fill = gr),alpha = 0.3)
      ppl <- geom_line(alpha = 1,aes(colour = gr))
    }
    outgg[[i]] = ggplot(data = tmp,aes(x = year,y = mean))+
      pp+
      ppe+
      ppl+
      ylab(vv)+
      xlab("")+
      labs(title = paste(vv,"Canada - Province"))+
      scale_colour_manual(values = my_col_b, aesthetics = c("colour","fill"))+
      #geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
      scale_y_continuous(limits = c(0,NA),labels = scales::comma)+
      scale_x_continuous(breaks = pretty_breaks())+
      # geom_text_repel(data = nwing_lab,aes(x = year,y = mean,label = np),
      #                 inherit.aes = FALSE,nudge_y = max(dat$mean)*-0.03,nudge_x = xndg,
      #                 colour = grey(0.7),min.segment.length = 0,size = 3)+
      theme_bw()+
      theme(legend.position = lg_pos,
            title = element_text(size = 9))+
      coord_cartesian(xlim = c(startYear, endYear))+
      facet_wrap(~ reg,scales = "free",ncol = 4,nrow = 4)
    
    i = i+1
    
    tmp = dat %>% filter(V == vv,
                         !is.na(zone))
    outgg[[i]] = ggplot(data = tmp,aes(x = year,y = mean))+
      pp+
      ppe+
      ppl+
      ylab(vv)+
      xlab("")+
      labs(title = paste(vv,"Canada - Province"))+
      scale_colour_manual(values = my_col_b, aesthetics = c("colour","fill"))+
      #geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
      scale_y_continuous(limits = c(0,NA),labels = scales::comma)+
      scale_x_continuous(breaks = pretty_breaks())+
      # geom_text_repel(data = nwing_lab,aes(x = year,y = mean,label = np),
      #                 inherit.aes = FALSE,nudge_y = max(dat$mean)*-0.03,nudge_x = xndg,
      #                 colour = grey(0.7),min.segment.length = 0,size = 3)+
      theme_bw()+
      theme(legend.position = lg_pos,
            title = element_text(size = 9))+
      coord_cartesian(xlim = c(startYear, endYear))+
      facet_wrap(~ reg+zone,scales = "free",ncol = 5,nrow = 5)
    
    i = i+1
    
  }
  
  
  return(outgg)
}







general_plot_c<- function(dat = c_tab,
                          startYear = FY,
                          endYear = Y,
                          lang = "En"){
  
  
  
  
  

    dat <- filter(dat,
                  year >= startYear & year <= endYear) %>% 
      mutate(gr = "Species")
    lg_pos <- "none"

  

  
  
  if(lang == "En"){
    lvl = unique(dat$Prov_En)
    dat <- dat %>% mutate(V = species,
                          reg = factor(Prov_En,ordered = TRUE, levels = lvl))
    
    
  }else{
    lvl = unique(dat$Prov_Fr)
    dat <- dat %>% mutate(V = espece,
                          reg = factor(Prov_Fr,ordered = TRUE, levels = lvl))
    
  }
  
  
  
  #source("Functions/palettes.R")  
  
  npg = length(unique(dat$V))*2
  
  
  outgg = vector("list",length = npg)
  i = 1
  for(vv in unique(dat$V)){
    tmp = dat %>% filter(V == vv,
                         is.na(zone))
      pp <- geom_point(aes(colour = gr),size = 1)
      ppe <- geom_errorbar(aes(ymin = lci,ymax = uci,fill = gr),alpha = 0.5,width = 0)
      ppl <- geom_line(alpha = 0.3,aes(colour = gr))

    outgg[[i]] = ggplot(data = tmp,aes(x = year,y = mean))+
      geom_abline(slope = 0,intercept = 1,colour = grey(0.5),alpha = 0.5)+
      pp+
      ppe+
      ppl+
      ylab(vv)+
      xlab("")+
      labs(title = paste(vv,"Canada - Province"))+
      scale_colour_manual(values = my_col_b, aesthetics = c("colour","fill"))+
      #geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
      scale_y_continuous(limits = c(0,NA),labels = scales::comma)+
      scale_x_continuous(breaks = pretty_breaks())+
      # geom_text_repel(data = nwing_lab,aes(x = year,y = mean,label = np),
      #                 inherit.aes = FALSE,nudge_y = max(dat$mean)*-0.03,nudge_x = xndg,
      #                 colour = grey(0.7),min.segment.length = 0,size = 3)+
      theme_bw()+
      theme(legend.position = lg_pos,
            title = element_text(size = 9))+
      coord_cartesian(xlim = c(startYear, endYear))+
      facet_wrap(~ reg,scales = "free",ncol = 4,nrow = 4)
    
    i = i+1
    
    tmp = dat %>% filter(V == vv,
                         !is.na(zone))
    outgg[[i]] = ggplot(data = tmp,aes(x = year,y = mean))+
      geom_abline(slope = 0,intercept = 1,colour = grey(0.5),alpha = 0.5)+
      pp+
      ppe+
      ppl+
      ylab(vv)+
      xlab("")+
      labs(title = paste(vv,"Canada - Province"))+
      scale_colour_manual(values = my_col_b, aesthetics = c("colour","fill"))+
      #geom_ribbon(aes(ymax = uci,ymin = lci),alpha = 0.2)+
      scale_y_continuous(limits = c(0,NA),labels = scales::comma)+
      scale_x_continuous(breaks = pretty_breaks())+
      # geom_text_repel(data = nwing_lab,aes(x = year,y = mean,label = np),
      #                 inherit.aes = FALSE,nudge_y = max(dat$mean)*-0.03,nudge_x = xndg,
      #                 colour = grey(0.7),min.segment.length = 0,size = 3)+
      theme_bw()+
      theme(legend.position = lg_pos,
            title = element_text(size = 9))+
      coord_cartesian(xlim = c(startYear, endYear))+
      facet_wrap(~ reg+zone,scales = "free",ncol = 5,nrow = 5)
    
    i = i+1
    
  }
  
  
  return(outgg)
}



