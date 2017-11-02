### Getting rid of out of service
baltimore_edit2 <- baltimore_edit %>% 
  filter(Priority != "Out of Service") %>% 
  filter(Year != 2017) %>% 
  filter(Priority != "Emergency")


### Looking at baltimore_edit
head(baltimore_edit)

### Looping
unique(baltimore_edit$Year)
unique(baltimore_edit$District)
unique(baltimore_edit$Week)

for (j in 1:length(unique(baltimore_edit2$Week))){
    for (k in 1:length(unique(baltimore_edit2$District))){
      
      df <- baltimore_edit2 %>% 
        filter(Year == 2015 & 
               Week == unique(baltimore_edit2$Week)[j] &
               District == unique(baltimore_edit2$District)[k])
      
      if (nrow(df) == 5) {
        assign(paste(unique(baltimore_edit2$District)[k], as.character(unique(baltimore_edit2$Week)[j]), "2015", sep = "_"),
               smooth.spline(df$prior_num, df$Proportion))
      }
    }
}

for (j in 1:length(unique(baltimore_edit2$Week))){
  for (k in 1:length(unique(baltimore_edit2$District))){
    
    df <- baltimore_edit2 %>% 
      filter(Year == 2016 & 
               Week == unique(baltimore_edit2$Week)[j] &
               District == unique(baltimore_edit2$District)[k])
    
    if (nrow(df) == 5) {
      assign(paste(unique(baltimore_edit2$District)[k], as.character(unique(baltimore_edit2$Week)[j]), "2016", sep = "_"),
             smooth.spline(df$prior_num, df$Proportion))
    }
  }
}

df <- baltimore_edit2 %>% 
  filter(Year == 2016 & 
           Week == unique(baltimore_edit2$Week)[24] &
           District == unique(baltimore_edit2$District)[1])



######### Extracting out ACF ##########

acf_weights_nonemergency <- NULL
acf_weights_low <- NULL
acf_weights_medium <- NULL
acf_weights_high <- NULL
acf_weights_veryhigh <- NULL

for (i in 1:length(unique(baltimore_edit2$Year))){
  for (k in 1:length(unique(baltimore_edit2$District))){
      
        test1 <- baltimore_edit2 %>% 
          filter(District == unique(baltimore_edit2$District)[k] 
                 & Year == unique(baltimore_edit2$Year)[i] & Priority == "Low") 
        
        test2 <- baltimore_edit2  %>% 
          filter(District == unique(baltimore_edit2$District)[k] 
                 & Year == unique(baltimore_edit2$Year)[i] & Priority == "Medium") 
        
        test3 <- baltimore_edit2 %>% 
          filter(District == unique(baltimore_edit2$District)[k] 
                 & Year == unique(baltimore_edit2$Year)[i] & Priority == "High") 
        
        test4 <- baltimore_edit2  %>% 
          filter(District == unique(baltimore_edit2$District)[k] 
                 & Year == unique(baltimore_edit2$Year)[i] & Priority == "Very High") 
        
        test5 <- baltimore_edit2  %>% 
          filter(District == unique(baltimore_edit2$District)[k] 
                 & Year == unique(baltimore_edit2$Year)[i] & Priority == "Non-Emergency") 
      
        if (nrow(test5) == 53){
        
          test5 <- test5[-(25:53),]
          
          test5 <- test5[seq(dim(test5)[1],1),]  
          
        assign(paste("acf_weights_nonemergency",
                     unique(baltimore_edit2$District)[k], 
                     as.character(unique(baltimore_edit2$Year)[i]), sep = "_"), acf(test5$Proportion)$acf)
        }
        
        if (nrow(test1) == 53) {
          
          test1 <- test1[-(25:53),]
          
          test1 <- test1[seq(dim(test1)[1],1),]
          
          assign(paste("acf_weights_low",
                       unique(baltimore_edit2$District)[k], 
                       as.character(unique(baltimore_edit2$Year)[i]), sep = "_"), acf(test1$Proportion)$acf)
        }
        
        if (nrow(test2) == 53) { 
          
          test2 <- test2[-(25:53),]
          
          test2 <- test2[seq(dim(test2)[1],1),]
          
          assign(paste("acf_weights_medium",
                       unique(baltimore_edit2$District)[k], 
                       as.character(unique(baltimore_edit2$Year)[i]), sep = "_"), acf(test2$Proportion)$acf)
        }
        
        if (nrow(test3) == 53) {
          
          test3 <- test3[-(25:53),]
          
          test3 <- test3[seq(dim(test3)[1],1),]
          
          assign(paste("acf_weights_high",
                       unique(baltimore_edit2$District)[k], 
                       as.character(unique(baltimore_edit2$Year)[i]), sep = "_"), acf(test3$Proportion)$acf)
        }
        
        if (nrow(test4) == 53) {
          
          test4 <- test4[-(25:53),]
          
          test4 <- test4[seq(dim(test4)[1],1),]
          
          assign(paste("acf_weights_veryhigh",
                       unique(baltimore_edit2$District)[k], 
                       as.character(unique(baltimore_edit2$Year)[i]), sep = "_"), acf(test4$Proportion)$acf)
        }
        
  }
}

acf_weights_SE_2016 <- data.frame(cbind(ne = acf_weights_nonemergency_SE_2016,
                                        low = acf_weights_low_SE_2016,
                                        medium = acf_weights_medium_SE_2016,
                                        high = acf_weights_high_SE_2016,
                                        very_high = acf_weights_veryhigh_SE_2016
))



acf_weights_SE_2016


acf(test5$Proportion)$acf

###### Function that creates the weighted results #######


CD_12_2015$y*c(acf_weights_high_CD_2015)


u <- c(1, 2, 3)
v <- c(1, 2, 3)                    

u*v

test1 <- baltimore_edit2 %>% 
  filter(District == unique(baltimore_edit2$District)[3] 
         & Year == unique(baltimore_edit2$Year)[1] & Priority == "Low")

nrow(test1)

test1 <- test1[-(26:53),]


test1 <- test1[seq(dim(test1)[1],1),]

?arrange

acf(test1$Proportion)

kkkk <- na.omit(test5$Proportion)

?acf


