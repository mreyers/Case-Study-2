balt_grouped32015 <- balt3 %>% 
  filter(Year == 2015) %>% 
  group_by(District, Week, Priority) %>% 
  summarise(number = n()) %>% 
  filter(Priority != "") %>% 
  group_by(District, Week) %>% 
  mutate(Proportion = number/sum(number)) %>% 
  ungroup(District, Week)  %>% 
  mutate(Year = 2015)

balt_grouped32016 <- balt3 %>% 
  filter(Year == 2016) %>% 
  group_by(District, Week, Priority) %>% 
  summarise(number = n()) %>% 
  filter(Priority != "") %>% 
  group_by(District, Week) %>% 
  mutate(Proportion = number/sum(number)) %>% 
  ungroup(District, Week) %>% 
  mutate(Year = 2016)

balt_grouped32017 <- balt3 %>% 
  filter(Year == 2017) %>% 
  group_by(District, Week, Priority) %>% 
  summarise(number = n()) %>% 
  filter(Priority != "") %>% 
  group_by(District, Week) %>% 
  mutate(Proportion = number/sum(number)) %>% 
  ungroup(District, Week) %>% 
  mutate(Year = 2017)

baltimore_edit <- data.frame(rbind(balt_grouped32015, balt_grouped32016, 
                                      balt_grouped32017))

baltimore_edit$Year <- as.factor(baltimore_edit$Year)


# Let's take a look at the proportion of each crime
baltimore_edit %>% filter(District == "CD") %>% 
  filter(Year == "2015") %>% 
  ggplot(aes(x = Week, y = number, color = Priority)) +
  geom_point()

write.csv(baltimore_edit, "baltimore_proportion.csv")

baltimore_proportion

unique(baltimore_edit$Priority)

baltimore_edit$prior_num[baltimore_edit$Priority == "Non-Emergency"] = 0
baltimore_edit$prior_num[baltimore_edit$Priority == "Low"] = 1
baltimore_edit$prior_num[baltimore_edit$Priority == "Medium"] = 2
baltimore_edit$prior_num[baltimore_edit$Priority == "High"] = 3
baltimore_edit$prior_num[baltimore_edit$Priority == "Very High"] = 4
baltimore_edit$prior_num[baltimore_edit$Priority == "Emergency"] = 5


baltimore_edit %>% 
  filter(District == "WD" & Year == 2016) %>% 
  filter(Priority != "Out of Service") %>% 
  ggplot(aes(x = Proportion, y = Week, color = Priority, group = Week)) + 
  geom_line(aes(color = Priority)) + geom_point() + 
  ggtitle("Proportion of Weekly Crimes in District WD Organized by Priority")

wd_test_week19 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2015 & Week == "19") %>% 
  filter(Priority != "Out of Service") 

wd_test_week20 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2015 & Week == "20") %>% 
  filter(Priority != "Out of Service") 

wd_test_week21 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2015 & Week == "21") %>% 
  filter(Priority != "Out of Service") 

wd_test_week12 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2015 & Week == "12") %>% 
  filter(Priority != "Out of Service") 

wd_test_week21
wd_test_week20

splines <- NULL

unique(baltimore_edit$Year)
unique(baltimore_edit$District)
unique(baltimore_edit$Week)

spline_1 <- smooth.spline(wd_test_week20$prior_num, wd_test_week20$Proportion)
spline_2 <- smooth.spline(wd_test_week19$prior_num, wd_test_week19$Proportion)

spline_1$y

newData <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2015 & Week == "20") %>% 
  select(prior_num)

newData2 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2015 & Week == "19") %>% 
  select(prior_num)


spline_1_pred <- predict(spline_1, newData)
spline_2_pred <- predict(spline_2, newData2)

ggplot(data = wd_test_week20, aes(x = wd_test_week20$prior_num, y = wd_test_week20$Proportion)) +
  geom_point(color = "red") + 
  geom_point(aes(x = spline_1_pred[1]$x, y = spline_1_pred[2]$y), color = "blue")

ggplot(data = wd_test_week19, aes(x = wd_test_week19$prior_num, y = wd_test_week19$Proportion)) +
  geom_point(color = "red") + 
  geom_point(aes(x = spline_2_pred[1]$x, y = spline_2_pred[2]$y), color = "blue")


test1 <- baltimore_edit2 %>% 
  filter(District == "NE" & Year == 2016 & Priority == "Low")

test1 <- test1[-(25:53),]

test1 <- test1[seq(dim(test1)[1],1),]

test2 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2016 & Priority == "Medium") 

test2 <- test2[-(25:53),]

test2 <- test2[seq(dim(test2)[1],1),]

test3 <- baltimore_edit2 %>% 
  filter(District == "NE" & Year == 2016 & Priority == "High") 

test3 <- test3[-(25:53),]

test3 <- test3[seq(dim(test3)[1],1),]

test4 <- baltimore_edit2 %>% 
  filter(District == "NE" & Year == 2016 & Priority == "Very High") 

test4 <- test4[-(25:53),]

test4 <- test4[seq(dim(test4)[1],1),]

test5 <- baltimore_edit2 %>% 
  filter(District == "NE" & Year == 2016 & Priority == "Non-Emergency") 

test5 <- test5[-(25:53),]

test5 <- test5[seq(dim(test5)[1],1),]

acf_test <- acf(test1$Proportion)
acf_test2 <- acf(test2$Proportion, title("Autocorrelation Plot for District WD, Priority: Medium"))
?acf

acf_test3 <- acf(test3$Proportion)
acf_test4 <- acf(test4$Proportion)
acf_test5 <- acf(test5$Proportion)




acf_test$acf

acf_test2 <- acf(test2$Proportion)

acf_test2$acf

acf_test3 <- acf(test3$Proportion)
acf_test4 <- acf(test4$Proportion)
acf_test5 <- acf(test5$Proportion)
