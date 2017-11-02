##### Predictions ######

CD_26_2015$y

CD_errors

# lag 4

u1 <- CD_25_2016$y
u2 <- CD_24_2016$y
u3 <- CD_23_2016$y
u4 <- CD_22_2016$y
u5 <- CD_21_2016$y

u_pred <- (u1 + u2 + u3 + u4 + u5)/5
u_pred

testWD_low <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2016 & Week == 26) %>% 
  filter(Priority != "Emergency")

testWD_25 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2016 & Week == 25) %>% 
  filter(Priority != "Emergency")

testWD_24 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2016 & Week == 24) %>% 
  filter(Priority != "Emergency")

testWD_23 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2016 & Week == 23) %>% 
  filter(Priority != "Emergency")

testWD_22 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2016 & Week == 22) %>% 
  filter(Priority != "Emergency")

testWD_21 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2016 & Week == 21) %>% 
  filter(Priority != "Emergency")

testWD_20 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2016 & Week == 20) %>% 
  filter(Priority != "Emergency")

testWD_19 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2016 & Week == 19) %>% 
  filter(Priority != "Emergency")

testWD_18 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2016 & Week == 18) %>% 
  filter(Priority != "Emergency")

testWD_17 <- baltimore_edit %>% 
  filter(District == "WD" & Year == 2016 & Week == 17) %>% 
  filter(Priority != "Emergency")

w1 <- testWD_25$Proportion
w2 <- testWD_24$Proportion
w3 <- testWD_23$Proportion
w4 <- testWD_22$Proportion
w5 <- testWD_21$Proportion
w6 <- testWD_20$Proportion
w7 <- testWD_19$Proportion
w8 <- testWD_18$Proportion
w9 <- testWD_17$Proportion


w_pred <- (w1 + w2 + w3 + w4 + w5)/5

w_pred2 <- (w1 + w2 + w3 + w4 + w5 + w6 + w7 + w8 + w9)/9

w_pred1_test <- mean((w_pred - testWD_low$Proportion)^2)

w_pred2_test <- mean((w_pred2 - testWD_low$Proportion)^2)

w_pred1_test
w_pred2_test
