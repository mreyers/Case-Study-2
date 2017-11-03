##### Predictions ######

CD_26_2015$y

CD_errors

# 5 Lags
stand_func5 <- function(x) {
  x <- x[1:6]
  sum_col <- sum(abs(x[-1]))
  x = abs(x)/sum_col
  return(x)
}

# 4 Lags
stand_func4 <- function(x) {
  x <- x[1:5]
  sum_col <- sum(abs(x[-1]))
  x = abs(x)/sum_col
  return(x)
}

# 6 Lags
stand_func6 <- function(x) {
  x <- x[1:7]
  sum_col <- sum(abs(x[-1]))
  x = abs(x)/sum_col
  return(x)
}

acf_weights_CD_2016t <- as.data.frame(do.call(cbind, 
                                             map(acf_weights_CD_2016i, stand_func5)))

acf_weights_ED_2016t <- as.data.frame(do.call(cbind, 
                                             map(acf_weights_ED_2016i, stand_func6)))

acf_weights_ND_2016t <- as.data.frame(do.call(cbind, 
                                             map(acf_weights_ND_2016i, stand_func6)))

acf_weights_NE_2016t <- as.data.frame(do.call(cbind, 
                                             map(acf_weights_NE_2016i, stand_func6)))

acf_weights_NW_2016t <- as.data.frame(do.call(cbind, 
                                             map(acf_weights_NW_2016i, stand_func4)))

acf_weights_SD_2016t <- as.data.frame(do.call(cbind, 
                                             map(acf_weights_SD_2016i, stand_func4)))

acf_weights_SE_2016t <- as.data.frame(do.call(cbind, 
                                             map(acf_weights_SE_2016i, stand_func5)))

acf_weights_SW_2016t <- as.data.frame(do.call(cbind, 
                                             map(acf_weights_SW_2016i, stand_func4)))

acf_weights_WD_2016t <- as.data.frame(do.call(cbind, 
                                             map(acf_weights_WD_2016i, stand_func4)))


############ CD PREDICTIONS ###############

# Optimal Lag = 5
CD_errors

# Getting Predictions with 5 lags
u1 <- CD_25_2016$y*acf_weights_CD_2016t[2,]
u2 <- CD_24_2016$y*acf_weights_CD_2016t[3,]
u3 <- CD_23_2016$y*acf_weights_CD_2016t[4,]
u4 <- CD_22_2016$y*acf_weights_CD_2016t[5,]
u5 <- CD_21_2016$y*acf_weights_CD_2016t[6,]

CD_pred <- u1 + u2 + u3 + u4 + u5
CD_pred

# Actual Results
CD_26_2016$y

# Mean Squared Error
CD_pred_test <- mean((CD_pred - CD_26_2016$y)^2)
CD_pred_test

############ ED PREDICTIONS ###############

# Optimal Lag = 6
ED_errors

# Getting Predictions with 5 lags
u1 <- ED_25_2016$y*acf_weights_ED_2016t[2,]
u2 <- ED_24_2016$y*acf_weights_ED_2016t[3,]
u3 <- ED_23_2016$y*acf_weights_ED_2016t[4,]
u4 <- ED_22_2016$y*acf_weights_ED_2016t[5,]
u5 <- ED_21_2016$y*acf_weights_ED_2016t[6,]
u6 <- ED_20_2016$y*acf_weights_ED_2016t[7,]

ED_pred <- u1 + u2 + u3 + u4 + u5 + u6
ED_pred

# Actual Results
ED_26_2016$y

# Mean Squared Error
ED_pred_test <- mean((ED_pred - ED_26_2016$y)^2)
ED_pred_test

############ ND PREDICTIONS ###############

# Optimal Lag = 6
ND_errors

# Getting Predictions with 5 lags
u1 <- ND_25_2016$y*acf_weights_ND_2016t[2,]
u2 <- ND_24_2016$y*acf_weights_ND_2016t[3,]
u3 <- ND_23_2016$y*acf_weights_ND_2016t[4,]
u4 <- ND_22_2016$y*acf_weights_ND_2016t[5,]
u5 <- ND_21_2016$y*acf_weights_ND_2016t[6,]
u6 <- ND_20_2016$y*acf_weights_ND_2016t[7,]

ND_pred <- u1 + u2 + u3 + u4 + u5 + u6
ND_pred

# Actual Results
ND_26_2016$y

# Mean Squared Error
ND_pred_test <- mean((ND_pred - ND_26_2016$y)^2)
ND_pred_test

############ NE PREDICTIONS ###############

# Optimal Lag = 6
NE_errors

# Getting Predictions with 5 lags
u1 <- NE_25_2016$y*acf_weights_NE_2016t[2,]
u2 <- NE_24_2016$y*acf_weights_NE_2016t[3,]
u3 <- NE_23_2016$y*acf_weights_NE_2016t[4,]
u4 <- NE_22_2016$y*acf_weights_NE_2016t[5,]
u5 <- NE_21_2016$y*acf_weights_NE_2016t[6,]
u6 <- NE_20_2016$y*acf_weights_NE_2016t[7,]

NE_pred <- u1 + u2 + u3 + u4 + u5 + u6
NE_pred

# Actual Results
NE_26_2016$y

# Mean Squared Error
NE_pred_test <- mean((NE_pred - NE_26_2016$y)^2)
NE_pred_test

############ NW PREDICTIONS ###############

# Optimal Lag = 4
NW_errors

# Getting Predictions with 5 lags
u1 <- NW_25_2016$y*acf_weights_NW_2016t[2,]
u2 <- NW_24_2016$y*acf_weights_NW_2016t[3,]
u3 <- NW_23_2016$y*acf_weights_NW_2016t[4,]
u4 <- NW_22_2016$y*acf_weights_NW_2016t[5,]

NW_pred <- u1 + u2 + u3 + u4
NW_pred

# Actual Results
NW_26_2016$y

# Mean Squared Error
NW_pred_test <- mean((NW_pred - NW_26_2016$y)^2)
NW_pred_test

############ SD PREDICTIONS ###############

# Optimal Lag = 4
SD_errors

# Getting Predictions with 5 lags
u1 <- SD_25_2016$y*acf_weights_SD_2016t[2,]
u2 <- SD_24_2016$y*acf_weights_SD_2016t[3,]
u3 <- SD_23_2016$y*acf_weights_SD_2016t[4,]
u4 <- SD_22_2016$y*acf_weights_SD_2016t[5,]


SD_pred <- u1 + u2 + u3 + u4
SD_pred

# Actual Results
SD_26_2016$y

# Mean Squared Error
SD_pred_test <- mean((SD_pred - SD_26_2016$y)^2)
SD_pred_test

############ SE PREDICTIONS ###############

# Optimal Lag = 5
SE_errors

# Getting Predictions with 5 lags
u1 <- SE_25_2016$y*acf_weights_SE_2016t[2,]
u2 <- SE_24_2016$y*acf_weights_SE_2016t[3,]
u3 <- SE_23_2016$y*acf_weights_SE_2016t[4,]
u4 <- SE_22_2016$y*acf_weights_SE_2016t[5,]
u5 <- SE_21_2016$y*acf_weights_SE_2016t[6,]

SE_pred <- u1 + u2 + u3 + u4 + u5
SE_pred

# Actual Results
SE_26_2016$y

# Mean Squared Error
SE_pred_test <- mean((SE_pred - SE_26_2016$y)^2)
SE_pred_test

############ SW PREDICTIONS ###############

# Optimal Lag = 4
SW_errors

# Getting Predictions with 5 lags
u1 <- SW_25_2016$y*acf_weights_SW_2016t[2,]
u2 <- SW_24_2016$y*acf_weights_SW_2016t[3,]
u3 <- SW_23_2016$y*acf_weights_SW_2016t[4,]
u4 <- SW_22_2016$y*acf_weights_SW_2016t[5,]

SW_pred <- u1 + u2 + u3 + u4 
SW_pred

# Actual Results
SW_26_2016$y

# Mean Squared Error
SW_pred_test <- mean((SW_pred - SW_26_2016$y)^2)
SW_pred_test

############ WD PREDICTIONS ###############

# Optimal Lag = 4
WD_errors

# Getting Predictions with 4 lags
u1 <- WD_25_2016$y*acf_weights_WD_2016t[2,]
u2 <- WD_24_2016$y*acf_weights_WD_2016t[3,]
u3 <- WD_23_2016$y*acf_weights_WD_2016t[4,]
u4 <- WD_22_2016$y*acf_weights_WD_2016t[5,]

WD_pred <- u1 + u2 + u3 + u4
sum(WD_pred)

# Actual Results
WD_26_2016$y

# Mean Squared Error
WD_pred_test <- mean((WD_pred - WD_26_2016$y)^2)

