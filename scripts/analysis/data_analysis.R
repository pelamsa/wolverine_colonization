rm(list=ls())
gc()

library(glmmTMB)
library(tidyverse)
library(car)
library(dplyr)
library(performance)
library(sf)
library(DHARMa)
library(MuMIn)
library(PresenceAbsence)
library(MKclass)
library(caret)
library(spdep)
library(sp)

data <- read.csv("") # here paste your file name and path

#### RUN CANDIDATE MODELS - LOCAL SCALE ####
loc0 <- glmmTMB( 
  y_bin ~ 1 + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc1 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc2 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc3 <- glmmTMB( 
  y_bin ~ PD_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc4 <- glmmTMB( 
  y_bin ~ VPINE_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)


loc5 <- glmmTMB( 
  y_bin ~ VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)


loc6 <- glmmTMB( 
  y_bin ~ PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc7 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc8 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + PD_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc9 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + VPINE_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc10 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc11 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc12 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + PD_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc13 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + VPINE_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc14 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc15 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc16 <- glmmTMB( 
  y_bin ~ PD_3131s + VPINE_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc17 <- glmmTMB( 
  y_bin ~ PD_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc18 <- glmmTMB( 
  y_bin ~ PD_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc19 <- glmmTMB( 
  y_bin ~ VPINE_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc20 <- glmmTMB( 
  y_bin ~ VPINE_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc21 <- glmmTMB( 
  y_bin ~ VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc22 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + PD_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc23 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + VPINE_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc24 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc25 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc26 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + PD_3131s + VPINE_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc27 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + PD_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc28 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + PD_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc29 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + VPINE_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc30 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + VPINE_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc31 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc32 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + PD_3131s + VPINE_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc33 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + PD_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc34 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + PD_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc35 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + VPINE_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc36 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + VPINE_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc37 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc38 <- glmmTMB( 
  y_bin ~ PD_3131s + VPINE_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc39 <- glmmTMB( 
  y_bin ~ PD_3131s + VPINE_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc40 <- glmmTMB( 
  y_bin ~ PD_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc41 <- glmmTMB( 
  y_bin ~ VPINE_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc42 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + PD_3131s + VPINE_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc43 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + PD_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc44 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + PD_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc45 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + VPINE_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc46 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + VPINE_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc47 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc48 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + PD_3131s + VPINE_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc49 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + PD_3131s + VPINE_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc50 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + PD_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc51 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + VPINE_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc52 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + PD_3131s + VPINE_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc53 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + PD_3131s + VPINE_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc54 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + PD_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc55 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + VPINE_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc56 <- glmmTMB( 
  y_bin ~ PD_3131s + VPINE_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc57 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + PD_3131s + VPINE_3131s + VTOT_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc58 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + PD_3131s + VPINE_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)


loc59 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + PD_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)


loc60 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + VPINE_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc61 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + PD_3131s + VPINE_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc62 <- glmmTMB( 
  y_bin ~ dclcu_2021_3131s + PD_3131s + VPINE_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

loc63 <- glmmTMB( 
  y_bin ~ dclcu_0910_3131s + dclcu_2021_3131s + PD_3131s + VPINE_3131s + VTOT_3131s + PABL_3131s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

#### RUN CANDIDATE MODELS - LANDSCAPE SCALE ####
land0 <- glmmTMB( 
  y_bin ~ lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land1 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land2 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land3 <- glmmTMB( 
  y_bin ~ PD_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land4 <- glmmTMB( 
  y_bin ~ VPINE_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)


land5 <- glmmTMB( 
  y_bin ~ VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)


land6 <- glmmTMB( 
  y_bin ~ PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land7 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land8 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + PD_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land9 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + VPINE_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land10 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land11 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land12 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + PD_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land13 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + VPINE_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land14 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land15 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land16 <- glmmTMB( 
  y_bin ~ PD_20000s + VPINE_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land17 <- glmmTMB( 
  y_bin ~ PD_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land18 <- glmmTMB( 
  y_bin ~ PD_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land19 <- glmmTMB( 
  y_bin ~ VPINE_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land20 <- glmmTMB( 
  y_bin ~ VPINE_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land21 <- glmmTMB( 
  y_bin ~ VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land22 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + PD_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land23 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + VPINE_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land24 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land25 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land26 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + PD_20000s + VPINE_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land27 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + PD_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land28 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + PD_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land29 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + VPINE_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land30 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + VPINE_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land31 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land32 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + PD_20000s + VPINE_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land33 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + PD_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land34 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + PD_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land35 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + VPINE_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land36 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + VPINE_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land37 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land38 <- glmmTMB( 
  y_bin ~ PD_20000s + VPINE_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land39 <- glmmTMB( 
  y_bin ~ PD_20000s + VPINE_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land40 <- glmmTMB( 
  y_bin ~ PD_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land41 <- glmmTMB( 
  y_bin ~ VPINE_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land42 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + PD_20000s + VPINE_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land43 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + PD_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land44 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + PD_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land45 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + VPINE_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land46 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + VPINE_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land47 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land48 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + PD_20000s + VPINE_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land49 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + PD_20000s + VPINE_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land50 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + PD_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land51 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + VPINE_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land52 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + PD_20000s + VPINE_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land53 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + PD_20000s + VPINE_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land54 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + PD_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land55 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + VPINE_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land56 <- glmmTMB( 
  y_bin ~ PD_20000s + VPINE_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land57 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + PD_20000s + VPINE_20000s + VTOT_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

land58 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + PD_20000s + VPINE_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)


land59 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + PD_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)


land60 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + VPINE_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)


land61 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + PD_20000s + VPINE_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)


land62 <- glmmTMB( 
  y_bin ~ dclcu_2021_20000s + PD_20000s + VPINE_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)


land63 <- glmmTMB( 
  y_bin ~ dclcu_0910_20000s + dclcu_2021_20000s + PD_20000s + VPINE_20000s + VTOT_20000s + PABL_20000s + lnEffort + (1 | knum),
  data = data, 
  family = binomial(link = "logit"),
  na.action = na.fail,
)

#### CHECK MODEL ASSUMPTIONS - LOCAL SCALE, FULL MODEL ####
simulationOutput1 <- simulateResiduals(loc63)
plot(simulationOutput1, quantreg = F,)
plot(simulationOutput1)
check_convergence(loc63)
testDispersion(simulationOutput1)
testZeroInflation(simulationOutput1)
plotResiduals(simulationOutput1, data$lnEffort)

par(mfrow = c(3, 3))
plotResiduals(simulationOutput1, form = data$dclcu_0910_3131s)
plotResiduals(simulationOutput1, form = data$dclcu_2021_3131s)
plotResiduals(simulationOutput1, form = data$PD_3131s)
plotResiduals(simulationOutput1, form = data$VPINE_3131s)
plotResiduals(simulationOutput1, form = data$PABL_3131s)
plotResiduals(simulationOutput1, form = data$VTOT_3131s)
plotResiduals(simulationOutput1, form = data$lnEffort)
plotResiduals(simulationOutput1, form = data$knum)
par(mfrow = c(1, 1))

#### CHECK MODEL ASSUMPTIONS - LANDSCAPE SCALE, FULL MODEL ####
simulationOutput2 <- simulateResiduals(land63)
plot(simulationOutput2, quantreg = F,)
plot(simulationOutput2)
check_convergence(land63)
testDispersion(simulationOutput2)
testZeroInflation(simulationOutput2)
plotResiduals(simulationOutput2, data$lnEffort)

par(mfrow = c(3, 3))
plotResiduals(simulationOutput2, form = data$dclcu_0910_20000)
plotResiduals(simulationOutput2, form = data$dclcu_2021_20000)
plotResiduals(simulationOutput2, form = data$PD_20000)
plotResiduals(simulationOutput2, form = data$VPINE_20000)
plotResiduals(simulationOutput2, form = data$VTOT_20000)
plotResiduals(simulationOutput2, form = data$PABL_20000)
plotResiduals(simulationOutput2, form = data$knum)
par(mfrow = c(1, 1))

#### MODEL SELECTION - LOCAL SCALE ####
model_list1 <- list(loc0, loc1, loc2, loc3,loc4,loc5,loc6,loc7,loc8,loc9,loc10,
                    loc11,loc12,loc13,loc14,loc15,loc16,loc17,loc18,loc19,loc20,
                    loc21,loc22,loc23,loc24,loc25,loc26,loc27,loc28,loc29,loc30,
                    loc31,loc32,loc33,loc34,loc35,loc36,loc37,loc38,loc39,loc40,
                    loc41,loc42,loc43,loc44,loc45,loc46,loc47,loc48,loc49,loc50,
                    loc51,loc52,loc53,loc54,loc55,loc56,loc57,loc58,loc59,loc60,
                    loc61,loc62,loc63)
model_comparison1 <- model.sel(model_list1, rank = "AICc")
model_comparison1$ER <- 1 / model_comparison1$weight
model_comparison1_df <- as.data.frame(model_comparison1)
write.csv(model_comparison1_df, "") # here paste your file location

#### MODEL SELECTION - LANDSCAPE SCALE ####
model_list2 <- list(land0,land1, land2, land3,land4,land5,land6,land7,land8,land9,land10,
                    land11,land12,land13,land14,land15,land16,land17,land18,land19,land20,
                    land21,land22,land23,land24,land25,land26,land27,land28,land29,land30,
                    land31,land32,land33,land34,land35,land36,land37,land38,land39,land40,
                    land41,land42,land43,land44,land45,land46,land47,land48,land49,land50,
                    land51,land52,land53,land54,land55,land56,land57,land58,land59,land60,
                    land61,land62,land63)
model_comparison2 <- model.sel(model_list2, rank = "AICc")
model_comparison2$ER <- 1 / model_comparison2$weight
model_comparison2_df <- as.data.frame(model_comparison2)
write.csv(model_comparison2_df, "") # here paste your file location

#### CHECK MODEL ASSUMPTIONS - LOCAL SCALE ####
simulationOutput3 <- simulateResiduals(loc18)
plot(simulationOutput3, quantreg = F,)
plot(simulationOutput3)
check_convergence(loc18)
testDispersion(simulationOutput3)
testZeroInflation(simulationOutput3)
plotResiduals(simulationOutput3, data$lnEffort)

par(mfrow = c(3, 3))
plotResiduals(simulationOutput3, form = data$dclcu_0910_3131s)
plotResiduals(simulationOutput3, form = data$dclcu_2021_3131s)
plotResiduals(simulationOutput3, form = data$PD_3131s)
plotResiduals(simulationOutput3, form = data$VPINE_3131s)
plotResiduals(simulationOutput3, form = data$PABL_3131s)
plotResiduals(simulationOutput3, form = data$VTOT_3131s)
plotResiduals(simulationOutput3, form = data$lnEffort)
plotResiduals(simulationOutput3, form = data$knum)
par(mfrow = c(1, 1))

#### CHECK MODEL ASSUMPTIONS - LANDSCAPE SCALE ####
simulationOutput4 <- simulateResiduals(land59)
plot(simulationOutput4, quantreg = F,)
plot(simulationOutput4)
check_convergence(land59)
testDispersion(simulationOutput4)
testZeroInflation(simulationOutput4)
plotResiduals(simulationOutput4, data$lnEffort)

par(mfrow = c(3, 3))
plotResiduals(simulationOutput4, form = data$dclcu_0910_20000)
plotResiduals(simulationOutput4, form = data$dclcu_2021_20000)
plotResiduals(simulationOutput4, form = data$PD_20000)
plotResiduals(simulationOutput4, form = data$VPINE_20000)
plotResiduals(simulationOutput4, form = data$VTOT_20000)
plotResiduals(simulationOutput4, form = data$PABL_20000)
plotResiduals(simulationOutput4, form = data$knum)
par(mfrow = c(1, 1))


#### MODEL EVALUATION - LOCAL SCALE ####
# confusion matrix
eval_data <- data %>%
  select(knum, vuosi, effort, y_bin, dclcu_0910_3131s, dclcu_2021_3131s, PD_3131s, VPINE_3131s, VTOT_3131s, PABL_3131s, lnEffort)

# predicted probabilities from the model
loc_pd <- predict(loc18, re.form = NA, newdata = eval_data, type = "response")

# combine observed values and predicted probabilities in a data frame
loc_pd_res <- data.frame(obs = data$y_bin, pred = loc_pd)

# optCutoff to determine the optimal threshold
loc_oc <- MKclass::optCutoff(loc_pd_res$pred, truth = loc_pd_res$obs, namePos = 1)

# classify predictions as 0 (absence) or 1 (presence) based on the optimal threshold
class_pred1 <- ifelse(loc_pd_res$pred >= loc_oc[[1]], 1, 0)

class_pred1 <- as.factor(class_pred1)
data$y_bin <- as.factor(data$y_bin)
confmat1 <- confusionMatrix(data=class_pred1, reference = data$y_bin, positive = "1")
confmat1

# TSS
Sensitivity1 <- confmat1$byClass["Sensitivity"]
Specificity1 <- confmat1$byClass["Specificity"]

loc_tss <- Sensitivity1[[1]] + Specificity1[[1]] - 1
print(loc_tss)

# AUC
auc_data <- data.frame(
  plot_ID = data$knum,        
  observed_values = data$y_bin,
  predicted_probs = loc_pd
)
head(auc_data)
auc_data$observed_values <- as.numeric(as.character(auc_data$observed_values))
auc1 <- PresenceAbsence::auc(auc_data, st.dev=FALSE)
print(auc1)

# BRIER SCORE
brier_score1 <- mean((loc_pd - auc_data$observed_values) ^ 2)
print(brier_score1)

# Moran's I
ranef_data <- as.data.frame(ranef(loc18)$cond$knum)
unique_data <- data[!duplicated(data$knum),]
coordinates(unique_data) <- ~lon + lat
neighbors_knn1 <- knearneigh(coordinates(unique_data), k = 8)
weights_list_knn1 <- nb2listw(knn2nb(neighbors_knn1))
moran_result1 <- moran.test(ranef_data[, 1], listw = weights_list_knn1)
print(moran_result1)

#### MODEL EVALUATION - LANDSCAPE SCALE ####
eval_data2 <- data %>%
  select(knum, vuosi, effort, y_bin, dclcu_0910_20000s, dclcu_2021_20000s, PD_20000s, VPINE_20000s, VTOT_20000s, PABL_20000s, lnEffort)

land_pd <- predict(land59, re.form = NA, newdata = eval_data2, type = "response")
land_pd_res <- data.frame(obs = data$y_bin, pred = land_pd)
land_oc <- MKclass::optCutoff(land_pd_res$pred, truth = land_pd_res$obs, namePos = 1)
class_pred2 <- ifelse(land_pd_res >= land_oc[[1]], 1, 0)

class_pred2 <- as.factor(class_pred2)
data$y_bin <- as.factor(data$y_bin)
confmat2 <- confusionMatrix(data=class_pred2, reference = data$y_bin, positive = "1")
confmat2

# AUC
auc_data2 <- data.frame(
  plot_ID = data$knum,        
  observed_values = data$y_bin,  
  predicted_probs = land_pd
)
head(auc_data2)
auc_data2$observed_values <- as.numeric(as.character(auc_data2$observed_values))
auc2 <- PresenceAbsence::auc(auc_data2, st.dev=FALSE)
print(auc2)

# TSS
Sensitivity2 <- confmat2$byClass["Sensitivity"]
Specificity2 <- confmat2$byClass["Specificity"]

land_tss <- Sensitivity2[[1]] + Specificity2[[1]] - 1
print(land_tss)

# BRIER SCORE
brier_score2 <- mean((land_pd - auc_data2$observed_values) ^ 2)
print(brier_score2)

# Moran's I
ranef_data2 <- as.data.frame(ranef(land59)$cond$knum)
unique_data2 <- data[!duplicated(data$knum),]
coordinates(unique_data2) <- ~lon + lat
neighbors_knn2 <- knearneigh(coordinates(unique_data2), k = 8)
weights_list_knn2 <- nb2listw(knn2nb(neighbors_knn2))
moran_result2 <- moran.test(ranef_data2[, 1], listw = weights_list_knn2)
print(moran_result2)