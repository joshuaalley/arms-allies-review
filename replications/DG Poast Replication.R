# Joshua Alley
# Texas A&M University
# Replication of Digiuseppe and Poast 2016


# Load packages
library(here)
library(haven)
library(MASS)
library(plm)
library(dplyr)
library(texreg)


# Set working directory to current folder 
setwd(here::here())
getwd()


# Load data
dg.poast <- read_dta("replications/AvsDA_replication.dta")



# Run their first model: Without panel-spec AR(1)
dg.poast.m1 <- lm(LMILEX ~ defense_dem + defense_nodem + LMILEX1 + DEMOC +
                    rivalmil + atwar + civilwar + LNRGDP + milcoor, data = dg.poast)
summary(dg.poast.m1)
plot(dg.poast.m1)


# Robust regression: again, no panel-specific AR(1)
dg.poast.m1rr <- rlm(LMILEX ~ defense_dem + defense_nodem + LMILEX1 + DEMOC +
                        rivalmil + atwar + civilwar + LNRGDP + milcoor, data = dg.poast)
summary(dg.poast.m1rr)
plot(dg.poast.m1rr)
plot(dg.poast.m1rr$residuals, dg.poast.m1rr$w)

