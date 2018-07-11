# Joshua Alley
# Texas A&M University
# Reproduction and Extension of Kimball 2010 and Allen & DiGiuseppe 2013


# Load packages
library(here)
library(haven)
library(dplyr)
library(ggplot2)
library(separationplot)
library(party)
library(lmtest)
library(sandwich) # Need sandwich package to compute robust standard errors.


# Set working directory to current folder 
setwd(here::here())
getwd()



# Possible robustness checks/extensions
# 1. Defense burden and formation
# 2. Current allied capability vis-a-vis threats interacted with need. 
# 3. Increases in the number of states in the system and number of alliances over time: Robust
# In general, lots of contextual variables probably modify those economic need indicators. (BARTmachine for fit and check)



# Load data
# Kimball 2010
kimball.data <- read_dta("replications/Political Survival & Alliance Formation Data.dta")
# Clean up the names of the spline variables
kimball.data <- rename(kimball.data, spline1 = `_spline1`, 
                       spline2 = `_spline2`,
                       spline3 = `_spline3`)
# A&DG 2013
allen.digi.data <- read_dta("replications/Allen_DiGI_Rep.dta")

# Load COW system membershp data and use to create a indicator of the number of states in the system
# This will be a proxy for opportunity to form alliances
system.mem <- read.csv("replications/system2016.csv")
system.mem <- select(system.mem, c(year))

system.mem <- system.mem %>% group_by(year) %>% mutate(system.members = n())
system.mem <- unique(system.mem)
# Merge with Kimball and A&DG data
kimball.data <- merge(kimball.data, system.mem)
allen.digi.data <- merge(allen.digi.data, system.mem)


# Load CINC data, to use military personnel as share of population as alternative indicator of defense effort
cinc.data <- read.csv("replications/NMC_5_0.csv")

milper.effort <- select(cinc.data, ccode, year, milper, tpop) %>%
                  mutate(
                    tpop = as.numeric(tpop),
                    defb.pers = milper / tpop
                    )

# Merge with Kimball and A&DG data 
kimball.data <- merge(kimball.data, milper.effort, all.x = TRUE)
allen.digi.data <- merge(allen.digi.data, milper.effort, all.x = TRUE)


# Load data on GDP and military spending from Nordhaus et al: covers 1950 to 2000
nordhaus.data <- read_dta("replications/Nordhaus et al 2012 data.dta")

# Create GDP and military expenditure variables
defense.data <- mutate(nordhaus.data,
                            ccode = STATE,
                            year = YEAR,
                            gdp = exp(LNRGDP),
                            milex = exp(LMILEX),
                            def.burden = milex / gdp) %>%
                      select(ccode, year, gdp, milex, def.burden)

# Merge expenditures and GDP data with kimball and A&DG data
# Create separate datasets because the samples are quite disparate with limited temporal coverage of Nordhaus data
kimball.data.defb <- merge(kimball.data, defense.data)
allen.digi.data.defb <- merge(allen.digi.data, defense.data)



# Replicate first model from each paper and assess fit using separation plot
# Both models are probit

### Kimball 2010: Table 2 Model 1A
kimball.t2.1a <- glm(AF_new ~ IMRlevel + cap + sumrival1 + WoverS1 + 
                       envmids1 + allyrs + spline1 + spline2 + spline3, 
                     family = binomial(link = "probit"), 
                    data = kimball.data)
summary(kimball.t2.1a)
coeftest(kimball.t2.1a, vcov = vcovHC(kimball.t2.1a, type="HC1"))


# Get the predicted probabilities
k.pred.prob <- as.numeric(predict.glm(kimball.t2.1a, type = "response"))
k.outcome <- as.numeric(kimball.t2.1a$y)

# Summarize DV: Around 3% of observations = 1 in the sample
summary(k.outcome)
summary(kimball.data$AF_new)

# Separation Plot
separationplot(k.pred.prob, k.outcome)

# Add a post-1945 indicator to Kimball's probit spec
kimball.data <- mutate(kimball.data,
                       post.45 = ifelse(year > 1945, 1, 0))

kimball.post45 <- glm(AF_new ~ IMRlevel + cap + sumrival1 + WoverS1 + 
                       envmids1 + post.45 + allyrs + spline1 + spline2 + spline3, 
                     family = binomial(link = "probit"), 
                     data = kimball.data)
summary(kimball.post45)
coeftest(kimball.post45, vcov = vcovHC(kimball.post45, type="HC1")) # Robust standard errors to complete the robustness check


# Add an indicator of the number of states in the system 
kimball.sysmem <- glm(AF_new ~ IMRlevel + cap + sumrival1 + WoverS1 + 
                        envmids1 + post.45 + system.members + allyrs + spline1 + spline2 + spline3, 
                      family = binomial(link = "probit"), 
                      data = kimball.data)
summary(kimball.sysmem)
coeftest(kimball.sysmem, vcov = vcovHC(kimball.sysmem, type="HC1")) # Robust standard errors to complete the robustness check

# Add defense burden: reduces sample to 1950 to 2000.
kimball.defb <- glm(AF_new ~ IMRlevel + sumrival1 + WoverS1 + 
                         envmids1 + allyrs + def.burden + milex + gdp +
                        spline1 + spline2 + spline3, 
                       family = binomial(link = "probit"), 
                       data = kimball.data.defb)
summary(kimball.defb)
coeftest(kimball.defb, vcov = vcovHC(kimball.defb, type="HC1")) # Robust standard errors to complete the robustness check


# Use military personnel as share of population as alternative defense burden measure
kimball.pers.defb <- glm(AF_new ~ IMRlevel + cap + sumrival1 + WoverS1 + 
                        envmids1 + system.members + post.45 
                        defb.pers + milper + tpop +
                        allyrs + spline1 + spline2 + spline3, 
                      family = binomial(link = "probit"), 
                      data = kimball.data)
summary(kimball.pers.defb)
coeftest(kimball.pers.defb, vcov = vcovHC(kimball.pers.defb, type="HC1")) # Robust standard errors to complete the robustness check



# Fit a random forest model to try and improve fit of Kimball 2010:
kimball.data.sub <- select(kimball.data, c(AF_new, year, S1, W1, WoverS1, IMRlevel, revchpre1, envmids1,
                                           envwars1, prie02_1, regprie02_1, regime02_1, caprat301_1, 
                                           nomids_1, nowars_1, sumrival1, sumcap1rivals, cap, lgW, smW, 
                                           lgIMR1, lgAVIMR1, worseIMR, Parl_Pres, w2, demaut2, allyrs, 
                                           NATO, dbigw, dw30, defb.pers, milper, tpop))

rf.kimball <- cforest(AF_new ~ ., data = kimball.data.sub[complete.cases(kimball.data.sub$AF_new), ])

vi.kimball <- varimp(rf.kimball)

vi.kimball.df <- data_frame(var_name = names(vi.kimball), importance = vi.kimball) %>%  # put importance in a df
  mutate(var_name = reorder(var_name, importance))  # order factor var_name by importance

# Plot importance
ggplot(vi.kimball.df, aes(x = var_name, y = importance)) + 
  geom_col() + 
  coord_flip() +
  ggtitle("Importance of Covariates from a Random Forest Model of Kimball 2010")

# Calculate predicted probabilities and use a separation plot
pred.rf.kimball <- as.numeric(predict(rf.kimball, type = "prob"))
outcome.rf.kimball <- kimball.data.sub$AF_new[complete.cases(kimball.data.sub$AF_new)]
separationplot(pred.rf.kimball, outcome.rf.kimball, newplot = FALSE) # Separation plot





### Allen and DiGiuseppe 2013
allen.dg.m1 <- glm(formationB2NNA ~ extdebtcrisis_1 + IMR + cinc + sumrival1 + wovers_1 + atwar5 + 
                      ayr + ayr2 + ayr3, family = binomial(link = "probit"),
                    data = allen.digi.data)
summary(allen.dg.m1)
coeftest(allen.dg.m1, vcov = vcovHC(allen.dg.m1, type="HC1"))

# Get the predicted probabilities
adg.pred.prob <- as.numeric(predict.glm(allen.dg.m1, type = "response"))
adg.outcome <- as.numeric(allen.dg.m1$y)

# Summarize DV: Around 3% of observations = 1 in the sample
summary(adg.outcome)
summary(allen.digi.data$formationB2NNA)

# Separation Plot
separationplot(adg.pred.prob, adg.outcome)

# Add a post-1945 indicator to A&DG's data and model specification
allen.digi.data <- mutate(allen.digi.data,
                       post.45 = ifelse(year > 1945, 1, 0))
allen.dg.post45 <- glm(formationB2NNA ~ extdebtcrisis_1 + IMR + cinc + sumrival1 + wovers_1 + atwar5 + post.45 +
                     ayr + ayr2 + ayr3, family = binomial(link = "probit"),
                   data = allen.digi.data)
summary(allen.dg.post45)
coeftest(allen.dg.post45, vcov = vcovHC(allen.dg.post45, type="HC1")) # Robust standard errors to complete the robustness check

# Add an indicator of the number of states in the system to the model
allen.dg.sysmem <- glm(formationB2NNA ~ extdebtcrisis_1 + IMR + cinc + sumrival1 + wovers_1 + atwar5 +
                         system.members +
                         ayr + ayr2 + ayr3, family = binomial(link = "probit"),
                       data = allen.digi.data)
summary(allen.dg.sysmem)
coeftest(allen.dg.sysmem, vcov = vcovHC(allen.dg.sysmem, type="HC1")) # Robust standard errors to complete the robustness check


# Control for defense burden: reduces sample to 1950-2000
allen.dg.defb <- glm(formationB2NNA ~ extdebtcrisis_1 + IMR + cinc + sumrival1 + wovers_1 + atwar5 +
                         def.burden + gdp + milex + 
                         ayr + ayr2 + ayr3, family = binomial(link = "probit"),
                       data = allen.digi.data.defb)
summary(allen.dg.defb)
coeftest(allen.dg.defb, vcov = vcovHC(allen.dg.defb, type="HC1")) # Robust standard errors to complete the robustness check


# Control for defense burden using military personnel as a share of the population
allen.dg.pers.defb <- glm(formationB2NNA ~ extdebtcrisis_1 + IMR + cinc + sumrival1 + wovers_1 + atwar5 +
                       defb.pers + milper + tpop + system.members + post.45 +
                       ayr + ayr2 + ayr3, family = binomial(link = "probit"),
                     data = allen.digi.data)
summary(allen.dg.pers.defb)
coeftest(allen.dg.pers.defb, vcov = vcovHC(allen.dg.pers.defb, type="HC1")) # Robust standard errors to complete the robustness check





# Fit a random forest model to try and improve fit of Allen and Digiuseppe 2013:
adg.data.sub <- select(allen.digi.data, -c(ccode, abbrev, ayr2, ayr3))

rf.adg <- cforest(formationB2NNA ~ ., data = adg.data.sub[complete.cases(adg.data.sub$formationB2NNA), ])

vi.adg <- varimp(rf.adg)

vi.adg.df <- data_frame(var_name = names(vi.adg), importance = vi.adg) %>%  # put importance in a df
  mutate(var_name = reorder(var_name, importance))  # order factor var_name by importance

# Plot importance
ggplot(vi.adg.df, aes(x = var_name, y = importance)) + 
  geom_col() + 
  coord_flip() +
  ggtitle("Importance of Covariates from a Random Forest Model of Allen and DiGiuseppe 2013")

# Calculate predicted probabilities
pred.rf.adg <- as.numeric(predict(rf.adg, type = "prob"))
outcome.rf.adg <- adg.data.sub$formationB2NNA[complete.cases(adg.data.sub$formationB2NNA)]

separationplot(pred.rf.adg, outcome.rf.adg, newplot = FALSE) # separation Plot from the ADG random forest




### Separation Plots for Comparison
# Put both the separation plots together 
par(mfrow = c(2, 1))
separationplot(k.pred.prob, k.outcome, heading = "Kimball 2010", 
               newplot = FALSE, BW = TRUE)
separationplot(adg.pred.prob, adg.outcome, heading = "Allen and Digiuseppe 2013",
               newplot = FALSE, BW = TRUE)


# Compare the separation plots for regular model and random forests
par(mfrow = c(2, 1))
separationplot(k.pred.prob, k.outcome, heading = "Kimball 2010: Probit", 
               newplot = FALSE, BW = TRUE)
separationplot(pred.rf.kimball, outcome.rf.kimball, heading = "Random Forest",
               newplot = FALSE, BW = TRUE)

# Allen and DiGiuseppe 2013
par(mfrow = c(2, 1))
separationplot(adg.pred.prob, adg.outcome, heading = "Allen and DiGiuseppe 2013: Probit", 
               newplot = FALSE, BW = TRUE)
separationplot(pred.rf.adg, outcome.rf.adg, heading = "Random Forest",
               newplot = FALSE, BW = TRUE)





### Fit Kimball and ADG data with BART 
# Besides better fit, can assess substantive importance of predictors with these models
options(java.parameters = "-Xmx5g") # increase amount of memory for Java processes underlying BART package
library(bartMachine)
set_bart_machine_num_cores(4)

# Set up data
kimball.IVs <- select(kimball.data.sub[complete.cases(kimball.data.sub$AF_new), ], -c(AF_new))
adg.IVs <- select(adg.data.sub[complete.cases(adg.data.sub$formationB2NNA), ], -c(formationB2NNA))

# run the model for kimball 2010
system.time(
  bart.kimball <- bartMachine(X = kimball.IVs, y = factor(outcome.rf.kimball, levels = c("1", "0")), serialize = TRUE, replace_missing_data_with_x_j_bar = TRUE)
)

bart.kimball

# Assess variable importance
investigate_var_importance(bart.kimball, num_replicates_for_avg = 20, plot = TRUE)





# run the model for A&DG 2013
system.time(
  bart.adg <- bartMachine(X = adg.IVs, y = factor(outcome.rf.adg, levels = c("1", "0")), serialize = TRUE, replace_missing_data_with_x_j_bar = TRUE)
)

bart.adg


# Assess variable importance
investigate_var_importance(bart.adg, num_replicates_for_avg = 20)

