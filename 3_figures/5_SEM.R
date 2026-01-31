# Fig. 5
# Structural Equation Model (SEM) analysis

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(conflicted)
library(ggpubr)
library(lavaan)
library(semPlot)
library(xfun)
library(MASS)
library(caret)
library(car)
library(mice)
library(semTools)

conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)

setwd("C:/Users/maria/Desktop/Research/2022/output_processed")
plot.dat <- read.csv("plot.dat.1.30.26.csv")
#-------------------------------------------------------------------------------
# Microbial ratio calc:
# Total N cyclers
plot.dat$n_microbes <- plot.dat$nitrification + plot.dat$nitrogen_fixation +
  plot.dat$ureolysis 

plot.dat$non_n_microbes <- plot.dat$Total_ASV - plot.dat$n_microbes

# n cycling / non-n cycling microbes  
plot.dat$n_asv_ratio2 <- plot.dat$n_microbes / plot.dat$non_n_microbes


# Rship with nitrification - yes significant, but not a high r-squared
mod <- lm(Net.Nitrification.rate..ug.N.g.Soil..1.d.1. ~ n_asv_ratio2, data = plot.dat)
summary(mod)

#-------------------------------------------------------------------------------
# Develop model to predict litter for plots without sensors installed

# Make column: yes/no observed or predicted data
plot.dat <- plot.dat %>%
  mutate(pred_obs = if_else(is.na(Litter), "predicted", "observed"))

# Training data:
train_data <- plot.dat %>% filter(!is.na(Litter))

# Test data:
test_data <- plot.dat %>% filter(is.na(Litter))

# Model to predict AM prop and A1510_mean
# Multiple R-squared:  0.8189,	Adjusted R-squared:  0.7999 
mod <- lm(Litter ~  AM_prop + A1510_mean, data = train_data)
summary(mod)
AIC(mod)

# Predict missing values
pred_vals <- predict(mod, newdata = test_data)

# Fill in missing Litter values
plot.dat$Litter[is.na(plot.dat$Litter)] <- pred_vals

#-------------------------------------------------------------------------------
# Get ready for modeling

# Simplify name, make sure numeric
plot.dat$Net.Nitrification.rate <-
  as.numeric(plot.dat$Net.Nitrification.rate..ug.N.g.Soil..1.d.1.)

# Set NBE = 0 when species richness = 1
plot.dat$NE[plot.dat$SR == 1] <- 0

# Polycultures only for NBE analysis
poly <- plot.dat %>% filter(SR.x != 1)


#-------------------------------------------------------------------------------
# Scaling
poly[, c("faith.no.root.PD",
         "Net.Nitrification.rate",
         "FAST_prop",
         "NE",
         "FC",
         "CCIred_edge_mean",
         "Litter",
         "swir.cv",
         "n_asv_ratio2")] <-
  scale(poly[, c("faith.no.root.PD",
                 "Net.Nitrification.rate",
                 "FAST_prop",
                 "NE",
                 "FC",
                 "CCIred_edge_mean",
                 "Litter",
                 "swir.cv",
                 "n_asv_ratio2")])

#-------------------------------------------------------------------------------
# Four SEMs

mod.pd <- '
          # regressions
          Litter ~ faith.no.root.PD
          n_asv_ratio2 ~ Litter
          Net.Nitrification.rate ~ n_asv_ratio2 + Litter
          CCIred_edge_mean ~ faith.no.root.PD + Net.Nitrification.rate
          NE ~ faith.no.root.PD + CCIred_edge_mean
          '

mod.cv <- '
          # regressions
          Litter ~ swir.cv
          n_asv_ratio2 ~ Litter
          Net.Nitrification.rate ~ n_asv_ratio2 + Litter
          CCIred_edge_mean ~ swir.cv + Net.Nitrification.rate
          NE ~ swir.cv + CCIred_edge_mean
          '

mod.fc <- '
          # regressions
          Litter ~ FC
          n_asv_ratio2 ~ Litter
          Net.Nitrification.rate ~ n_asv_ratio2 + Litter
          CCIred_edge_mean ~ FC + Net.Nitrification.rate
          NE ~ FC + CCIred_edge_mean
          '


mod.fast <- '
          # regressions
          Litter ~ FAST_prop
          n_asv_ratio2 ~ Litter
          Net.Nitrification.rate ~ n_asv_ratio2 + Litter
          CCIred_edge_mean ~ FAST_prop + Net.Nitrification.rate
          NE ~ FAST_prop + CCIred_edge_mean
          '



#-------------------------------------------------------------------------------
# SEM
# Bootstrap
fit <- sem(mod.pd, data = poly,
           estimator = "ML",
           se = "bootstrap",
           bootstrap = 1000)

summary(fit, standardized = TRUE, ci = TRUE, rsquare = TRUE)
AIC(fit)

#-------------------------------------------------------------------------------
# Alternate methods give similar results
fit <- sem(mod.pd, data = poly,
           estimator = "MLR")  # robust ML

# Inspect covariance matrix
inspect(fit, "cor.ov")

#summary(fit)
standardizedSolution(fit, type="std.all")
AIC(fit)
summary(fit, standardized = TRUE, fit.measures = TRUE)
#-------------------------------------------------------------------------------
# Checking multicollinearity
mod <- lm(FC ~ faith.no.root.PD + A1510_mean, data = poly)
mod <- lm(A1510_mean ~ FC + Net.Nitrification.rate, data = poly)


# CCIred edge is lowest multicollinearity
mod <- lm(NE ~ faith.no.root.PD + CCIred_edge_mean, data = poly)
mod <- lm(NE ~ faith.no.root.PD + A1510_mean, data = poly)
mod <- lm(NE ~ faith.no.root.PD + NDRE_mean, data = poly)

vif(mod)


# A1510 technically lowest VIF, but all between 1.2 and 1.3 VIF
mod <- lm(NE ~ swir.cv + CCIred_edge_mean, data = poly)
mod <- lm(NE ~ swir.cv + A1510_mean, data = poly)
mod <- lm(NE ~ swir.cv + NDRE_mean, data = poly)

vif(mod)



# CCIred edge is lowest multicollinearity (some correlation but acceptable)
# Others are really correlated with FC
mod <- lm(NE ~ FC + CCIred_edge_mean, data = poly)
mod <- lm(NE ~ FC + A1510_mean, data = poly)
mod <- lm(NE ~ FC + NDRE_mean, data = poly)

vif(mod)

# CCIred edge is lowest multicollinearity (some correlation but acceptable)
# Others are still acceptable, but 
mod <- lm(NE ~ FAST_prop + CCIred_edge_mean, data = poly)
mod <- lm(NE ~ FAST_prop + A1510_mean, data = poly)
mod <- lm(NE ~ FAST_prop + NDRE_mean, data = poly)

vif(mod)
