# Statistical explorations

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
library(viridis)
library(ggpmisc)
library(scales)

conflict_prefer("mutate", "dplyr")
conflict_prefer("summarise", "dplyr")
conflicts_prefer(dplyr::filter)

setwd("C:/Users/maria/Desktop/Research/2022/output_processed")
plot.dat <- read.csv("plot.dat.1.30.26.csv")

#-------------------------------------------------------------------------------
# Litter
summary(lm(Litter ~ FAST_prop, data = plot.dat))

summary(lm(Litter ~ plot_vol, data = plot.dat))

summary(lm(Litter ~ FC, data = plot.dat))

#-------------------------------------------------------------------------------
# pH

summary(lm(pH ~ FC, data = plot.dat))

summary(lm(pH ~ Litter, data = plot.dat))
#-------------------------------------------------------------------------------
# Microbe ratio
# Total N cyclers
plot.dat$n_microbes <- plot.dat$nitrification + plot.dat$nitrogen_fixation +
  plot.dat$ureolysis 

plot.dat$non_n_microbes <- plot.dat$Total_ASV - plot.dat$n_microbes

# n cycling / non-n cycling microbes  
plot.dat$n_asv_ratio2 <- plot.dat$n_microbes / plot.dat$non_n_microbes

#-------------------------------------------------------------------------------
# Litter ~ N Microbes and nitrification

summary(lm(n_asv_ratio2 ~ Litter, data = plot.dat))
summary(lm(Net.Nitrification.rate..ug.N.g.Soil..1.d.1. ~ Litter, data = plot.dat))

#-------------------------------------------------------------------------------
# Herbaceous cover relationships with litter

plot(plot.dat$Equisetum ~ plot.dat$Litter)

# Negative
summary(lm(C3.grass ~ Litter, data = plot.dat))

# Negative
summary(lm(C4.grass ~ Litter, data = plot.dat))

summary(lm(Legume ~ Litter, data = plot.dat))

summary(lm(Sedge ~ Litter, data = plot.dat))

summary(lm(Forb ~ Litter, data = plot.dat))

# Negative
summary(lm(Equisetum ~ Litter, data = plot.dat))

summary(lm(Lichen_Fungi_Moss ~ Litter, data = plot.dat))

summary(lm(Bareground ~ Litter, data = plot.dat))

#-------------------------------------------------------------------------------
# Nitrification
summary(lm(CCIred_edge_mean ~ Net.Nitrification.rate..ug.N.g.Soil..1.d.1., data = plot.dat))
mod <- lm(CCIred_edge_mean ~ Net.Nitrification.rate..ug.N.g.Soil..1.d.1., data = plot.dat)
AIC(mod)

summary(lm(NDRE_mean ~ Net.Nitrification.rate..ug.N.g.Soil..1.d.1., data = plot.dat))
mod <- lm(NDRE_mean ~ Net.Nitrification.rate..ug.N.g.Soil..1.d.1., data = plot.dat)
AIC(mod)

summary(lm(A1510_mean ~ Net.Nitrification.rate..ug.N.g.Soil..1.d.1., data = plot.dat))
mod <- lm(A1510_mean ~ Net.Nitrification.rate..ug.N.g.Soil..1.d.1., data = plot.dat)
AIC(mod)

# Ammonification
summary(lm(CCIred_edge_mean ~ Net.Ammonification.rate..ug.N.g.Soil..1.d.1., data = plot.dat))

summary(lm(NDRE_mean ~ Net.Ammonification.rate..ug.N.g.Soil..1.d.1., data = plot.dat))

summary(lm(A1510_mean ~ Net.Ammonification.rate..ug.N.g.Soil..1.d.1., data = plot.dat))

# Mineralization
summary(lm(CCIred_edge_mean ~ Net.Mineralization.rate..ug.N.g.Soil..1.d.1., data = plot.dat))

summary(lm(NDRE_mean ~ Net.Mineralization.rate..ug.N.g.Soil..1.d.1., data = plot.dat))

summary(lm(A1510_mean ~ Net.Mineralization.rate..ug.N.g.Soil..1.d.1., data = plot.dat))

#-------------------------------------------------------------------------------
# Complementarity is driving NBE

poly <- plot.dat %>% filter(SR.y != 1)

mod <- lm(NE ~ CE, data = poly)
summary(mod)

mod <- lm(NE ~ SE, data = poly)
summary(mod)

mod <- lm(NE ~ CE + SE, data = poly)
summary(mod)



