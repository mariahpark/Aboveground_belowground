# Fig.3
# Compare canopy N estimates from leaf samples and spectra

rm(list=ls())
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(data.table)
library(ggplot2)
library(ggpmisc)
library(ggpubr)
library(rsq)
library(viridis)
library(tidyverse)
library(gridExtra)


setwd("C:/Users/maria/Desktop/Research/2022/output_processed")

plot.dat <- read.csv("plot.dat.1.30.26.csv")


# New variable: ln of (1 + groundtruthed canopy N)
plot.dat$log.canopy.N.percent.sp <- log(1+plot.dat$canopy.N.percent.sp)


#-------------------------------------------------------------------------------
# Model fits

# AIC values
mod <- lm(canopy.N.percent.sp ~ CCIred_edge_mean, data = plot.dat)
summary(mod)
AIC(mod)

mod <- lm(canopy.N.percent.sp ~ NDRE_mean, data = plot.dat)
summary(mod)
AIC(mod)

mod <- lm(canopy.N.percent.sp ~ A1510_mean, data = plot.dat)
summary(mod)
AIC(mod)


# AIC values: log transformed
mod <- lm(log.canopy.N.percent.sp ~ CCIred_edge_mean, data = plot.dat)
summary(mod)
AIC(mod)

mod <- lm(log.canopy.N.percent.sp ~ NDRE_mean, data = plot.dat)
summary(mod)
AIC(mod)

mod <- lm(log.canopy.N.percent.sp ~ A1510_mean, data = plot.dat)
summary(mod)
AIC(mod)


#-------------------------------------------------------------------------------
# Palette and figure legend

gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

colour_PA_FC <- scale_fill_viridis("Fractional cover",
                                   option = "D",
                                   direction = -1,
                                   begin = 0,
                                   end = 1,
                                   limits = c(0, 1),
                                   breaks = c(0, 0.5, 1))



#-------------------------------------------------------------------------------
# Canopy N groundtruthing -- matching spectral indices with canopy N estimates from leaf CN

cci.n <- ggplot(plot.dat, aes(x = log(1+canopy.N.percent.sp), y = CCIred_edge_mean,
                              fill = FC)) +
  colour_PA_FC+
  gui+
  geom_point(size = 5, shape = 21, colour = "black")+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,#log(x),
               label.x = "left",
               label.y = "top",
               size = 5.5)+
  xlab(" ") +
  #ylab("CCI red edge") +
  ylab(bquote(CI[red-edge]))+
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
cci.n

ndre.n <- ggplot(plot.dat, aes(x = log(1+canopy.N.percent.sp), y = NDRE_mean,
                               fill = FC)) +
  colour_PA_FC+
  gui+
  geom_point(size = 5, shape = 21, colour = "black")+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,#log(x),
               label.x = "left",
               label.y = "top",
               size = 5.5)+
  xlab(" ") +
  ylab("NDRE") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
ndre.n

# New variable - absorbance calc isn't as straightforward is it seems

r1510.n <- ggplot(plot.dat, aes(x = log(1+canopy.N.percent.sp), y = A1510_mean,
                                fill = FC)) +
  colour_PA_FC+
  gui+
  geom_point(size = 5, shape = 21, colour = "black")+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,#log(x),
               label.x = "left",
               label.y = "top",
               size = 5.5)+
  xlab(" ") +
  ylab("1 - R1510") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
r1510.n


#-------------------------------------------------------------------------------
# Combine figures
# 1500 x 600 export

fig.3 <- ggarrange(cci.n, ndre.n, r1510.n,
                   ncol = 3, nrow = 1,
                   common.legend= TRUE,
                   legend = "top"
                   #labels = c("C","D")
)

fig.3

fig.3 <- annotate_figure(
  fig.3,
  bottom = text_grob("ln (1 + scaled canopy N %)", size = 18
  )
)
fig.3
