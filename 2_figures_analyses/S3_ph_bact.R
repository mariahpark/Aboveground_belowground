# Supplementary Fig. 3
# pH ~ microbes

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


setwd("C:/Users/maria/Desktop/Research/2022/output_processed")

plot.dat <- read.csv("plot.dat.1.30.26.csv")

#-------------------------------------------------------------------------------
# What drives pH
mod <- lm(pH ~ FC, data = plot.dat)
summary(mod)

# Not just the gymnosperms
mod <- lm(pH ~ gymno_prop, data = plot.dat)
summary(mod)

mod <- lm(pH ~ Litter, data = plot.dat)
summary(mod)

# pH diversity
mod <- lm(Observed_richness ~ pH, data = plot.dat)
summary(mod)

mod <- lm(Shannon_diversity ~ pH, data = plot.dat)
summary(mod)

mod <- lm(Pielou_evenness ~ pH, data = plot.dat)
summary(mod)

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
# pH

ph <- ggplot(plot.dat, aes(x = FC, y = pH,
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
  xlab("Fractional cover") +
  ylab("Soil pH") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
ph

# Diversity
shan <- ggplot(plot.dat, aes(x = pH, y = Shannon_diversity,
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
               label.x = "right",
               label.y = "bottom",
               size = 5.5)+
  xlab("Soil pH") +
  ylab("Shannon diversity of soil microbes") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
shan

# Evenness
pielou <- ggplot(plot.dat, aes(x = pH, y = Pielou_evenness,
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
               label.x = "right",
               label.y = "bottom",
               size = 5.5)+
  xlab("Soil pH") +
  ylab("Pielou evenness of soil microbes") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
pielou

#-------------------------------------------------------------------------------
# Combine figures
# Export: 1500 x 500

fig <- ggarrange(ph, shan, pielou,
                   ncol = 3, nrow = 1,
                   common.legend= TRUE,
                   legend = "top"
)

fig


