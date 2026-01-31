# Supplementary Fig. 1
# Connecting canopy N and soil N

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
# CCI red edge

# Nitrification
spectra.nit.cci <- ggplot(plot.dat, aes(x = Net.Nitrification.rate..ug.N.g.Soil..1.d.1., y = CCIred_edge_mean,
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
  xlab(" ") +
  ylab(" ") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
spectra.nit.cci



# Ammonification
spectra.am.cci <- ggplot(plot.dat, aes(x = Net.Ammonification.rate..ug.N.g.Soil..1.d.1., y = CCIred_edge_mean, 
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
               label.y = "bottom",
               size = 5.5)+
  xlab(" ") +
  ylab(" ") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
spectra.am.cci


# Nmin
spectra.nmin.cci <- ggplot(plot.dat, aes(x = Net.Mineralization.rate..ug.N.g.Soil..1.d.1., y = CCIred_edge_mean, 
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
  xlab(" ") +
  ylab(" ") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
spectra.nmin.cci



#-------------------------------------------------------------------------------
# NDRE

# Nitrification
spectra.nit.ndre <- ggplot(plot.dat, aes(x = Net.Nitrification.rate..ug.N.g.Soil..1.d.1., y = NDRE_mean,
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
  xlab(" ") +
  ylab(" ") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
spectra.nit.ndre



# Ammonification
spectra.am.ndre <- ggplot(plot.dat, aes(x = Net.Ammonification.rate..ug.N.g.Soil..1.d.1., y = NDRE_mean,
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
               label.y = "bottom",
               size = 5.5)+
  xlab(" ") +
  ylab(" ") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
spectra.am.ndre


# Nmin
spectra.nmin.ndre <- ggplot(plot.dat, aes(x = Net.Mineralization.rate..ug.N.g.Soil..1.d.1., y = NDRE_mean,
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
  xlab(" ") +
  ylab(" ") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
spectra.nmin.ndre




#-------------------------------------------------------------------------------
# R1510

# Nitrification
spectra.nit.1510 <- ggplot(plot.dat, aes(x = Net.Nitrification.rate..ug.N.g.Soil..1.d.1., y = A1510_mean,
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
  xlab(" ") +
  ylab(" ") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
spectra.nit.1510



# Ammonification
spectra.am.1510 <- ggplot(plot.dat, aes(x = Net.Ammonification.rate..ug.N.g.Soil..1.d.1., y = A1510_mean,
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
               label.y = "bottom",
               size = 5.5)+
  xlab(" ") +
  ylab(" ") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
spectra.am.1510


# Nmin
spectra.nmin.1510 <- ggplot(plot.dat, aes(x = Net.Mineralization.rate..ug.N.g.Soil..1.d.1., y = A1510_mean,
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
  xlab(" ") +
  ylab(" ") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
spectra.nmin.1510

#-------------------------------------------------------------------------------
# Combine all figures

# Combine figures
#1800 x 1260 pixels
# Annotated in Microsoft PowerPoint

total.fig <- ggarrange( spectra.am.cci, spectra.nit.cci, spectra.nmin.cci,
                        spectra.am.ndre, spectra.nit.ndre, spectra.nmin.ndre,
                        spectra.am.1510, spectra.nit.1510,  spectra.nmin.1510,
                        ncol = 3, nrow = 3,
                        common.legend= TRUE,
                        legend = "top"
                        #labels = c("C","D")
)

total.fig

