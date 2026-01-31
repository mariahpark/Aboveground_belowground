# Supplementary Fig. 4
# Test for C limitation

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(conflicted)
library(ggpubr)
library(ggpmisc)
library(lavaan)
library(semPlot)
library(xfun)
library(MASS)
library(caret)
library(car)
library(brms)
library(glmmTMB)
library(piecewiseSEM)
library(bestNormalize)
conflicts_prefer(dplyr::filter)

setwd("C:/Users/maria/Desktop/Research/2022/output_processed")
plot.dat <- read.csv("plot.dat.1.30.26.csv")
#-------------------------------------------------------------------------------
# Microbes
# Total N cyclers
plot.dat$n_microbes <- plot.dat$nitrification + plot.dat$nitrogen_fixation + 
  plot.dat$ureolysis 

plot.dat$non_n_microbes <- plot.dat$Total_ASV - plot.dat$n_microbes

# n cycling / non-n cycling microbes  
plot.dat$n_asv_ratio2 <- plot.dat$n_microbes / plot.dat$non_n_microbes


#-------------------------------------------------------------------------------
# Plotting

#put legend on top
gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

# Color palette
colour_PA <- scale_fill_gradientn("FAST species proportion",
                                  colours = c("#fc8d59","#ffffbf","#91bfdb"),
                                  values = scales::rescale(c(0,0.5,1)),
                                  limits = c(0,1),
                                  breaks = c(0,0.5,1))

#-------------------------------------------------------------------------------
# Plot relationships

# Plot volume on carbon
plot.c <- ggplot(plot.dat, aes(x = plot_vol, y = Carbon.Weight,
                               fill = FAST_prop)) +
  colour_PA+
  gui+
  geom_point(size = 5, shape = 21, colour = "black")+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               size = 5.5)+
  xlab(expression("Plot volume (m"^3*")"))+
  ylab("Soil carbon (%)") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
plot.c


cn.microbe <- ggplot(plot.dat, aes(x = Carbon.Weight, y = n_asv_ratio2,
                                   fill = FAST_prop)) +
  colour_PA+
  gui+
  geom_point(size = 5, shape = 21, colour = "black")+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "right",
               label.y = "top",
               size = 5.5)+
  xlab("Soil carbon (%)") +
  ylab("N cycling microbial ASV ratio") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
cn.microbe


vol.microbe <- ggplot(plot.dat, aes(x = plot_vol, y = n_asv_ratio2,
                                    fill = FAST_prop)) +
  colour_PA+
  gui+
  geom_point(size = 5, shape = 21, colour = "black")+
  geom_smooth(method = "lm",
              formula = y ~ x,
              colour = "black") +
  stat_poly_eq(aes(label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               size = 5.5)+
  xlab(expression("Plot volume (m"^3*")"))+
  ylab("N cycling microbial ASV ratio") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
vol.microbe

#-------------------------------------------------------------------------------
# Combine figures
#1500 x 600 pixels
fig <- ggarrange(plot.c, cn.microbe, vol.microbe,
                   ncol = 3, nrow = 1,
                   common.legend= TRUE,
                   legend = "top"
)

fig

