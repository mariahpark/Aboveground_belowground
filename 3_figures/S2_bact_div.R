# Supplementary Fig. 2
# Fractional cover ~ microbes

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
# Test what variable is driving decrease in microbial diversity and evenness

# FC is significant
mod <- lm(Observed_richness ~ FC*faith.no.root.PD, data = plot.dat)
summary(mod)

mod <- lm(Shannon_diversity ~ FC*faith.no.root.PD, data = plot.dat)
summary(mod)

mod <- lm(Pielou_evenness ~ FC*faith.no.root.PD, data = plot.dat)
summary(mod)

#-------------------------------------------------------------------------------
# Palette and figure legend

gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

colour_PA_PD <- scale_fill_continuous("Phylogenetic diversity",
                                      limits = c(0, 1170),
                                      breaks = c(0, 500, 1000))

#-------------------------------------------------------------------------------
# Fractional cover ~ microbes

fc.rich <- ggplot(plot.dat, aes(x = FC, y = Observed_richness,
                                fill = faith.no.root.PD)) +
  colour_PA_PD+
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
  ylab("Observed microbial richness") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
fc.rich

# Shannon diversity
fc.shan <- ggplot(plot.dat, aes(x = FC, y = Shannon_diversity,
                                fill = faith.no.root.PD)) +
  colour_PA_PD+
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
  ylab("Shannon diversity of microbes") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
fc.shan

# evenness
fc.even <- ggplot(plot.dat, aes(x = FC, y = Pielou_evenness,
                                fill = faith.no.root.PD)) +
  colour_PA_PD+
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
  ylab("Pielou evenness of microbes") +
  theme_classic()+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
fc.even


#-------------------------------------------------------------------------------
# Combine figures

fig <- ggarrange(fc.rich, fc.shan, fc.even,
                 ncol = 3, nrow = 1,
                 common.legend= TRUE,
                 legend = "top"
                 #labels = c("C","D")
)

fig


annotate_figure(
  fig,
  bottom = text_grob("Fractional cover", size = 18
  )
)


