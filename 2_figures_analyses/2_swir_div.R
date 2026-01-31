# Fig. 2
# Connecting remote sensing to aboveground measurements

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

# Only polycultures for NBE
poly <- plot.dat %>% filter(SR.y != 1)
#-------------------------------------------------------------------------------
# Legend position
gui <- guides(fill = guide_colourbar(barwidth = 15, 
                                     barheight = 0.7, 
                                     title.position = "top",
                                     title.hjust = 0.5))

# Color palette
colour_PA_FC <- scale_fill_viridis("Fractional cover",
                                   option = "D",
                                   direction = -1,
                                   begin = 0,
                                   end = 1,
                                   limits = c(0, 1),
                                   breaks = c(0, 0.5, 1))

#-------------------------------------------------------------------------------
# Phylogenetic diversity ~ SWIR CV

swir.pd <- ggplot(plot.dat, aes(x = faith.no.root.PD, y = swir.cv,
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
  xlab("Phylogenetic diversity (Mya)") +
  ylab(" ") +
  theme_classic()+
  scale_x_continuous(breaks = c(0, 500, 1000))+
  theme(axis.title.y = element_text(size = 18))+
  theme(axis.title.x = element_text(size = 18))+
  theme(
    axis.text.x = element_text(size = 16),  # Change x-axis text size
    axis.text.y = element_text(size = 16),   # Change y-axis text size
    legend.text = element_text(size = 15),  # Change legend text size
    legend.title = element_text(size = 16)  # Change legend title size
  )
swir.pd

#-------------------------------------------------------------------------------
# Canopy complexity
 
swir.rumple <- ggplot(plot.dat, aes(x = rumple, y = swir.cv,
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
  xlab("Rumple") +
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
swir.rumple

#-------------------------------------------------------------------------------
# NBE

swir.nbe <- ggplot(poly, aes(x = NE, y = swir.cv,
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
  xlab(expression("Net biodiversity effect (m"^3*" y"^-1*" ha"^-1*")"))+
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
swir.nbe


#-------------------------------------------------------------------------------
# Combine figures
# 1500 x 600 pixels

fig.2 <- ggarrange(swir.pd, swir.rumple, swir.nbe,
                   ncol = 3, nrow = 1,
                   common.legend= TRUE,
                   legend = "top"
)

fig.2


fig.2 <- annotate_figure(
  fig.2,
  left = text_grob("SWIR CV", size = 18, rot = 90
  )
)
fig.2

