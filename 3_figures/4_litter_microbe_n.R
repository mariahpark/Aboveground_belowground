# Fig. 4
# Connections between litter cover, microbes, net nitrification, canopy N

rm(list=ls())
#libraries
library(ggplot2)
library(dplyr)
library(data.table)
library(conflicted)
library(ggpubr)
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
# Microbial ratio calc:
# Total N cyclers
plot.dat$n_microbes <- plot.dat$nitrification + plot.dat$nitrogen_fixation +
  plot.dat$ureolysis 

plot.dat$non_n_microbes <- plot.dat$Total_ASV - plot.dat$n_microbes

# n cycling / non-n cycling microbes  
plot.dat$n_asv_ratio2 <- plot.dat$n_microbes / plot.dat$non_n_microbes

lit.dat <- plot.dat %>% filter(!is.na(Litter))

#-------------------------------------------------------------------------------
# What influences litter? Plot volume and AM proportion

# Narrow down potential variables
mod <- lm(Litter ~ FAST_prop + plot_vol + faith.no.root.PD + AM_prop, data = plot.dat)
summary(mod)

# A strong model
mod <- lm(Litter ~ plot_vol + AM_prop + swir.cv, data = plot.dat)
summary(mod)

# Make predicted litter variable Multiple R-squared:  0.8189
mod <- lm(Litter ~ AM_prop + A1510_mean, data = plot.dat)
summary(mod)


#-------------------------------------------------------------------------------
# Develop model to predict litter for plots without percent cover measurements

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

# Plot to visualize model fit
test <- ggplot(plot.dat, aes(x = A1510_mean, y = Litter,
                             color = AM_prop)) +
  scale_colour_viridis_c(direction = 1)+ 
  geom_point(size=3)+
  geom_smooth(
    method = "lm",
    formula = y ~ x)+
  stat_poly_eq(method = "lm",
               formula = y ~ x,
               label.x = "left",
               label.y = "top",
               #col = 2, coordinates
               size = 4)+
  xlab("Canopy nitrogen index") +
  ylab("Combined training and testing Litter") +
  labs(color = "AM proportion")+
  theme_classic()
test

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
# Litter ~ Microbes

lit.mic <- ggplot(plot.dat, aes(x = Litter, y = n_asv_ratio2))+
  geom_point(shape = 21, size = 4,
             aes(fill = FAST_prop, colour = pred_obs, stroke = 1.25)) +
  scale_colour_manual(" " , values =
                        c("observed" = "grey", "predicted" = "black")) +
  colour_PA +
  geom_smooth(method = "lm",
              formula = y ~ poly(x),
              color = "black")+
  stat_poly_eq(aes(x =  Litter, y = n_asv_ratio2,
                   label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               data = plot.dat,
               formula = y ~ x,
               parse = TRUE,
               label.x = "left",
               label.y = "top",
               color = "black",
               inherit.aes = FALSE,
               size = 5
  )+
  ylab("N cycling microbial ASV ratio")+
  xlab("Percent cover of litter")+
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = breaks_pretty(n = 3))+
  theme_classic()+
  theme(legend.position="top",
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 17),
        plot.title = element_text(size = 19, face = "italic"))+
  gui

lit.mic

#-------------------------------------------------------------------------------
# Litter ~ Nitrification

lit.nit <- ggplot(plot.dat, aes(x = Litter,
                                y = Net.Nitrification.rate..ug.N.g.Soil..1.d.1.))+
  geom_point(shape = 21, size = 4,
             aes(fill = FAST_prop, colour = pred_obs, stroke = 1.25)) +
  scale_colour_manual(" " , values =
                        c("observed" = "grey", "predicted" = "black")) +
  colour_PA +
  geom_smooth(method = "lm",
              formula = y ~ poly(x),
              color = "black")+
  stat_poly_eq(aes(x =  Litter, y = Net.Nitrification.rate..ug.N.g.Soil..1.d.1.,
                   label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               data = plot.dat,
               formula = y ~ x,
               parse = TRUE,
               label.x = "left",
               label.y = "top",
               color = "black",
               inherit.aes = FALSE,
               size = 5
  )+
  ylab(expression("Net nitrification rate (ug N g soil"^-1*" d"^-1*")"))+
  xlab("Percent cover of litter")+
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = breaks_pretty(n = 3))+
  theme_classic()+
  theme(legend.position="top",
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 17),
        plot.title = element_text(size = 19, face = "italic"))+
  gui

lit.nit

#-------------------------------------------------------------------------------
# Microbes ~ Nitrification

mic.nit <- ggplot(plot.dat, aes(x = n_asv_ratio2,
                                y = Net.Nitrification.rate..ug.N.g.Soil..1.d.1.))+
  geom_point(shape = 21, size = 4,
             aes(fill = FAST_prop, stroke = 1.25)) +
  colour_PA +
  geom_smooth(method = "lm",
              formula = y ~ poly(x),
              color = "black")+
  stat_poly_eq(aes(x =  n_asv_ratio2, y = Net.Nitrification.rate..ug.N.g.Soil..1.d.1.,
                   label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               data = plot.dat,
               formula = y ~ x,
               parse = TRUE,
               label.x = "left",
               label.y = "top",
               color = "black",
               inherit.aes = FALSE,
               size = 5
  )+
  ylab(expression("Net nitrification rate (ug N g soil"^-1*" d"^-1*")"))+
  xlab("N cycling microbial ASV ratio")+
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = breaks_pretty(n = 3))+
  theme_classic()+
  theme(legend.position="top",
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 17),
        plot.title = element_text(size = 19, face = "italic"))+
  gui

mic.nit

#-------------------------------------------------------------------------------
# Nitrification ~ Canopy N

nit.index <- ggplot(plot.dat, aes(x = Net.Nitrification.rate..ug.N.g.Soil..1.d.1.,
                                  y = CCIred_edge_mean))+
  geom_point(shape = 21, size = 4,
             aes(fill = FAST_prop, stroke = 1.25)) +
  colour_PA +
  geom_smooth(method = "lm",
              formula = y ~ poly(x),
              color = "black")+
  stat_poly_eq(aes(x =  Net.Nitrification.rate..ug.N.g.Soil..1.d.1., y = CCIred_edge_mean,
                   label = paste(..rr.label..,
                                 ..p.value.label.., sep = "~~~")),
               data = plot.dat,
               formula = y ~ x,
               parse = TRUE,
               label.x = "left",
               label.y = "top",
               color = "black",
               inherit.aes = FALSE,
               size = 5
  )+
  xlab(expression("Net nitrification rate (ug N g soil"^-1*" d"^-1*")"))+
  ylab("Canopy N index")+
  scale_y_continuous(labels = label_number(accuracy = 0.01),
                     breaks = breaks_pretty(n = 3))+
  theme_classic()+
  theme(legend.position="top",
        axis.title.x = element_text(size = 17),
        axis.title.y = element_text(size = 17),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.text = element_text(size = 16),
        legend.title = element_text(size = 17),
        plot.title = element_text(size = 19, face = "italic"))+
  gui

nit.index


#-------------------------------------------------------------------------------
# Combine figures
#1200 x 1000 pixels
fig.4 <- ggarrange(lit.mic, lit.nit, mic.nit, nit.index,
                   ncol = 2, nrow = 2,
                   common.legend= TRUE,
                   legend = "top"
)

fig.4

