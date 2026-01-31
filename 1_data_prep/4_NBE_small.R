################################################################################
#' @title Net biodiversity effect
################################################################################

#' @description Estimation of the net biodiversity effect
#' 
#' @return A tiff file

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(dplyr)
library(conflicted)
options(scipen = 99999)
conflicts_prefer(data.table::year)
conflicts_prefer(data.table::yday)
conflicts_prefer(dplyr::filter)
#' -----------------------------------------------------------------------------
#' Working path
setwd("C:/Users/maria/Desktop/Research/2022/biomass")
root_path <- "C:/Users/maria/Desktop/Research/2022/biomass/"
#root_path <- "G:/Projects/LiDAR/data"

#' -----------------------------------------------------------------------------
#' Processing

# Load data
data <- fread(paste0(root_path, "fab2_allometry.csv"))

# Define date
data$measurement_date <- as.Date(data$measurement_date, format= "%m/%d/%Y")
data$measurement_date <- as.IDate(data$measurement_date)
data$measurement_year <- year(data$measurement_date)

# Remove data minor errors
data[species == "Juniperus virginia", species := "Juniperus virginiana"]
data[species == "Tilia america", species := "Tilia americana"]

data[plot == "1020-154", plot := "1020_154"]
data[plot == "1020-2016", plot := "1020_2016"]
data[plot == "1020-2017", plot := "1020_2017"]
data[plot == "1020-2018", plot := "1020_2018"]
data[plot == "1011-149", plot := "1011_149"]
data[plot == "1011-150", plot := "1011_150"]
data[plot == "1019-152", plot := "1019_152"]
data[plot == "1019-153", plot := "1019_153"]

#Removing columns
data <- data[, c("year_planted", 
                 "species_richness", 
                 "species", 
                 "treatment", 
                 "Area_m2",
                 "block",
                 "plot",
                 "row",
                 "column",
                 "position",
                 "individual_id",
                 "survey",
                 "measurement_date",
                 "measurement_year",
                 "deadmissing",
                 "V.conoid_conoidoid_infill",
                 "Biomass.conoid_conoidoid_infill")]

# Transform volume - change cm^3 -> m^3
data$V.conoid_conoidoid_infill <- data$V.conoid_conoidoid_infill/1000000

#-------------------------------------------------------------------------------
# Work on small plots and 2022 data 

frame_small <- subset(data, Area_m2 == 100)

# Rename row and columns
frame_small[row == 11, row := 1]
frame_small[row == 12, row := 2]
frame_small[row == 13, row := 3]
frame_small[row == 14, row := 4]
frame_small[row == 15, row := 5]
frame_small[row == 16, row := 6]
frame_small[row == 17, row := 7]
frame_small[row == 18, row := 8]
frame_small[row == 19, row := 9]
frame_small[row == 20, row := 10]

frame_small[column == 11, column := 1]
frame_small[column == 12, column := 2]
frame_small[column == 13, column := 3]
frame_small[column == 14, column := 4]
frame_small[column == 15, column := 5]
frame_small[column == 16, column := 6]
frame_small[column == 17, column := 7]
frame_small[column == 18, column := 8]
frame_small[column == 19, column := 9]
frame_small[column == 20, column := 10]

# Remove edges
frame_small <- frame_small[column != 1,]
frame_small <- frame_small[column != 10,]
frame_small <- frame_small[row != 1,]
frame_small <- frame_small[row != 10,]

# Rename plot for new merge with large plots
frame_small$plot_new <- frame_small$plot

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Just small plots

trees <- frame_small

# Take out 20x20 plots
trees$plot_new <- as.numeric(trees$plot_new)
trees <- trees[!is.na(trees$plot_new), ]
#-------------------------------------------------------------------------------
# Estimation of tree volume growth from 2021 to 2022.

# # Reshape 2021 #by plot
# trees_2021 <- subset(trees, year(measurement_date) == 2021)
# trees_2021 <- trees_2021[, .(Volume_2021 = sum(V.conoid_conoidoid_infill, na.rm = TRUE), 
#                              measurement_date_2021 = mean(measurement_date, na.rm = TRUE),
#                              year_planted_2021 = mean(year_planted, na.rm = TRUE)),
#                          by = c("plot", "plot_new", "species")]
# 
# trees_2022 <- subset(trees, year(measurement_date) == 2022)
# trees_2022 <- trees_2022[, .(Volume_2022 = sum(V.conoid_conoidoid_infill, na.rm = TRUE), 
#                              measurement_date_2022 = mean(measurement_date, na.rm = TRUE),
#                              year_planted_2022 = mean(year_planted, na.rm = TRUE)),
#                          by = c("plot", "plot_new", "species")]
# 
# plot_merge <- merge(trees_2021, trees_2022, by = c("plot", "plot_new", "species"))

##OG
trees_2021 <- subset(trees, year(measurement_date) == 2021)
trees_2021 <- trees_2021[, c("plot", "plot_new", "individual_id", 
                             "species", "year_planted",
                             "deadmissing", "measurement_date", "V.conoid_conoidoid_infill")]
colnames(trees_2021)[6:8] <- c("deadmissing_2021", "date_2021", "volume_2021")

# Reshape 2022
trees_2022 <- subset(trees, year(measurement_date) == 2022)
trees_2022 <- trees_2022[, c("plot", "plot_new", "individual_id", 
                             "species", "year_planted",
                             "deadmissing", "measurement_date", "V.conoid_conoidoid_infill")]
#trees_2022 <- trees_2022[deadmissing != "Yes", ]
#trees_2022 <- trees_2022[, c(1:3, 5:8)]
colnames(trees_2022)[6:8] <- c("deadmissing_2022", "date_2022", "volume_2022")

# Merge years
inventories <- merge(trees_2021, trees_2022, by = c("plot", "plot_new", "individual_id", "species", "year_planted"),
                     all.x = TRUE, all.y = TRUE)

# Add potential missing dates based on averages
mean(yday(inventories$date_2022), na.rm = TRUE)
inventories[is.na(date_2022), date_2022 := as.IDate("2022-10-12")]

mean(yday(inventories$date_2021), na.rm = TRUE)
inventories[is.na(date_2021), date_2021 := as.IDate("2021-11-29")]

# Remove trees that were replanted in after 2019 (Antonio did including 2019)
inventories <- inventories[year_planted != 2022 & year_planted != 2021 &
                             year_planted != 2020 & year_planted != 2019, ]

# Estimate AWP per tree
inventories$tree_AWP <- (inventories$volume_2022 - inventories$volume_2021) /
  ((inventories$date_2022 - inventories$date_2021)/365.25)

# Include the effect of mortality
inventories[!is.na(volume_2021) & is.na(volume_2022), tree_AWP := 0]


# ------------------------------------------------------------------------------
# Productivity per species
# species_inventories <- inventories[, .(ntrees = .N,
#                                        tree_AWP = sum(tree_AWP, na.rm = TRUE)),
#                                    by = c("plot_new", "species")]
# species_inventories$proportion <- species_inventories$ntrees/64  ##divide by 64

# Productivity per species - remove NA trees completely
species_inventories <- inventories[!is.na(tree_AWP), .(
  ntrees = .N,
  tree_AWP = sum(tree_AWP, na.rm = TRUE)
), by = c("plot_new", "species")]

species_inventories$proportion <- species_inventories$ntrees/64

# For plot-level calcs, export proportion
fwrite(species_inventories, "22.species.proportions.6.27.25.csv")

# ------------------------------------------------------------------------------
# Get plot basic information
summary_plot <- trees_2021[, .(sr = length(unique(species)),
                               mean_year_planted = mean(year_planted)),
                           by = c("plot_new")]

# Some plots were very much replanted in 2019 (like TIAM monoculture;
#include all plots in analysis) Antonio did 2019 filter
summary_plot <- summary_plot[mean_year_planted < 2020]

# ------------------------------------------------------------------------------
# Get monocultures
plots_mono <- summary_plot[sr == 1,]


mono_inventories <- merge(plots_mono, species_inventories, by = "plot_new",
                          all.x = TRUE, all.y = FALSE)

mono_species_AWP <- mono_inventories[, .(plot_AWP = mean(tree_AWP)),
                                     by = "species"]

# ------------------------------------------------------------------------------
# Merge mixtures and monocultures
#AWP = aboveground wood poductivity

# Plots with mean year planted before 2019
species_inventories <- merge(species_inventories, summary_plot, by = "plot_new", all.x = FALSE, all.y = TRUE)
species_inventories <- merge(species_inventories, mono_species_AWP, by = "species")
colnames(species_inventories)[c(4, 8)] <- c("observed_AWP", "monocultures_AWP")

# ------------------------------------------------------------------------------
# Correct by proportion of species
species_inventories$expected_AWP <- species_inventories$monocultures_AWP*species_inventories$proportion
species_inventories <- species_inventories[order(plot_new, species)]

# Density: by hectare
species_inventories$mono_AWP_hec <- species_inventories$monocultures_AWP/0.0081
species_inventories$exp_AWP_hec <- species_inventories$expected_AWP/0.0081
species_inventories$obs_AWP_hec <- species_inventories$observed_AWP/0.0081
species_inventories$obs_exp_AWP_hec <- species_inventories$obs_AWP_hec - species_inventories$exp_AWP_hec

# Productivity calculations for ALL species
tiam <- filter(species_inventories, species == "Tilia americana")
acne <- filter(species_inventories, species == "Acer negundo")
acru <- filter(species_inventories, species == "Acer rubrum")
qual <- filter(species_inventories, species == "Quercus alba")
quru <- filter(species_inventories, species == "Quercus rubra")
quel <- filter(species_inventories, species == "Quercus ellipsoidalis")
quma <- filter(species_inventories, species == "Quercus macrocarpa")
bepa <- filter(species_inventories, species == "Betula papyrifera")
juvi <- filter(species_inventories, species == "Juniperus virginiana")
piba <- filter(species_inventories, species == "Pinus banksiana")
pire <- filter(species_inventories, species == "Pinus resinosa")
pist <- filter(species_inventories, species == "Pinus strobus")

focal.productivity <- rbind(tiam, acne, acru, qual, quru, quel, quma, bepa, juvi, piba, pire, pist)
focal.productivity$difference_AWP <- focal.productivity$observed_AWP - focal.productivity$expected_AWP
fwrite(focal.productivity, paste0(root_path, "22.focal.productivity.9.7.25.csv"))
#-------------------------------------------------------------------------------
#species_inventories$NBE <- species_inventories$observed_AWP - species_inventories$expected_AWP_corrected
BE <- data.table()
idplots <- unique(species_inventories$plot_new)

for(i in 1:length(idplots)) {
  
  # Select plot
  plot <- subset(species_inventories, plot_new == idplots[i])
  
  # Number of species
  Y <- plot$observed_AWP
  M <- plot$expected_AWP
  N <- nrow(plot)
  dRY <- (Y/M) - (1/N)
  covar <- sum((dRY-mean(dRY)) * (M-mean(M)))/N
  SE <- N*covar
  CE <- N*mean(dRY)*mean(M)
  NE <- SE + CE
  
  result <- data.table(plot_new = idplots[i], 
                       CE = CE, 
                       SE = SE, 
                       NE = NE)
  
  BE <- rbind(BE, result)
  
}

# Return values in density

BE$CE <- BE$CE/0.0081
BE$SE <- BE$SE/0.0081
BE$NE <- BE$NE/0.0081

#CE = complementarity effect
#SE = selection effect
#NE = net effect
write.csv(BE, "C:/Users/maria/Desktop/Research/2022/input_raw/2_22.plot_NBE (2025-6-27).csv")
