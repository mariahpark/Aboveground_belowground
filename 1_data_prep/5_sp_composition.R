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

# Rename plot
frame_small$plot_new <- frame_small$plot


#-------------------------------------------------------------------------------
# Only small plots

trees <- frame_small

# Take out 20x20 plots
trees$plot_new <- as.numeric(trees$plot_new)
trees <- trees[!is.na(trees$plot_new), ]

#-------------------------------------------------------------------------------
# Estimation of tree volume in 2022.

# Reshape 2022
trees_2022 <- subset(trees, year(measurement_date) == 2022)
trees_2022 <- trees_2022[, c("plot", "plot_new", "individual_id", 
                             "species", "year_planted",
                             "deadmissing", "measurement_date", "V.conoid_conoidoid_infill")]

colnames(trees_2022)[6:8] <- c("deadmissing_2022", "date_2022", "volume_2022")

# Add potential missing dates based on averages
mean(yday(trees_2022$date_2022), na.rm = TRUE)
trees_2022[is.na(date_2022), date_2022 := as.IDate("2022-10-12")]


# Remove trees that were replanted in after 2019 (Antonio did including 2019)
trees_2022 <- trees_2022[year_planted != 2022 & year_planted != 2021 &
                             year_planted != 2020 & year_planted != 2019, ]

# Wood volume per species - remove NA trees completely
species_inventories <- trees_2022[!is.na(volume_2022), .(
  ntrees = .N,
  tree_vol = sum(volume_2022, na.rm = TRUE)
), by = c("plot_new", "species")]

# Column for total plot volume
plot.vol <- species_inventories[, .(plot_vol = sum(tree_vol)), by = "plot_new"]

species_inventories <- merge(species_inventories, plot.vol, by = "plot_new")

# ------------------------------------------------------------------------------
#categorize: gymnosperms, FAST

# Gymnosperm / Angiosperm
gymno_species <- function(df, species_col) {
  df$Clade <- ifelse(df[[species_col]] %in% c("Tilia americana", "Quercus macrocarpa", "Quercus rubra",
                                              "Acer negundo", "Quercus alba", "Quercus ellipsoidalis",
                                              "Acer rubrum", "Betula papyrifera"), "Angiosperm",
                     ifelse(df[[species_col]] %in% c("Juniperus virginiana", "Pinus resinosa", "Pinus banksiana",
                                                     "Pinus strobus"), "Gymnosperm", NA))
  return(df)
}

# FAST / non_FAST
FAST_species <- function(df, species_col) {
  df$FAST <- ifelse(df[[species_col]] %in% c("Tilia americana", "Quercus macrocarpa", "Quercus rubra",
                                             "Acer negundo", "Quercus alba", "Quercus ellipsoidalis",
                                             "Acer rubrum", "Juniperus virginiana"), "non_FAST",
                    ifelse(df[[species_col]] %in% c("Pinus resinosa", "Pinus banksiana",
                                                    "Pinus strobus","Betula papyrifera"), "FAST", NA))
  return(df)
}

# AM / EM
fungal_assoc <- function(df, species_col) {
  df$Fungal <- ifelse(df[[species_col]] %in% c("Pinus resinosa", "Pinus banksiana",
                                               "Pinus strobus","Betula papyrifera",
                                               "Tilia americana", "Quercus macrocarpa",
                                               "Quercus rubra", "Quercus alba",
                                               "Quercus ellipsoidalis"), "ECM",
                    ifelse(df[[species_col]] %in% c("Acer negundo","Acer rubrum",
                                                    "Juniperus virginiana"), "AM", NA))
  return(df)
}

# Pinus
pinus <- function(df, species_col) {
  df$Pinus <- ifelse(df[[species_col]] %in% c("Pinus resinosa", "Pinus banksiana",
                                               "Pinus strobus"), "Pinus",
                      ifelse(df[[species_col]] %in% c("Acer negundo","Acer rubrum",
                                                      "Juniperus virginiana",
                                                      "Betula papyrifera",
                                                      "Tilia americana", "Quercus macrocarpa",
                                                      "Quercus rubra", "Quercus alba",
                                                      "Quercus ellipsoidalis"), "Non-pinus", NA))
  return(df)
}


species.dat <- gymno_species(species_inventories, "species")
species.dat <- FAST_species(species.dat, "species")
species.dat <- fungal_assoc(species.dat, "species")
species.dat <- pinus(species.dat, "species")

#-------------------------------------------------------------------------------
#calculate proportions by volume

gymno.vol <- species.dat[, .(
  gymno_prop = sum(tree_vol)), by = c("plot_new")]

# gymno proportion: gymno plot vol / total plot vol
gymno_prop <- species.dat[, .(
  gymno_prop = sum(tree_vol)/plot_vol
), by = c("plot_new", "Clade")]

# FAST proportion: FAST plot vol / total plot vol
FAST_prop <- species.dat[, .(
  FAST_prop = sum(tree_vol)/plot_vol
), by = c("plot_new", "FAST")]

# AM proportion: AM plot vol / total plot vol
AM_prop <- species.dat[, .(
  AM_prop = sum(tree_vol)/plot_vol
), by = c("plot_new", "Fungal")]

# Pinus proportion: Pinus plot vol / pinus plot vol
Pinus_prop <- species.dat[, .(
  Pinus_prop = sum(tree_vol)/plot_vol
), by = c("plot_new", "Pinus")]


#-------------------------------------------------------------------------------
# Create gymnosperm, FAST, Fungal, Pinus row for each plot
# Find plots that do NOT have an "FAST" row
plots_missing_FAST_yes <- FAST_prop[FAST == "FAST", unique(plot_new)]
all_plots <- unique(FAST_prop$plot_new)
plots_to_add <- setdiff(all_plots, plots_missing_FAST_yes)

# Create new rows for those plots
new_rows <- data.table(plot_new = plots_to_add, FAST = "FAST")

FAST_infill <- rbind(FAST_prop, new_rows, fill = TRUE)

# Set FAST_prop to 0 if equals NA
FAST_infill$FAST_prop[is.na(FAST_infill$FAST_prop)] <- 0

#-------------------------------------------------------------------------------
# Do same for gymnosperms
plots_missing_gymno_yes <- gymno_prop[Clade == "Gymnosperm", unique(plot_new)]
all_plots <- unique(gymno_prop$plot_new)
plots_to_add <- setdiff(all_plots, plots_missing_gymno_yes)

# Create new rows for those plots
new_rows <- data.table(plot_new = plots_to_add, Clade = "Gymnosperm")

gymno_infill <- rbind(gymno_prop, new_rows, fill = TRUE)

# Set FAST_prop to 0 if equals NA
gymno_infill$gymno_prop[is.na(gymno_infill$gymno_prop)] <- 0

#-------------------------------------------------------------------------------
# Do same for fungal associations
plots_missing_fungal_yes <- AM_prop[Fungal == "AM", unique(plot_new)]
all_plots <- unique(AM_prop$plot_new)
plots_to_add <- setdiff(all_plots, plots_missing_fungal_yes)

# Create new rows for those plots
new_rows <- data.table(plot_new = plots_to_add, Fungal = "AM")

fungal_infill <- rbind(AM_prop, new_rows, fill = TRUE)

# Set FAST_prop to 0 if equals NA
fungal_infill$AM_prop[is.na(fungal_infill$AM_prop)] <- 0

#-------------------------------------------------------------------------------
# Do same for pinus prop
plots_missing_pinus_yes <- Pinus_prop[Pinus == "Pinus", unique(plot_new)]
all_plots <- unique(Pinus_prop$plot_new)
plots_to_add <- setdiff(all_plots, plots_missing_pinus_yes)

# Create new rows for those plots
new_rows <- data.table(plot_new = plots_to_add, Pinus = "Pinus")

pinus_infill <- rbind(Pinus_prop, new_rows, fill = TRUE)

# Set FAST_prop to 0 if equals NA
pinus_infill$Pinus_prop[is.na(pinus_infill$Pinus_prop)] <- 0


#-------------------------------------------------------------------------------
# Have only one FAST proportion and gymnosperm proportion entry per plot

# Gynmnosperm filtering
gymno_filter <- gymno_infill[gymno_infill$Clade != "Angiosperm", ]

df_gymnosperm <- gymno_filter[!duplicated(gymno_filter$plot_new), ]

# FAST filtering
FAST_filter <- FAST_infill[FAST_infill$FAST != "non_FAST", ]

df_FAST <- FAST_filter[!duplicated(FAST_filter$plot_new), ]

# AM filtering
AM_filter <- fungal_infill[fungal_infill$Fungal != "ECM", ]

df_AM <- AM_filter[!duplicated(AM_filter$plot_new), ]

# Pinus filtering
pinus_filter <- pinus_infill[pinus_infill$Pinus != "Non-pinus", ]

df_pinus <- pinus_filter[!duplicated(pinus_filter$plot_new), ]

#-------------------------------------------------------------------------------
# Remerge with master dataframe
species.dat <- merge(species.dat, df_gymnosperm, by = "plot_new")
species.dat <- merge(species.dat, df_FAST, by = "plot_new")
species.dat <- merge(species.dat, df_AM, by = "plot_new")
species.dat <- merge(species.dat, df_pinus, by = "plot_new")

plot.dat <- species.dat[!duplicated(species.dat$plot_new), ]
plot.dat <- select(plot.dat, plot_new, gymno_prop, FAST_prop, AM_prop, Pinus_prop, plot_vol)

#-------------------------------------------------------------------------------
# Merge wiwth LiDAR data
# Read data
setwd("c:/Users/maria/Desktop/Research/2022/input_raw")
lidar <- read.csv("1_LIDAR_2022.csv")

# Rename Plot column
colnames(plot.dat)[1] <- "Plot"

# Merge dataframes
dat <- merge(lidar, plot.dat, by = "Plot")

# Make sure FC is <= 1 (errors if slightly above 1)
dat$FC <- ifelse(dat$FC > 1, 1, dat$FC)

fwrite(dat, "C:/Users/maria/Desktop/Research/2022/input_processed/lidar.fast.1.22.26.csv")

