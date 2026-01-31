################################################################################
# Estimate foliar C and N for species from ground tissue samples
# Allometry data from:
# https://doi.org/10.6073/pasta/c96c9f2f6cc23b2f419baf7f23f11280

#' -----------------------------------------------------------------------------
#' Libraries
library(data.table)
library(dplyr)
library(stringr)
library(conflicted)
options(scipen = 99999)
conflicts_prefer(data.table::year)
conflicts_prefer(data.table::yday)
conflicts_prefer(dplyr::filter)
#' -----------------------------------------------------------------------------
#' Working path
setwd("C:/Users/maria/Desktop/Research/2022/biomass")
root_path <- "C:/Users/maria/Desktop/Research/2022/biomass/"

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
# Work on large plots and 2022 data

frame_large <- subset(data, Area_m2 == 400)

# Rename plot for new merge with small plots
frame_large$plot_new <- "0"
frame_large[row >= 1 & row <= 10 & column >= 1 & column <= 10, plot_new := paste0(plot, "a")]
frame_large[row >= 1 & row <= 10 & column >= 11 & column <= 20, plot_new := paste0(plot, "b")]
frame_large[row >= 11 & row <= 20 & column >= 1 & column <= 10, plot_new := paste0(plot, "c")]
frame_large[row >= 11 & row <= 20 & column >= 11 & column <= 20, plot_new := paste0(plot, "d")]

# Rename row and columns
frame_large[row == 11, row := 1]
frame_large[row == 12, row := 2]
frame_large[row == 13, row := 3]
frame_large[row == 14, row := 4]
frame_large[row == 15, row := 5]
frame_large[row == 16, row := 6]
frame_large[row == 17, row := 7]
frame_large[row == 18, row := 8]
frame_large[row == 19, row := 9]
frame_large[row == 20, row := 10]

frame_large[column == 11, column := 1]
frame_large[column == 12, column := 2]
frame_large[column == 13, column := 3]
frame_large[column == 14, column := 4]
frame_large[column == 15, column := 5]
frame_large[column == 16, column := 6]
frame_large[column == 17, column := 7]
frame_large[column == 18, column := 8]
frame_large[column == 19, column := 9]
frame_large[column == 20, column := 10]

# Remove edges
frame_large <- frame_large[column != 1,]
frame_large <- frame_large[column != 10,]
frame_large <- frame_large[row != 1,]
frame_large <- frame_large[row != 10,]

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


#-------------------------------------------------------------------------------

setwd("C:/Users/maria/Desktop/Research/2022/ch3_clean")

dat <- read.csv("2.1_FAB2_leaf_CN.csv")

#-------------------------------------------------------------------------------

# For each tree in each month, average the environmental metrics

# Leaf traits
avgs.per.species <- function(dat){
  dat %>%
    group_by(Species_Code) %>%
    summarise(avg.N.percent = mean(Nitrogen.weight.percent),
              avg.C.percent = mean(Carbon.weight.percent),
              avg.CN = mean(C.N)) %>%
    ungroup()
}

avg.c.n.species <- avgs.per.species(dat)
print(avg.c.n.species)

# Make PIBA row
piba = data.frame(Species_Code = "PIBA",
                  avg.N.percent = NA,
                  avg.C.percent = NA,
                  avg.CN = NA)

avg.c.n <- bind_rows(avg.c.n.species, piba)


# Estimate for PIBA -- is a pine but similar in traits to JUVI
# (drought, shade tolerance)
# Average all gymnosperm traits

mean.n.piba <- mean(avg.c.n$avg.N.percent[avg.c.n$Species_Code %in%
                                            c("JUVI","PIRE","PIST")])

mean.c.piba <- mean(avg.c.n$avg.C.percent[avg.c.n$Species_Code %in%
                                            c("JUVI","PIRE","PIST")])

mean.cn.piba <- mean(avg.c.n$avg.CN[avg.c.n$Species_Code %in%
                                      c("JUVI","PIRE","PIST")])


#-------------------------------------------------------------------------------
# Include PIBA estimates

# Assign values
avg.c.n$avg.N.percent[avg.c.n$Species_Code == "PIBA"] <- mean.n.piba
avg.c.n$avg.C.percent[avg.c.n$Species_Code == "PIBA"] <- mean.c.piba
avg.c.n$avg.CN[avg.c.n$Species_Code == "PIBA"] <- mean.cn.piba

#-------------------------------------------------------------------------------
# N percent weighted by wood volume of species per plot


# Species codes
species_codes <- c("TIAM" = "Tilia americana",
                   "QUMA" = "Quercus macrocarpa",
                   "QURU" = "Quercus rubra",
                   "ACNE" = "Acer negundo",
                   "QUAL" = "Quercus alba",
                   "QUEL" = "Quercus ellipsoidalis",
                   "QUEL" = "Quercus ellipsodalis",
                   "ACRU" = "Acer rubrum",
                   "BEPA" = "Betula papyrifera",
                   "JUVI" = "Juniperus virginiana",
                   "PIRE" = "Pinus resinosa",
                   "PIBA" = "Pinus banksiana",
                   "PIST" = "Pinus strobus")

avg.c.n$species <- avg.c.n$Species_Code
avg.c.n <- avg.c.n %>%
  mutate(species = str_replace_all(species, species_codes))

dat.merged <- merge(species_inventories, avg.c.n, by = "species")

# Multiply N% and C:N by volume proportion of each species per plot
dat.merged$N.vol.sp <- dat.merged$avg.N.percent * dat.merged$tree_vol

dat.merged$CN.vol.sp <- dat.merged$avg.CN * dat.merged$tree_vol

#-------------------------------------------------------------------------------
# Sum N% and CN by plot
# Leaf traits
avg.plot <- function(dat){
  dat %>%
    group_by(plot_new) %>%
    summarise(canopy.N.percent.sp = sum(N.vol.sp),
              canopy.CN.sp = sum(CN.vol.sp)) %>%
    ungroup()
}

plot.dat <- avg.plot(dat.merged)
print(plot.dat)

# Rename Plot
colnames(plot.dat)[1] <- "Plot"

#-------------------------------------------------------------------------------
# Export
fwrite(plot.dat, "C:/Users/maria/Desktop/Research/2022/input_processed/canopy.cn.sp.12.12.25.csv")


