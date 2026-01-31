# Combining dataframes

rm(list=ls())
library(dplyr)
library(readr)
library(purrr)
library(stringr)
library(data.table)
library(conflicted)
library(tidyr)
conflicts_prefer(dplyr::summarise)
conflicts_prefer(plyr::mutate)
conflicts_prefer(dplyr::filter)

setwd("C:/Users/maria/Desktop/Research/2022/input_raw")
nbe <- read.csv("2_22.plot_NBE (2025-6-27).csv")

setwd("C:/Users/maria/Desktop/Research/2022/ch3_clean")
div <- read.csv("2_combined.div.metrics.csv")
herb <- read.csv("2_percent.cover.24.csv")
cn <- read.csv("3_FAB_CN_tins2_2022.csv")
ph <- read.csv("3_FAB2_Soil_pH.csv")
nmin <- read.csv("3_nmin_2022.csv")
bact <- read.csv("3_bac_diversity_metrics.csv")
microbes <- read.csv("3.1_microbes.csv")
lidar.prop <- read.csv("lidar.fast.1.22.26.csv")

setwd("C:/Users/maria/Desktop/Research/2022/input_processed")
canopy.cn <- read.csv("canopy.cn.sp.12.12.25.csv")

# spectra
overall.cv <- read.csv("overall.cv.1.30.26.csv") 
vis.cv <- read.csv("vis.cv.1.30.26.csv")
nir.cv <- read.csv("nir.cv.1.30.26.csv")
swir.cv <- read.csv("swir.cv.1.30.26.csv")
spectral.indices <- read.csv("hyspex.indices.1.30.26.csv")


#-------------------------------------------------------------------------------
# To start, get monthly timepoints
# Get date columns into standard date-time format

lidar.prop$date <- as.POSIXct(lidar.prop$date, format = "%m/%d/%Y")


# Add month column to dataframes with more than 1 month

add_month_column <- function(df, date_column) {
  df$month <- format(as.Date(df[[date_column]]), "%m")
  df$month <- as.numeric(df$month)
  return(df)
}

lidar.prop <- add_month_column(lidar.prop, "date")


overall.cv <- add_month_column(overall.cv, "date")
vis.cv <- add_month_column(vis.cv, "date")
nir.cv <- add_month_column(nir.cv, "date")
swir.cv <- add_month_column(swir.cv, "date")
spectral.indices <- add_month_column(spectral.indices, "date")


#-------------------------------------------------------------------------------
# Tidy up consistencies

# Make sure "Plot" is consistently named
colnames(nbe)[2] <- "Plot"
colnames(div)[2] <- "Plot"
colnames(herb)[4] <- "Plot"

# Rename cv columns for overall, vis, nir, swir
colnames(overall.cv)[4] <- "overall.cv"
colnames(vis.cv)[4] <- "vis.cv"
colnames(nir.cv)[4] <- "nir.cv"
colnames(swir.cv)[4] <- "swir.cv"

#-------------------------------------------------------------------------------
# Combine plot-level dataframes based on month

df.list <- list(lidar.prop, overall.cv, vis.cv, nir.cv, swir.cv, spectral.indices)

# Make sure "Plot" is consist
df.list <- lapply(df.list, function(df) {
  df$Plot <- as.numeric(df$Plot)
  return(df)
})

# Filter out NA plots
df.list <- map(df.list, ~filter(.x, !is.na(Plot)))

plot.df <- reduce(df.list, full_join, by = c("month", "Plot"))

#-------------------------------------------------------------------------------
# Just July timepoint at the plot level

july.df <- plot.df %>% filter(month == 7)

# Merge in the rest of the plot-level data
plot.list <- list(july.df, nbe, div, herb, cn, ph, nmin, bact,
                  canopy.cn, microbes)

# Make sure "Plot" is consistently numeric
plot.list <- lapply(plot.list, function(df) {
  df$Plot <- as.numeric(df$Plot)
  return(df)
})


# Filter out NA plots
plot.list <- map(plot.list, ~filter(.x, !is.na(Plot)))

plot.all <- reduce(plot.list, full_join, by = "Plot")

# Save only original small 10x10m plots
# plot.dat <- plot.all %>%
#   filter(!is.na(nitrification))
plot.dat <- plot.all %>%
  filter(Plot >= 1 & Plot <= 148)

# Save only one row per plot
plot.dat <- plot.dat[!duplicated(plot.dat$Plot), ]


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Outlier detection and removal for net mineralization data


plot.dat$Net.Nitrification.rate..ug.N.g.Soil..1.d.1. <-
  as.numeric(plot.dat$Net.Nitrification.rate..ug.N.g.Soil..1.d.1.)
plot.dat$Net.Ammonification.rate..ug.N.g.Soil..1.d.1. <-
  as.numeric(plot.dat$Net.Ammonification.rate..ug.N.g.Soil..1.d.1.)
plot.dat$Net.Mineralization.rate..ug.N.g.Soil..1.d.1. <-
  as.numeric(plot.dat$Net.Mineralization.rate..ug.N.g.Soil..1.d.1.)


# Only polycultures for NBE

poly <- plot.dat %>% filter(SR.y != 1)


# Check outliers in Ammonification
outlier_am <- plot.dat %>%
  mutate(
    mean = mean(Net.Ammonification.rate..ug.N.g.Soil..1.d.1., na.rm = TRUE),
    sd = sd(Net.Ammonification.rate..ug.N.g.Soil..1.d.1.,  na.rm = TRUE),
    is_outlier = abs(Net.Ammonification.rate..ug.N.g.Soil..1.d.1. - mean) > 2 * sd
  ) %>%
  filter(is_outlier) %>%
  select(Plot, Net.Ammonification.rate..ug.N.g.Soil..1.d.1.)

print(outlier_am)

# Check outliers in Nitrification
outlier_nit <- plot.dat %>%
  mutate(
    mean = mean(Net.Nitrification.rate..ug.N.g.Soil..1.d.1., na.rm = TRUE),
    sd = sd(Net.Nitrification.rate..ug.N.g.Soil..1.d.1.,  na.rm = TRUE),
    is_outlier = abs(Net.Nitrification.rate..ug.N.g.Soil..1.d.1. - mean) > 2 * sd
  ) %>%
  filter(is_outlier) %>%
  select(Plot, Net.Nitrification.rate..ug.N.g.Soil..1.d.1.)

print(outlier_nit)


# Check outliers in Mineralization
outlier_nmin <- plot.dat %>%
  mutate(
    mean = mean(Net.Mineralization.rate..ug.N.g.Soil..1.d.1., na.rm = TRUE),
    sd = sd(Net.Mineralization.rate..ug.N.g.Soil..1.d.1.,  na.rm = TRUE),
    is_outlier = abs(Net.Mineralization.rate..ug.N.g.Soil..1.d.1. - mean) > 2 * sd
  ) %>%
  filter(is_outlier) %>%
  select(Plot, Net.Mineralization.rate..ug.N.g.Soil..1.d.1.)

print(outlier_nmin)

#-------------------------------------------------------------------------------
# Take out outliers

# Ammonification
plot.dat <- plot.dat %>%
  mutate(Net.Ammonification.rate..ug.N.g.Soil..1.d.1. =
           if_else(Plot %in% outlier_am$Plot, NA, Net.Ammonification.rate..ug.N.g.Soil..1.d.1.)
  )

plot.dat <- plot.dat %>%
  mutate(Net.Nitrification.rate..ug.N.g.Soil..1.d.1. =
           if_else(Plot %in% outlier_nit$Plot, NA, Net.Nitrification.rate..ug.N.g.Soil..1.d.1.)
  )

plot.dat <- plot.dat %>%
  mutate(Net.Mineralization.rate..ug.N.g.Soil..1.d.1. =
           if_else(Plot %in% outlier_nmin$Plot, NA, Net.Mineralization.rate..ug.N.g.Soil..1.d.1.)
  )


#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
# Calculate N-cycling microbe ratio

setwd("C:/Users/maria/Desktop/Research/2022/ch3_clean")
dat <- read.csv("3.1_FAB2_Soils_16S_rare_ASV_Tax.csv")

# Delete LTERYEAR
dat <- dat[, -1]
#-------------------------------------------------------------------------------
# Total ASV

# Summing abundances of ASVs

sum_df <- bind_rows(
  dat[1, ],  # keep first row
  dat %>%
    summarise(across(where(is.numeric), ~sum(.x, na.rm = TRUE))) %>%
    mutate(!!names(dat)[1] := "Total_ASV")  # label first column
)

# Transpose the dataframe

dat.t <- as.data.frame(t(sum_df))

# Save rownames as a column
dat.t$Plot <- rownames(dat.t)


# Move col to front

dat.t <- dat.t[, c("Plot", setdiff(names(dat.t), "Plot"))]

# Make first row the column names
colnames(dat.t) <- as.character(dat.t[1, ])   # set first row as header
dat.t <- dat.t[-1, ]                          # remove the first row

# Plot renaming
colnames(dat.t)[1] <- "Plot"
dat.t$Plot <- gsub("FAB2Plot", "", dat.t$Plot)

# Delete unnecessary rows/columns
dat.t <- dat.t[ -( (nrow(dat.t)-5):nrow(dat.t) ),!(names(dat.t) %in% "ASV0001") ]

# Total ASV as numeric
dat.t$Total_ASV <- as.numeric(dat.t$Total_ASV)

# Merge dataframes
data <- merge(plot.dat, dat.t, by = "Plot", all = TRUE)


#-------------------------------------------------------------------------------
# Export csv files
fwrite(data, "C:/Users/maria/Desktop/Research/2022/output_processed/plot.dat.1.30.26.csv")

