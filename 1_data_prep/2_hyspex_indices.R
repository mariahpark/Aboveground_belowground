# Calculate spectral indices from Hyspex data in July

rm(list=ls())
library(dplyr)
library(readr)
library(data.table)

# Read data
setwd("C:/Users/maria/Desktop/Research/2022/ch3_clean")
jul <- fread("FAB2_20220721_HySpex_VN.csv")


# Cell = pixel number
# ID is unique for polygons in GIS
# Plot = plot
# Has been BRDF corrected

# Plot spectra
plot(as.numeric(colnames(jul[1, c(-3:-1)])), as.matrix(jul[1500, c(-3:-1)])[1,])

# Check that NIR > 0.06
hist(jul$`805`)

# Make sure reflectance values are numeric
jul[ , 5:353] <- lapply(jul[ , 5:353], function(x) as.numeric(as.character(x)))

# Rescale values back to decimals (divide by 1000)
divide_columns_by_1000 <- function(df) {
  df[, 5:353] <- df[, 5:353] / 1000
  return(df)
}

jul <- divide_columns_by_1000(jul)


#-------------------------------------------------------------------------------
# Calculate spectral N indices
    
df_indices <- jul %>%
  mutate(
    NDVI = (`750`-`705`) / (`750`+`705`),
    CCIred_edge = (`800`/`740`)-1, # Red-edge chl index I - Li et al. 2012
    NDRE = (`800`-`720`)/(`800`+`720`), # Barnes et al. 2000, Morier et al. 2015
    A1510 = (1-`1510`) # Biochemical absorption zone
  )

# Keep ID columns and new indices
jul.indices <- cbind(df_indices[ ,c(2:4)], df_indices %>% select(
                  NDVI, CCIred_edge, NDRE, A1510))




#-------------------------------------------------------------------------------
# Check NDVI threshold
jul.indices <- as.data.frame(jul.indices)

plot(jul.indices$NDVI) # there are cells with NDVI < 0.4 (grass/shrub/soil)

# Filter NDVI >= 0.4 in a list of dataframes
filtered.indices <- jul.indices[jul.indices$NDVI >= 0.4, ]


#-------------------------------------------------------------------------------

# Mean and sd for each spectral index
average_indices_by_plot <- function(df) {
    # Make cell and ID non-numeric
    df$cell <- as.factor(df$cell)
    df$ID <- as.factor(df$cell)
    
    # Select only numeric columns + plot
    df_numeric <- df %>%
      select(Plot, where(is.numeric))
    
    df_numeric %>%
      group_by(Plot) %>%
      dplyr::summarise( # have to specify summarise is from dplyr
        across(
          .cols = where(is.numeric),
          .fns = list(mean = ~mean(.x, na.rm = TRUE),
                      sd = ~sd(.x, na.rm = TRUE)),
          .names = "{.col}_{.fn}"
        ),
        .groups = "drop"
      )
  }

#plot.indices <- average_indices_by_plot(indices.list)
plot.indices.filt <- average_indices_by_plot(filtered.indices)

#-------------------------------------------------------------------------------
# Add date column
date_vector <- as.Date("2022-07-21")

plot.indices.filt$date <- as.Date("2022-07-21")

write.csv(plot.indices.filt,
          "C:/Users/maria/Desktop/Research/2022/input_processed/hyspex.indices.1.30.26.csv")












