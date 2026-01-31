# Calculate spectral variation from Hyspex data in July

rm(list=ls())
library(dplyr)
library(readr)
library(data.table)

# Read data
setwd("C:/Users/maria/Desktop/Research/2022/ch3_clean")
jul <- fread("FAB2_20220721_HySpex_VN.csv")

# Add month column
jul$month <- "7"

# Make datatables into dataframes
jul <- as.data.frame(jul)

# Make datatable into list for future step
spectra_list <- list(jul = jul)

#-------------------------------------------------------------------------------
# Filter out low NDVI (< 0.4)

df_indices <- jul %>%
  mutate(
    NDVI = (`750`-`705`) / (`750`+`705`)
  )
  

# Check NDVI threshold
jul.indices <- as.data.frame(df_indices)

# Filter NDVI >= 0.4 in a list of dataframes
filtered.indices <- jul.indices[jul.indices$NDVI >= 0.4, ]

# Remove column named "NDVI" from each dataframe in the list
filtered.spectra <- filtered.indices[, !names(filtered.indices) %in% "NDVI"]

#-------------------------------------------------------------------------------
# Make metadata columns not numeric
make_columns_factors <-  function(df, cols_to_factor) {
    # Only convert columns that exist in the dataframe
    cols_present <- intersect(cols_to_factor, colnames(df))
    df[cols_present] <- lapply(df[cols_present], as.factor)
    return(df)
  }
factor_df <- make_columns_factors(filtered.spectra,
                                    c("cell", "ID", "Plot", "month"))

# Make df into a list for next steps
factor_list <- list(factor_df)


#-------------------------------------------------------------------------------
# Separate out different parts of electromagnetic spectrum
# Visible (400 - 700 nm), NIR (701 - 1300 nm), SWIR (1301 - 2500)

# Define wavelength ranges
visible_range <- 400:700
nir_range     <- 701:1300
swir_range    <- 1301:2500

# Create empty lists to store output
visible_list <- list()
nir_list     <- list()
swir_list    <- list()

# Loop through each dataframe in the list
for (i in seq_along(factor_list)) {
  df <- factor_list[[i]]
  
  # Identify metadata vs wavelength columns
  col_names <- colnames(df)
  wl_cols_numeric <- suppressWarnings(as.numeric(col_names))
  
  # Metadata columns = non-numeric names
  metadata_cols <- col_names[is.na(wl_cols_numeric)]
  
  # Wavelength columns = numeric names
  wavelength_cols <- col_names[!is.na(wl_cols_numeric)]
  numeric_wls <- wl_cols_numeric[!is.na(wl_cols_numeric)]
  
  # Get columns in each spectral range
  visible_cols <- wavelength_cols[numeric_wls %in% visible_range]
  nir_cols     <- wavelength_cols[numeric_wls %in% nir_range]
  swir_cols    <- wavelength_cols[numeric_wls %in% swir_range]
  
  # Combine metadata + spectral range
  visible_list[[paste0("visible_df_", i)]] <- df[, c(metadata_cols, visible_cols), drop = FALSE]
  nir_list[[paste0("nir_df_", i)]]         <- df[, c(metadata_cols, nir_cols), drop = FALSE]
  swir_list[[paste0("swir_df_", i)]]       <- df[, c(metadata_cols, swir_cols), drop = FALSE]
}

#-------------------------------------------------------------------------------
# Calculate coefficient of variation (CV) for spectral reflectance
# Spectral diversity index (Wang et al. 2022)

# Calculate standard deviation and mean value of reflectance


# Mean and sd for each spectral index
spec_div_calc <- function(spectra_list) {
  lapply(spectra_list, function(df) {
    # Make sure cell, ID, month non-numeric
    df$cell <- as.factor(df$cell)
    df$ID <- as.factor(df$ID)
    df$month <- as.factor(df$month)
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
  })
}

spec.div <- spec_div_calc(spectra_list)
vis.div <- spec_div_calc(visible_list)
nir.div <- spec_div_calc(nir_list)
swir.div <- spec_div_calc(swir_list)

#-------------------------------------------------------------------------------
# Calculate CV
plot_cv_fn <- function(df_list) {
  lapply(df_list, function(df) {
    
    # Find matching _sd and _mean column names
    sd_cols <- grep("_sd$", names(df), value = TRUE)
    mean_cols <- gsub("_sd$", "_mean", sd_cols)  # create matching _mean names
    
    # Filter only pairs that actually exist in the dataframe
    valid_pairs <- sd_cols[mean_cols %in% names(df)]
    mean_cols <- mean_cols[mean_cols %in% names(df)]
    
    n_cols <- length(valid_pairs)
    if (n_cols == 0) {
      warning("No valid _sd/_mean pairs found. Returning NULL.")
      return(NULL)
    }
    
    # Compute per-row ratio: sd / mean
    df_ratio <- df %>%
      dplyr::mutate(across(all_of(valid_pairs), 
                           ~ .x / get(gsub("_sd$", "_mean", cur_column())), 
                           .names = "ratio_{.col}")) %>%
      rowwise() %>%
      dplyr::mutate(cv = sum(c_across(starts_with("ratio_")), na.rm = TRUE) / n_cols) %>%
      ungroup()
    
    # Return per-plot summary
    df_ratio %>%
      group_by(Plot) %>%
      dplyr::summarise(cv = mean(cv, na.rm = TRUE), .groups = "drop")
    # taking mean = dividing by number of wavelengths (N)
  })
}

overall.cv.list <- plot_cv_fn(spec.div)
vis.cv.list <- plot_cv_fn(vis.div)
nir.cv.list <- plot_cv_fn(nir.div)
swir.cv.list <- plot_cv_fn(swir.div)


#-------------------------------------------------------------------------------
# Add date column
date_vector <- c("2022-07-21")

all_lists <- list(overall.cv.list, vis.cv.list, nir.cv.list, swir.cv.list)


processed_lists <- lapply(all_lists, function(sublist) {
  df.list <- lapply(seq_along(sublist), function(i) {
    df <- sublist[[i]]
    df$date <- date_vector[i]
    df <- df[, c("date", setdiff(names(df), "date"))]
    return(df)
  })
})


# Combine dataframes within list of lists
bound.list <- lapply(processed_lists, bind_rows)

# Extract dataframes
overall.cv <- bound.list[[1]]
vis.cv <- bound.list[[2]]
nir.cv <- bound.list[[3]]
swir.cv <- bound.list[[4]]


# Export dataframe
write.csv(overall.cv, "C:/Users/maria/Desktop/Research/2022/input_processed/overall.cv.1.30.26.csv")
write.csv(vis.cv, "C:/Users/maria/Desktop/Research/2022/input_processed/vis.cv.1.30.26.csv")
write.csv(nir.cv, "C:/Users/maria/Desktop/Research/2022/input_processed/nir.cv.1.30.26.csv")
write.csv(swir.cv, "C:/Users/maria/Desktop/Research/2022/input_processed/swir.cv.1.30.26.csv")





