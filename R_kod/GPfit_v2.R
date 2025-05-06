library(extRemes)
library(dplyr)
library(lubridate)

# List of datasets to loop over
dataset_files <- list(
  "Original" = "final_filtered_station_data.csv",
  "Aligned" = "aligned_final_filtered_station_data.csv"
  #"Original" = "stations_5stations_debug.csv",
  #"Aligned" = "aligned_stations_5stations_debug.csv"
)

# Initialize final results dataframe
final_results_df <- data.frame(
  Station = character(),
  Dataset = character(),
  Type = character(),
  Model = character(),
  Scale_Intercept = numeric(),
  Scale_SE_Intercept = numeric(),
  Scale_Slope = numeric(),
  Scale_SE_Slope = numeric(),
  Shape = numeric(),
  Shape_SE = numeric(),
  Trend_lr_pval = numeric(),
  Shape_lr_pval = numeric(),
  First_Observation_Date = as.Date(character()),
  Last_Observation_Date = as.Date(character()),
  Elapsed_Years = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each dataset
for (dataset_name in names(dataset_files)) {
  file_path <- dataset_files[[dataset_name]]
  data <- read.csv(file_path)
  colnames(data) <- c("From_Date", "To_Date", "Date", "Rainfall_mm", "Quality", "Station_name", "Station_ID")
  
  # Get unique station names
  station_name_list <- unique(data$Station_name)
    
    # Loop over each station
    for (station in station_name_list) {
      
      # Filter for current station
      station_data <- data %>%
        filter(Station_name == station) %>%
        mutate(Date = as.Date(Date),
               Days_Elapsed = as.numeric(Date - min(Date, na.rm = TRUE)),
               Years_Elapsed = as.numeric(difftime(Date, min(Date, na.rm = TRUE), units = "days")) / 365.25)
      
      # Calculate first and last observation dates
      first_obs_date = min(station_data$Date, na.rm = TRUE)
      last_obs_date = max(station_data$Date, na.rm = TRUE)
      
      # Calculate elapsed years based on these new dates
      elapsed_years <- as.numeric(difftime(last_obs_date, first_obs_date, units = "days")) / 365.25
      
      # Define threshold (e.g., 99.5th percentile)
      threshold <- quantile(station_data$Rainfall_mm, 0.995, na.rm = TRUE)
      
      # Declustering 
      station_data_declus <- decluster(station_data$Rainfall_mm, threshold)
      
      # Fit stationary GP models
      stat_fit_gp <- fevd(x = station_data_declus, threshold = threshold, type = "GP", verbose = FALSE)
      stat_fit_gp_summary <- summary(stat_fit_gp)
      
      stat_fit_exp <- fevd(x = station_data_declus, threshold = threshold, type = "Exponential", verbose = FALSE)
      stat_fit_exp_summary <- summary(stat_fit_exp)
      
      # Dataframe for model fitting
      GP_df_year_elaps <- data.frame(rain_data = station_data_declus, years = station_data$Years_Elapsed)
      
      print(station)
      print(dataset_name)
      
      # Fit non-stationary models
      nonstat_fit_gp <- fevd(x = station_data_declus, 
                   data = GP_df_year_elaps, 
                   threshold = threshold, 
                   scale.fun = ~years, 
                   type = "GP",
                   use.phi = TRUE,
                   time.units = "years",
                   verbose = FALSE)
      nonstat_fit_gp_summary <- summary(nonstat_fit_gp)
      
      nonstat_fit_exp <- fevd(x = station_data_declus, 
                             data = GP_df_year_elaps, 
                             threshold = threshold, 
                             scale.fun = ~years, 
                             type = "Exponential",
                             use.phi = TRUE,
                             time.units = "years",
                             verbose = FALSE)
      
      nonstat_fit_exp_summary <- summary(nonstat_fit_exp)
      
      # Likelihood ratio tests: stationary vs non stationary models
      lr_trend_gp <- lr.test(stat_fit_gp, nonstat_fit_gp, alpha = 0.05)
      lr_trend_exp <- lr.test(stat_fit_exp, nonstat_fit_exp, alpha = 0.05)
      
      # Likelihood ratio test: GP vs Exponential
      lr_shape <- lr.test(nonstat_fit_gp, nonstat_fit_exp, alpha = 0.05)
      
      
      # Save results for stationary models
      final_results_df <- rbind(final_results_df, data.frame(
        Station = station,
        Dataset = dataset_name,
        Type = "GP",
        Model = "Stationary",
        Scale_Intercept = as.numeric(stat_fit_gp_summary$par["scale"]),
        Scale_SE_Intercept = as.numeric(stat_fit_gp_summary$se["scale"]),
        Scale_Slope = NA,
        Scale_SE_Slope = NA,
        Shape = as.numeric(stat_fit_gp_summary$par["shape"]),
        Shape_SE = as.numeric(stat_fit_gp_summary$se["shape"]),
        Trend_lr_pval = NA,
        Shape_lr_pval = NA,
        First_Observation_Date = first_obs_date,
        Last_Observation_Date = last_obs_date,
        Elapsed_Years = elapsed_years
      ))
      
      final_results_df <- rbind(final_results_df, data.frame(
        Station = station,
        Dataset = dataset_name,
        Type = "Exponential",
        Model = "Stationary",
        Scale_Intercept = as.numeric(stat_fit_exp_summary$par["scale"]),
        Scale_SE_Intercept = as.numeric(stat_fit_exp_summary$se["scale"]),
        Scale_Slope = NA,
        Scale_SE_Slope = NA,
        Shape = as.numeric(stat_fit_exp_summary$par["shape"]),
        Shape_SE = as.numeric(stat_fit_exp_summary$se["shape"]),
        Trend_lr_pval = NA,
        Shape_lr_pval = NA,
        First_Observation_Date = first_obs_date,
        Last_Observation_Date = last_obs_date,
        Elapsed_Years = elapsed_years
      ))
      
      # Save results for non-stationary models
      final_results_df <- rbind(final_results_df, data.frame(
        Station = station,
        Dataset = dataset_name,
        Type = "GP",
        Model = "Multiplicative",
        Scale_Intercept = as.numeric(nonstat_fit_gp_summary$par["phi0"]),
        Scale_SE_Intercept = as.numeric(nonstat_fit_gp_summary$se["phi0"]),
        Scale_Slope = as.numeric(nonstat_fit_gp_summary$par["phi1"]),
        Scale_SE_Slope = as.numeric(nonstat_fit_gp_summary$se["phi1"]),
        Shape = as.numeric(nonstat_fit_gp_summary$par["shape"]),
        Shape_SE = as.numeric(nonstat_fit_gp_summary$se["shape"]),
        Trend_lr_pval = as.numeric(lr_trend_gp$p.value),
        Shape_lr_pval = NA,
        First_Observation_Date = first_obs_date,
        Last_Observation_Date = last_obs_date,
        Elapsed_Years = elapsed_years
      ))
      
        final_results_df <- rbind(final_results_df, data.frame(
        Station = station,
        Dataset = dataset_name,
        Type = "Exponential",
        Model = "Multiplicative",
        Scale_Intercept = as.numeric(nonstat_fit_exp_summary$par["phi0"]),
        Scale_SE_Intercept = as.numeric(nonstat_fit_exp_summary$se["phi0"]),
        Scale_Slope = as.numeric(nonstat_fit_exp_summary$par["phi1"]),
        Scale_SE_Slope = as.numeric(nonstat_fit_exp_summary$se["phi1"]),
        Shape = as.numeric(nonstat_fit_exp_summary$par["shape"]),
        Shape_SE = as.numeric(nonstat_fit_exp_summary$se["shape"]),
        Trend_lr_pval = as.numeric(lr_trend_exp$p.value),
        Shape_lr_pval = as.numeric(lr_shape$p.value),
        First_Observation_Date = first_obs_date,
        Last_Observation_Date = last_obs_date,
        Elapsed_Years = elapsed_years
      ))
    }
}


# Check p-values
final_results_df <- final_results_df %>%
  mutate(Significant_Trend = ifelse(Model %in% c("Multiplicative") &
                                                !is.na(Trend_lr_pval),
                                              Trend_lr_pval < 0.05, NA))
final_results_df <- final_results_df %>%
  mutate(Significant_Shape = ifelse(Model %in% c("Multiplicative") &
                                                      !is.na(Shape_lr_pval),
                                                    Shape_lr_pval < 0.05, NA))


print("Done!")


# Save the results to a CSV file
library(readr)
write_csv(final_results_df, "GPresults2.csv")
