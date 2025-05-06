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

# Initialize final results
final_results_df <- data.frame()

# Loop over datasets
for (dataset_name in names(dataset_files)) {
  file_path <- dataset_files[[dataset_name]]
  data <- read.csv(file_path)
  colnames(data) <- c("From_Date", "To_Date", "Date", "Rainfall_mm", "Quality", "Station_name", "Station_ID")
  
  station_name_list <- unique(data$Station_name)
# Loop over stations    
    for (station in station_name_list) {
      
      # Fix water-years
      station_data <- data %>%
        filter(Station_name == station) %>%
        mutate(Date = as.Date(Date),
               Water_Year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)),
               Dataset_Type = dataset_name) %>%
        group_by(Station_ID) %>%
        arrange(Date) %>%
        mutate(
          First_Date = first(Date),
          Last_Date = last(Date),
          First_Oct1 = if_else(
            month(First_Date) == 10 & day(First_Date) == 1,
            First_Date,
            make_date(year(First_Date) + if_else(month(First_Date) >= 10, 1, 0), 10, 1)
          ),
          Last_Water_Year = max(Water_Year),
          Last_Sep30 = if_else(Dataset_Type == "Aligned",
                               make_date(Last_Water_Year, 9, 30),  # For aligned dataset
                               make_date(Last_Water_Year - 1, 9, 30)  # For original dataset
          )
        ) %>%
        filter(Date >= First_Oct1 & Date <= Last_Sep30) %>%
        select(-First_Date, -Last_Date, -First_Oct1, -Last_Water_Year, -Last_Sep30) %>%
        ungroup()
      
      
      station_data <- station_data %>%
        mutate(Days_Elapsed = as.numeric(Date - min(Date, na.rm = TRUE)),
               Year = year(Date),
               Years_Elapsed = as.numeric(difftime(Date, min(Date, na.rm = TRUE), units = "days")) / 365.25)
      
      # These are for the results
      first_obs_date = min(station_data$Date)
      last_obs_date = max(station_data$Date)
      
      elapsed_years <- as.numeric(difftime(last_obs_date, first_obs_date, units = "days")) / 365.25
      
      # Annual maxes
      rain_ann_max_with_indices <- station_data %>%
        group_by(Water_Year) %>%
        filter(Rainfall_mm == max(Rainfall_mm, na.rm = TRUE)) %>%
        slice(1) %>%
        ungroup() %>%
        select(Date, Max_Rainfall = Rainfall_mm, Years_Elapsed)
      
      data_df <- data.frame(ann_max = rain_ann_max_with_indices$Max_Rainfall,
                            years = rain_ann_max_with_indices$Years_Elapsed)
      
      # Fit stationary GP models and store results
      stat_gev <- fevd(x = rain_ann_max_with_indices$Max_Rainfall, type = "GEV", verbose = FALSE)
      stat_gumbel <- fevd(x = rain_ann_max_with_indices$Max_Rainfall, type = "Gumbel", verbose = FALSE)
      
      stat_summary_gev <- summary(stat_gev)
      stat_summary_gumbel <- summary(stat_gumbel)
      
      print(station)
      print(dataset_name)
      
      final_results_df <- rbind(final_results_df, data.frame(
        Station = station,
        Dataset = dataset_name,
        Type = "GEV",
        Model = "Stationary",
        Location_Intercept = as.numeric(stat_summary_gev$par["location"]),
        Location_SE_Intercept = as.numeric(stat_summary_gev$se["location"]),
        Location_Slope = NA,
        Location_SE_Slope = NA,
        Scale_Intercept = as.numeric(stat_summary_gev$par["scale"]),
        Scale_SE_Intercept = as.numeric(stat_summary_gev$se["scale"]),
        Scale_Slope = NA,
        Scale_SE_Slope = NA,
        Shape = as.numeric(stat_summary_gev$par["shape"]),
        Shape_SE = as.numeric(stat_summary_gev$se["shape"]),
        Trend_lr_pval = NA,
        Shape_lr_pval = NA,
        First_Observation_Date = as.Date(first_obs_date),
        Last_Observation_Date = as.Date(last_obs_date),
        Elapsed_Years = elapsed_years
      ))
      
      final_results_df <- rbind(final_results_df, data.frame(
        Station = station,
        Dataset = dataset_name,
        Type = "Gumbel",
        Model = "Stationary",
        Location_Intercept = as.numeric(stat_summary_gumbel$par["location"]),
        Location_SE_Intercept = as.numeric(stat_summary_gumbel$se["location"]),
        Location_Slope = NA,
        Location_SE_Slope = NA,
        Scale_Intercept = as.numeric(stat_summary_gumbel$par["scale"]),
        Scale_SE_Intercept = as.numeric(stat_summary_gumbel$se["scale"]),
        Scale_Slope = NA,
        Scale_SE_Slope = NA,
        Shape = as.numeric(stat_summary_gumbel$par["shape"]),
        Shape_SE = as.numeric(stat_summary_gumbel$se["shape"]),
        Trend_lr_pval = NA,
        Shape_lr_pval = NA,
        First_Observation_Date = as.Date(first_obs_date),
        Last_Observation_Date = as.Date(last_obs_date),
        Elapsed_Years = elapsed_years
      ))
      
      # Function to extract parameters from non-stat summaries
      extract_pars <- function(fit_summary) {
        pars <- fit_summary$par
        ses <- fit_summary$se
        list(
          mu0 = ifelse("mu0" %in% names(pars), as.numeric(pars["mu0"]), ifelse("location" %in% names(pars), as.numeric(pars["location"]), NA)),
          mu1 = ifelse("mu1" %in% names(pars), as.numeric(pars["mu1"]), NA),
          phi0 = ifelse("phi0" %in% names(pars), as.numeric(pars["phi0"]), ifelse("scale" %in% names(pars), as.numeric(pars["scale"]), NA)),
          phi1 = ifelse("phi1" %in% names(pars), as.numeric(pars["phi1"]), NA),
          shape = ifelse("shape" %in% names(pars), as.numeric(pars["shape"]), NA),
          se_mu0 = ifelse("mu0" %in% names(ses), as.numeric(ses["mu0"]), ifelse("location" %in% names(ses), as.numeric(ses["location"]), NA)),
          se_mu1 = ifelse("mu1" %in% names(ses), as.numeric(ses["mu1"]), NA),
          se_phi0 = ifelse("phi0" %in% names(ses), as.numeric(ses["phi0"]), ifelse("scale" %in% names(ses), as.numeric(ses["scale"]), NA)),
          se_phi1 = ifelse("phi1" %in% names(ses), as.numeric(ses["phi1"]), NA),
          se_shape = ifelse("shape" %in% names(ses), as.numeric(ses["shape"]), NA)
        )
      }
      
      # Non-stat models
      model_defs <- list(
        "Multiplicative (mu)" = list(location.fun = ~years, scale.fun = ~1),
        "Multiplicative (sigma)" = list(location.fun = ~1, scale.fun = ~years),
        "Multiplicative (mu/sigma)" = list(location.fun = ~years, scale.fun = ~years)
      )
      
      
      for (model_name in names(model_defs)) {
        
        model_def <- model_defs[[model_name]]
        
        # Fit GEV nonstationary
        gev_nonstat_fit <- fevd(x = rain_ann_max_with_indices$Max_Rainfall,
                                data = data_df,
                                type = "GEV",
                                location.fun = model_def$location.fun,
                                scale.fun = model_def$scale.fun,
                                use.phi = TRUE,
                                time.units = "years",
                                verbose = FALSE)
        
        gev_summary <- summary(gev_nonstat_fit)
        gev_pars <- extract_pars(gev_summary)
        
        # Fit Gumbel nonstationary
        gumbel_nonstat_fit <- fevd(x = rain_ann_max_with_indices$Max_Rainfall,
                                   data = data_df,
                                   type = "Gumbel",
                                   location.fun = model_def$location.fun,
                                   scale.fun = model_def$scale.fun,
                                   use.phi = TRUE,
                                   time.units = "years",
                                   verbose = FALSE)
        
        gumbel_summary <- summary(gumbel_nonstat_fit)
        gumbel_pars <- extract_pars(gumbel_summary)
        
        # LR test between stationary GEV and nonstationary GEV
        lr_stat_nonstat_gev <- lr.test(stat_gev, gev_nonstat_fit, alpha = 0.05)
        
        # LR test between stationary Gumbel and nonstationary Gumbel
        lr_stat_nonstat_gumbel <- lr.test(stat_gumbel, gumbel_nonstat_fit, alpha = 0.05)
        
        # LR test between Gumbel nonstationary and GEV nonstationary
        lr_gumbel_gev <- lr.test(gumbel_nonstat_fit, gev_nonstat_fit, alpha = 0.05)
        
        # Save the GEV nonstationary fit
        final_results_df <- rbind(final_results_df, data.frame(
          Station = station,
          Dataset = dataset_name,
          Type = "GEV",
          Model = model_name,
          Location_Intercept = gev_pars$mu0,
          Location_SE_Intercept = gev_pars$se_mu0,
          Location_Slope = gev_pars$mu1,
          Location_SE_Slope = gev_pars$se_mu1,
          Scale_Intercept = gev_pars$phi0,
          Scale_SE_Intercept = gev_pars$se_phi0,
          Scale_Slope = gev_pars$phi1,
          Scale_SE_Slope = gev_pars$se_phi1,
          Shape = gev_pars$shape,
          Shape_SE = gev_pars$se_shape,
          Trend_lr_pval = as.numeric(lr_stat_nonstat_gev$p.value),  
          Shape_lr_pval = NA, 
          First_Observation_Date = as.Date(first_obs_date),
          Last_Observation_Date = as.Date(last_obs_date),
          Elapsed_Years = elapsed_years
        ))
        
        # Save the Gumbel nonstationary fit
        final_results_df <- rbind(final_results_df, data.frame(
          Station = station,
          Dataset = dataset_name,
          Type = "Gumbel",
          Model = model_name,
          Location_Intercept = gumbel_pars$mu0,
          Location_SE_Intercept = gumbel_pars$se_mu0,
          Location_Slope = gumbel_pars$mu1,
          Location_SE_Slope = gumbel_pars$se_mu1,
          Scale_Intercept = gumbel_pars$phi0,
          Scale_SE_Intercept = gumbel_pars$se_phi0,
          Scale_Slope = gumbel_pars$phi1,
          Scale_SE_Slope = gumbel_pars$se_phi1,
          Shape = gumbel_pars$shape,
          Shape_SE = gumbel_pars$se_shape,
          Trend_lr_pval = as.numeric(lr_stat_nonstat_gumbel$p.value),  
          Shape_lr_pval = as.numeric(lr_gumbel_gev$p.value), 
          First_Observation_Date = as.Date(first_obs_date),
          Last_Observation_Date = as.Date(last_obs_date),
          Elapsed_Years = elapsed_years
        ))
      }
  }
}

# Check p-vals
final_results_df <- final_results_df %>%
  mutate(Significant_Trend = ifelse(Model %in% c("Multiplicative (mu)",
                                                           "Multiplicative (sigma)",
                                                           "Multiplicative (mu/sigma)") &
                                                !is.na(Trend_lr_pval),
                                              Trend_lr_pval < 0.05, NA))
final_results_df <- final_results_df %>%
  mutate(Significant_Shape = ifelse(Model %in% c("Multiplicative (mu)", 
                                                                 "Multiplicative (sigma)", 
                                                                 "Multiplicative (mu/sigma)") &
                                                      !is.na(Shape_lr_pval),
                                                    Shape_lr_pval < 0.05, NA))

print("Done!")

library(readr)
write_csv(final_results_df, "GEVresults2.csv")
