library(extRemes)
library(dplyr)
library(lubridate)
library(NHPoisson)

# List of datasets to loop over
dataset_files <- list(
  "Original" = "final_filtered_station_data.csv",
  "Aligned" = "aligned_final_filtered_station_data.csv"
  #"Original" = "stations_5stations_debug.csv",
  #"Aligned" = "aligned_stations_5stations_debug.csv"
)

# Initialize results dataframe
results_df <- data.frame(
  Station = character(),
  Dataset = character(),
  b0_Estimate = numeric(),
  b0_SE = numeric(),
  b1_Estimate = numeric(),
  b1_SE = numeric(),
  lr_pval = numeric(),
  First_Observation_Date = as.Date(character()),
  Last_Observation_Date = as.Date(character()),
  Elapsed_Days = numeric(),
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
             Days_Elapsed = as.numeric(Date - min(Date, na.rm = TRUE)))
    
    # Calculate first and last observation dates
    first_obs_date = min(station_data$Date, na.rm = TRUE)
    last_obs_date = max(station_data$Date, na.rm = TRUE)
    
    # Calculate elapsed years based on these new dates
    elapsed_days <- as.numeric(difftime(last_obs_date, first_obs_date, units = "days")) 
    
    # Define a threshold (e.g., 99.5th percentile)
    threshold <- quantile(station_data$Rainfall_mm, 0.995, na.rm = TRUE)
    
    # Declustering
    station_data_declus <- c(decluster(station_data$Rainfall_mm, threshold, replace.with = threshold - 1))
    
    # Fit  inhom Poisson Process model
    modelfit <- fitPP.fun(
      covariates = cbind(station_data$Days_Elapsed),
      POTob = list(T = station_data_declus, thres = threshold),
      start = list(b0 = 0, b1 = 0),
      dplot = FALSE,
      modSim = TRUE
    )
    
    # Extract summary
    PP_summary <- summary(modelfit)
    
    lr <- LRTpv.fun(modelfit)
    
    # Extract estimates and standard errors
    b0_Estimate <- PP_summary@coef[1]
    b1_Estimate <- PP_summary@coef[2]
    b0_SE <- PP_summary@coef[3]
    b1_SE <- PP_summary@coef[4]
    
    
    # Save results
    results_df <- rbind(results_df, data.frame(
      Station = station,
      Dataset = dataset_name,
      b0_Estimate = b0_Estimate,
      b0_SE = b0_SE,
      b1_Estimate = b1_Estimate,
      b1_SE = b1_SE,
      lr_pval = as.numeric(lr),
      First_Observation_Date = first_obs_date,
      Last_Observation_Date = last_obs_date,
      Elapsed_Days = elapsed_days
    ))
  }
}

results_df <- results_df %>%
  mutate(Significant = lr_pval < 0.05)

print("pling")

# Save the results to a CSV file
library(readr)
write_csv(results_df, "PPresults2.csv")
