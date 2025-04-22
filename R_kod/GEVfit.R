library(extRemes)
library(ismev)
library(gnFit)
library(dplyr)
library(lubridate)

# Read data
#data <- read.csv("stations_60plusyears_nogaps.csv")
data <- read.csv("aligned_stations_60plusyears_nogaps.csv")

type_of_fit <- "Gumbel"

colnames(data) <- c("From_Date", "To_Date", "Date", "Rainfall_mm", "Quality", "Station_name", "Station_ID")

# Get unique station names
station_name_list <- unique(data$Station_name)

# Initialize results dataframe
results_df <- data.frame(
  Station = character(),
  Model = character(),
  Location_Intercept = numeric(),
  Location_Slope = numeric(),
  Scale_Intercept = numeric(),
  Scale_Slope = numeric(),
  Shape = numeric(),
  Location_SE_Intercept = numeric(),
  Location_SE_Slope = numeric(),
  Scale_SE_Intercept = numeric(),
  Scale_SE_Slope = numeric(),
  Shape_SE = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each station
for (station in station_name_list) {
  
  station_data <- data %>%
    filter(Station_name == station)
  
  station_data <- station_data %>%
    mutate(Date = as.Date(Date),
           Water_Year = if_else(month(Date) >= 10, year(Date) + 1, year(Date))) %>%
    group_by(Station_ID) %>%
    arrange(Date) %>%
    mutate(
      # Get first and last available dates per station
      First_Date = first(Date),
      Last_Date = last(Date),
      
      # Start from first Oct 1 on or after first date
      First_Oct1 = if_else(
        month(First_Date) == 10 & day(First_Date) == 1,
        First_Date,
        make_date(year(First_Date) + if_else(month(First_Date) >= 10, 1, 0), 10, 1)
      ),
      
      # Last full water year = Water_Year - 1 (if data doesn't reach Sept 30 of max WY)
      Last_Water_Year = max(Water_Year),
      # End date is Sept 30 *before* the start of the last (possibly incomplete) water year
      #Last_Sep30 = make_date(Last_Water_Year - 1, 9, 30)
      Last_Sep30 = make_date(Last_Water_Year, 9, 30)
    ) %>%
    filter(Date >= First_Oct1 & Date <= Last_Sep30) %>%
    select(-First_Date, -Last_Date, -First_Oct1, -Last_Water_Year, -Last_Sep30) %>%
    ungroup()
  
  # Filter for station and format Date column
  station_data <- station_data %>%
    mutate(Date = as.Date(Date),
           Days_Elapsed = as.numeric(Date - min(Date, na.rm = TRUE)),
           Year = year(Date),
           Years_Elapsed = as.numeric(difftime(Date, min(Date, na.rm = TRUE), units = "days")) / 365.25)
  
  # Compute the annual maximum rainfall and keep track of corresponding indices for "Years_Elapsed"
  rain_ann_max_with_indices <- station_data %>%
    group_by(Water_Year) %>%
    filter(Rainfall_mm == max(Rainfall_mm, na.rm = TRUE)) %>%  # Find max rainfall within each "water year"
    slice(1) %>%  # In case of ties, take the first occurrence
    ungroup() %>%
    select(Max_Rainfall = Rainfall_mm, Years_Elapsed)  # Keep the max and the corresponding elapsed time index
  
  # Create the dataframe with the correct indices
  data_df <- data.frame(ann_max = rain_ann_max_with_indices$Max_Rainfall,
                        years = rain_ann_max_with_indices$Years_Elapsed)
  
  # Fit a stationary GEV model
  gev_fit <- fevd(x = rain_ann_max_with_indices$Max_Rainfall, type = type_of_fit)
  gev_summary <- summary(gev_fit)
  
  # Save results for the stationary model
  results_df <- rbind(results_df, data.frame(
    Station = station,
    Model = "Stationary",
    Location_Intercept = as.numeric(gev_summary$par["location"]),
    Location_Slope = NA, 
    Scale_Intercept = as.numeric(gev_summary$par["scale"]),
    Scale_Slope = NA, 
    Shape = as.numeric(gev_summary$par["shape"]),
    Location_SE_Intercept = as.numeric(gev_summary$se["location"]),
    Location_SE_Slope = NA,
    Scale_SE_Intercept = as.numeric(gev_summary$se["scale"]),
    Scale_SE_Slope = NA,
    Shape_SE = as.numeric(gev_summary$se["shape"])
  ))
  
  # Fit an exponential model
  fit1 <- fevd(x = rain_ann_max_with_indices$Max_Rainfall, 
               data = data_df, 
               type = type_of_fit, 
               #scale.fun = ~years, 
               location.fun = ~years, 
               use.phi = TRUE, 
               time.units = "years")
  fit1_summary <- summary(fit1)
  
  # Save results for the exponential model
  results_df <- rbind(results_df, data.frame(
    Station = station,
    Model = "Exp model",
    #Location_Intercept = as.numeric(fit1_summary$par["location"]),
    Location_Intercept = as.numeric(fit1_summary$par["mu0"]),
    Location_Slope = as.numeric(fit1_summary$par["mu1"]),
    Scale_Intercept = as.numeric(fit1_summary$par["scale"]),
    #Scale_Intercept = as.numeric(fit1_summary$par["phi0"]),
    Scale_Slope = as.numeric(fit1_summary$par["phi1"]),
    Shape = as.numeric(fit1_summary$par["shape"]),
    #Location_SE_Intercept = as.numeric(fit1_summary$se["location"]),
    Location_SE_Intercept = as.numeric(fit1_summary$se["mu0"]),
    Location_SE_Slope = as.numeric(fit1_summary$se["mu1"]),
    Scale_SE_Intercept = as.numeric(fit1_summary$se["scale"]),
    #Scale_SE_Intercept = as.numeric(fit1_summary$se["phi0"]),
    Scale_SE_Slope = as.numeric(fit1_summary$se["phi1"]),
    Shape_SE = as.numeric(fit1_summary$se["shape"])
  ))
  # Fit an linear model
  fit2 <- fevd(x = rain_ann_max_with_indices$Max_Rainfall, 
               data = data_df, 
               type = type_of_fit, 
               #scale.fun = ~years, 
               location.fun = ~years, 
               time.units = "years")
  fit2_summary <- summary(fit2)
  
  # Save results for the exponential model
  results_df <- rbind(results_df, data.frame(
    Station = station,
    Model = "Linear model",
    #Location_Intercept = as.numeric(fit2_summary$par["location"]),
    Location_Intercept = as.numeric(fit2_summary$par["mu0"]),
    Location_Slope = as.numeric(fit2_summary$par["mu1"]),
    Scale_Intercept = as.numeric(fit2_summary$par["scale"]),
    #Scale_Intercept = as.numeric(fit2_summary$par["sigma0"]),
    Scale_Slope = as.numeric(fit2_summary$par["sigma1"]),
    Shape = as.numeric(fit2_summary$par["shape"]),
    #Location_SE_Intercept = as.numeric(fit2_summary$se["location"]),
    Location_SE_Intercept = as.numeric(fit2_summary$se["mu0"]),
    Location_SE_Slope = as.numeric(fit2_summary$se["mu1"]),
    Scale_SE_Intercept = as.numeric(fit2_summary$se["scale"]),
    #Scale_SE_Intercept = as.numeric(fit2_summary$se["sigma0"]),
    Scale_SE_Slope = as.numeric(fit2_summary$se["sigma1"]),
    Shape_SE = as.numeric(fit2_summary$se["shape"])
  ))
}

print("pling")


results_df <- results_df %>%
  mutate(
    # Significance of trend in Location
    Location_Slope_Significant = ifelse(
      Model %in% c("Exp model", "Linear model"),
      (Location_Slope - 1.96 * Location_SE_Slope > 0) | 
        (Location_Slope + 1.96 * Location_SE_Slope < 0),
      NA
    ),
    
    # Significance of Shape parameter
    Shape_Significant = ifelse(
      Model %in% c("Exp model", "Linear model"),
      (Shape - 1.96 * Shape_SE > 0) | 
        (Shape + 1.96 * Shape_SE < 0),
      NA
    ),
    
    # Significance of trend in Scale
    Scale_Slope_Significant = ifelse(
      Model %in% c("Exp model", "Linear model"),
      (Scale_Slope - 1.96 * Scale_SE_Slope > 0) | 
        (Scale_Slope + 1.96 * Scale_SE_Slope < 0),
      NA
    )
  )

library(readr)
write_csv(results_df, "aligned_Gumbelfit_results_notrend_scale.csv")


