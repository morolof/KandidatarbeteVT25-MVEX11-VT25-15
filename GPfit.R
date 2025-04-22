library(extRemes)
library(dplyr)
library(lubridate)

# Read data
data <- read.csv("stations_60plusyears_nogaps.csv")
#data <- read.csv("aligned_stations_60plusyears_nogaps.csv")
colnames(data) <- c("From_Date", "To_Date", "Date", "Rainfall_mm", "Quality", "Station_name", "Station_ID")

# Get unique station names
station_name_list <- unique(data$Station_name)

type_of_fit <- "Exponential"

# Initialize results dataframe
results_df <- data.frame(
  Station = character(),
  Model = character(),
  Scale_Intercept = numeric(),
  Scale_Slope = numeric(),
  Scale_SE_Intercept = numeric(),
  Scale_SE_Slope = numeric(),
  Shape = numeric(),
  Shape_SE = numeric(),
  stringsAsFactors = FALSE
)

# Loop over each station
for (station in station_name_list) {
  
  # Filter for current station
  station_data <- data %>%
    filter(Station_name == station) %>%
    mutate(Date = as.Date(Date),
           Days_Elapsed = as.numeric(Date - min(Date, na.rm = TRUE)),
           Year = year(Date),
           Years_Elapsed = as.numeric(difftime(Date, min(Date, na.rm = TRUE), units = "days")) / 365.25)  # Convert days to years
  
  # Define a threshold (e.g., 995th percentile)
  threshold <- quantile(station_data$Rainfall_mm, 0.995, na.rm = TRUE)
  
  # Declustering 
  station_data_declus <- c(decluster(station_data$Rainfall_mm, threshold, replace.with = threshold - 1))
  
  # Fit a stationary GPD model
  gpd_fit <- fevd(x = station_data_declus, threshold = threshold, type = type_of_fit)
  gpd_summary <- summary(gpd_fit)
  
  # Dataframe for model fitting
  GP_df_year_elaps <- data.frame(rain_data = station_data_declus, years = station_data$Years_Elapsed)
  
  # Fit an exponential model
  fit1 <- fevd(x = station_data_declus, 
               data = GP_df_year_elaps, 
               threshold = threshold, 
               scale.fun = ~years, 
               type = type_of_fit,
               use.phi = TRUE,
               span = station_data$Years_Elapsed[length(station_data$Year)],
               time.units = "years")
  fit1_summary <- summary(fit1)
  
  # Fit a linear model
  fit2 <- fevd(x = station_data_declus, 
               data = GP_df_year_elaps, 
               threshold = threshold, 
               scale.fun = ~years, 
               type = type_of_fit,
               span = station_data$Years_Elapsed[length(station_data$Year)],
               time.units = "years")
  fit2_summary <- summary(fit2)
  
  # Save results for the stationary model (no slope parameter)
  results_df <- rbind(results_df, data.frame(
    Station = station,
    Model = "Stationary",
    Scale_Intercept = as.numeric(gpd_summary$par["scale"]),
    Scale_Slope = NA,  # No slope for stationary model
    Scale_SE_Intercept = as.numeric(gpd_summary$se["scale"]),
    Scale_SE_Slope = NA,  # No slope SE for stationary model
    Shape = as.numeric(gpd_summary$par["shape"]),
    Shape_SE = as.numeric(gpd_summary$se["shape"])
  ))
  
  # Save results for the exponential model (fit1)
  results_df <- rbind(results_df, data.frame(
    Station = station,
    Model = "Exp model",
    Scale_Intercept = as.numeric(fit1_summary$par["phi0"]),
    Scale_Slope = as.numeric(fit1_summary$par["phi1"]),
    Scale_SE_Intercept = as.numeric(fit1_summary$se["phi0"]),
    Scale_SE_Slope = as.numeric(fit1_summary$se["phi1"]),
    Shape = as.numeric(fit1_summary$par["shape"]),
    Shape_SE = as.numeric(fit1_summary$se["shape"])
  ))
  
  # Save results for the linear model (fit2)
  results_df <- rbind(results_df, data.frame(
    Station = station,
    Model = "Linear model",
    Scale_Intercept = as.numeric(fit2_summary$par["sigma0"]),
    Scale_Slope = as.numeric(fit2_summary$par["sigma1"]),
    Scale_SE_Intercept = as.numeric(fit2_summary$se["sigma0"]),
    Scale_SE_Slope = as.numeric(fit2_summary$se["sigma1"]),
    Shape = as.numeric(fit2_summary$par["shape"]),
    Shape_SE = as.numeric(fit2_summary$se["shape"])
  ))
}
print("pling")

# Check if shape parameter is significantly different from zero
results_df <- results_df %>%
  mutate(Shape_Lower = Shape - 1.96 * Shape_SE,
         Shape_Upper = Shape + 1.96 * Shape_SE,
         Shape_Significant = ifelse(Shape_Lower > 0 | Shape_Upper < 0, TRUE, FALSE))


# Check if the scale slope parameter (sigma1) is significantly different from zero in the linear model
results_df <- results_df %>%
  mutate(Scale_Slope_Lower = Scale_Slope - 1.96 * Scale_SE_Slope,
         Scale_Slope_Upper = Scale_Slope + 1.96 * Scale_SE_Slope,
         Scale_Slope_Significant = ifelse(Scale_Slope_Lower > 0 | Scale_Slope_Upper < 0, TRUE, FALSE))

results_df <- results_df %>%
  select(-Shape_Lower, -Shape_Upper, -Scale_Slope_Lower, -Scale_Slope_Upper)


library(readr)
write_csv(results_df, "Exponentialfit_results.csv")

