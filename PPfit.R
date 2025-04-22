library(extRemes)
library(ismev)
library(gnFit)
library(dplyr)
library(lubridate)
library(NHPoisson)

# Read data
#data <- read.csv("stations_60plusyears_nogaps.csv")
data <- read.csv("aligned_stations_60plusyears_nogaps.csv")
colnames(data) <- c("From_Date", "To_Date", "Date", "Rainfall_mm", "Quality", "Station_name", "Station_ID")


# Get unique station names
station_name_list <- unique(data$Station_name)

# Initialize results dataframe
results_df <- data.frame(
  Station = character(),
  Model = character(),
  b0_Estimate = numeric(),
  b0_SE = numeric(),
  b1_Estimate = numeric(),
  b1_SE = numeric(),
  b1_Significant = logical(),
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
  
  # Define a threshold (e.g., 99.5th percentile)
  threshold <- quantile(station_data$Rainfall_mm, 0.995, na.rm = TRUE)
  
  # Declustering
  station_data_declus <- c(decluster(station_data$Rainfall_mm, threshold, replace.with = threshold - 1))
  
  # Fit Poisson Process Model
  out <- fitPP.fun(
    covariates = cbind(station_data$Days_Elapsed),
    #covariates = cbind(seq(1, length(station_data_declus), length.out = length(station_data_declus))), 
    POTob = list(T = station_data_declus, thres = threshold),
    start = list(b0 = 0, b1 = 0),
    dplot = FALSE,
    modSim = TRUE
  )
  
  # Extract summary
  PP_summary <- summary(out)
  
  # Extract estimates and standard errors
  b0_Estimate <- PP_summary@coef[1]
  b1_Estimate <- PP_summary@coef[2]
  b0_SE <- PP_summary@coef[3]
  b1_SE <- PP_summary@coef[4]
  
  # Check if b1 is significant
  b1_Significant <- ifelse(b1_Estimate - 1.96 * b1_SE > 0 | b1_Estimate + 1.96 * b1_SE < 0, TRUE, FALSE)
  
  # Save results
  results_df <- rbind(results_df, data.frame(
    Station = station,
    Model = "Poisson Process",
    b0_Estimate = b0_Estimate,
    b0_SE = b0_SE,
    b1_Estimate = b1_Estimate,
    b1_SE = b1_SE,
    b1_Significant = b1_Significant
  ))
}

# Print results
print("pling")
print(results_df)

sum(results_df$b1_Significant, na.rm = TRUE) / length(results_df$b1_Significant)

library(readr)
write_csv(results_df, "aligned_PPfit_results.csv")

