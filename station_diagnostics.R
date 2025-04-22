library(extRemes)
library(ismev)
library(gnFit)
library(dplyr)
library(lubridate)
library(ggplot2)

# Read data
#data <- read.csv("rainfall_data_all_stations.csv")
#data <- read.csv("filtered_stations_no_large_gaps.csv")
#data <- read.csv("filtered_stations_60_no_gaps.csv")
data <- read.csv("stations_60plusyears_nogaps.csv")
colnames(data) <- c("From_Date", "To_Date", "Date", "Rainfall_mm", "Quality", "Station_name", "Station_ID")

# Get unique station names
stations <- unique(data$Station_name)

# Loop through each station and show plots
for (station in stations) {
  
  # Filter for the current station
  station_data <- data %>%
    filter(Station_name == station) %>%
    mutate(Date = as.Date(Date),
           Days_Elapsed = as.numeric(Date - min(Date, na.rm = TRUE)),
           Year = year(Date),
           Years_Elapsed = as.numeric(difftime(Date, min(Date, na.rm = TRUE), units = "days")) / 365.25)
  
  # Create water year (assuming Oct-Sep water year)
  station_data <- station_data %>%
    mutate(Water_Year = if_else(month(Date) >= 10, year(Date) + 1, year(Date)))
  
  # Create a dataframe for plotting annual maximum rainfall
  plot_data <- station_data %>%
    group_by(Water_Year) %>%
    summarise(Max_Rainfall = max(Rainfall_mm, na.rm = TRUE))
  
  # Generate the bar plot
  bar_plot <- ggplot(plot_data, aes(x = Water_Year, y = Max_Rainfall)) +
    geom_bar(stat = "identity", fill = "steelblue") +
    labs(title = paste("Annual Maximum Rainfall (Water Year) - Bar Plot -", station), 
         x = "Water Year", 
         y = "Rainfall (mm)") +
    theme_minimal() +
    theme(plot.title = element_text(hjust = 0.5, size = 14),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          axis.text = element_text(size = 10),
          axis.text.x = element_text(angle = 45, hjust = 1))
  
  print(bar_plot)  # Display the plot
  
  # Ask again before moving to the next station
  user_input <- readline(prompt = "Press [Enter] to continue, or type 'q' to quit: ")
  
  if (tolower(user_input) == "q") {
    cat("Exiting the loop.\n")
    break  # Exit the loop if the user types 'q'
  }
  # GP QQ Plot
  # Define a threshold 
  threshold <- quantile(station_data$Rainfall_mm, 0.995, na.rm = TRUE)
  
  # Declustering 
  station_data_declus <-  c(decluster(station_data$Rainfall_mm, threshold, replace.with=threshold-1))
  
  GP_df_year_elaps <- data.frame(rain_data = station_data_declus, years = station_data$Years_Elapsed)
  
  # Exp model all data
  fit2 <- fevd(x = station_data_declus, 
               data = GP_df_year_elaps, 
               threshold = threshold, 
               scale.fun = ~years, 
               type = "GP",
               span = station_data$Years_Elapsed[length(station_data$Year)],
               time.units = "years")
  
  # Simulated example: Assume we have non-stationary GPD estimates
  years <- station_data$Years_Elapsed  
  
  excesses <- station_data$Rainfall_mm[station_data$Rainfall_mm > threshold]
  n <- length(excesses)
  # Assume scale varies with time 
  sigma_t <- as.numeric(fit2$results$par["sigma0"]) + as.numeric(fit2$results$par["sigma1"]) * (years)  # Scale increasing over time
  sigma_t_excesses <- sigma_t[station_data$Rainfall_mm > threshold]
  
  y_t_k <- log(exp(((excesses - threshold) / sigma_t_excesses)))
  
  QQ_y <- -log(1 - 1:n / (n + 1))
  
  plot(sort(y_t_k), QQ_y, main = paste("QQ Exponential", station))
  abline(0, 1, col = "red", lwd = 2)  # Reference line
  
  # Ask again before moving to the next station
  user_input <- readline(prompt = "Press [Enter] to continue, or type 'q' to quit: ")
  
  if (tolower(user_input) == "q") {
    cat("Exiting the loop.\n")
    break  # Exit the loop if the user types 'q'
  }
  
  # GEV QQ Plot
  
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
      Last_Sep30 = make_date(Last_Water_Year - 1, 9, 30)
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
  
  fit1 <- fevd(x = data_df$ann_max, data = data_df, type = "GEV", scale.fun = ~years, location.fun = ~years, use.phi = TRUE, time.units = "years")
  summary_test <- summary(fit1)
  
  n <- length(data_df$ann_max)
  sigma_t <- exp(as.numeric(fit1$results$par["phi0"]) + as.numeric(fit1$results$par["phi1"]) * (1:n))
  mu_t <- as.numeric(fit1$results$par["mu0"]) + as.numeric(fit1$results$par["mu1"]) * (1:n)
  
  y_t_k <- (data_df$ann_max - mu_t) / sigma_t
  QQ_y <- -log(-log(1:n / (n + 1)))
  
  plot(sort(y_t_k), QQ_y, main = paste("QQ GEV", station))
  abline(0, 1, col = "red", lwd = 2)  # Reference line
  
  # Ask again before moving to the next station
  user_input <- readline(prompt = "Press [Enter] to continue, or type 'q' to quit: ")
  
  if (tolower(user_input) == "q") {
    cat("Exiting the loop.\n")
    break  # Exit the loop if the user types 'q'
  }
}


result_df <- read.csv("PPfit_results.csv")
