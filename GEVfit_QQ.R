library(extRemes)
library(ismev)
library(gnFit)
library(dplyr)
library(lubridate)

# Read data
data <- read.csv("rainfall_data_all_stations.csv")
colnames(data) <- c("From_Date", "To_Date", "Date", "Rainfall_mm", "Quality", "Station_name", "Station_ID")

station_data <- data %>%
  filter(Station_name == "Gendalen")

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

plot(sort(y_t_k), QQ_y)
abline(0, 1, col = "red", lwd = 2)  # Reference line
