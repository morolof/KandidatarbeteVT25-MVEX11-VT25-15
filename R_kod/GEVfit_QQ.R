library(extRemes)
library(dplyr)
library(lubridate)

# Read data
data <- read.csv("final_filtered_station_data.csv")
colnames(data) <- c("From_Date", "To_Date", "Date", "Rainfall_mm", "Quality", "Station_name", "Station_ID")

# Choose station
station_data <- data %>%
  filter(Station_name == "Gendalen")

# Get the "water years"
station_data <- station_data %>%
  mutate(Date = as.Date(Date),
         Water_Year = if_else(month(Date) >= 10, year(Date) + 1, year(Date))) %>%
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

# Compute the annual maximum (water years) rainfall and keep track of corresponding indices 
rain_ann_max_with_indices <- station_data %>%
  group_by(Water_Year) %>%
  filter(Rainfall_mm == max(Rainfall_mm, na.rm = TRUE)) %>%  
  slice(1) %>%  
  ungroup() %>%
  select(Max_Rainfall = Rainfall_mm, Years_Elapsed)  

# Create the data frame with the correct indices
data_df <- data.frame(ann_max = rain_ann_max_with_indices$Max_Rainfall,
                      years = rain_ann_max_with_indices$Years_Elapsed)

# Fit non-stat GEV
fit1 <- fevd(x = data_df$ann_max, data = data_df, type = "GEV", scale.fun = ~years, location.fun = ~years, use.phi = TRUE, time.units = "years")
summary_test <- summary(fit1)

# Get standardized quantiles
n <- length(data_df$ann_max)
sigma_t <- exp(as.numeric(fit1$results$par["phi0"]) + as.numeric(fit1$results$par["phi1"]) * (1:n))
mu_t <- as.numeric(fit1$results$par["mu0"]) + as.numeric(fit1$results$par["mu1"]) * (1:n)
y_t_k <- (data_df$ann_max - mu_t) / sigma_t

# Theoretical quantiles
QQ_y <- -log(-log(1:n / (n + 1)))

plot(QQ_y, sort(y_t_k),
     xlab = "Teoretiska GEV-kvantiler",
     ylab = "Observerade kvantiler (standardiserade)")
abline(0, 1, col = "red", lwd = 2)
