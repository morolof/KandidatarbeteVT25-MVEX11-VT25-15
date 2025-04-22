library(extRemes)
library(ismev)
library(gnFit)
library(dplyr)
library(lubridate)

# Read data
data <- read.csv("stations_60plusyears_nogaps.csv")
colnames(data) <- c("From_Date", "To_Date", "Date", "Rainfall_mm", "Quality", "Station_name", "Station_ID")

# Filter for station
station_data <- data %>%
  filter(Station_name == "INSERT STATION NAME") %>%
  mutate(Date = as.Date(Date),
         Days_Elapsed = as.numeric(Date - min(Date, na.rm = TRUE)),
         Year = year(Date),
         Years_Elapsed = as.numeric(difftime(Date, min(Date, na.rm = TRUE), units = "days")) / 365.25)  # Convert days to years) 

# Define a threshold (e.g., 995th percentile)
threshold <- quantile(station_data$Rainfall_mm, 0.995, na.rm = TRUE)

# Declustering 
station_data_declus <-  c(decluster(station_data$Rainfall_mm, threshold, replace.with=threshold-1))

GP_df_year_elaps <- data.frame(rain_data = station_data_declus, years = station_data$Years_Elapsed)

# Exp model all data
fit1 <- fevd(x = station_data_declus, 
             data = GP_df_year_elaps, 
             threshold = threshold, 
             scale.fun = ~years, 
             type = "GP",
             use.phi = TRUE,
             span = station_data$Years_Elapsed[length(station_data$Year)],
             time.units = "years")

summary(fit1)

# Lin model all data
fit2 <- fevd(x = station_data_declus, 
             data = GP_df_year_elaps, 
             threshold = threshold, 
             scale.fun = ~years, 
             type = "GP",
             span = station_data$Years_Elapsed[length(station_data$Year)],
             time.units = "years")

summary(fit2)

years <- station_data$Years_Elapsed  

excesses <- station_data$Rainfall_mm[station_data$Rainfall_mm > threshold]
n <- length(excesses)

sigma_t <- exp(as.numeric(fit1$results$par["phi0"]) + as.numeric(fit1$results$par["phi1"]) * (years))
#sigma_t <- as.numeric(fit2$results$par["sigma0"]) + as.numeric(fit2$results$par["sigma1"]) * (years)  # Scale increasing over time
sigma_t_excesses <- sigma_t[station_data$Rainfall_mm > threshold]

y_t_k <- (excesses - threshold) / sigma_t_excesses

theoretical <- -log(1 - 1:n / (n + 1))

plot(sort(y_t_k), theoretical)
abline(0, 1, col = "red", lwd = 2)  # Reference line
