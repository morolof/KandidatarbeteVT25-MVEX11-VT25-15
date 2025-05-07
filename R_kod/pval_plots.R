library(dplyr)

# Read in desired results 
df <- read.csv("GPresults2.csv")

df <- read.csv("GEVresults2.csv")

df <- read.csv("PPresults2.csv")

# Get the right rows
df_filtered <- df %>%
  filter(Dataset == "Original") %>%
  filter(Model == "Multiplicative (mu/sigma)") #%>%
  #filter(Type == "GEV")

df_filtered <- df %>%
  filter(Dataset == "Original") %>%
  filter(Model == "Multiplicative") %>%
  filter(Type == "GP")

df_filtered <- df %>%
  filter(Dataset == "Aligned") 

station_name_list <- unique(df_filtered$Station)

# Get the expected and obserbed p-values
x <- 1:length(station_name_list)/length(station_name_list)
p_vals <- sort(df_filtered$lr_pval)

# Plot
plot(x, p_vals,
     xlab = "Förväntade p-värden",
     ylab = "Observerade p-värden"
     #main = "Q–Q-plott av p-värden från likelihoodkvottest"
     )
abline(0, 1, col = "red", lwd = 2)

