library(dplyr)

df <- read.csv("PPresults2.csv")

df_filtered <- df %>%
  filter(Dataset == "Original") #%>%
  #filter(Model == "Multiplicative") %>%
  #filter(Type == "GP")


#expected_exceedances <- function(b0, b1, x_days) {
#  (exp(b0) / b1) * (exp(b1 * x_days) - 1)
#}

station_name_list <- unique(df_filtered$Station)
lambda1s <- rep(0, length(station_name_list))
i <- 1

for (station in station_name_list) {
  station_data <- df_filtered %>% filter(Station == station)
  b0 <- station_data$b0_Estimate
  b1 <- station_data$b1_Estimate
  
  lambda1s[i] <- b1 * 365.25
  i <- i + 1
}

# Breaks for the bins
bin_edges_lambda1_rounded <- c(-0.006, 0.000, 0.0035, 0.007, 0.0105, 0.014)

# Color of the bars
colors <- c(
  rgb(  5, 113, 176, maxColorValue = 255 ),       
  rgb(222, 235, 241, maxColorValue = 255 ),     
  rgb(245, 190, 165, maxColorValue = 255 ),         
  rgb(228, 101,  92, maxColorValue = 255 ),      
  rgb(202,   0,  32, maxColorValue = 255 )   
)

# Compute histogram 
h <- hist(lambda1s, breaks = bin_edges_lambda1_rounded, plot = FALSE, right = FALSE)

# Compute bar midpoints
mids <- h$mids
heights <- h$counts
widths <- diff(bin_edges_lambda1_rounded)

# Start blank plot
plot(0, 0, type = "n",
     xlim = range(bin_edges_lambda1_rounded),
     ylim = c(0, max(heights)),
     xlab = expression(lambda[1]),
     ylab = "Antal",
     xaxt = "n")

# Add x-axis manually
axis(side = 1, at = bin_edges_lambda1_rounded, labels = format(bin_edges_lambda1_rounded, digits = 3))

# Draw bars with colored rectangles
for (i in seq_along(heights)) {
  rect(xleft  = bin_edges_lambda1_rounded[i],
       xright = bin_edges_lambda1_rounded[i + 1],
       ybottom = 0,
       ytop = heights[i],
       col = colors[i],
       border = "black")
}

