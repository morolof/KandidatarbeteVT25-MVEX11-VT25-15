import pandas as pd

# This code creates the "aligned" dataset, mentioned as dataset 2 in the text

# Load the rainfall data
df = pd.read_csv("csv_and_json/final_filtered_station_data.csv")

# Convert Date column to datetime
df['Date'] = pd.to_datetime(df['Date'])

# Get start and end date for each station
station_ranges = df.groupby('Station Name')['Date'].agg(['min', 'max'])

# Get the latest start date (common across all stations)
common_start = station_ranges['min'].max()

# Set the custom end date to match water year (end of September)
custom_end = pd.Timestamp('2024-09-30')

# Filter for the overlapping custom range
df_aligned = df[(df['Date'] >= common_start) & (df['Date'] <= custom_end)].copy()

# Optional: Check that all stations have same number of entries
counts = df_aligned.groupby('Station Name').size()
print(counts)

# Save to CSV
df_aligned.to_csv("csv_and_json/aligned_final_filtered_station_data.csv", index=False)
