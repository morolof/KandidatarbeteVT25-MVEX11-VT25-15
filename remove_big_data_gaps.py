import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import json

# Load filtered data from the JSON file
with open("csv_and_json/filtered_stations_60.json", "r", encoding="utf-8") as json_file:
    loaded_data = json.load(json_file)

# Extract station names and IDs
station_names = [station['name'] for station in loaded_data]

# Import the data from CSV
df = pd.read_csv("csv_and_json/filtered_stations_60_data.csv")

# Convert 'Date' column to datetime if it's not already
df['Date'] = pd.to_datetime(df['Date'])

# Dictionary to store dataframes for stations without large gaps
filtered_stations = {}

gap_threshold = pd.Timedelta(days=5)

# Loop over each station and check for gaps
for station in station_names:
    print("Checking station: " + station)
    # Filter the data for the current station
    station_data = df[df['Station Name'] == station].sort_values('Date')

    # Calculate the difference between consecutive dates
    date_diffs = station_data['Date'].diff()

    # Check if any gap is greater than the threshold
    if date_diffs.max() <= gap_threshold:
        filtered_stations[station] = station_data  # Store stations with no large gaps
    else:
        # Case 2: Try cutting from 1960 onwards
        try:
            station_data_post_1960 = station_data[station_data['Date'] >= '1960-01-01'].copy()
            time_span = station_data_post_1960['Date'].max() - station_data_post_1960['Date'].min()
            if time_span >= pd.Timedelta(days=60*365.25):
                if not station_data_post_1960.empty:
                    date_diffs_post_1960 = station_data_post_1960['Date'].diff()

                    if date_diffs_post_1960.max() <= gap_threshold:
                        filtered_stations[station] = station_data_post_1960
                        print(f"Station {station} included only from 1960 onward due to earlier gaps.")
        except Exception as e:
            print(f"Error processing station {station} for 1960-cut: {e}")

# Create a new combined dataframe with filtered stations
filtered_df = pd.concat(filtered_stations.values())

# Save filtered data to a new CSV or continue working with the filtered_df
filtered_df.to_csv("csv_and_json/filtered_stations_60_5_day_gap_v2.csv", index=False)

# Display a message summarizing the result
print(f"Number of stations without gaps greater than chosen threshold: {len(filtered_stations)}")
