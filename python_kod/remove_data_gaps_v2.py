import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
import json

# This code removes all the gaps in the data and cuts the data accordingly to get the maximum length of the time series

# Load filtered data from the JSON file
with open("csv_and_json/filtered_stations_60.json", "r", encoding="utf-8") as json_file:
    loaded_data = json.load(json_file)

# Extract station names
station_names = [station['name'] for station in loaded_data]

# Import the data from CSV
df = pd.read_csv("csv_and_json/filtered_stations_60_data.csv")

# Convert 'Date' column to datetime
df['Date'] = pd.to_datetime(df['Date'])

# Dictionary to store dataframes for stations without large gaps
filtered_stations = {}

gap_threshold = pd.Timedelta(days=2)

# Loop over each station and check for gaps
for station in station_names:
    print("Checking station:", station)
    # Filter the data for the current station
    station_data = df[df['Station Name'] == station].sort_values('Date')

    # Calculate the difference between consecutive dates
    date_diffs = station_data['Date'].diff()

    # Find the rows where the date_diff is greater than the gap threshold
    large_gaps = date_diffs[date_diffs > gap_threshold]

    if not large_gaps.empty:
        latest_large_gap_idx = large_gaps.index[-1]  # last occurrence
        latest_large_gap_date = station_data.loc[latest_large_gap_idx, 'Date']
        gap_at_latest_large_gap = date_diffs.loc[latest_large_gap_idx]

        print(f"Latest large gap for station {station} at {latest_large_gap_date} with gap {gap_at_latest_large_gap}.")

        # Now check the time from latest gap to end
        station_data_after_gap = station_data[station_data['Date'] >= latest_large_gap_date].copy()

        if station_data_after_gap.empty:
            print(f"No data after latest large gap for station {station}. Skipping.")
            continue

        time_span = station_data_after_gap['Date'].max() - station_data_after_gap['Date'].min()

        if time_span >= pd.Timedelta(days=60 * 365.25):
            # Check gaps again in the post-gap data
            date_diffs_post_gap = station_data_after_gap['Date'].diff()

            if date_diffs_post_gap.max() <= gap_threshold:
                filtered_stations[station] = station_data_after_gap
                print(f"Station {station} included from {latest_large_gap_date.date()} onward.")
            else:
                print(f"Station {station} still has large gaps after latest gap. Skipping.") # This should not happen but lets check anyhow.
        else:
            print(f"Station {station} has less than 60 years of data after latest gap. Skipping.")
    else:
        # No large gaps found, keep the full station data
        filtered_stations[station] = station_data
        print(f"No large gaps found for station {station}. Station included.")

# Create a new combined dataframe with filtered stations
filtered_df = pd.concat(filtered_stations.values())

# Save filtered data to a new CSV
filtered_df.to_csv("csv_and_json/final_filtered_station_data.csv", index=False)

# Display a message summarizing the result
print(f"Number of stations without gaps greater than chosen threshold: {len(filtered_stations)}")
