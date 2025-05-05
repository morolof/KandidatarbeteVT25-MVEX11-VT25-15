import pandas as pd
import json

# Code to get the coordinates of each chosen station to visualize the results in QGIS

# Load the rainfall data CSV
df = pd.read_csv("C:/Users/tomas/Desktop/R_projekt/KA kod/final_filtered_station_data.csv")

# Extract the unique station names from the CSV
csv_stations = set(df['Station Name'].unique())

# Load the JSON data (a list of station dicts)
with open("csv_and_json/filtered_stations_60.json", "r", encoding="utf-8") as json_file:
    loaded_data = json.load(json_file)

# Filter and extract desired fields
filtered_data = [
    {'name': entry['name'], 'longitude': entry['longitude'], 'latitude': entry['latitude']}
    for entry in loaded_data if entry['name'] in csv_stations
]

# Convert to a DataFrame and save as CSV
filtered_df = pd.DataFrame(filtered_data)
filtered_df.to_csv("csv_and_json/lonlat_chosen_stations.csv", index=False)
