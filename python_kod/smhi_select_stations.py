import requests
from datetime import datetime, timedelta
import math
import json

# This code looks at the metadata for all stations to create a list of interesting stations for our project

# Function to get a stations metadata given a specific smhi_param, see https://opendata.smhi.se/metobs/resources/parameter
def get_stations_metadata(smhi_param):
    url_stations = f"https://opendata-download-metobs.smhi.se/api/version/1.0/parameter/{smhi_param}.json"
    r_stations = requests.get(url_stations)
    stations_metadata = json.loads(r_stations.text)
    return stations_metadata['station']

# Function to convert Unix epoch (milliseconds) to a readable datetime
def convert_unix_epoch(milliseconds):
    seconds = milliseconds / 1000
    if seconds < 0:
        return (datetime(1970, 1, 1) + timedelta(seconds=seconds)).strftime('%Y-%m-%d %H:%M:%S')
    else:
        return datetime.utcfromtimestamp(seconds).strftime('%Y-%m-%d %H:%M:%S')

# Function to calculate Euclidean distance between two points (latitude and longitude)
def calculate_distance(lat1, lon1, lat2, lon2):
    return math.sqrt((lat2 - lat1)**2 + (lon2 - lon1)**2)

# Function to calculate the interval between two timestamps in years
def interval_in_years(from_ts, to_ts):
    return (to_ts - from_ts) / (1000 * 60 * 60 * 24 * 365.25)  # Convert ms to years

# Change this parameter to either get daily rainfall, 15 min rainfall, etc see SMHI API parametrization for more
# Remember to change header accordingly as the csv files might look different
# 5 - daily rainfall, 14 - rainfall 15 min
smhi_parameter = 5
output_file_name = "csv_and_json/filtered_stations_60.json"

# Get stations metadata
stations_metadata = get_stations_metadata(smhi_parameter)

# Compute extracted data and minimum distance to other stations
extracted_data = []
for i, station in enumerate(stations_metadata):
    station_data = {
        'id': station['id'],
        'name': station['name'],
        'latitude': station['latitude'],
        'longitude': station['longitude'],
        'from_timestamp': station['from'],  # Keep original timestamp for calculations
        'to_timestamp': station['to'],      # Keep original timestamp for calculations
        'from': convert_unix_epoch(station['from']),  # Human-readable date
        'to': convert_unix_epoch(station['to']),      # Human-readable date
        'active': station['active']
    }
    # Calculate distances to all other stations
    distances = [
        (calculate_distance(
            station['latitude'], station['longitude'],
            other['latitude'], other['longitude']
        ), other['name'])
        for j, other in enumerate(stations_metadata) if i != j
    ]
    # Find the minimum distance and the corresponding station
    if distances:
        min_distance, closest_station = min(distances, key=lambda x: x[0])
        station_data['min_distance_to_other_stations'] = min_distance
        station_data['closest_station'] = closest_station
    else:
        station_data['min_distance_to_other_stations'] = None
        station_data['closest_station'] = None

    extracted_data.append(station_data)

# Filter the data to keep only active stations and those with a x+ year interval
year_threshold = 60
filtered_data = [
    station for station in extracted_data
    if station['active'] and interval_in_years(station['from_timestamp'], station['to_timestamp']) >= year_threshold
    #if interval_in_years(station['from_timestamp'], station['to_timestamp']) >= year_threshold
]
print(len(filtered_data))

# Save the filtered data as a JSON file
with open(output_file_name, "w", encoding="utf-8") as json_file:
    json.dump(filtered_data, json_file, indent=4)

