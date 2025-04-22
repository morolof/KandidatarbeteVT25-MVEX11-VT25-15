import requests
import json
import pandas as pd
from io import StringIO

# Change this parameter to either get daily rainfall, 15 min rainfall, etc see SMHI API parametrization for more
# Remember to change header accordingly as the csv files might look different
# 5 - daily rainfall, 14 - rainfall 15 min
smhi_parameter = 5
output_file_name = "csv_and_json/filtered_stations_60_data.csv"

# Load filtered data from the JSON file
with open("csv_and_json/filtered_stations_60.json", "r", encoding="utf-8") as json_file:
    loaded_data = json.load(json_file)

# Extract station names and IDs
station_names = [station['name'] for station in loaded_data]
station_ids = [station['id'] for station in loaded_data]

# Define the header line you want to find
header = "Från Datum Tid (UTC);Till Datum Tid (UTC);Representativt dygn;Nederbördsmängd;Kvalitet;;Tidsutsnitt:" # smhi_parameter = 5
#header = "Datum;Tid (UTC);Nederbördsmängd;Kvalitet;;Tidsutsnitt:" # smhi_parameter = 14

# Initialize an empty DataFrame to store all stations' data
all_stations_df = pd.DataFrame()

# Loop through all stations and collect data
for station_id, station_name in zip(station_ids, station_names):
    print(f"Fetching data for station: {station_name} (ID: {station_id})")

    # Construct the URL
    url = f"https://opendata-download-metobs.smhi.se/api/version/latest/parameter/{smhi_parameter}/station/{station_id}/period/corrected-archive/data.csv"

    # Request data from the API
    r_data = requests.get(url)
    csv_data = r_data.content.decode("utf-8")  # Convert bytes to string
    lines = csv_data.splitlines()

    # Find the number of rows to skip
    skip_rows = next((i for i, line in enumerate(lines) if line.startswith(header)), None)
    if skip_rows is None:
        print(f"Skipping station {station_name} due to missing header.")
        continue  # Skip this station if the header isn't found

    # Load data into pandas
    df = pd.read_csv(StringIO(csv_data), sep=";", skiprows=skip_rows, usecols=[0, 1, 2, 3, 4])  # smhi_parameter = 5
    #df = pd.read_csv(StringIO(csv_data), sep=";", skiprows=skip_rows, usecols=[0, 1, 2, 3]) # smhi_parameter = 14

    # Rename columns for clarity
    df.columns = ["From Date", "To Date", "Date", "Rainfall (mm)", "Quality"] # smhi_parameter = 5
    #df.columns = ["Date", "Time (UTC)", "Rainfall (mm)", "Quality"] # smhi_parameter = 14

    # Convert 'Date' column to datetime and remove time component
    df["Date"] = pd.to_datetime(df["Date"]).dt.date

    # Add station information
    df["Station Name"] = station_name
    df["Station ID"] = station_id

    # Append to the main DataFrame
    all_stations_df = pd.concat([all_stations_df, df], ignore_index=True)

# Save collected data to a CSV file
all_stations_df.to_csv(output_file_name, index=False, encoding="utf-8")

print(f"Data collection complete. Saved to '{output_file_name}'.")
