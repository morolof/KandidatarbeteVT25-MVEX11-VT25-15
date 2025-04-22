import pandas as pd
import json

# Load filtered station data
with open("C:/Users/tomas/PycharmProjects/pythonProject/Kandidatarbete/csv_and_json/filtered_stations_60.json", "r", encoding="utf-8") as json_file:
    loaded_data = json.load(json_file)

# Extract station names
station_names = [station['name'] for station in loaded_data]

# Load the rainfall data
df = pd.read_csv("C:/Users/tomas/PycharmProjects/pythonProject/Kandidatarbete/csv_and_json/filtered_stations_60_5_day_gap_v2.csv")

# Ensure datetime format
df['Date'] = pd.to_datetime(df['Date'])

# Store results for all stations
all_gaps_list = []

# Iterate over each station
for station in station_names:
    if station not in df['Station Name'].unique():
        continue  # Skip stations not in the dataset

    station_df = df[df['Station Name'] == station].copy()  # Extract data for this station
    station_df = station_df.sort_values(by='Date')  # Sort by date

    # Compute gaps in days
    station_df['Gap_Size'] = station_df['Date'].diff().dt.days

    # Find all gaps where the gap is 2 days or more
    gap_rows = station_df[station_df['Gap_Size'] >= 2]

    for idx, row in gap_rows.iterrows():
        gap_date = row['Date']
        gap_size = row['Gap_Size']

        # Get 3 preceding days
        prev_days = station_df[(station_df['Date'] < gap_date)].tail(3)

        # Get 3 upcoming days
        next_days = station_df[(station_df['Date'] > gap_date)].head(3)

        # Store results
        all_gaps_list.append({
            "Station": station,
            "Gap Start": gap_date - pd.Timedelta(days=gap_size - 1),
            "Gap End": gap_date,
            "Gap Size (days)": gap_size,
            "Prev Day 1 Rainfall": prev_days.iloc[-3]['Rainfall (mm)'] if len(prev_days) >= 3 else None,
            "Prev Day 2 Rainfall": prev_days.iloc[-2]['Rainfall (mm)'] if len(prev_days) >= 2 else None,
            "Prev Day 3 Rainfall": prev_days.iloc[-1]['Rainfall (mm)'] if len(prev_days) >= 1 else None,
            "Next Day 1 Rainfall": next_days.iloc[0]['Rainfall (mm)'] if len(next_days) >= 1 else None,
            "Next Day 2 Rainfall": next_days.iloc[1]['Rainfall (mm)'] if len(next_days) >= 2 else None,
            "Next Day 3 Rainfall": next_days.iloc[2]['Rainfall (mm)'] if len(next_days) >= 3 else None,
        })

# Convert results to a DataFrame
gaps_df = pd.DataFrame(all_gaps_list)

# Display results
print(gaps_df)

# Save to CSV for further analysis
#gaps_df.to_csv("rainfall_gaps_summary.csv", index=False)
