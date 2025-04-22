import pandas as pd

# Load the rainfall data
df = pd.read_csv("C:/Users/tomas/PycharmProjects/pythonProject/Kandidatarbete/csv_and_json/filtered_stations_60_5_day_gap_v2.csv")

# Ensure datetime format
df['Date'] = pd.to_datetime(df['Date'])

# 1. Cut off Stations
df = df[~((df['Station Name'] == 'Finnbacka D') & (df['Date'] < '1962-12-26'))]
df = df[~((df['Station Name'] == 'Heden') & (df['Date'] < '1962-04-23'))]
df = df[~((df['Station Name'] == 'Hyltan') & (df['Date'] < '1952-08-17'))]
df = df[~((df['Station Name'] == 'Håvelund') & (df['Date'] < '1948-02-06'))]
df = df[~((df['Station Name'] == 'Sjögärde') & (df['Date'] < '1960-01-09'))]
df = df[~((df['Station Name'] == 'Åkroken D') & (df['Date'] < '1955-12-31'))]

# 2. Remove specified stations entirely
stations_to_remove = ['Parkajoki', 'Vallentuna', 'Västvalla']
df = df[~df['Station Name'].isin(stations_to_remove)]

# Optional: save the cleaned DataFrame
df.to_csv("C:/Users/tomas/PycharmProjects/pythonProject/Kandidatarbete/csv_and_json/stations_60plusyears_nogaps.csv", index=False)

print("DataFrame cleaned and saved.")
