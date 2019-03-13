# Shiny App for US National Park Visitor Trends (1900s-2018) and Travel Costs

This app has three main goals:

1. Display previous yearly visitor counts for ten national parks
2. Forecast monthly vistior counts for 2019-2023
3. Calculate the travel cost to get to the national park from southern California (Santa Barbar/LA)

## National Parks Included in the App:
- Arches
- Badlands
- Channel Islands
- Glacier
- Grand Teton
- Redwood
- Shenandoah
- Yellowstone
- Yosemite
- Zion

## Files in this repository:

- all_month_visitation: Monthly visitation data used in the app for the ten parks, ~1979-2018
- all_year_visitation: Yearly visitation data used in the app for the ten parks, ~1900-2018
- Code: contains scripts for the UI and server for this app
- historic_trends_notes: Working document to explore and plot yearly visitation data
- National_Park_Visitation: contains the shiny app
- np_travel_costs: Travel cost data used in the app for the ten parks
- predict_trends_notes: Working document to explore monthly visitation data, forecast monthly visitation to five years (2019-2023), and plot
- Raw_Visitation_Data: monthly and yearly visitation data obtained from the National Park Service
- Travel_cost_metadata: Sources and methods for travel cost data calculations
- travel_cost_notes: Working document to calculate travel cost



## Data Sources:

### Month & Yearly Visitation Data
https://irma.nps.gov/Stats

### Travel Cost Data
all information collected by me (Meghan Bowen)

1. Entrace Fee Data: https://www.nps.gov/aboutus/entrance-fee-prices.htm
2. Campsite Fee Data: https://www.nps.gov/subjects/camping/campground.htm
3. Lodging/Hotel Data: https://www.nationalparkreservations.com/
4. Gas & Mileage Data: https://gasprices.aaa.com/state-gas-price-averages/ :: https://www.bts.gov/content/average-fuel-efficiency-us-light-duty-vehicles :: https://maps.google.com
5. Plane Data: https://www.google.com/flights
6. Boat trip data (Channel Islands NP): https://www.islandpackers.com

