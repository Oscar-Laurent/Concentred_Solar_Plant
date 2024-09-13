# Concentred_Solar_Plant
This repository hosts the **application and associated data for simulating Concentrated Solar Power (CSP)** systems integrated with molten salt technologies for thermal energy storage in breweries. It aims to assess the feasibility and performance of CSP technologies in various geographic locations, using meteorological and operational data spanning from 2005 to 2020.

This application was developed for the course **LBIRC2201 - Industrial project in chemical and biochemical engineering at the Université catholique de Louvain.**

## Repository Structure
```
CSP_Simulation_App/
│
├── app.R                          # Main R script to run the Shiny app
├── Data/                          # Contains all data files
│   ├── Apan_2005_2020.csv         # Data for Apan from 2005 to 2020
│   ├── Barranquilla_2005_2020.csv # Data for Barranquilla from 2005 to 2020
│   ├── Belgium_2005_2020.csv      # Data for Belgium from 2005 to 2020
│   ├── Canaries_2005_2020.csv     # Data for Canaries from 2005 to 2020
│   ├── hour_data.csv              # Hourly data aggregation
│   ├── Optical_efficiency.csv     # Data on optical efficiency of CSP systems
│   └── weather_data*.csv          # Weather data files for various conditions
│
├── R/                             # R scripts for the app's server and UI logic
│   ├── server.R                   # Server logic for the Shiny app
│   ├── ui.R                       # UI definitions for the Shiny app
│   └── simulation.R               # Simulation functions for CSP modeling
│
├── packrat/                       # Packrat library for managing R package dependencies
└── README.md                      # Detailed information about the project
```
