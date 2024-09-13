# Concentred_Solar_Plant
This repository hosts the **application and associated data for simulating Concentrated Solar Power (CSP)** systems integrated with molten salt technologies for thermal energy storage in breweries. It aims to assess the feasibility and performance of CSP technologies in various geographic locations, using meteorological and operational data spanning from 2005 to 2020.

This application was developed for the course **LBIRC2201 - Industrial project in chemical and biochemical engineering at the Université catholique de Louvain.** as a part of a report titled “Concentrated solar power and molten salt technologies for the supply and storage of thermal energy for breweries,” aiming to explore the viability of using CSP and molten salts for sustainable and efficient energy management in breweries located in varying climatic zones.

## Repository Structure
```
CSP_Simulation_App/
│
├── app.R                          # Main R script to run the Shiny app
├── Data/                          # Contains all data files
│   ├── Apan_2005_2020.csv         
│   ├── Barranquilla_2005_2020.csv 
│   ├── Belgium_2005_2020.csv      
│   ├── Canaries_2005_2020.csv     
│   ├── hour_data.csv              
│   ├── Optical_efficiency.csv     
│   └── weather_data*.csv          
│
├── R/                             # R scripts for the app's server and UI logic
│   ├── server.R                   
│   ├── ui.R                       
│   └── simulation.R               
│
├── packrat/                       # Packrat library for managing R package dependencies
└── README.md                      
```

## Model Theory
The model works in **two stages** called the sizing and the simulation respectively. The first aims to size the mains components of the CSP plant while the second, uses the sized system to simulate the production and the storage of his thermal energy over one year. 

### Sizing System
The sizing stage of the model is designed to determine the main components of the CSP plant based on either the available area for the solar field or the required thermal output at the evaporator. By calculating the necessary aperture area or potential power deliverable based on the direct normal irradiance (DNI) and collector efficiency, the model provides a estimation to component sizes. 
![image](https://github.com/user-attachments/assets/06b9d734-c070-4d5b-8a96-27e4cadd23d1)

### Production and Storage Simulation
The second stage of the model, utilizes the sized CSP system components to project the system’s operational efficacy throughout a typical meteorological year. This simulation processes inputs such as economic data, thermal energy consumption of the brewery, and meteorological data to forecast outputs including CAPEX, OPEX, and potential savings in natural gas and CO2 emissions. It evaluates thermal energy delivery to the brewery, accounts for solar energy losses through defocusing, and calculates necessary operational metrics such as electricity consumption for pumping and tanks’ heat losses. 
![image](https://github.com/user-attachments/assets/a81b1123-89e1-498d-9966-8f9590989c96)

