server <- function(input, output) {
  require(tidyverse)


  eta_heliostat = 0.6  # rendement du champ de pannaux
  eta_receiver = 0.9  # rendement du recepteur
  eta_exanger = 0.995  # rendement de l'échangeur
  eta_pump <- 0.75  # Rendement pompe
  eta_heater <- 0.95 #Rendement chauffage électrique tank
  eta_boiler <- 0.95 #Rendement chaudière à gaz
  surcapacity_factor = 0.3
  cp_salt <- 1.56  # kJ/kg.K
  T_salt_in <- 300  # C°
  T_salt_out <- 200  # C°
  Delta_T <- abs(T_salt_out - T_salt_in)  # C°
  salt_density_mean <- 1895  # kg/m3
  salt_density_cold <- 1930 #kg/m3
  steel_density <- 7980  # kg/m3
  g <- 9.81  # [m²/s]
  Sd <- 138  # MPa
  Ca <- 1.6  # mm
  G <- 1.895  # \
  volume_multiplier <- 1.15  # Security volume
  sigma_t <- 100  # [W/m²]  tank heat loss flux
  viscosity_cold <- 0.00417 #[Pa s]
  rugosity_316 <- 1E-6 #[m]
  salt_speed <- 4 #[m/s]
  CA_106 <- 1.6 #[mm] corrosion allowance 106
  S_106 <- 118 #[MPa] maximum allowable stress 106
  density_106 <- 7900 #[kg/m3] density 106
  LVH_gas <- 48 #MJ/kg gas
  price_gas_2019 <- 15.688 #euro/MWh
  price_gas_2022 <- 136.086 #euro/MWh
  electricity_price_2019 <- 111.17 #euro/MWh
  electricity_price_2022 <- 170.95 #euro/MWh
  CO2_eq_gas <- 0.227 #kgCO2eq/kWh PCI
  CO2_eq_electricity <- 0.275 #kgCO2eq/kWh
  salt_density_hot <- 1860


  # DATA RETRIEVAL ----------------------------------------------------------



  output$data_files <- reactive({list.files("./Data", full.names = T)})


  output$columns <- renderUI({
    selectInput('DNI_choosen', 'Choose weather data', names(weather_df)[2:length(weather_df)],
                selected = 1)
  })

  weather_df <- read.csv("./Data/weather_data3.csv")
  weather_df <- weather_df |> mutate(time = as.POSIXct(time))
  #weather_df_spec_year <- weather_df[1:8760, ]
  weather_df_spec_year <- read.csv("./Data/weather_data6.csv")
  weather_df_spec_year <- weather_df_spec_year |> mutate(time = as.POSIXct(time))
  OF_df <- read.csv("./Data/Optical_efficiency2.csv", sep = ",")




  #data_files <- list.files("../Data", full.names = T)
  #time_series_df <- read.table(data_files[1], header = T, skip = 8, nrows = 140256, sep=',' )
  #time_series <- as.POSIXct(time_series_df$time, format = "%Y%m%d:%H%M") |> round(units="hours")
  #df <- data.frame(time = time_series)
  #for (file_name in data_files ) {
  #  dfi <- read.csv(file_name, header = T, skip = 8, nrows = 140256 )
  #  string <- strsplit(file_name, "/")[[1]] |> tail(n=1) |> strsplit("_")
  #  new_name <- string[[1]][1]
  #  colnames(dfi)[2] <-paste(new_name, "DNI", sep = "_")
  #  df <- cbind(df, dfi[2])
  #}
  # df <- df[names(df)[-1]] |> aggregate(by=weather_df["time"], mean)
  #write.csv(df, "Data/weather_data.csv", row.names = FALSE)
  #})

  # !!sym(input$...) :=


  hour_df <- {
    hour_df <- weather_df
    hour_df |> group_by("hour_of_day" = lubridate::hour(time), "day_of_year" = lubridate::yday(time)) |>
      summarise_at(names(hour_df)[-1], list(mean))
  }


  day_df <- {

    day_df <- hour_df
    day_df |> group_by(day_of_year) |>
      summarise_at(names(day_df)[c(-1,-2)], list(sum))
  }


  day_number <- c(355,264,172)


  df_special_day <- reactive({
    req(input$DNI_choosen,hour_df)
    df_special_day <- data.frame(NULL)
    for (i in c(1, 2, 3)){
      dfday <- subset(hour_df, day_of_year == day_number[i]) |> select(day_of_year, hour_of_day, "DNI" = input$DNI_choosen)
      dfday$day_of_year <- as.factor(dfday$day_of_year)
      df_special_day <- rbind(df_special_day, dfday)
    }
    return(df_special_day)
  })


  month_df <- {
    month_df <- weather_df |> mutate(month = format(time, "%m")) |>
      group_by(month) |>
      summarise_at(names(weather_df)[-1], list(mean)) |>
      slice(c(1:12))
  }


  # WEATHER SUMMARY ------------------------------------------------------------



  output$plot_DNI <- renderPlot({
    p <- ggplot(data = df_special_day(), aes(x=hour_of_day, y = DNI, color = day_of_year)) +
      geom_point( size = 1.5 ) +
      geom_line(alpha = 0.7, size =1 )+
      scale_color_manual(values = c("#6699CC", "#CC3333", "#FFCC33"), labels = c("Winter Solstice", "Equinox", "Summer Solstice")) +
      labs(title = sprintf("Evolution of DNI in %s on 3 specifics days", str_split(input$DNI_choosen, "_")[[1]][1]),
           y = "DNI [W/m2]") +
      scale_x_continuous(breaks = scales::pretty_breaks(n = 23)) +
      theme_classic()+
      theme(plot.title = element_text( size=14, face="bold"))
    p
  })


  output$plot_average_DNI <- renderPlot({
    req(input$DNI_choosen)

    df <- month_df |> gather(country, DNI, -month) |>
      subset(country == toString(input$DNI_choosen))

    ggplot(data = df, aes(x=month, y=DNI, group = country, color = country )) +
      geom_line(size=1, alpha = 0.7) +
      geom_point(size = 1.5) +
      scale_x_discrete(labels = month.abb[c(1:12)]) +
      labs(title = "Evolution of monthly average DNI on a average year", y = "DNI [W/m2]") +
      theme_classic()+
      theme(plot.title = element_text( size=14, face="bold"))
  })



  # CSP SIMULATION : SURFACE CALCULATION  -----------------------------------



  design_point_DNI <- reactive({
    month_to_day <- list("06" = 172, "09" = 264, "01" = 355)
    round(df_special_day() |> subset(day_of_year == month_to_day[input$design_month], "DNI")
          |> max() * 0.75)
  })



  #solar_multiple <- reactive({
  #month_to_day <- list("06" = 172, "09" = 264, "01" = 355)
  #h_eq <- df_special_day() |>
  # subset(day_of_year == month_to_day[input$design_month] & DNI > design_point_DNI()) |>
  # nrow()  # count the number of hours above the DNI desing point
  # round((h_eq+input$autonomy_time)/h_eq ,2)
  #})

  solar_multiple <- reactive({
    month_to_day <- list("06" = 172, "09" = 264, "01" = 355)
    h_eq_1 <- df_special_day() |>
      subset(day_of_year == month_to_day[input$design_month] ) |>
      select(c("DNI")) |>
      sum()  # sum of the the values
    h_eq <- h_eq_1/design_point_DNI()
    round((h_eq+input$autonomy_time)/h_eq ,2)
  })

  aperture_surface <- reactive({
    eta_heliostat_LF <- OF_df |> subset(month == as.numeric(input$design_month), input$DNI_choosen) |> pull()
    return(round(input$nominal_power * 1E3 * solar_multiple() * input$area_multiplier / (design_point_DNI()* eta_exanger * eta_receiver* eta_heliostat_LF)))
  })


  total_surface_surface_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return((aperture_surface()/0.295) |> round())
    }

    if(CSP_type() == "Linear Fresnel"){
      return((aperture_surface()/0.582) |> round())
    }
  })


  tower_height_surface_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return((41.85 + (9E-5 * total_surface_surface_sim())) |> round())
    }

    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })


  flux_mass_salt_surface_sim = reactive({
    (input$nominal_power * solar_multiple())/((Delta_T)*cp_salt*eta_exanger)
  })


  nominal_power_surface_sim <- reactive(input$nominal_power)


  storage_volume_surface_sim <- reactive({ #m3
    round((flux_mass_salt_surface_sim()/solar_multiple() * 60 * 60 * input$autonomy_time * input$volume_sufactor) / salt_density_mean)
  })


  CSP_type <- reactive({
    types <- c("Belgium_DNI" = "Linear Fresnel", "Canaries_DNI" = "Linear Fresnel",
               "Apan_DNI" = "Central Tower", "Baranquilla_DNI" = "Central Tower")
    return(types[input$DNI_choosen])
  })


  localisation_surf <- reactive({
    loc <- c("Belgium_DNI" = "Jupille (Belgium)", "Canaries_DNI" = "Las Palmas (Canary Islands)",
             "Apan_DNI" = "Apan (Mexico)", "Baranquilla_DNI" = "Baranquilla (Colombia)")
    return(loc[input$DNI_choosen])
  })


  brewery_en_y_surf <- reactive({
    energy_consumption <- c("Belgium_DNI" = 148205 , "Canaries_DNI" = 15272,
                            "Apan_DNI" = 168600, "Baranquilla_DNI" = 86667)
    return(energy_consumption[input$DNI_choosen])
  })


  brewery_capacity_surf <- reactive({
    brewcap <- c("Belgium_DNI" = 148205 , "Canaries_DNI" = 15272,
                 "Apan_DNI" = 168600, "Baranquilla_DNI" = 86667)
    return(brewcap[input$DNI_choosen])
  })


  advised_power_surf <- reactive({
    advp <- c("Belgium_DNI" = 16.9 , "Canaries_DNI" = 1.7,
              "Apan_DNI" = 19.2, "Baranquilla_DNI" = 9.9)
    return(advp[input$DNI_choosen])
  })


  output$summary_box_surface_sim <- renderUI({
    req(storage_volume_surface_sim(), design_point_DNI(), solar_multiple(), CSP_type())
    box(
      title = span( icon("circle-info", verify_fa = FALSE), "  Summary"),
      status = "warning",
      solidHeader = TRUE,
      align="center",
      width = 6,
      h3("Localisation : ", localisation_surf()),
      h3(paste("Annual energy consumption of brewery (full capacity) : ", brewery_capacity_surf(), " Mwht")),
      h3(paste("Advised nominal power : ", advised_power_surf(), " MWt")),
      h3("CSP type : ", CSP_type()),
      h3("Design point DNI : ", design_point_DNI(), " W/m²"),
      h3("Solar multiple : ", solar_multiple()),
      h3("Flux in of salt theoric : ", round(flux_mass_salt_surface_sim(),2), " Kg/s")
    )
  })


  output$aperure_surface_box <- renderUI({
    fluidRow(
      box(
        title = span( icon("solar-panel", verify_fa = FALSE), " Heliostat field"), status = "primary",
        solidHeader = TRUE,
        align="center",
        width = 6,
        h2("Aperture Surface : ", aperture_surface(), " m²"),
        h2("Total surface : ", total_surface_surface_sim(), "m²"),

        if(CSP_type() == "Central Tower"){
          h2("Tower Height : ", tower_height_surface_sim(), "m" )
        },

        br(),
        withMathJax("This is compute unsing this equation :
                  $$\\frac{Q_{nominal} * 10^3 * SM }
                  {DNI_{design point}* \\eta_{Optical} * \\eta_{Thermal} * \\eta_{exanger}} $$")
      ),

      box(
        title = span( icon("database", verify_fa = FALSE), " Storage"), status = "primary",
        solidHeader = TRUE,
        align="center",
        width = 6,
        h4("You can choose the autonomy time in the SCP Option slider tab"),
        h2("Salt Volume at 250°C : ", storage_volume_surface_sim(), " m³"),
        br(),
        withMathJax("This is compute unsing this equation :
                    $$\\frac{\\dot{m}_{salt} * 60 * 60 * h_{TES}}
                    {\\rho_{salt}} $$")
      ),
    )
  })


  output$production_surface_box <- renderUI({
    box(
      title = span( icon("fire", verify_fa = FALSE), " Annual Heat Production"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      align="center",
      h3(paste("Total Production : ", total_production_surface_sim() , " Mwht")),
      h3(paste("Self sufficiency rate of brewery for full capacity : ", round(total_production_surface_sim()/brewery_en_y_surf()*100,1) , "%")),
      h3(paste("Energy coverage rate for input power : ", self_sufficiency_rate_surface_sim() , " %")),
      h3(paste("Utilized energy vs theoretical available energy without defocalisation : ", efficiency_ratio_surface_sim() , " %"))

    )
  })


  total_production_surface_sim <- reactive({
    (simulation_output_df_surface_sim() |> select("production") |> pull() |>
       sum() ) |> round() /1e3
  })



  self_sufficiency_rate_surface_sim <- reactive({
    (total_production_surface_sim()*100 * 1e3/(input$nominal_power * 24 * 365 )) |> round()
  })


  simulation_output_df_surface_sim <- reactive({
    df <- simulation_output_surface_sim() |> as.data.frame()
    df[,"power_to_HTF"] <-  df |> select("m_salt_in") * cp_salt * Delta_T /3600
    df[,"production"] <-  df |> select("m_salt_out") * cp_salt * Delta_T * eta_exanger /3600
    return(df)

  })


  simulation_output_surface_sim <- eventReactive(input$Run_surface_sim, {
    CSP_simulation(weather_df_spec_year, OF_df, country = input$DNI_choosen, firm_capacity = input$nominal_power * 1E3 ,
                   aperture_surface = aperture_surface(), storage_volume = storage_volume_surface_sim(), by = "hour", solar_multiple = solar_multiple()* 0.995)
  })


  output$plot_sim_surface <- renderPlotly(
    plot_out_simulation(simulation_output_surface_sim())
  )


  output$heatmap_sim_surface <- renderPlotly({
    require(hrbrthemes)
    time_data <- seq(from=as.POSIXct("2000-1-1 0:00"), to=as.POSIXct("2000-12-30 23:00"),
                     by="hour")|>
      format("%d-%m %H") |>
      as.POSIXct(format="%d-%m %H ")

    hours <- c(0:23)
    day_of_year <- c(1:365)
    heat_map_data <- expand.grid(hour=hours, day_of_year=day_of_year)
    heat_map_data$production <- simulation_output_df_surface_sim() |>
      select("production") |>
      pull()
    heat_map_data <- heat_map_data %>%
      mutate(text = paste0("Hour of day: ", hour, "\n", "Day of year: ", day_of_year,
                           "\n", "Production : ",round(production), " kWht"))

    p <- ggplot(heat_map_data, aes(x=as.Date(day_of_year, origin = "1999-12-31"),
                                   y=hour, fill= production, text=text)) +
      geom_tile() +
      scale_fill_gradient(low = "blue4", high = "#de2828") +
      theme_ipsum() +
      labs(x = "Month") +
      #theme(legend.position = "none") +
      theme(plot.margin = unit(c(0.5,0,0.5,0.5), "cm"), axis.text.x=element_text(size=15),
            axis.text.y=element_text(size=15), axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15)) +
      scale_x_date(date_breaks = "month", date_labels = "%b")

    ggplotly(p, tooltip = "text")
  })


  efficiency_df_surface_sim <- reactive({
    eff_df <- weather_df_spec_year |> select(time, input$DNI_choosen)
    eff_df$time <- eff_df$time |> format("%d-%m %H:%m") |> as.POSIXct(format="%d-%m %H:%m")
    #eff_df <- eff_df |> group_by(time) |> summarise_at(names(eff_df)[-1], list(mean))
    eff_df[,"OF"] <- OF_df[lubridate::month(as.Date(eff_df$time, origin = "1999-12-31")), input$DNI_choosen]
    eff_df[, "P_th"] <- eff_df |> select(input$DNI_choosen) * aperture_surface() * eff_df$OF * eta_receiver /1000
    eff_df[, "P_real"] <- simulation_output_df_surface_sim() |> select("power_to_HTF")
    eff_df[,"loss"] <- ifelse(eff_df$P_th - eff_df$P_real >=0 , eff_df$P_th - eff_df$P_real, 0)
    return(eff_df)
  })


  output$efficiency_plot_surface_sim <- renderPlotly({
    p <- ggplot(data = efficiency_df_surface_sim(), aes(x = time |> as.Date(), y = loss)) +
      scale_x_date(date_breaks = "month", date_labels = "%b") +
      geom_line(color = "brown2") +
      labs(title = "Evolution of the loss power", y = "Loss power [kWh]")
    ggplotly(p)
  })


  efficiency_ratio_surface_sim <- reactive({
    real_tot_prod <- efficiency_df_surface_sim() |> select("P_real")  |> sum(na.rm = TRUE)
    tot_losses <- efficiency_df_surface_sim() |> select("loss") |> sum(na.rm = TRUE)
    (real_tot_prod * 100 /(real_tot_prod+tot_losses)) |> round()
  })


  production_surface_df <- reactive({ simulation_output_df_surface_sim() |> select("production")
  })

  output$downloadData_surface_sim <- downloadHandler(
    filename = function() {"Surface_sim.csv"},

    content = function(file) {
      write.csv(production_surface_df(), file, row.names = FALSE)
    }
  )


  output$stacked_plot_surface_sim <- renderPlotly({
    OF  <- OF_df[lubridate::month(as.Date(weather_df_spec_year$time, origin = "1999-12-31")), input$DNI_choosen]
    Q_solar <- weather_df_spec_year |> select(input$DNI_choosen) * aperture_surface() * OF /1e3  # [kW]
    df1 <- simulation_output_df_surface_sim()
    df1$time <- (df1$time |> round() * 3600) |> as.POSIXct(origin = "1999-12-31 23:00:00")
    df1$Q_solar <- Q_solar |> pull()
    df1$Q_lost <- ifelse(df1$Q_solar - (input$nominal_power * solar_multiple()*0.995) < 0, 0 , df1$Q_solar - (input$nominal_power * solar_multiple()*0.995))
    df1 <- df1 |> group_by("day" = lubridate::yday(df1$time)) |> summarise_at(c("power_to_HTF" , "Q_solar", "Q_lost"), list(sum))

    df2 <- simulation_output_df_surface_sim()
    df2$time <- (df2$time |> round() * 3600) |> as.POSIXct(origin = "1999-12-31 23:00:00")

    p <- stacked_plot(df1, df2, comparaison = input$comparaison_surface_sim)
    return(p)
  })


  output$bar_plot_surface_sim <-renderPlotly({
    OF  <- OF_df[lubridate::month(as.Date(weather_df_spec_year$time, origin = "1999-12-31")), input$DNI_choosen]
    Q_solar <- weather_df_spec_year |> select(input$DNI_choosen) * aperture_surface() * OF /1e3  # [kW]
    df1 <- simulation_output_df_surface_sim()
    df1$Q_solar <- Q_solar |> pull()
    df1$Q_lost <- ifelse(df1$Q_solar - (input$nominal_power * solar_multiple()*0.995) < 0, 0 , df1$Q_solar - (input$nominal_power * solar_multiple()*0.995))
    p <- bar_plot(df1, comparaison = input$comparaison_bar_plot_surface_sim)
    return(p)
  })



  # CSP SIMULATION : POWER CALCULATION  -------------------------------------


  aperture_surface_power_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return((input$total_surface*0.295) |> round())
    }

    if(CSP_type() == "Linear Fresnel"){
      return((input$total_surface*0.582) |> round())
    }
  })


  tower_height_power_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return((41.85 + (9E-5 * input$total_surface)) |> round())
    }

    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })


  nominal_power <- reactive({
    eta_heliostat_LF <- OF_df |> subset(month == as.numeric(input$design_month), input$DNI_choosen) |> pull()
    return(round(aperture_surface_power_sim()  * design_point_DNI()* eta_exanger * eta_receiver* eta_heliostat_LF /(1E3 * solar_multiple())))
  })


  flux_mass_salt_power_sim = reactive({
    (nominal_power() * solar_multiple())/((Delta_T)*cp_salt*eta_exanger)
  })


  storage_volume_power_sim <- reactive({ #m3
    round((flux_mass_salt_power_sim()/solar_multiple() * 60 * 60 * input$autonomy_time * input$volume_sufactor) / salt_density_mean)
  })


  brewery_en_y_power <- reactive({
    energy_consumption <- c("Belgium_DNI" = 148205 , "Canaries_DNI" = 15272,
                            "Apan_DNI" = 168600, "Baranquilla_DNI" = 86667)
    return(energy_consumption[input$DNI_choosen])
  })


  brewery_capacity_power <- reactive({
    brewcap <- c("Belgium_DNI" = 148205 , "Canaries_DNI" = 15272,
                 "Apan_DNI" = 168600, "Baranquilla_DNI" = 86667)
    return(brewcap[input$DNI_choosen])
  })


  available_area <- reactive({
    ava <- c("Belgium_DNI" = "66650" , "Canaries_DNI" = "22302" ,
             "Apan_DNI" = "Unconstrained", "Baranquilla_DNI" = "Unconstrained")
    return(ava[input$DNI_choosen])
  })


  localisation_pow <- reactive({
    loc <- c("Belgium_DNI" = "Jupille (Belgium)", "Canaries_DNI" = "Las Palmas (Canary Islands)",
             "Apan_DNI" = "Apan (Mexico)", "Baranquilla_DNI" = "Baranquilla (Colombia)")
    return(loc[input$DNI_choosen])
  })


  output$summary_box_power_sim <- renderUI({
    req(storage_volume_power_sim(), design_point_DNI(), solar_multiple())
    box(
      title = span( icon("circle-info", verify_fa = FALSE), "  Summary"),
      status = "warning",
      solidHeader = TRUE,
      align="center",
      width = 6,
      h3("Localisation : ", localisation_pow()),
      h3(paste("Annual energy consumption of brewery (full capacity) : ", brewery_capacity_power(), " Mwht")),
      h3(paste("Maximal theoretical available area : ", available_area() , "(m²)")),
      h3("CSP type : ", CSP_type()),
      h3("Design point DNI : ", design_point_DNI(), " W/m²"),
      h3("Solar multiple : ", solar_multiple()),
      h3("Flux out of salt : ", round(flux_mass_salt_power_sim(),2), " Kg/s"),
      h3("Total Field area : ", input$total_surface, " m²"),
      h3("Apperture area : ", aperture_surface_power_sim(), " m²"),
      if(CSP_type() == "Central Tower"){
        h3("Tower Height : ", tower_height_power_sim(), " m")
      }
    )
  })


  output$nominal_power_box <- renderUI({
    fluidRow(
      box(
        title = span( icon("fire", verify_fa = FALSE), " Nominal Power to Steam"), status = "primary",
        solidHeader = TRUE,
        align="center",
        width = 6,
        h2(paste(nominal_power(), " kW")),
        br(),
        withMathJax("This is compute unsing this equation :
                    $$\\frac{S_{aperture}*DNI_{design point}* \\eta_{Optical} * \\eta_{Thermal} * \\eta_{exanger}}
                    {10^3 * SM} $$")
      ),

      box(
        title = span( icon("database", verify_fa = FALSE), " Storage"), status = "primary",
        solidHeader = TRUE,
        align="center",
        width = 6,
        h4("You can choose the autonomy time in the SCP Option slider tab"),
        h2("Salt Volume at 250°C : ", storage_volume_power_sim(), " m³"),
        br(),
        withMathJax("This is compute unsing this equation :
                    $$\\frac{\\dot{m}_{salt} * 60 * 60 * h_{TES}}
                    {\\rho_{salt}} $$")
      ),

    )
  })


  output$production_power_box <- renderUI({
    box(
      title = span( icon("fire", verify_fa = FALSE), " Annual Heat Production"),
      status = "primary",
      solidHeader = TRUE,
      width = 12,
      align="center",
      h3(paste("Total Production : ", total_production_power_sim() , " Mwht")),
      h3(paste("Self sufficiency rate of brewery for full capacity : ", round(total_production_power_sim()/brewery_en_y_power()*100,1) , "%")),
      h3(paste("Energy coverage rate for input power : ", self_sufficiency_rate_power_sim() , " %")),
      h3(paste("Utilized energy vs theoretical available energy without defocalisation : ", efficiency_ratio_power_sim() , " %"))
    )
  })


  simulation_output_power_sim <- eventReactive(input$Run_power_sim, {
    CSP_simulation(weather_df_spec_year, OF_df, country = input$DNI_choosen, firm_capacity = nominal_power() * 1E3,
                   aperture_surface = aperture_surface_power_sim(),
                   storage_volume = storage_volume_power_sim(), by = "hour" , solar_multiple = solar_multiple() )*0.995
  })


  total_production_power_sim <- reactive({
    (simulation_output_df_power_sim() |> select("production") |> pull() |>
       sum() ) |> round() /1e3
  })


  self_sufficiency_rate_power_sim <- reactive({
    (total_production_power_sim()*100 * 1e3/(nominal_power() * 24 * 365 )) |> round()
  })


  simulation_output_df_power_sim <- reactive({
    df <- simulation_output_power_sim() |> as.data.frame()
    df[,"power_to_HTF"] <-  df |> select("m_salt_in") * cp_salt * Delta_T /3600
    df[,"production"] <-  df |> select("m_salt_out") * cp_salt * Delta_T * eta_exanger /3600
    return(df)

  })


  output$plot_power_sim <- renderPlotly(
    return(plot_out_simulation(simulation_output_power_sim()))
  )


  output$heatmap_power_sim <- renderPlotly({
    require(hrbrthemes)
    hours <- c(0:23)
    day_of_year <- c(1:365)
    heat_map_data <- expand.grid(hour=hours, day_of_year=day_of_year)
    heat_map_data$production <- simulation_output_df_power_sim() |>
      select("production") |>
      pull()
    heat_map_data <- heat_map_data %>%
      mutate(text = paste0("Hour of day: ", hour, "\n", "Day of year: ", day_of_year,
                           "\n", "Production: ",round(production), " kWht"))

    p <- ggplot(heat_map_data, aes(x=as.Date(day_of_year, origin = "1999-12-31"),
                                   y=hour, fill= production, text=text)) +
      geom_tile() +
      scale_fill_gradient(low = "blue4", high = "#de2828") +
      theme_ipsum() +
      labs(x = "Month") +
      #theme(legend.position = "none") +
      theme(plot.margin = unit(c(0.5,0,0.5,0.5), "cm"), axis.text.x=element_text(size=15),
            axis.text.y=element_text(size=15), axis.title.x=element_text(size=15),
            axis.title.y=element_text(size=15)) +
      scale_x_date(date_breaks = "month", date_labels = "%B")

    ggplotly(p, tooltip = "text")
  })


  efficiency_df_power_sim <- reactive({
    eff_df <- weather_df_spec_year |> select(time, input$DNI_choosen)
    eff_df$time <- eff_df$time |> format("%d-%m %H:%m") |> as.POSIXct(format="%d-%m %H:%m")
    #eff_df <- eff_df |> group_by(time) |> summarise_at(names(eff_df)[-1], list(mean))
    eff_df[,"OF"] <- OF_df[lubridate::month(as.Date(eff_df$time, origin = "1999-12-31")), input$DNI_choosen]
    eff_df[, "P_th"] <- eff_df |> select(input$DNI_choosen) * aperture_surface_power_sim() * eff_df$OF * eta_receiver /1000
    eff_df[, "P_real"] <- simulation_output_df_power_sim() |> select("power_to_HTF")
    eff_df[,"loss"] <- ifelse(eff_df$P_th - eff_df$P_real >=0 , eff_df$P_th - eff_df$P_real, 0)
    return(eff_df)
  })


  output$efficiency_plot_power_sim <- renderPlotly({
    p <- ggplot(data = efficiency_df_power_sim(), aes(x = time |> as.Date(), y = loss)) +
      scale_x_date(date_breaks = "month", date_labels = "%b") +
      geom_line(color = "brown2") +
      labs(title = "Evolution of the loss power", y = "Loss power [kWh]")
    ggplotly(p)
  })


  efficiency_ratio_power_sim <- reactive({
    real_tot_prod <- efficiency_df_power_sim() |> select("P_real")  |> sum(na.rm = TRUE)
    tot_losses <- efficiency_df_power_sim() |> select("loss") |> sum(na.rm = TRUE)
    (real_tot_prod * 100 /(real_tot_prod+tot_losses)) |> round()
  })

  production_power_df <- reactive({ simulation_output_df_power_sim() |> select("production")
  })

  output$downloadData_power_sim <- downloadHandler(
    filename = function() {"Power_sim.csv"},

    content = function(file) {
      write.csv(production_power_df(), file, row.names = FALSE)
    }
  )


  output$stacked_plot_power_sim <- renderPlotly({
    OF  <- OF_df[lubridate::month(as.Date(weather_df_spec_year$time, origin = "1999-12-31")), input$DNI_choosen]
    Q_solar <- weather_df_spec_year |> select(input$DNI_choosen) * aperture_surface_power_sim() * OF /1e3  # [kW]
    df1 <- simulation_output_df_power_sim()
    df1$time <- (df1$time |> round() * 3600) |> as.POSIXct(origin = "1999-12-31 23:00:00")
    df1$Q_solar <- Q_solar |> pull()
    df1$Q_lost <- ifelse(df1$Q_solar - (nominal_power() * solar_multiple()*0.995) < 0, 0 , df1$Q_solar - (nominal_power() * solar_multiple()*0.995))
    df1 <- df1 |> group_by("day" = lubridate::yday(df1$time)) |> summarise_at(c("power_to_HTF" , "Q_solar", "Q_lost"), list(sum))

    df2 <- simulation_output_df_power_sim()
    df2$time <- (df2$time |> round() * 3600) |> as.POSIXct(origin = "1999-12-31 23:00:00")

    p <- stacked_plot(df1, df2, comparaison = input$comparaison_power_sim)
    return(p)
  })

  output$bar_plot_power_sim <-renderPlotly({
    OF  <- OF_df[lubridate::month(as.Date(weather_df_spec_year$time, origin = "1999-12-31")), input$DNI_choosen]
    Q_solar <- weather_df_spec_year |> select(input$DNI_choosen) * aperture_surface_power_sim() * OF /1e3  # [kW]
    df1 <- simulation_output_df_power_sim()
    df1$Q_solar <- Q_solar |> pull()
    df1$Q_lost <- ifelse(df1$Q_solar - (nominal_power() * solar_multiple()*0.995) < 0, 0 , df1$Q_solar - (nominal_power() * solar_multiple()*0.995))
    p <- bar_plot(df1, comparaison = input$comparaison_bar_plot_power_sim)
    return(p)
  })

  # ECONOMICAL ANALYSIS : SURFACE CALCULATION  -----------------------------------



  tank_volume_surface_sim <- reactive({  # Real tank volume
    storage_volume_surface_sim() * volume_multiplier
  })


  tank_electrical_heating_surface_sim <- reactive({
    ((2 * sigma_t * 6 * pi * 365 * 24 * 1E-6 * (tank_volume_surface_sim()/ (2 * pi))^(2/3))/eta_heater) |> round()
  })

  Pump_energy_surface_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      D_pipe <- (flux_mass_salt_surface_sim()/salt_density_cold)/salt_speed
      Re_surface_sim <- (salt_density_cold*4*D_pipe)/viscosity_cold
      A <- (2.457*log(1/((7/Re_surface_sim)^(0.9)+(0.27*rugosity_316/D_pipe))))^(16)
      B <- (37530/Re_surface_sim)^(16)
      D_W_friction <- 4*(((8/Re_surface_sim)^(12))+(1/((A+B)^(3/2))))^(1/2)
      Friction_losses_surface_sim <- (D_W_friction*tower_height_surface_sim()*(salt_speed)^2)/(2*D_pipe)
      Mechanical_energy_surface_sim <- (g*tower_height_surface_sim()) + Friction_losses_surface_sim
      power_array <- simulation_output_df_surface_sim() |> select("m_salt_in") |>
        pull() * Mechanical_energy_surface_sim /(eta_pump * 3.61E9)
      return(power_array |> sum() |> round())
    }
    if(CSP_type() == "Linear Fresnel"){
      return(0)
    }
  })
  #salt_density_cold taken because pumped salt is cold

  mass_106_surface_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      D_pipe_mean <- (flux_mass_salt_surface_sim()/salt_density_cold)/salt_speed
      r_pipe_mean <- D_pipe_mean/2
      design_pressure <- 1E-6 * (201E5 + tower_height_surface_sim()*g*salt_density_mean)
      pipe_thickness_106 <- ((design_pressure*1000*r_pipe_mean)/(S_106 - 0.6*design_pressure)+CA_106)/1000
      pipe_volume_106 <- pi * tower_height_surface_sim() *((r_pipe_mean+pipe_thickness_106)^(2)-(r_pipe_mean)^(2))
      return(pipe_volume_106*density_106)
    }
    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })

  salt_mass_surface_sim <- reactive({
    if(CSP_type() == "Central Tower") {
      return((tank_volume_surface_sim()*salt_density_mean/1000) |> round())
    }
    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })


  #pump_losses_surfaces_sim <- reactive({
  #   if(CSP_type() == "Central Tower"){
  #    power_array <- simulation_output_df_surface_sim() |> select("m_salt_in") |>
  #     pull() * g * tower_height_surface_sim() /(eta_pump * 3.61E9)
  #  return(power_array |> sum() |> round())
  #}

  #if(CSP_type() == "Linear Fresnel"){
  # return(0)
  #}
  #})


  mass_316_surface_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      tank_diameter <- 2 * (tank_volume_surface_sim()/(2*pi))^(1/3)
      tank_thickness_surface_sim <- (((4.9 * tank_diameter * (tank_diameter - 0.3) * G) / Sd) + Ca) * 1E-3
      tank_radius <- tank_diameter/2
      tank_internal_volume <- (tank_radius)^(2)*tank_diameter*pi
      tank_external_volume <- pi*(tank_radius + tank_thickness_surface_sim)^(2)*(tank_diameter+2*tank_thickness_surface_sim)
      return(steel_density*(tank_external_volume - tank_internal_volume))
    }
    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })

  steel_mass_surface_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return((mass_106_surface_sim() + mass_316_surface_sim())/1000 |> round())
    }
    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })

  steel_costs_surface_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return((1.15*mass_106_surface_sim()+4.99*mass_316_surface_sim()) |> round())
    }
    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })

  total_electrical_energy_surface_sim <- reactive(tank_electrical_heating_surface_sim()+Pump_energy_surface_sim())


  total_gas_savings_surface_sim <- reactive(((3.6*total_production_surface_sim())/(LVH_gas*eta_boiler)) |> round())

  gas_savings_2019_surface_sim <- reactive(total_production_surface_sim()*price_gas_2019/eta_boiler)

  gas_savings_2022_surface_sim <- reactive(total_production_surface_sim()*price_gas_2022/eta_boiler)

  CO2_gas_surface_sim <- reactive(LVH_gas*CO2_eq_gas*total_gas_savings_surface_sim()/3.6)

  CO2_electricity_surface_sim <- reactive(CO2_eq_electricity*total_electrical_energy_surface_sim())

  CO2_balance_surface_sim <- reactive(- CO2_gas_surface_sim() + CO2_electricity_surface_sim())

  electricity_2019_surface_sim <- reactive(total_electrical_energy_surface_sim()*electricity_price_2019)

  electricity_2022_surface_sim <- reactive(total_electrical_energy_surface_sim()*electricity_price_2022)

  output$summary_box_eco_analysis_surface <- renderUI({
    if(input$Run_surface_sim){
      if(CSP_type() == "Central Tower"){
        box(
          title = span( icon("circle-info", verify_fa = FALSE), "  Energy and dimensioning summary"),
          status = "warning",
          solidHeader = TRUE,
          align="center",
          width = 16,
          h3(paste("Total production : ", total_production_surface_sim() |> round(), " MWht")),
          h3("Tank heating energy :" , tank_electrical_heating_surface_sim(), " MWhe"),
          h3("Pumping energy : " , Pump_energy_surface_sim(), " MWhe"),
          h3("Total electricity consumption : " , total_electrical_energy_surface_sim(), " MWhe"),
          h3("Pipes and tanks steel mass : " , steel_mass_surface_sim() |> round(), " tons"),
          h3("Salt mass : ", salt_mass_surface_sim(), "tons")
        )
      }
      else{
        box(
          title = span( icon("circle-info", verify_fa = FALSE), "  Energy and dimensioning summary"),
          status = "warning",
          solidHeader = TRUE,
          align="center",
          width = 16,
          h3(paste("Total production : ", total_production_surface_sim() |> round(), " MWht")),
          h3("Tank heating energy :" , tank_electrical_heating_surface_sim(), " MWhe"),
          h3("Total electricity consumption : " , total_electrical_energy_surface_sim(), " MWhe")
        )
      }
    }
    else{
      code("You must first run the SURFACE simulation in the CSP simulation : surface
        calculation tab in order to get the economical analysis", color = "red")
    }
  }
  )

  output$Energy_equivalents_box_eco_analysis_surface <- renderUI({
    if(input$Run_surface_sim){
      box(
        title = span( icon("circle-info", verify_fa = FALSE), " Energy and dimensioning equivalents"),
        status = "warning",
        solidHeader = TRUE,
        align="center",
        width = 16,
        h3("Gas replaced : " , total_gas_savings_surface_sim() |> round(), " tons"),
        h3("Gas savings based on 2019 price : " , gas_savings_2019_surface_sim() |> round(), " €"),
        h3("Gas savings based on 2022 price : " , gas_savings_2022_surface_sim() |> round(), " €"),
        h3("Electricity spendings based on 2019 price :", electricity_2019_surface_sim() |> round()," €"),
        h3("Electricity spendings based on 2022 price :", electricity_2022_surface_sim() |> round()," €"),
        h3("Carbon dioxide or equivalents reduction : ", CO2_gas_surface_sim() |> round(), " tons"),
        h3("Carbon dioxide or equivalents emissions from electricity : ", CO2_electricity_surface_sim() |> round(), " tons"),
        h3("Carbon dioxide balance : ", CO2_balance_surface_sim() |> round(), " tons")
      )
    }
    else{
      code("You must first run the SURFACE simulation in the CSP simulation : surface
        calculation tab in order to get the economical analysis", color = "red")
    }
  })

  evaporator_costs_surface_sim <- reactive(((18000*nominal_power_surface_sim()/(1000*1.05)) |> round()))

  tower_receiver_costs_surface_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return(285*nominal_power_surface_sim()/1.05 |> round())
    }
    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })

  heliostat_field_costs_surface_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return((aperture_surface()*140/1.05) |> round())
    }
    if(CSP_type() == "Linear Fresnel"){
      return((aperture_surface()*126))
    }
  })

  total_capex_Chris_surface_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return(((1.04*(steel_costs_surface_sim() + evaporator_costs_surface_sim() + tower_receiver_costs_surface_sim() + heliostat_field_costs_surface_sim())) + salt_mass_surface_sim()*1600) |> round())
    }
  })

  storage_costs_surface_sim <- reactive((22*((tank_volume_surface_sim()*cp_salt*salt_density_mean*Delta_T)/(3600*1.05))) |> round())


  total_capex_surface_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return((1.04*(22*((tank_volume_surface_sim()*cp_salt*salt_density_mean*Delta_T)/(3600*1.05)) + evaporator_costs_surface_sim() + tower_receiver_costs_surface_sim() + heliostat_field_costs_surface_sim())))
    }
    if(CSP_type() == "Linear Fresnel"){
      return(1.04*(heliostat_field_costs_surface_sim() + evaporator_costs_surface_sim() + 22*(tank_volume_surface_sim()*cp_salt*salt_density_mean*Delta_T)/(3600*1.05)))
    }
  })

  electricity_average_surface_sim <- reactive((electricity_2019_surface_sim() + electricity_2022_surface_sim())/2)

  costs_kw_surface_sim <- reactive((total_capex_surface_sim()/nominal_power_surface_sim()) |> round(2))


  brewery_en_y_heatp <- reactive({
    energy_consumption <- c("Belgium_DNI" = 148205 , "Canaries_DNI" = 15272,
                            "Apan_DNI" = 168600, "Baranquilla_DNI" = 86667)
    return(energy_consumption[input$DNI_choosen])
  })


  contingency_surface_sim <- 4

  opex_surface_sim <- reactive({(0.02*total_capex_surface_sim()) |> round()})

  Costs_kwh_10_years_surface_sim <- reactive(((total_capex_surface_sim()+10*opex_surface_sim()+10*electricity_average_surface_sim())/(10*(total_production_surface_sim()))) |> round(2))

  Costs_kwh_20_years_surface_sim <- reactive(((total_capex_surface_sim()+20*opex_surface_sim()+20*electricity_average_surface_sim())/(20*(total_production_surface_sim()))) |> round(2))

  Costs_MWh_10_years_gas_2019_surface_sim <- reactive(
    ((total_capex_surface_sim()+10*opex_surface_sim()+10*electricity_average_surface_sim()+10*(price_gas_2019*(brewery_en_y_heatp() - total_production_surface_sim())))/(10*brewery_en_y_heatp())) |> round(2))

  Costs_MWh_20_years_gas_2019_surface_sim <- reactive(
    ((total_capex_surface_sim()+20*opex_surface_sim()+20*electricity_average_surface_sim()+20*(price_gas_2019*(brewery_en_y_heatp() - total_production_surface_sim())))/(20*brewery_en_y_heatp())) |> round(2))

  Costs_MWh_10_years_gas_2022_surface_sim <- reactive(
    ((total_capex_surface_sim()+10*opex_surface_sim()+10*electricity_average_surface_sim()+10*(price_gas_2022*(brewery_en_y_heatp() - total_production_surface_sim())))/(10*brewery_en_y_heatp())) |> round(2))

  Costs_MWh_20_years_gas_2022_surface_sim <- reactive(
    ((total_capex_surface_sim()+20*opex_surface_sim()+20*electricity_average_surface_sim()+20*(price_gas_2022*(brewery_en_y_heatp() - total_production_surface_sim())))/(20*brewery_en_y_heatp())) |> round(2))


  output$System_costs_components_prices_surface <- renderUI({
    if(input$Run_surface_sim){
      if(CSP_type() == "Central Tower"){
        box(
          title = span( icon("circle-info", verify_fa = FALSE), " System costs using components prices"),
          status = "warning",
          solidHeader = TRUE,
          align="center",
          width = 16,
          #h3("Tanks and pipes steel : " , steel_costs_surface_sim()/1.05 |> round(), " €"),
          #h3("Salt : " , (salt_mass_surface_sim()*1600) |> round(), " €"),
          h3("Evaporator : ", evaporator_costs_surface_sim() |> round(), " €"),
          h3("Electricity spendings based on 2019 and 2022 average price : ", electricity_average_surface_sim() |> round(), " €"),
          h3("Tower and receiver : ", tower_receiver_costs_surface_sim() |> round(), " €"),
          h3("Heliostat field: ", heliostat_field_costs_surface_sim() |> round(), "€"),
          h3("Thermal storage: ", storage_costs_surface_sim() |> round(), " €"),
          h3("Total Capex : ",total_capex_surface_sim() |> round(), " €"),
          #h3("Total Capex using components prices: ", total_capex_Chris_surface_sim(), " €"),
          h3("using ", contingency_surface_sim,"% of contingency"),
          h3("Total Opex based on 2% of Capex: ", opex_surface_sim() |> round(), " €"),
          h3("Costs per MWh produced on 10 years : ", Costs_kwh_10_years_surface_sim(), " €/MWh"),
          h3("Costs per MWh produced on 20 years : ", Costs_kwh_20_years_surface_sim(), " €/MWh"),
          h3("Costs per MWh consumed on 10 years with natural gas 2019: ", Costs_MWh_10_years_gas_2019_surface_sim() |> round(2), " €/MWh"),
          h3("Costs per MWh consumed on 10 years with natural gas 2022: ", Costs_MWh_10_years_gas_2022_surface_sim() |> round(2), " €/MWh"),
          h3("Costs per MWh consumed on 20 years with natural gas 2019: ", Costs_MWh_20_years_gas_2019_surface_sim() |> round(2), " €/MWh"),
          h3("Costs per MWh consumed on 20 years with natural gas 2022: ", Costs_MWh_20_years_gas_2022_surface_sim() |> round(2), " €/MWh")
        )
      }
      else{
        box(
          title = span( icon("circle-info", verify_fa = FALSE), " System costs using components prices"),
          status = "warning",
          solidHeader = TRUE,
          align="center",
          width = 16,
          h3("Evaporator : ", evaporator_costs_surface_sim()/1.05 |> round(), " €"),
          h3("Electricity spendings based on 2019 and 2022 average price : ", electricity_average_surface_sim() |> round(), " €"),
          h3("Thermal storage: ", storage_costs_surface_sim() |> round(), " €"),
          h3("Heliostat field and receiver: ", heliostat_field_costs_surface_sim() |> round(), "€"),
          h3("Total capex : ",total_capex_surface_sim() |> round(), " €"),
          h3("using ", contingency_surface_sim,"% of contingency"),
          h3("Total Opex based on 2% of Capex: ", opex_surface_sim() |> round(), " €"),
          h3("Costs per MWh produced on 10 years : ", Costs_kwh_10_years_surface_sim(), " €/MWh"),
          h3("Costs per MWh produced on 20 years : ", Costs_kwh_20_years_surface_sim(), " €/MWh"),
          h3("Costs per MWh consumed on 10 years with natural gas 2019: ", Costs_MWh_10_years_gas_2019_surface_sim() |> round(2), " €/MWh"),
          h3("Costs per MWh consumed on 10 years with natural gas 2022: ", Costs_MWh_10_years_gas_2022_surface_sim() |> round(2), " €/MWh"),
          h3("Costs per MWh consumed on 20 years with natural gas 2019: ", Costs_MWh_20_years_gas_2019_surface_sim() |> round(2), " €/MWh"),
          h3("Costs per MWh consumed on 20 years with natural gas 2022: ", Costs_MWh_20_years_gas_2022_surface_sim() |> round(2), " €/MWh")
        )
      }
    }
    else{
      code("You must first run the SURFACE simulation in the CSP simulation : surface
        calculation tab in order to get the economical analysis", color = "red")
    }
  })





  # ECONOMICAL ANALYSIS : POWER CALCULATION  ------------------------------------

  tank_volume_power_sim <- reactive({  # Real tank volume
    storage_volume_power_sim() * volume_multiplier
  })


  tank_electrical_heating_power_sim <- reactive({
    ((2 * sigma_t * 6 * pi * 365 * 24 * 1E-6 * (tank_volume_power_sim()/ (2 * pi))^(2/3))/eta_heater) |> round()
  })


  Pump_energy_power_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      D_pipe <- (flux_mass_salt_power_sim()/salt_density_cold)/salt_speed
      Re_power_sim <- (salt_density_cold*4*D_pipe)/viscosity_cold
      A <- (2.457*log(1/((7/Re_power_sim)^(0.9)+(0.27*rugosity_316/D_pipe))))^(16)
      B <- (37530/Re_power_sim)^(16)
      D_W_friction <- 4*(((8/Re_power_sim)^(12))+(1/((A+B)^(3/2))))^(1/2)
      Friction_losses_power_sim <- (D_W_friction*tower_height_power_sim()*(salt_speed)^2)/(2*D_pipe)
      Mechanical_energy_power_sim <- (g*tower_height_surface_sim()) + Friction_losses_power_sim
      power_array <- simulation_output_df_power_sim() |> select("m_salt_in") |>
        pull() * Mechanical_energy_power_sim /(eta_pump * 3.61E9)
      return(power_array |> sum() |> round())
    }
    if(CSP_type() == "Linear Fresnel"){
      return(0)
    }
  })


  pump_losses_power_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      power_array <- simulation_output_df_power_sim() |> select("m_salt_in") |>
        pull() * g * tower_height_power_sim() /(eta_pump * 3.6E9)
      return(power_array |> sum() |> round())
    }

    if(CSP_type() == "Linear Fresnel"){
      return(0)
    }
  })


  mass_106_power_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      D_pipe_mean <- (flux_mass_salt_power_sim()/salt_density_cold)/salt_speed
      r_pipe_mean <- D_pipe_mean/2
      design_pressure <- 1E-6 * (201E5 + tower_height_power_sim()*g*salt_density_mean)
      pipe_thickness_106 <- ((design_pressure*1000*r_pipe_mean)/(S_106 - 0.6*design_pressure)+CA_106)/1000
      pipe_volume_106 <- pi * tower_height_power_sim() *((r_pipe_mean+pipe_thickness_106)^(2)-(r_pipe_mean)^(2))
      return(pipe_volume_106*density_106)
    }
    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })


  salt_mass_power_sim <- reactive({
    if(CSP_type() == "Central Tower") {
      return((tank_volume_power_sim()*salt_density_mean/1000) |> round())
    }
    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })


  mass_316_power_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      tank_diameter <- 2 * (tank_volume_power_sim()/(2*pi))^(1/3)
      tank_thickness_power_sim <- (((4.9 * tank_diameter * (tank_diameter - 0.3) * G) / Sd) + Ca) * 1E-3
      tank_radius <- tank_diameter/2
      tank_internal_volume <- (tank_radius)^(2)*tank_diameter*pi
      tank_external_volume <- pi*(tank_radius + tank_thickness_power_sim)^(2)*(tank_diameter+2*tank_thickness_power_sim)
      return(steel_density*(tank_external_volume - tank_internal_volume))
    }
    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })


  steel_mass_power_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return((mass_106_power_sim() + mass_316_power_sim())/1000 |> round())
    }
    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })


  steel_costs_power_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return((1.15*mass_106_power_sim()+4.99*mass_316_power_sim()) |> round())
    }
    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })


  total_electrical_energy_power_sim <- reactive(tank_electrical_heating_power_sim()+Pump_energy_power_sim())

  total_gas_savings_power_sim <- reactive(((3.6*total_production_power_sim())/(LVH_gas*eta_boiler)) |> round())

  gas_savings_2019_power_sim <- reactive(total_production_power_sim()*price_gas_2019/eta_boiler)

  gas_savings_2022_power_sim <- reactive(total_production_power_sim()*price_gas_2022/eta_boiler)

  CO2_gas_power_sim <- reactive(LVH_gas*CO2_eq_gas*total_gas_savings_power_sim()/3.6)

  CO2_electricity_power_sim <- reactive(CO2_eq_electricity*total_electrical_energy_power_sim())

  CO2_balance_power_sim <- reactive(- CO2_gas_power_sim() + CO2_electricity_power_sim())

  electricity_2019_power_sim <- reactive(total_electrical_energy_power_sim()*electricity_price_2019)

  electricity_2022_power_sim <- reactive(total_electrical_energy_power_sim()*electricity_price_2022)


  output$summary_box_eco_analysis_power <- renderUI({
    if(input$Run_power_sim){
      if(CSP_type() == "Central Tower"){
        box(
          title = span( icon("circle-info", verify_fa = FALSE), "  Energy and dimensioning summary"),
          status = "warning",
          solidHeader = TRUE,
          align="center",
          width = 16,
          h3(paste("Total production : ", total_production_power_sim() |> round(), " MWht")),
          h3("Tank heating energy :" , tank_electrical_heating_power_sim(), " MWhe"),
          h3("Pumping energy : " , Pump_energy_power_sim(), " MWhe"),
          h3("Total electricity consumption : " , total_electrical_energy_power_sim(), " MWhe"),
          h3("Pipes and tanks steel mass : " , steel_mass_power_sim() |> round(), " tons"),
          h3("Salt mass : ", salt_mass_power_sim(), "tons")
        )
      }
      else{
        box(
          title = span( icon("circle-info", verify_fa = FALSE), "  Energy and dimensioning summary"),
          status = "warning",
          solidHeader = TRUE,
          align="center",
          width = 16,
          h3(paste("Total production : ", total_production_power_sim() |> round(), " MWht")),
          h3("Tank heating energy :" , tank_electrical_heating_power_sim(), " MWhe"),
          h3("Total electricity consumption : " , total_electrical_energy_power_sim(), " MWhe")
        )
      }
    }
    else{
      code("You must first run the POWER simulation in the CSP simulation : surface
        calculation tab in order to get the economical analysis", color = "red")
    }
  }
  )

  output$Energy_equivalents_box_eco_analysis_power <- renderUI({
    if(input$Run_power_sim){
      box(
        title = span( icon("circle-info", verify_fa = FALSE), " Energy and dimensioning equivalents"),
        status = "warning",
        solidHeader = TRUE,
        align="center",
        width = 16,
        h3("Gas replaced : " , total_gas_savings_power_sim() |> round(), " tons"),
        h3("Gas savings based on 2019 price : " , gas_savings_2019_power_sim() |> round(), " €"),
        h3("Gas savings based on 2022 price : " , gas_savings_2022_power_sim() |> round(), " €"),
        h3("Electricity spendings based on 2019 price :", electricity_2019_power_sim() |> round()," €"),
        h3("Electricity spendings based on 2022 price :", electricity_2022_power_sim() |> round()," €"),
        h3("Carbon dioxide or equivalents reduction : ", CO2_gas_power_sim() |> round(), " tons"),
        h3("Carbon dioxide or equivalents emissions from electricity : ", CO2_electricity_power_sim() |> round(), " tons"),
        h3("Carbon dioxide balance : ", CO2_balance_power_sim() |> round(), " tons")
      )
    }
    else{
      code("You must first run the POWER simulation in the CSP simulation : surface
        calculation tab in order to get the economical analysis", color = "red")
    }
  })

  evaporator_costs_power_sim <- reactive(((18000*nominal_power()/(1000*1.05)) |> round()))

  tower_receiver_costs_power_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return(285*nominal_power()/1.05 |> round())
    }
    if(CSP_type() == "Linear Fresnel"){
      return(NULL)
    }
  })

  heliostat_field_costs_power_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return((aperture_surface_power_sim()*140/1.05) |> round())
    }
    if(CSP_type() == "Linear Fresnel"){
      return((aperture_surface_power_sim()*126))
    }
  })

  total_capex_Chris_power_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return(((1.04*(steel_costs_power_sim() + evaporator_costs_power_sim() + tower_receiver_costs_power_sim() + heliostat_field_costs_power_sim())) + salt_mass_power_sim()*1600) |> round())
    }
  })

  storage_costs_power_sim <- reactive((22*((tank_volume_power_sim()*cp_salt*salt_density_mean*Delta_T)/(3600*1.05))) |> round())


  total_capex_power_sim <- reactive({
    if(CSP_type() == "Central Tower"){
      return((1.04*(22*((tank_volume_power_sim()*cp_salt*salt_density_mean*Delta_T)/(3600*1.05)) + evaporator_costs_power_sim() + tower_receiver_costs_power_sim() + heliostat_field_costs_power_sim())))
    }
    if(CSP_type() == "Linear Fresnel"){
      return(1.04*(heliostat_field_costs_power_sim() + evaporator_costs_power_sim() + 22*(tank_volume_power_sim()*cp_salt*salt_density_mean*Delta_T)/(3600*1.05)))
    }
  })

  electricity_average_power_sim <- reactive((electricity_2019_power_sim() + electricity_2022_power_sim())/2)

  costs_kw_power_sim <- reactive((total_capex_power_sim()/nominal_power_power_sim()) |> round(2))

  contingency_power_sim <- 4

  opex_power_sim <- reactive({(0.02*total_capex_power_sim()) |> round()})

  Costs_kwh_10_years_power_sim <- reactive(((total_capex_power_sim()+10*opex_power_sim()+10*electricity_average_power_sim())/(10*(total_production_power_sim()))) |> round(2))

  Costs_kwh_20_years_power_sim <- reactive(((total_capex_power_sim()+20*opex_power_sim()+20*electricity_average_power_sim())/(20*(total_production_power_sim()))) |> round(2))

  Costs_MWh_10_years_gas_2019_power_sim <- reactive(
    ((total_capex_power_sim()+10*opex_power_sim()+10*electricity_average_power_sim()+10*(price_gas_2019*(brewery_en_y_heatp() - total_production_power_sim())))/(10*brewery_en_y_heatp())) |> round(2))

  Costs_MWh_20_years_gas_2019_power_sim <- reactive(
    ((total_capex_power_sim()+20*opex_power_sim()+20*electricity_average_power_sim()+20*(price_gas_2019*(brewery_en_y_heatp() - total_production_power_sim())))/(20*brewery_en_y_heatp())) |> round(2))

  Costs_MWh_10_years_gas_2022_power_sim <- reactive(
    ((total_capex_power_sim()+10*opex_power_sim()+10*electricity_average_power_sim()+10*(price_gas_2022*(brewery_en_y_heatp() - total_production_power_sim())))/(10*brewery_en_y_heatp())) |> round(2))

  Costs_MWh_20_years_gas_2022_power_sim <- reactive(
    ((total_capex_power_sim()+20*opex_power_sim()+20*electricity_average_power_sim()+20*(price_gas_2022*(brewery_en_y_heatp() - total_production_power_sim())))/(20*brewery_en_y_heatp())) |> round(2))

  output$System_costs_components_prices_power <- renderUI({
    if(input$Run_power_sim){
      if(CSP_type() == "Central Tower"){
        box(
          title = span( icon("circle-info", verify_fa = FALSE), " System costs using components prices"),
          status = "warning",
          solidHeader = TRUE,
          align="center",
          width = 16,
          #h3("Tanks and pipes steel : " , steel_costs_surface_sim()/1.05 |> round(), " €"),
          #h3("Salt : " , (salt_mass_surface_sim()*1600) |> round(), " €"),
          h3("Evaporator : ", evaporator_costs_power_sim() |> round(), " €"),
          h3("Electricity spendings based on 2019 and 2022 average price : ", electricity_average_power_sim() |> round(), " €"),
          h3("Tower and receiver : ", tower_receiver_costs_power_sim() |> round(), " €"),
          h3("Heliostat field: ", heliostat_field_costs_power_sim() |> round(), "€"),
          h3("Thermal storage: ", storage_costs_power_sim() |> round(), " €"),
          h3("Total Capex : ",total_capex_power_sim() |> round(), " €"),
          #h3("Total Capex using components prices: ", total_capex_Chris_surface_sim(), " €"),
          h3("using ", contingency_power_sim,"% of contingency"),
          h3("Total Opex based on 2% of Capex: ", opex_power_sim() |> round(), " €"),
          h3("Costs per MWh produced on 10 years : ", Costs_kwh_10_years_power_sim(), " €/MWh"),
          h3("Costs per MWh produced on 20 years : ", Costs_kwh_20_years_power_sim(), " €/MWh"),
          h3("Costs per MWh consumed on 10 years with natural gas 2019: ", Costs_MWh_10_years_gas_2019_power_sim() |> round(2), " €/MWh"),
          h3("Costs per MWh consumed on 10 years with natural gas 2022: ", Costs_MWh_10_years_gas_2022_power_sim() |> round(2), " €/MWh"),
          h3("Costs per MWh consumed on 20 years with natural gas 2019: ", Costs_MWh_20_years_gas_2019_power_sim() |> round(2), " €/MWh"),
          h3("Costs per MWh consumed on 20 years with natural gas 2022: ", Costs_MWh_20_years_gas_2022_power_sim() |> round(2), " €/MWh")
        )
      }
      else{
        box(
          title = span( icon("circle-info", verify_fa = FALSE), " System costs using components prices"),
          status = "warning",
          solidHeader = TRUE,
          align="center",
          width = 16,
          h3("Evaporator : ", evaporator_costs_power_sim()/1.05 |> round(), " €"),
          h3("Electricity spendings based on 2019 and 2022 average price : ", electricity_average_power_sim() |> round(), " €"),
          h3("Thermal storage: ", storage_costs_power_sim() |> round(), " €"),
          h3("Heliostat field and receiver: ", heliostat_field_costs_power_sim() |> round(), "€"),
          h3("Total capex : ",total_capex_power_sim() |> round(), " €"),
          h3("using ", contingency_power_sim,"% of contingency"),
          h3("Total Opex based on 2% of Capex: ", opex_power_sim() |> round(), " €"),
          h3("Costs per MWh produced on 10 years : ", Costs_kwh_10_years_power_sim(), " €/MWh"),
          h3("Costs per MWh produced on 20 years : ", Costs_kwh_20_years_power_sim(), " €/MWh"),
          h3("Costs per MWh consumed on 10 years with natural gas 2019: ", Costs_MWh_10_years_gas_2019_power_sim() |> round(2), " €/MWh"),
          h3("Costs per MWh consumed on 10 years with natural gas 2022: ", Costs_MWh_10_years_gas_2022_power_sim() |> round(2), " €/MWh"),
          h3("Costs per MWh consumed on 20 years with natural gas 2019: ", Costs_MWh_20_years_gas_2019_power_sim() |> round(2), " €/MWh"),
          h3("Costs per MWh consumed on 20 years with natural gas 2022: ", Costs_MWh_20_years_gas_2022_power_sim() |> round(2), " €/MWh")
        )
      }
    }
    else{
      code("You must first run the POWER simulation in the CSP simulation : surface
        calculation tab in order to get the economical analysis", color = "red")
    }
  })



}
