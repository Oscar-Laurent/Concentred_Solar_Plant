library(dplyr)
library(deSolve)



# CSP SIMULATION FUNCTION -------------------------------------------------



CSP_simulation <- function(DNI_df, OF_df, aperture_surface=1000, storage_volume=100,
                           country, firm_capacity=NULL, by = "day",solar_multiple){

  eta_heliostat = 0.6  # rendement du champ de pannaux
  eta_receiver = 0.9  # rendement du recepteur
  eta_exanger = 0.995  # rendement de l'échangeur
  surcapacity_factor = 0.3
  cp_salt <- 1.56e3  # J/kg.K
  T_salt_in <- 300  # C°
  T_salt_out <- 200  # C°
  Delta_T <- abs(T_salt_out - T_salt_in)
  salt_density_mean <- 1895  # kg/m3
  V_max_hot_tank <- storage_volume   # m3
  V_max_cold_tank <- storage_volume  # m3
  fresnel_thermal_heat_efficiency <- 0.9  # Pertes thermiques pour le fresnel


  if (by == "day"){
    df <- DNI_df |> select(time, country)
    days_df <- df |> group_by("day_of_year" = lubridate::yday(time)) |> summarise_at(names(df)[-1], list(sum))
    days_df[, "OF"] <- OF_df[lubridate::month(as.Date(days_df$day_of_year, origin = "1999-12-31")), country]
    days_df[,"Q_solar"] <- days_df |> select(country) * aperture_surface * days_df$OF   # [W]
    days_df[,'m_salt_in'] <- days_df$Q_solar *60*60*24 / (cp_salt*Delta_T)  # [kg/j]
    days_df$m_salt_out <- firm_capacity *60*60*24 / (Delta_T*cp_salt * 0.995) # [kg/j]
    parms <- c(max_flux = firm_capacity *60*60*24 / (cp_salt*Delta_T*0.995*salt_density_mean))  # [m^3/j]

    times <- days_df$day_of_year
    input <- data.frame(times = c(1:nrow(days_df)), m_salt_in = days_df$m_salt_in, m_salt_out = days_df$m_salt_out )
  }


  if (by == "hour"){
    df <- DNI_df |> select(time, country)
    df$time <- df$time |> format("%d-%m %H:%m") |> as.POSIXct(format="%d-%m %H:%m")
    #df <- df |> group_by(time) |> summarise_at(names(df)[-1], list(mean))
    df[, "OF"] <- OF_df[lubridate::month(as.Date(df$time, origin = "1999-12-31")), country]
    df[,"Q_solar"] <- df |> select(country) * aperture_surface * df$OF   # [W]
    df[,'m_salt_in'] <- df$Q_solar *60*60 / (cp_salt*Delta_T)  # [kg/h]
    df$m_salt_out <- firm_capacity *60*60 / (Delta_T*cp_salt * 0.995) # [kg/h]
    parms <- c(max_flux = firm_capacity *60*60 / (cp_salt*Delta_T*0.995*salt_density_mean))  # [m^3/h]

    times <- c(1:nrow(df))
    input <- data.frame(times = c(1:nrow(df)), m_salt_in = df$m_salt_in, m_salt_out = df$m_salt_out )
  }


  # ODE RESOLUTION ----------------------------------------------------------


  m_salt_in <- approxfun(x = input$time, y = input$m_salt_in, rule = 2)
  m_salt_out <- approxfun(x = input$time, y = input$m_salt_out, rule = 2)


  SPCmod <- function(t, x, parms) {
    with(as.list(c(parms, x)), {

      m_salt_in <-ifelse(((m_salt_in(t)/salt_density_mean)+V_hot_tank - max_flux > V_max_hot_tank + 1.0*max_flux),(V_max_hot_tank-V_hot_tank+ 1.0*max_flux)*salt_density_mean,
                         ifelse(((m_salt_in(t)/salt_density_mean)+V_hot_tank - max_flux >= V_max_hot_tank + max_flux && (m_salt_in(t)/salt_density_mean)+V_hot_tank - max_flux < V_max_hot_tank + 1.0*max_flux),(V_max_hot_tank - V_hot_tank + max_flux)*salt_density_mean,m_salt_in(t)))
      m_salt_in <- ifelse(m_salt_in>= (solar_multiple*max_flux*salt_density_mean), solar_multiple*max_flux*salt_density_mean, m_salt_in) #ecretage



      m_salt_out <- ifelse(V_hot_tank + (m_salt_in(t)/salt_density_mean) < max_flux && V_hot_tank + (m_salt_in(t)/salt_density_mean) >= 0.2* max_flux, m_salt_in(t) + V_hot_tank*salt_density_mean,
                           ifelse(((m_salt_in(t)/salt_density_mean)+V_hot_tank - max_flux > V_max_hot_tank + 1.0*max_flux), 1.0*max_flux*salt_density_mean,
                                  ifelse(V_hot_tank + (m_salt_in(t)/salt_density_mean) >= max_flux, max_flux*salt_density_mean, 0)))


      dV_hot_tank <- ifelse(V_hot_tank >= V_max_hot_tank & m_salt_in-m_salt_out >= 0, 0,
                            ifelse(V_hot_tank <= 0 & m_salt_in-m_salt_out <= 0, 0,
                                   ifelse(V_hot_tank + (m_salt_in-m_salt_out)/salt_density_mean > V_max_hot_tank, V_max_hot_tank-V_hot_tank,
                                          (m_salt_in-m_salt_out)/salt_density_mean)))

      dV_cold_tank <- -dV_hot_tank

      res <- c(dV_hot_tank, dV_cold_tank)

      list(res, m_salt_in = m_salt_in, m_salt_out = m_salt_out)
    })
  }


  xstart <- c(V_hot_tank = 0, V_cold_tank =  V_max_cold_tank)

  out <- ode(y = xstart, times = times, func = SPCmod, parms, hmax= 1, maxsteps = 50000,  method = "rk4")

  return(out)

}



# PLOT FUNCTION -----------------------------------------------------------


plot_out_simulation <- function(input){
  cp_salt <- 1560  # J/kg.K
  T_salt_in <- 300  # C°
  T_salt_out <- 200  # C°
  Delta_t <- abs(T_salt_out - T_salt_in)
  eta_exchanger = 0.995
  power_to_HTF <- reactive({input[,"m_salt_in"]*cp_salt*Delta_t*1e-6/3600
  })
  power_to_steam <- reactive({(input[,"m_salt_out"]*cp_salt*Delta_t*eta_exchanger*1e-6/3600)
  })
  fig1 <- plot_ly(x = input[,"time"], y = input[,"V_hot_tank"], type = 'scatter', mode = 'lines',
                  line = list(width = 0.8), name = "Hot tank Volume",
                  hovertemplate = "Hour of year : %{x}<br>Hot Tank Volume: %{y:.2f} m^3 <extra></extra>") |>
    layout(yaxis = list(title = 'Volume [m^3]'))

  fig2 <- plot_ly(x = input[,"time"], y = input[,"V_cold_tank"], type = 'scatter', mode = 'lines',
                  line = list(width = 0.8), name = "Cold tank Volume",
                  hovertemplate = "Hour of year : %{x}<br>Cold Tank Volume: %{y:.2f} m^3 <extra></extra>")

  fig3 <- plot_ly(x = input[,"time"], y = input[,"m_salt_in"], type = 'scatter', mode = 'lines',
                  line = list(width = 0.8), name = "Salt flux to Hot Tank",
                  hovertemplate = "Hour of year : %{x}<br>Salt in flux: %{y:.2f} kg/h <extra></extra>") |>
    layout(yaxis = list(title = "Flux [kg/h]"))

  fig4 <- plot_ly(x = input[,"time"], y = input[,"m_salt_out"], type = 'scatter', mode = 'lines',
                  line = list(width = 0.8), name = "Salt flux to Evaporator",
                  hovertemplate = "Hour of year : %{x}<br>Salt out flux: %{y:.2f} kg/h <extra></extra>")

  fig5 <- plot_ly(x = input[,"time"], y = power_to_HTF(), type = 'scatter', mode = 'lines',
                  line = list(width = 0.8), name = "Power to HTF",
                  hovertemplate = "Hour of year : %{x}<br>Power to HTF: %{y:.2f} MW <extra></extra>") |>
    layout(yaxis = list(title = "Power (Mw)"))

  fig6 <- plot_ly(x = input[,"time"], y = power_to_steam(), type = 'scatter', mode = 'lines',
                  line = list(width = 0.8), name = "Power to steam",
                  hovertemplate = "Hour of year : %{x}<br>Power to steam: %{y:.2f} MW <extra></extra>")

  subfigure1 <- subplot(fig1, fig2, shareY = TRUE ) |>
    layout(
      annotations = list(
        list(x = 0.15 , y = 1.1, text = "Hot tank volume", showarrow = F, xref='paper', yref='paper', font = list(size=20)),
        list(x = 0.85 , y = 1.1, text = "Cold tank volume", showarrow = F, xref='paper', yref='paper', font = list(size=20))
      )
    )

  subfigure2 <- subplot(fig3, fig4, shareY = TRUE) |>
    layout(
      annotations = list(
        list(x = 0.15 , y = 1.1, text = "Salt flux to Hot Tank", showarrow = F, xref='paper', yref='paper' , font = list(size=20)),
        list(x = 0.85 , y = 1.1, text = "Salt flux to Evaporator", showarrow = F, xref='paper', yref='paper' , font = list(size=20))
      )
    )
  subfigure3 <- subplot(fig5, fig6, shareY = TRUE ) |>
    layout(
      annotations = list(
        list(x = 0.15 , y = 1, text = "Power to HTF", showarrow = F, xref='paper', yref='paper' , font = list(size=20)),
        list(x = 0.85 , y = 1, text = "Power to steam", showarrow = F, xref='paper', yref='paper' , font = list(size=20))
      )
    )

  subplot(subfigure1, subfigure2, subfigure3, shareX = FALSE, nrows = 3, margin = 0.04, titleY = T) |>
    layout(showlegend=F, showlegend2 = F, plot_bgcolor='#e5ecf6',
           annotations = list(x = 0.5 , y = -0.06, text = "Hour of year", showarrow = F,
                              xref='paper', yref='paper', font = list(size=15)))
}

stacked_plot <- function(df1, df2, comparaison = FALSE){
  df_pivot1 <- df1 |> select(day, power_to_HTF, Q_lost, Q_solar) |>
    pivot_longer(c(Q_lost, power_to_HTF), names_to = "group", values_to = "Energy")

  df_pivot1$group <- factor(df_pivot1$group , levels=c("Q_lost", "power_to_HTF"))
  df_pivot1$day  <- df_pivot1$day |> as.Date(origin = "1999-12-31")

  p1 <- df_pivot1 |>
    ggplot(aes(x=day)) +
    geom_area(aes(y=Energy, fill=group)) +
    scale_x_date(date_breaks = "month", date_labels = "%b") +
    labs(title = "Evolution of the daily Energy to HTF and lost power", y = "Energy [kWh]") +
    scale_fill_manual(values = c("#c8635d", "#5dc863"))


  if(comparaison){
    p1 <- p1 + geom_area(aes(y=Q_solar , fill = "Total Energy Without Defocalisation"), alpha = 0.4) +
      scale_fill_manual(values = c( "#5dc863","#c8635d", "#635dc8"))
  }

  p1 <- ggplotly(p1, tooltip = "group")


  df_pivot2 <- df2 |> select(time, V_hot_tank, V_cold_tank) |>
    pivot_longer(!time , names_to = "group", values_to = "Volume")

  df_pivot2$time  <- df_pivot2$time |> as.Date(origin = "1999-12-31")
  df_pivot2$group <- factor(df_pivot2$group , levels=c("V_hot_tank", "V_cold_tank"))

  p2 <- df_pivot2 |>
    ggplot( aes(x=time, y=Volume, fill=group, text = group)) +
    geom_area() +
    scale_x_date(date_breaks = "month", date_labels = "%b") +
    labs(y = "Volume m^3")
  p2 <- ggplotly(p2, tooltip = "group")

  subfigure <- subplot(p1, p2, shareX = F, nrows = 2, margin = 0.04, titleY = T) |>
    layout(autosize = T, height = 700)
  return(subfigure)
}


bar_plot <- function(input_df, comparaison) {
  eta_exanger = 0.995
  df_pivot <- input_df |> select(time, power_to_HTF , Q_lost, Q_solar) |>
    mutate(Q_solar = Q_solar/2) |>
    pivot_longer(c(Q_lost, power_to_HTF) , names_to = "group", values_to = "Energy")

  df_pivot$group <- factor(df_pivot$group , levels=c("Q_lost", "power_to_HTF"))

  p <- ggplot(df_pivot, aes(x = time, y = Energy)) + geom_bar(stat = "identity", aes(fill = group)) +
    labs(title = "Loss power through defocalisation VS Power to HTF", y = "Energy [kW]")

  if(comparaison){
    p <- p + geom_bar(stat = "identity", aes(y=Q_solar, fill = "Total Energy Without Defocalisation "), alpha = 0.4)
  }

  return(ggplotly(p))
}


