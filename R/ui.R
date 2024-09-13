library(shinydashboard)
require(plotly)
ui <- dashboardPage(skin="yellow",


                    dashboardHeader(title = "CSP Simulation"),


                    dashboardSidebar(
                      width = 300,
                      sidebarMenu(

                        menuItem("Data loading", tabName = "weather_data", icon = icon("folder-open" , verify_fa = FALSE),
                                 uiOutput("columns")),

                        menuItem("CSP Options", tabName = "CSP_options", icon = icon("dashboard" , verify_fa = FALSE),
                                 radioButtons("design_month", label = "Select the design day",
                                              choices = list("Winter Slostice" = "01", "Equinox" = "09", "Summer Solstice" = "06"),
                                              selected = "09"),
                                 br(),
                                 #radioButtons("CSP_type", label = "Select the type of CSP",
                                 #             choices = list("Central Tower" , "Linera Fresnel" ),
                                 #             selected = "Central Tower"),
                                 sliderInput("autonomy_time", "Select the autonomy desired [h]",
                                             min = 1, max = 72, value = 1),
                                 br(),
                                 sliderInput("volume_sufactor", label = "Select the storage volume multiplier",
                                             min = 0.5, max = 2, value = 1, step = 0.1),
                                 sliderInput("area_multiplier", label = "Select the aperture and total area multipler (not applicable for constrained area (i.e. Power calculation))",
                                             min = 0.5, max = 2, value = 1, step = 0.1)
                        )
                      )
                    ),


                    dashboardBody(

                      tags$head(tags$script(type = "text/x-mathjax-config",
                                            'MathJax.Hub.Config({
                          "HTML-CSS": { linebreaks: { automatic: true } },
                           SVG: { linebreaks: { automatic: true } }});'
                      )
                      ),

                      tabsetPanel(type = "pills",
                                  tabPanel("Weather summary",
                                           br(),
                                           fluidRow(
                                             box( status = "primary",
                                                  plotOutput("plot_DNI")),

                                             box( status = "primary",
                                                  plotOutput("plot_average_DNI"))
                                           )
                                  ),

                                  tabPanel("CSP Simulation : Area calculation",
                                           br(),
                                           fluidRow(
                                             box(
                                               title = span( icon("list", verify_fa = FALSE), " Simulation options"),
                                               status = "warning",
                                               solidHeader = TRUE,
                                               align="center",
                                               width = 6,
                                               sliderInput("nominal_power", label = "Select the nominal power discharge to steam",
                                                           min = 0, max = 70000, step = 5, post = " KW",
                                                           value = 500),
                                               br(),
                                               actionButton("Run_surface_sim", "Run Production Simulation", class = "btn-warning"),
                                               br(),
                                               br(),
                                               downloadButton("downloadData_surface_sim", "Download hourly power transfer to steam in kw (csv)")
                                             ),
                                             uiOutput("summary_box_surface_sim"),
                                           ),

                                           br(),
                                           column(width = 8,
                                                  uiOutput("aperure_surface_box")
                                           ),
                                           column(width = 4,
                                                  uiOutput("production_surface_box")
                                           ),
                                           br(),
                                           uiOutput("production_ui"),
                                           fluidRow(
                                             tabBox(
                                               width = 12,
                                               side = "left",
                                               tabPanel(span( icon("chart-simple", verify_fa = FALSE), "Simulation plots"),
                                                        plotlyOutput("plot_sim_surface", height = 700)),
                                               tabPanel(span( icon("border-all", verify_fa = FALSE), "Heat map"),
                                                        plotlyOutput("heatmap_sim_surface", height = 700)),
                                               tabPanel(span( icon("border-all", verify_fa = FALSE), "Efficiency plot"),
                                                        plotlyOutput("efficiency_plot_surface_sim"), height = 700),
                                               tabPanel(span( icon("border-all", verify_fa = FALSE), "Stacked plot"),
                                                        checkboxInput("comparaison_surface_sim", "Display the Total Solar Power Reacive",
                                                                      value = FALSE, width = NULL),
                                                        plotlyOutput("stacked_plot_surface_sim"), height = 700),
                                               tabPanel(span( icon("border-all", verify_fa = FALSE), "Bar plot"),
                                                        checkboxInput("comparaison_bar_plot_surface_sim", "Display the Total Solar Power Reacive",
                                                                      value = FALSE, width = NULL),
                                                        plotlyOutput("bar_plot_surface_sim"), height = 700),
                                             height = 800)
                                           )
                                  ),

                                  tabPanel("CSP Simulation : Power calculation",
                                           br(),
                                           fluidRow(
                                             box(
                                               title = span( icon("list", verify_fa = FALSE), " Simulation options"),
                                               status = "warning",
                                               solidHeader = TRUE,
                                               align="center",
                                               width = 6,
                                               sliderInput("total_surface", label = "Select the total available area",
                                                           min = 0, max = 500000, step = 100, post = " m²",
                                                           value = 1000),
                                               br(),
                                               actionButton("Run_power_sim", "Run Production Simulation", class = "btn-warning"),
                                               br(),
                                               br(),
                                               downloadButton("downloadData_power_sim", "Download hourly power transfer to steam in kw (csv)")
                                             ),
                                             uiOutput("summary_box_power_sim")
                                           ),
                                           column(width = 8,
                                                  uiOutput("nominal_power_box")
                                           ),
                                           column(width = 4,
                                                  uiOutput("production_power_box")
                                           ),
                                           fluidRow(
                                             tabBox(
                                               width = 12,
                                               side = "left",
                                               tabPanel(span( icon("chart-simple", verify_fa = FALSE), " Simulation plots"),
                                                        plotlyOutput("plot_power_sim", height = 700)),
                                               tabPanel(span( icon("border-all", verify_fa = FALSE), " Heat map"),
                                                        plotlyOutput("heatmap_power_sim", height = 700)),
                                               tabPanel(span( icon("border-all", verify_fa = FALSE), " Efficiency plot"),
                                                        plotlyOutput("efficiency_plot_power_sim"), height = 700),
                                               tabPanel(span( icon("chart-simple", verify_fa = FALSE), "Stacked plot"),
                                                        checkboxInput("comparaison_power_sim", "Display the Total Solar Power Reacive",
                                                                      value = FALSE, width = NULL),
                                                        plotlyOutput("stacked_plot_power_sim"), height = 700),
                                               tabPanel(span( icon("border-all", verify_fa = FALSE), "Bar plot"),
                                                        checkboxInput("comparaison_bar_plot_power_sim", "Display the Total Solar Power Reacive",
                                                                      value = FALSE, width = NULL),
                                                        plotlyOutput("bar_plot_power_sim"), height = 700),
                                               height = 800

                                             )
                                           )
                                  ),

                                  tabPanel("Economical analysis : SURFACE CALCULATION",
                                           br(),
                                           fluidRow(
                                             box(
                                               width = 6,
                                               uiOutput("summary_box_eco_analysis_surface")
                                             ),

                                             box(
                                               width = 6,
                                               uiOutput("Energy_equivalents_box_eco_analysis_surface")
                                             ),
                                           ),
                                           br(),
                                           fluidRow(
                                             box(
                                               width = 6,
                                               uiOutput("System_costs_components_prices_surface")
                                                ),
                                                    )
                                  ),
                                  tabPanel("Economical analysis : POWER CALCULATION",
                                           br(),
                                           fluidRow(
                                             box(
                                               width = 6,
                                               uiOutput("summary_box_eco_analysis_power")
                                             ),

                                             box(
                                               width = 6,
                                               uiOutput("Energy_equivalents_box_eco_analysis_power")
                                             ),
                                           ),
                                           br(),
                                           fluidRow(
                                             box(
                                               width = 6,
                                               uiOutput("System_costs_components_prices_power")
                                             ),
                                           )
                                  )
                      )
                    )




)

