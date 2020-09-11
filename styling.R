

# Define colours for time series plot

clr_allcause <- "#f0027f"
clr_confirmed <- "#386cb0"
clr_tpr <- "#ff7f00"
clr_deaths <- "#a6d854" 
clr_severe <- "#7570b3"
clr_anc <- "#33a02c" 
clr_covid <- "#a65628"

clrset <-  c(clr_allcause, clr_confirmed, clr_tpr, clr_severe,
             clr_deaths, clr_anc, clr_covid)

clr_values <- c("2020" = "#e41a1c", "Other Years" = "#e41a1c", "Historical Mean" = "#e41a1c",
                "COVID-19" = clr_covid, "COVID-19 Forecast" = clr_covid, 
                "Report Rate" = "#ff7f00")

alpha_values <- c("2020" = 1, "Other Years" = 0.4, "Historical Mean" = 1,
                  "COVID-19" = 1, "COVID-19 Forecast" = 1, 
                  "Report Rate" = 1)

shape_values <- c("2020" = 19, "Other Years" = 46, "Historical Mean" = 46,
                  "COVID-19" = 16, "COVID-19 Forecast" = 1, 
                  "Report Rate" = 19)

size_values <- c("2020" = 1.5, "Other Years" = 0.0001, "Historical Mean" = 0.0001,
                 "COVID-19" = 1.5, "COVID-19 Forecast" = 1.5, 
                 "Report Rate" = 1.5)

linetype_values <- c("2020" = "solid", "Other Years" = "solid", "Historical Mean" = "dotted",
                     "COVID-19" = "solid", "COVID-19 Forecast" = "solid", 
                     "Report Rate" = "solid")
