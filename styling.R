# Define colours for time series plot

clr_allcause <- "#f0027f"
clr_confirmed <- "#386cb0"
clr_tpr <- "#ff7f00"
clr_deaths <- "#a6d854" 
clr_severe <- "#7570b3"
clr_anc <- "#33a02c" 
clr_covid <- "#a65628"
clr_report <- "#b2abd2" #ff7f00

clrset <-  c(clr_allcause, clr_confirmed, clr_tpr, clr_severe,
             clr_deaths, clr_anc, clr_covid)

clr_values <- c("2020" = "#e41a1c", "Other Years" = "#e41a1c", "Historical Mean" = "#e41a1c",
                "COVID-19" = clr_covid, "COVID-19 Forecast" = clr_covid, 
                "Report Rate" = clr_report)

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


grf_clr <- c("Rainfall (2020)" = "#3288bd", 
            "Rainfall (2019)" = "#99d594", 
            "Rainfall (Other Years)" = "grey40",
            "Accumulated Rainfall (2018/2019)" = "#99d594",
            "Accumulated Rainfall (2019/2020)" = "#3288bd",
            "Accumulated Rainfall (2020/2021)" = "#762a83",
            "Accumulated Rainfall (2019)" = "#33a02c",
            "Accumulated Rainfall (2020)" = "#3288bd", 
            "Accumulated Rainfall (Other Years)" = "grey40",
            "Rainfall Historical Mean" = "black", 
            "Selected Indicator (2020)" = "#e41a1c",
            "Selected Indicator (2019/2020)" = "#e41a1c",
            "Selected Indicator (2019)" = "#ff7f00",
            "Selected Indicator (2018/2019)" = "#ff7f00",
            "Selected Indicator (2020/2021)" = "#762a83")


# grf_clr <- c("Rainfall (2020)" = "#3288bd", 
#              "Rainfall (2019)" = "#99d594", 
#              "Rainfall (Other Years)" = "grey40",
#              "Accumulated Rainfall (2018/2019)" = "#99d594",
#              "Accumulated Rainfall (2019/2020)" = "#3288bd",
#              "Accumulated Rainfall (2020/2021)" = "#762a83",
#              "Accumulated Rainfall (2019)" = "#33a02c",
#              "Accumulated Rainfall (2020)" = "#3288bd", 
#              "Accumulated Rainfall (Other Years)" = "grey40",
#              "Rainfall Historical Mean" = "black", 
#              "Selected Indicator (2020)" = "#3288bd",
#              "Selected Indicator (2019/2020)" = "#3288bd",
#              "Selected Indicator (2019)" = "#99d594",
#              "Selected Indicator (2018/2019)" = "#99d594",
#              "Selected Indicator (2020/2021)" = "#762a83")

grf_alpha <- c("Rainfall (2020)" = 1, 
              "Rainfall (Other Years)" = 0.2,
              "Rainfall (2019)" = 1, 
              "Accumulated Rainfall (2018/2019)" = 1,
              "Accumulated Rainfall (2019/2020)" = 1,
              "Accumulated Rainfall (2020/2021)" = 1,
              "Accumulated Rainfall (2020)" = 1, 
              "Accumulated Rainfall (2019)" = 1,
              "Accumulated Rainfall (Other Years)" = 0.2,
              "Rainfall Historical Mean" = 1, 
              "Selected Indicator (2020)" = 1,
              "Selected Indicator (2019)" = 1,
              "Selected Indicator (2018/2019)"  = 1,
              "Selected Indicator (2019/2020)" = 1,
              "Selected Indicator (2020/2021)" = 1)

grf_linetype <- c("Rainfall (2020)" = "solid", 
                 "Rainfall (Other Years)" = "solid",
                 "Rainfall (2019)" = "solid", 
                 "Accumulated Rainfall (2018/2019)" = "solid",
                 "Accumulated Rainfall (2019/2020)" = "solid",
                 "Accumulated Rainfall (2020/2021)" = "solid",
                 "Accumulated Rainfall (2020)" = "solid", 
                 "Accumulated Rainfall (2019)" = "solid",
                 "Accumulated Rainfall (Other Years)" = "solid",
                 "Rainfall Historical Mean" = "dotted", 
                 "Selected Indicator (2020)" = "solid",
                 "Selected Indicator (2019)" = "solid",
                 "Selected Indicator (2018/2019)"  = "solid",
                 "Selected Indicator (2019/2020)" = "solid",
                 "Selected Indicator (2020/2021)" = "solid")


grf_shape <- c("Rainfall (2020)" = 19, 
                 "Rainfall (Other Years)" = 46,
                 "Rainfall (2019)" = 17, 
                 "Accumulated Rainfall (2018/2019)" = 17,
                 "Accumulated Rainfall (2019/2020)" = 19,
                 "Accumulated Rainfall (2020/2021)" = 8,
                 "Accumulated Rainfall (2020)" = 19, 
                 "Accumulated Rainfall (2019)" = 17,
                 "Accumulated Rainfall (Other Years)" = 46,
                 "Rainfall Historical Mean" = 46, 
                 "Selected Indicator (2020)" = 19,
                 "Selected Indicator (2019)" = 17,
                 "Selected Indicator (2018/2019)"  = 17,
                 "Selected Indicator (2019/2020)" = 19,
                 "Selected Indicator (2020/2021)" = 8)


grf_size <- c("Rainfall (2020)" = 2, 
              "Rainfall (Other Years)" = 0.01,
              "Rainfall (2019)" = 2, 
              "Accumulated Rainfall (2018/2019)" = 2,
              "Accumulated Rainfall (2019/2020)" = 2,
              "Accumulated Rainfall (2020/2021)" = 2,
              "Accumulated Rainfall (2020)" = 2, 
              "Accumulated Rainfall (2019)" = 2,
              "Accumulated Rainfall (Other Years)" = 0.01,
              "Rainfall Historical Mean" = 0.01, 
              "Selected Indicator (2020)" = 2,
              "Selected Indicator (2019)" = 2,
              "Selected Indicator (2018/2019)"  = 2,
              "Selected Indicator (2019/2020)" = 2,
              "Selected Indicator (2020/2021)" = 2)


# delta_map_clr <- c(rev(c("#fec44f", "#fe9929", "#ec7014", "#993404")),
#                 c("#ef3b2c", "#cb181d", "#bd0026", "#800026"))

# delta_map_clr <- rev(c("#542788", "#8073ac", "#b2abd2", "#d8daeb",
#                    "#ef3b2c", "#cb181d", "#bd0026", "#800026"))

delta_map_clr <- rev(c("#542788", "#8073ac", "#b2abd2", "#d8daeb",
                       "#fee5d9", "#fcae91", "#fb6a4a", "#de2d26"))

# snap_bar_clr <- c("negative" = "#bd0026", "positive" = "#8073ac")

snap_bar_clr <- c("negative" = "#de2d26", "positive" = "#b2abd2")


