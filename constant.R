
#-------------------------------------------------------------------
# Order country alphabetically or by latitude +
# define country regions
#-------------------------------------------------------------------

ctry_bylat <- c("Niger", "Mali", "Senegal","Burkina Faso", "Guinea", "Benin", "Nigeria", "Ethiopia", 
                 "Sierra Leone", "Ghana", "Cote D'Ivoire", "Liberia", "Cameroon", "Uganda", "Kenya", 
                 "Rwanda", "DR Congo", "Tanzania (Zanzibar)", "Tanzania (Mainland)", "Angola", "Malawi", 
                "Zambia", "Mozambique", "Zimbabwe", "Madagascar", "Myanmar", "Thailand", "Cambodia")

ctry_byaz <- c(rev(ctry_bylat[-c(1:3)]), "Cambodia", "Myanmar", "Thailand")


# northern hemisphere (Uganda and Kenya centroids are above equator but put them in southern 
# hemisphere region ecause their rainy season starts in September just like in southern Africa)
ctry_nhem <- c('Niger', 'Mali', 'Senegal', 'Burkina Faso', 'Guinea', 'Benin', 'Nigeria', 
                'Ethiopia', 'Sierra Leone', 'Ghana', "Cote D'Ivoire", 'Liberia', 'Cameroon', 
                'Myanmar', 'Thailand', 'Cambodia')

# Southern hemisphere
ctry_shem <- c('Uganda', 'Kenya', 'Rwanda', 'DR Congo', 'Tanzania (Zanzibar)', 'Tanzania (Mainland)', 
               'Angola', 'Malawi', 'Zambia', 'Mozambique', 'Zimbabwe', 'Madagascar')

# West Africa: Cameroon + any countries west of it
ctry_wafr <- c('Niger', 'Mali', 'Senegal', 'Burkina Faso', 'Guinea', 'Benin', 'Nigeria', 
               'Sierra Leone', 'Ghana', "Cote D'Ivoire", 'Liberia', 'Cameroon')

# East Africa: Rwanda + any countries east of it
ctry_eafr <- c('Ethiopia', 'Uganda', 'Kenya', 'Rwanda', 'Tanzania (Zanzibar)', 'Tanzania (Mainland)', 
               'Malawi', 'Mozambique', 'Zimbabwe', 'Madagascar')

# Central + Southern Africa: Rwanda + anythin south of it
ctry_csafr <- c('Rwanda', 'DR Congo', 'Tanzania (Zanzibar)', 'Tanzania (Mainland)', 
                'Angola', 'Malawi', 'Zambia', 'Mozambique', 'Zimbabwe', 'Madagascar')

# Southeast Asia
ctry_sea <- c("Myanmar", "Thailand", "Cambodia")



# # Enforce the factor level again so the color/linetype matches
# 
# xctry <- mutate(xctry0,
#                 colourgroup = factor(colourgroup, 
#                                      levels = c("allcause", "confirmed", "tpr", 
#                                                 "severe", "deaths", "anc1_visit", "covid")),
#                 boxgroup = factor(boxgroup, levels = c("All Cause Consultation", "Malaria Confirmed Cases", 
#                                                        "Malaria Test Positivity Rate", "Severe Malaria Cases",
#                                                        "Malaria Deaths", "ANC Visit"))) %>%
#   mutate(line_label = if_else(linegroup %in% "Long-term mean", "Long-term mean",
#                               if_else((colourgroup %in% "covid") & (pointgroup %in% "Forecasts"), 
#                                       paste("COVID-19 Forecasts: ", mygroup, ", ", month.abb[month], " ", year, sep = ""),
#                                       if_else( (colourgroup %in% "covid") & (pointgroup %in% "Observed"), 
#                                                paste("COVID-19 Reported, ", month.abb[month], " ", year, sep = ""),
#                                                paste(month.abb[month], " ", year, sep = "")))),
#          point_label = if_else( (variable %in% "covid_projection") & (pointgroup %in% "Forecasts"), 
#                                 paste("COVID-19 Forecasts: ", mygroup, ", ", month.abb[month], " ", year, sep = ""),
#                                 if_else( (colourgroup %in% "covid") & (pointgroup %in% "Observed"),
#                                          "COVID-19 Reported", paste(month.abb[month], " ", year, sep = ""),
#                                          paste(month.abb[month], " ", year, sep = "")) ) ) %>%
#   filter(!(country %in% c("Tanzania (Mainland)")))
# 
# 
# xctryExc <- xctryExc0 %>%
#   mutate(date = as.Date(as.character(date)),
#          colourgroup = factor(colourgroup, 
#                               levels = c("All Cause Consultation", "Malaria Confirmed Cases", 
#                                          "Malaria Test Positivity Rate", "Severe Malaria Cases",
#                                          "Malaria Deaths", "ANC Visit"))) %>%
#   filter( (date <= as.Date("2020-03-01")) & (date >= as.Date("2020-01-01"))   &  !(country %in% "Tanzania (Mainland)") )
# 
# 
# 
# xprov <- xprov0 %>% 
#   mutate(colourgroup = factor(colourgroup, levels = c("allcause", "confirmed", "tpr", 
#                                                       "severe", "deaths", "anc1_visit", "covid"),
#                               labels = c("All Cause Consultation", "Malaria Confirmed Cases", 
#                                          "Malaria Test Positivity Rate", "Severe Malaria Cases", "Malaria Deaths", 
#                                          "ANC Visit", "COVID-19")),
#          boxgroup = factor(boxgroup, levels = c("All Cause Consultation", "Malaria Confirmed Cases", 
#                                                 "Malaria Test Positivity Rate", "Severe Malaria Cases",
#                                                 "Malaria Deaths", "ANC Visit")),
#          alphagroup = factor(alphagroup, levels = c("2020", "Other Years")),
#          linegroup = factor(linegroup, levels = c( "Long-term mean", "Monthly Data/Forecast")),
#          pointgroup = factor(pointgroup, levels = c("Forecast", "Observed")))
# 
# xprovExc <- xprovExc0 %>%
#   mutate(colourgroup = factor(colourgroup, 
#                               levels = c("All Cause Consultation", "Malaria Confirmed Cases", 
#                                          "Malaria Test Positivity Rate", "Severe Malaria Cases",
#                                          "Malaria Deaths", "ANC Visit"))) %>%
#   filter(country %in% c("Benin", "Guinea"))  %>%
#   mutate(country = factor(country, levels = as.character(unique(country)), labels = paste("Country", 1:length(unique(country)), sep = " ")),
#          admin_level_1 = factor(admin_level_1, levels = as.character(unique(admin_level_1)), labels = paste("Admin1_", length(unique(admin_level_1)), sep = "")))
# 
# 
# 
# # COVID Risk Map ---------------------------------------------------------------
# 
# # s <- readOGR("data/Benin Trimmed/Population_Layer.shp")
# 
# 
# #-------------------------------------------------------------------
# # Rainfall Data 
# # Note that at this time, rf data is only to show the graph in rainfall tab 
# # as a demonstration
# #-------------------------------------------------------------------
# 
# xc4rf <- subset(xctry0, variable %in% "confirmed_rate") %>%
#   mutate(date = as.Date(as.character(date))) %>%
#   select(c(country, date, month, total_population, variable, value)) %>%
#   pivot_wider(names_from = "variable", values_from = "value")
# 
# xrf0 <- read_civis("covid.appdata_rainfall")
# xrf_adm0 <- xrf0 %>%
#   subset(., date < as.Date("2020-07-01")) %>%
#   merge(xc4rf, ., all.x = TRUE, all.y = TRUE) %>%
#   group_by(country) %>%
#   mutate(scaling = max(rf, na.rm = TRUE)/(1.1 * max(confirmed_rate, na.rm = TRUE))) %>%
#   mutate(new_confirmed_rate = (confirmed_rate * scaling) + (0.1*(max(rf, na.rm = TRUE)))) %>%
#   mutate(colour_rf = factor(if_else(year %in% c(2019:2020), paste(year), "Other years"),
#                             levels = c("2020", "2019", "Other years")),
#          colour_mal = paste("Confirmed Malaria (", year, ")", sep =""))
# 





  
